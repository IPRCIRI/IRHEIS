# 000-FunctionDefs.R
# Gather All Function Definitions in one place, use in all codes
#
# Copyright © 2022: Majid Einian
# Licence: GPL-3

library(spatstat)
library(data.table)

trim_to_number <- function(x){as.numeric(str_trim(x))}

DoDeciling <- function(HHDT,PriceIndexDT=NULL
                       ,OrderingVar="Consumption",Size="_per"){
  

  if(!is.null(PriceIndexDT)){
    if("PriceIndex" %in% names(HHDT)) HHDT <- HHDT[,PriceIndex:=NULL]
    HHDT <- merge(HHDT,PriceIndexDT,by=c("Region","NewArea_Name"))
    PriceAdj <- "PriceAdj"
    varname0 <- paste0("Total_",OrderingVar,"_Month",Size)
    varname <- paste0("Total_",OrderingVar,"_Month",Size,"_PriceAdj")
    HHDT <- HHDT[,(varname):=get(varname0)/PriceIndex] 
  }else{
    varname <- paste0("Total_",OrderingVar,"_Month",Size)
  }
  
  HHDT <- HHDT[,OrderingVar:=get(varname)]

  HHDT <- HHDT[order(OrderingVar)]  # I removed Region from ordering, deciling is not divided into rural/urban (M.E. 5/11/2020)
  HHDT <- HHDT[,crw:=cumsum(Weight*Size)/sum(Weight*Size)]  # Cumulative Relative Weight
  HHDT <- HHDT[,xr25th:=.SD[25,OrderingVar],by=.(Region,NewArea_Name)]
  HHDT <- HHDT[,First25:=ifelse(OrderingVar<=xr25th,1,0)]
  #Calculate deciles by weights
  HHDT <- HHDT[,Decile:=cut(crw,breaks = seq(0,1,.1),labels = 1:10)]
  HHDT <- HHDT[,Percentile:=cut(crw,breaks=seq(0,1,.01),labels=1:100)]
 
  return(HHDT[,.(HHID,Decile,Percentile,First25)])
}

UpdateForDurableDepr <- function(DataTable,ODIDep){
  DataTable[,OwnedDurableItemsDepreciation:=NULL]
  DataTable <- merge(DataTable,ODIDep)
  
  for (col in union(Settings$ExpenditureCols,Settings$ConsumptionCols))
    DataTable[is.na(get(col)), (col) := 0]
  
  DataTable[,Total_Expenditure_Month := Reduce(`+`, .SD), .SDcols=Settings$ExpenditureCols]
  DataTable[,Total_Consumption_Month := Reduce(`+`, .SD), .SDcols=Settings$ConsumptionCols]
  
  DataTable[,Total_Expenditure_Month_per:=Total_Expenditure_Month/EqSizeOECD]
  DataTable[,Total_Consumption_Month_per:=Total_Consumption_Month/EqSizeOECD]
}


Calculate_OwnedDurableItemsDepreciation <- function(DurableData_ExpDetail,
                                                    DurableItems_OwningDetail,
                                                    by="Item",
                                                    Decile=NULL,
                                                    DurableItems=NA,
                                                    g2=c(53111, 53112, 53113,
                                                         53116, 53125, 53129, 
                                                         53132, 53216, 71111, 
                                                         71112, 71116, 71117, 
                                                         71211, 71311, 71312, 
                                                         72111, 72118, 72119, 
                                                         82111, 82113, 91111, 
                                                         91112, 91113, 91114, 
                                                         91115, 91117, 91122, 
                                                         91128, 91129, 91311)){
  
  Ownsm <- melt(data = DurableItems_OwningDetail,id.vars = "HHID",
                measure.vars = names(DurableItems_OwningDetail)[-1],
                variable.name = "Item",value.name = "Owns")
  Ownsm <- Ownsm[Owns==1]  
  if(is.null(Decile)){
    by = setdiff(by,"Decile")
    DurableDepr <- data.table(Item=DurableItems$Item)
  }else{
    DurableData_ExpDetail <- merge(DurableData_ExpDetail,Decile,by="HHID")
    Ownsm <- merge(Ownsm,Decile,by="HHID")
    
    DurableDepr <- data.table(expand.grid(Item=DurableItems$Item,
                                          Decile=factor(1:10)))
  }
  
  DurableValues <- DurableData_ExpDetail[Code %in% g2 & Durable_Exp>0
                                         ,.(.N,Value=mean(Durable_Exp))
                                         ,by=by]
  DurableValues[is.na(Item),Item:="Other"]
  DurableDepr <- merge(DurableDepr,DurableValues,by=by,all.x = TRUE)
  DurableDepr <- merge(DurableDepr,DurableItems[,.(Item,Depri)],by="Item")
  DurableDepr[is.na(DurableDepr)] <- 0
  
  f <- function(X){
    v <- X$Value
    d <- as.integer(as.character(X$Decile))
    d2 <- d^2
    d3 <- d^3
    mdl <- lm(v~d+d2+d3)
    vp <- predict(mdl,newdata = data.frame(d=d,d2=d2,d3=d3))
    return(vp)
  }
  if("Decile" %in% by){
    
    DurableDepr <- DurableDepr[order(Item,Decile)]
    DurableDepr[,estVal:=f(.SD),by=Item]
    
    for(i in unique(DurableDepr$Item))
      for(d in 9:1){
        ev <- DurableDepr[Item==i & as.integer(Decile)==d]$estVal
        evnext <- DurableDepr[Item==i & as.integer(Decile)==d+1]$estVal
        if(ev>evnext)
          DurableDepr[Item==i & as.integer(Decile)==d,estVal:=evnext]
      }
    DurableDepr[,DepreciationValue:=estVal*Depri/100]
  }else{
    DurableDepr[,DepreciationValue:=Value*Depri/100]
  }
  D <- merge(Ownsm,DurableDepr[,c("DepreciationValue",by),with=FALSE],by=by)
  
  OwnedDurableItemsDepreciation <- D[,.(OwnedDurableItemsDepreciation=
                                          sum(.SD$DepreciationValue)),by=HHID]
  
  return(OwnedDurableItemsDepreciation)
}

CalcTornqvistIndex <- function(DataTable){
  #DataTable <- SMD
  DataTable <- DataTable[,MeterPrice:=ifelse(tenure=="Free"|tenure=="Other"|tenure=="AgainstService"
                                             ,NA,MeterPrice)]
  DataTable <- DataTable[,House_Exp:=ifelse(tenure=="Free"|tenure=="Other"|tenure=="AgainstService"
                                            ,NA,House_Exp)]
  
  X <- DataTable[,.(N=.N,wj1=weighted.median(FoodExpenditure/Total_Expenditure_Month,Weight,na.rm = TRUE),
                    wj2=weighted.median(House_Exp/Total_Expenditure_Month,Weight,na.rm = TRUE),
                    pj1=weighted.median(Bundle_Value,Weight,na.rm = TRUE),
                    pj2=weighted.median(MeterPrice,Weight,na.rm = TRUE)),by=.(Region,NewArea_Name)]
  
  X[,wj:=wj1+wj2]
  X[,wj1:=wj1/wj]
  X[,wj2:=wj2/wj]
  XTeh<-X[NewArea_Name=="Sh_Tehran"]
  wk1<-XTeh$wj1   # k == Sh_Tehran
  wk2<-XTeh$wj2
  pk1<-XTeh$pj1
  pk2<-XTeh$pj2
  
  X[,SimpleIndex:= .5 * pj1/pk1 + .5 * pj2/pk2]
  X[,AnotherIndex:= wj1 * pj1/pk1 + wj2 * pj2/pk2]
  
  X[,TornqvistIndex:= exp( (wk1+wj1)/2 * log(pj1/pk1) + 
                             (wk2+wj2)/2 * log(pj2/pk2) ) ]
  
  return(X[,.(Region,NewArea_Name,PriceIndex=TornqvistIndex)])
}