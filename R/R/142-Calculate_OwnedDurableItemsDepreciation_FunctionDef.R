Calculate_OwnedDurableItemsDepreciation <- function(DurableData_ExpDetail,
                                                    DurableItems_OwningDetail,
                                                    by="Item",
                                                    Decile=NULL,
                                                    DurableItems=NA){
  
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
    
    DurableDepr <- data.table(expand.grid(Item=DurableItems$Item,Decile=factor(1:10)))
  }
  # if(is.na(DurableItems)){
  #   library(readxl)
  #   DurableItems <- data.table(read_excel(Settings$MetaDataFilePath,
  #                                         sheet=Settings$MDS_DurableItemsDepr))
  # }
  DurableValues <- DurableData_ExpDetail[,.(Value=mean(Durable_Exp)),by=by]
  DurableDepr <- merge(DurableDepr,DurableValues,by=by,all.x = TRUE)
  DurableDepr <- merge(DurableDepr,DurableItems,by="Item")
  
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
  D <- merge(Ownsm,DurableDepr,by=by)
  
  OwnedDurableItemsDepreciation <- D[,.(OwnedDurableItemsDepreciation=sum(.SD$DepreciationValue)),by=HHID]
  
  return(OwnedDurableItemsDepreciation)
}
