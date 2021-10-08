# 167-Step7-Engel : Calculated Engel and modified Engel and Poverty Lines
# 
# Copyright © 2018-2020: Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Engel =====================================\n")
library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)

BigEngelTable <- data.table(Region=NA_character_,cluster3=NA_integer_,
                            N=NA_integer_,Engel=NA_real_,
                            FPLine=NA_real_,Year=NA_integer_,WW=NA_real_)[0]
BigEngelTable1 <- data.table()
Settings$startyear<-Settings$startyear-2

for(year in (88:99)){
  cat(paste0("\nYear:",year,"\t"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FoodPoor.rda"))
  
  MD<-MD[,EngelH:=(TOriginalFoodExpenditure/Total_Exp_Month)]
  
  
  EngelD <- MD[ TOriginalFoodExpenditure_Per>0.8*FPLine &
                   TOriginalFoodExpenditure_Per<1.2*FPLine,
                 .(.N,Engel=weighted.mean(EngelH,Weight),
                   FPLine=mean(FPLine))
               ,by=.(Region,cluster3)]
  
  save(EngelD,file=paste0(Settings$HEISProcessedPath,"Y",year,"EngelD.rda"))
  
  Engel1 <- MD[ TOriginalFoodExpenditure_Per>0.8*FPLine &
                  TOriginalFoodExpenditure_Per<1.2*FPLine,
                .(.N,Engel=weighted.mean(EngelH,Weight))]
  W <- MD[ ,.(.N,WW=sum(Weight)),by=.(Region,cluster3)]
  
  
  
  EngelD <- merge(EngelD,W[,.(cluster3,Region,WW)], by=c("cluster3","Region"))
  EngelD<-EngelD[,Year:=year]
  Engel1<-Engel1[,Year:=year]
  BigEngelTable <- rbind(BigEngelTable,EngelD)
  BigEngelTable1 <- rbind(BigEngelTable1,Engel1)
  
}


library(writexl)

#write_xlsx(BigEngelTable1,"E:/engle.xlsx")

InflationData <- data.table(read_excel(path = Settings$InflationDataFilePath))
InflationData[,F1 := (1+D2FoodInf)/(1+D2Inf)]
InflationData<-InflationData[order(Year)]
InflationData[,l.F1:=data.table::shift(F1)]
InflationData[,F2 := F1*l.F1]


BigEngelTable<-merge(BigEngelTable,InflationData,by="Year")
BigEngelTable<-BigEngelTable[order(Year,cluster3)]
BigEngelTable[,l.Engel:=data.table::shift(Engel),by=cluster3]
BigEngelTable[,l2.Engel:=data.table::shift(Engel,2),by=cluster3]

BigEngelTable[,EngelX:=l.Engel*F1]
BigEngelTable[,EngelX2:=l2.Engel*F2]


BigEngelTable[is.na(EngelX) & is.na(EngelX2),ModifiedEngel:=Engel]
BigEngelTable[!is.na(EngelX) & is.na(EngelX2),ModifiedEngel:=(Engel+EngelX)/2]
BigEngelTable[is.na(ModifiedEngel),ModifiedEngel:=(Engel+EngelX+EngelX2)/3]
BigEngelTable[,PovertyLine:=FPLine/ModifiedEngel]
BigEngelTable[,PovertyLine0:=FPLine/Engel]

save(BigEngelTable,file=paste0(Settings$HEISProcessedPath,"BigEngelTable.rda"))

BigEngelTable1 <- BigEngelTable
BigEngelTable1 <- BigEngelTable1[,M_En:=weighted.mean(ModifiedEngel,WW),by="Year"]
BigEngelTable1 <- unique(BigEngelTable1[,.(Year,M_En)])
#write_xlsx(BigEngelTable1,"E:/engle_modi.xlsx")

# btm <- melt(BigEngelTable,id.vars = c("Year","cluster3","Region"),
#             measure.vars = c("Engel","ModifiedEngel"))
# 
# library(ggplot2)
# library(gridExtra)
# library(ggpubr)
# 
# plotlist <- list()
# for(cl in 1:13){
#   plt = ggplot(btm[cluster3==cl,], 
#                aes(x=Year,y=value,
#                    fill=variable,color=variable,linetype=variable)) +
#     geom_line() +
#     geom_point() +
#     annotate(geom="label", x=84, y=max(btm[cluster3==cl,value])-.01, label=cl, color="red")+
#     theme(legend.position = "none",
#           axis.title.x=element_blank(),
#           axis.title.y=element_blank())
#   
#   plotlist[[cl]] <- plt
# }
# plt = ggplot(btm[cluster3==cl,], aes(x=Year,y=value,fill=variable,color=variable)) +
#   geom_line() +
#   geom_point()
# leg <- get_legend(plt)
# plotlist[[cl+1]] <- as_ggplot(leg)
# do.call("grid.arrange", c(plotlist, ncol=2))

endtime <- proc.time()
cat("\n\n============================\nIt took",(endtime-starttime)["elapsed"],"seconds")
