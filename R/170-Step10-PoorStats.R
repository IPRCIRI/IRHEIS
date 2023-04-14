# 170-Step10-PoorStats.R
# 
# Copyright © 2022:Majid Einian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Comparing the poor and non-poor ==================\n")
library(yaml)

Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(writexl)

Groupings <- list(NULL,"Region")
names(Groupings) <- c("Country","Region")
Variables <- c("HEmployed","HLiterate","HEduYears","HAge")
PovStatsList <- list()
names(Variables) <- c("HEmployed","HLiterate","HEduYears","HAge")
for(VarName in names(Variables)){
  for(GroupingVarName in names(Groupings)){
    
    PovStatsList[[paste0(VarName,"-",GroupingVarName)]] <- data.table()
  }
}
#year <- 99
for(year in ((Settings$startyear+2):Settings$endyear)){
  cat(paste0("\nYear:",year,"\t"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalPoor.rda"))
  
  for(GroupingVarName in names(Groupings)){
    for(StudyVar in Variables){
      GroupingVar <- Groupings[[GroupingVarName]]
      
      X1 <- MD[,.(Year=year,
                  SampleSize=.N,
                  StudyVar=weighted.mean(get(StudyVar),Weight,na.rm=TRUE)),
               by=c("FinalPoor",GroupingVar)]
      X1w <- cbind(year,dcast(X1,formula(paste0(ifelse(is.null(GroupingVar),"...",GroupingVar)," ~ FinalPoor"))
                              ,value.var = "StudyVar"))
      PovStatsList[[paste0(StudyVar,"-",GroupingVarName)]] <- rbind(PovStatsList[[paste0(StudyVar,"-",GroupingVarName)]],X1w)
    }
  }
}
cat("\n\n")

write_xlsx(PovStatsList,path = paste0(Settings$HEISResultsPath,"/PoorStats.xlsx"))

endtime <- proc.time()
cat("\n\n============================\nIt took",(endtime-starttime)["elapsed"],"seconds")