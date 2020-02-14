# 403-Filtering job status.R
# 2019: Arin Shahbazian

rm(list=ls())

starttime <- proc.time()
cat("\n\n================Filtering job status =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(stringr)
library(spatstat)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Total1.rda"))
  
  
  job<-Total[,.(HHID,Region,HActivityState,Main_Job_Name_Pub,PubWageNetIncomeY,
                Main_Job_Name_Prv,PrvWageNetIncomeY,
                Main_Job_Name_Cooperative,CooperativeWageNetIncomeY,
                AgriEarners,Main_Job_Name_Agri,BussEarners,
                Main_Job_Name_Buss,
                Job_Main_Code_Pub,Job_Main_Code_Prv,Job_Main_Code_Cooperative,
                Job_Main_Code_Buss,Job_Main_Code_Agri,
                Aid,Homemade,
                Interest,Intra,Rent,Retirement,Subsidy)]
  
  job<-job[,job_Best:=min(Job_Main_Code_Pub,Job_Main_Code_Prv,
                          Job_Main_Code_Cooperative,
                          Job_Main_Code_Buss,Job_Main_Code_Agri)]
  

  save(job,file=paste0(Settings$HEISProcessedPath,"Y",year,"job.rda"))
  
  
}

endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)
