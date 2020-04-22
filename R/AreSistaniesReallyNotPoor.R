

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Classification Models =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(stringr)
library(janitor)
library(dplyr)
library(caret)
library(gbm)
library(pROC)
library(glmnet)

for(year in 97){
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Data4ClassifactionModels.rda"))
  Data <- Data[,-39,with=FALSE]
  # Data <- Data[,Percentile:=as.numeric(paste0(Percentile))]
  # Data <- Data[,Percentile_Nominal:=as.numeric(paste0(Percentile_Nominal))]
  D <- Data[,ProvinceName:=NULL]
  D <- D[,Percentile:=NULL]
  D <- D[,Percentile_Nominal:=NULL]
  D <- D[,Decile:=NULL]
  D <- D[,Decile_Nominal:=NULL]
  D <- D[,NewArea:=NULL]
  D <- D[,cluster3:=as.factor(cluster3)]
  remnames <- c("Total_Exp_Month_Per_nondurable","PovertyLine","TOriginalFoodExpenditure_Per",
                "Engel","InitialPoor","FPLine","HHID","Real_Total_Exp_Month","Weight","Year","NetIncome",
                "Real_FoodExpenditure","Real_Durable_Exp","NewArea2","cluster3")
  D <- D[,SIS := ifelse(NewArea2 %in% c("Sh_Zahedan","Sistan"),1,0)]
  
  dm <- dummyVars("~.",data = D[,setdiff(names(D),remnames),with=FALSE], fullRank = FALSE)
  D2 <- as.data.table(predict(dm,D))
  prop.table(table(D2$FinalPoor))
  
  outcomename <- "FinalPoor"
  predictornames <- names(D2)[! names(D2) %in% c(outcomename,"SIS")]
  
  D2 <- D2[,FP:=as.factor(ifelse(FinalPoor==1,"Poor","NotPoor"))]
  outcomename <- "FP"
  
  SISD <- D2[SIS > 0,]
  D3 <- D2[SIS == 0,]
  splitIndex <- createDataPartition(D3[[outcomename]],p=.75,list = FALSE,times = 1)
  trainD <- D3[splitIndex,]
  testD <- D3[-splitIndex,]
  
  objControl <- trainControl(method='cv', number=3, returnResamp='none',
                             summaryFunction = twoClassSummary, classProbs = TRUE)
  
  objModel <- train(trainD[,predictornames,with=FALSE],trainD[[outcomename]],
                    method = "gbm",
                    trControl = objControl,
                    metric = "ROC",
                    preProcess = c("center","scale"))
  head(summary(objModel,plot=FALSE),10)
  plot(varImp(objModel,scale = FALSE))
  #print(objModel)
  cat("\n=================== Test ========================\n")
  predictions <- predict(object = objModel, testD[,predictornames,with=FALSE], type="raw")
  print(table(data.table(pred=predictions, obs=as.factor(testD[[outcomename]]))))
  print(postResample(pred=predictions, obs=as.factor(testD[[outcomename]])))
  predictions <- predict(object=objModel,  testD[,predictornames,with=FALSE], type='prob')
  #head(predictions)
  auc <- roc(ifelse(testD[[outcomename]]=="Poor",1,0), predictions[[2]])
  print(auc$auc)
  
  
  cat("\n=================== Check Sistan  ========================\n")
  check <- predict(object = objModel, SISD[,predictornames,with=FALSE], type="raw")
  print(table(data.table(pred=check, obs=as.factor(SISD[[outcomename]]))))
  print(table(Data[NewArea2 %in% c("Sh_Zahedan","Sistan"),FinalPoor]))
  print(postResample(pred=check, obs=as.factor(SISD[[outcomename]])))
  check <- predict(object=objModel,  SISD[,predictornames,with=FALSE], type='prob')
  #head(check)
  auc <- roc(ifelse(SISD[[outcomename]]=="Poor",1,0), check[[2]])
  print(auc$auc)
  
  
  
  dontrun <- function(){
  
  outcomename <- "FinalPoor"
  prednames <- setdiff(predictornames,c("RedMeat_Per"                              
                                         , "WhiteMeat_Per"                            
                                         , "Meat_Per"                                 
                                         , "R_RedMeat_Per"                            
                                         , "R_WhiteMeat_Per"                          
                                         , "R_Meat_Per"                               
                                         , "Dabestan_Per"                             
                                         , "Rahnamayi_Per"                            
                                         , "Dabirestan_Per"                           
                                         , "Pish_Per"                                 
                                         , "Education_Per"                            
                                         , "Ratio_Education_Per"                      
                                         , "Visit"                                    
                                         , "Tooth"                                    
                                         , "Medicine"                                 
                                         , "R_Medicine"                               
                                         , "Ratio_R_Medicine"                         
                                         , "Pub_Employee"                             
                                         , "Prv_Employee"                             
                                         , "Cooperative_Employee"                     
                                         , "Simple_Jobs_Staff"                        
                                         , "Opreators_machinery_equipment"            
                                         , "Craftsman"                                
                                         , "Skilled_staff_agriculture_forestr_fishing"
                                         , "Staff_service_sales"                      
                                         , "Office_staff"                             
                                         , "Technician"                               
                                         , "Expert"                                   
                                         , "Manager"                                  
                                         , "Aid"                                      
                                         , "Aid_Per"                                  
                                         , "Ratio_Aid_Per"                            
                                         , "HFemale"))
  objControl <- trainControl(method='cv', number=3, returnResamp='none')
  objModel <- train(trainD[,prednames,with=FALSE], trainD[[outcomename]], 
                    method='glmnet',  
                    metric = "RMSE", 
                    trControl=objControl)
  predictions <- predict(object=objModel, testD[,prednames,with=FALSE])
  auc <- roc(testD[[outcomename]], predictions)
  print(auc$auc)
  
  #plot(varImp(objModel,scale = FALSE))
  }
}  