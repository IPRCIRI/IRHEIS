rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Classification Models =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")
options(java.parameters = "-Xmx5g")
library(readxl)
library(data.table)
library(stringr)
library(janitor)
library(dplyr)
library(Hmisc)
library(caret)
library(caretEnsemble)
library(gbm)
library(pROC)
library(glmnet)
library(parallel)
library(doParallel)

acml <- c("glm","gbm","nnet")
year <- 97

load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Data4ClassifactionModels.rda"))
#Data <- Data[,-39,with=FALSE]
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
              "Real_FoodExpenditure","Real_Durable_Exp","cluster3","NewArea_Name")
NewAreaList <- unique(D$NewArea_Name)

ResultsTestsData <- data.table()


registerDoParallel(6)
getDoParWorkers()
 


for(AreaName in NewAreaList){
  cat("\n\n======================\n",AreaName,"\n")
  D <- D[,ThisArea := ifelse(NewArea_Name == AreaName,1,0)]
  D <- D[,SIS := ifelse(NewArea_Name %in% c("Sh_Zahedan","Sistan"),1,0)]
  
  dm <- dummyVars("~.",data = D[,setdiff(names(D),remnames),with=FALSE], fullRank = FALSE)
  D2 <- as.data.table(predict(dm,D))
  #prop.table(table(D2$FinalPoor))
  
  outcomename <- "FinalPoor"
  predictornames <- names(D2)[! names(D2) %in% c(outcomename,"ThisArea","SIS")]
  
  D2 <- D2[,FP:=as.factor(ifelse(FinalPoor==1,"Poor","NotPoor"))]
  outcomename <- "FP"
  
  SepD <- D2[ThisArea==1,]
  D3 <- D2[ThisArea == 0 & SIS==0,]
  splitIndex <- createDataPartition(D3[[outcomename]],p=.75,list = FALSE,times = 1)
  trainD <- D3[splitIndex,]
  testD <- D3[-splitIndex,]
  
  
  
  X_train <- trainD[,predictornames,with=FALSE]
  X_train[is.na(X_train)] <- 0
  y_train <- trainD[[outcomename]]
  
  X_test <- testD[,predictornames,with=FALSE]
  X_test[is.na(X_test)] <- 0
  y_test <- testD[[outcomename]]
  
  X_Sep <- SepD[,predictornames,with=FALSE]
  X_Sep[is.na(X_Sep)] <- 0
  y_Sep <- SepD[[outcomename]]
  
  
  
  #describe(X_train)
  # featurePlot(x = X_test,y = y_test,
  #             between=list(x=1,y=1),
  #             type=c("g","p","smooth"))
  
  myctl <- trainControl(method = "cv",
                        number = 5,
                        savePredictions = "final",
                        allowParallel = TRUE,
                        classProbs = TRUE)
  
  
  
  t0 <- Sys.time()
  Results <- c()
  SepRes <- c()
  for(mdl in acml){
    try({
      cat("\n",mdl," ... ")
      if(mdl=="glm"){ objModel <- caret::train(X_train,y_train,
                               method = mdl,
                               trControl = myctl,
                               family = "binomial",
                               #    metric = "ROC",
                               preProcess = c("center","scale"))
      }else{
        
      objModel <- caret::train(X_train,y_train,
                               method = mdl,
                               trControl = myctl,
                               #    metric = "ROC",
                               preProcess = c("center","scale"))
      }
      predictions_test <- predict(object = objModel, X_test, type="raw")
      t1 <- Sys.time()
      cat(as.character(t1),"\t[",as.character(t1-t0),"]")
      Results[mdl] <- postResample(pred=as.factor(predictions_test), obs=as.factor(y_test))[1]
      cat("\tAccuracy:",Results[mdl],'\t')
      
      predictions_Sep <- predict(object = objModel, X_Sep, type="raw")
      cat(as.character(t1),"\t[",as.character(t1-t0),"]\n")
      SepRes[mdl] <- postResample(pred=as.factor(predictions_Sep), obs=as.factor(y_Sep))[1]
      
      t0 <- t1
      
      ResultsTestsData <- rbind(ResultsTestsData,
                                data.table(SepArea=AreaName,Model=mdl,Test=Results[mdl],SepRes=SepRes[mdl]))
    })
  }
  
  print(Results)
  print(SepRes)
}


View(ResultsTestsData)