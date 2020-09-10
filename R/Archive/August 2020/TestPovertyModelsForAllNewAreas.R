rm(list=ls())

wtable <- function(dataframe, weights=NULL){
  if(is.null(weights))
    weights(dataframe$weigths)
  if(is.null(weights))
    return(table(dataframe))
  t <- table(dataframe)
  l <- attr(t,"dimnames")
  v1 <- names(l)[1]
  v2 <- names(l)[2]
  vals1 <- l[[v1]]
  vals2 <- l[[v2]]
  t[1,1] <- sum(weights[which(dataframe[[v1]]==vals1[1] & dataframe[[v2]]==vals2[1])])
  t[1,2] <- sum(weights[which(dataframe[[v1]]==vals1[1] & dataframe[[v2]]==vals2[2])])
  t[2,1] <- sum(weights[which(dataframe[[v1]]==vals1[2] & dataframe[[v2]]==vals2[1])])
  t[2,2] <- sum(weights[which(dataframe[[v1]]==vals1[2] & dataframe[[v2]]==vals2[2])])
  return(t)
}

starttime <- proc.time()
cat("\n\n================ Classification Models =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")
# options(java.parameters = "-Xmx5g")
# library(readxl)
library(data.table)
# library(stringr)
# library(janitor)
# library(dplyr)
# library(Hmisc)
library(caret)
# library(caretEnsemble)
library(gbm)
library(nnet)
# library(pROC)
# library(glmnet)
library(parallel)
library(doParallel)
library(factoextra)
library(rpart)

#sink("classificationmodelsresults.txt")
acml <- c("tree","logit","probit","bernoulli","adaboost","huberized","nnet")
acml <- "tree"
year <- 94
for(year in 97:97){
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Data4ClassifactionModels.rda"))
  #Data <- Data[,-39,with=FALSE]
  # Data <- Data[,Percentile:=as.numeric(paste0(Percentile))]
  # Data <- Data[,Percentile_Nominal:=as.numeric(paste0(Percentile_Nominal))]
  D <- copy(Data)
  D <- D[,ProvinceName:=NULL]
  D <- D[,Percentile:=NULL]
  D <- D[,Percentile_Nominal:=NULL]
  D <- D[,Decile:=NULL]
  D <- D[,Decile_Nominal:=NULL]
  D <- D[,NewArea:=NULL]
  D <- D[,cluster3:=as.factor(cluster3)]
  remnames <- c("Total_Exp_Month_Per_nondurable","PovertyLine","TOriginalFoodExpenditure_Per",
                "Engel","InitialPoor","FPLine","HHID","Real_Total_Exp_Month","Weight","Year","NetIncome",
                "Real_FoodExpenditure","Real_Durable_Exp","cluster3","NewArea_Name")
  ClusterList <- unique(D$cluster3)
  
  #  ResultsTestsData <- data.table()
  Results <- data.table()
  SepRes <- data.table()
  
  
  registerDoParallel(6)
  getDoParWorkers()
  
  
  Cluster <- "13"
  
  # XResults <- data.table()
  
#  for(Reg in c("Rural","Urban")){
    for(Cluster in ClusterList){
      cat("\n\n======================\n",":",Cluster,"\n")
      D <- D[,ThisArea := ifelse(cluster3 == Cluster,1,0)]
      D <- D[,SIS := ifelse(cluster3 %in% c(7,13),1,0)]
      
      
      dm <- dummyVars("~.",data = D[,setdiff(names(D),remnames),with=FALSE], fullRank = FALSE)
      D2 <- as.data.table(predict(dm,D))
      #prop.table(table(D2$FinalPoor))
      
      outN <- "FinalPoor"
      predictornames <- names(D2)[! names(D2) %in% c(outN,"ThisArea","SIS")]
      if(Cluster=="1"){
        predictornames <- setdiff(predictornames,c("Car"))
      }
      D2[is.na(D2)] <- 0
      
      D2 <- D2[,FP:=as.factor(ifelse(FinalPoor==1,"Poor","NotPoor"))]
      outF <- "FP"
      
      pc <- prcomp(D2[,predictornames,with=FALSE])
      sepid <- which(D2$ThisArea==1)
      ttid <- which(D2$ThisArea==0 & D2$SIS==0)
      
      # SepD <- D2[ThisArea==1,c(outF,outN,predictornames),with=FALSE]
      # D3 <- D2[ThisArea == 0 & SIS==0,]
      t0 <- Sys.time()
      imp <- summary(pc)$importance[3,]
      pidx <- min(which(imp>.99))
      XcD <- data.frame(pc$x[ttid,1:pidx])
      XScD <- data.frame(pc$x[sepid,1:pidx])
      
      t1 <- Sys.time()
      cat("\nPCA   ...",as.character(t1),"\t[",as.character(t1-t0),"]\n")
      
      XoD <- D2[ttid,predictornames,with=FALSE]
      yfD <- D2[[outF]][ttid]
      ynD <- D2[[outN]][ttid]
      
      
      XSoD <- D2[sepid,predictornames,with=FALSE]
      
      splitIndex <- createDataPartition(yfD,p=.75,list = FALSE,times = 1)
      
      for(dtype in c("o")){#},"c")){
        if(dtype=="o"){
          XD <- XoD
          XSep <- XSoD
        }else{
          XD <- XcD
          XSep <- XScD
        }
        
        X_train <- XD[splitIndex,] #D3[splitIndex,predictornames,with=FALSE]
        y_trainF <- yfD[splitIndex]#[[outF]]
        y_trainN <- ynD[splitIndex]#[[outN]]
        
        X_test <- XD[-splitIndex,] # D3[-splitIndex,predictornames,with=FALSE]
        y_testF <- yfD[-splitIndex]#[[outF]]
        y_testN <- ynD[-splitIndex]#[[outN]]
        
        X_Sep <- XSep
        y_SepF <- D2[[outF]][sepid]
        y_SepN <- D2[[outN]][sepid]
        
        DTrainN <- data.frame(y=y_trainN, X_train)
        DTrainF <- data.frame(y=y_trainF, X_train)
        DTestN <- data.frame(y=y_testN, X_test)
        DTestF <- data.frame(y=y_testF, X_test)
        DSepN <- data.frame(y=y_SepN, X_Sep)
        DSepF <- data.frame(y=y_SepF, X_Sep)
        
        
        # for(ratio in 10:40/10){
        #   cat(".")
        # 
        # loss_matr <- matrix(c(0, 1, ratio, 0), nrow = 2, byrow = TRUE)
        # 
        # objModel <- rpart(y ~ .,DTrainF ,
        #                   control=rpart.control(maxdepth=10),
        #                   method="class",
        #                   parms = list(loss = loss_matr))
        # 
        # predictions_test <- predict(object = objModel, X_test, type="class")
        # predictions_sep <- predict(object = objModel, X_Sep, type="class")
        # testresults <- data.frame(alg=y_testF,
        #                           pred=predictions_test)
        # 
        # ttab <- table(testresults)
        # ttab
        # XResults <- rbind(XResults,data.table(AreaName,Reg,"tree",ratio,
        #                                     Accuracy=sum(diag(ttab))/sum(ttab),
        #                                     Precision=sum(ttab[1,1])/sum(ttab[,1]),
        #                                     Recall=sum(ttab[1,1])/sum(ttab[1,]),
        #                                     FalsePositive=sum(ttab[2,1])/sum(ttab[2,]),
        #                                     DataHCR=sum(ttab[2,])/sum(ttab),
        #                                     PredHCR=sum(ttab[,2])/sum(ttab)))
        # }
        #describe(X_train)
        # featurePlot(x = X_test,y = y_test,
        #             between=list(x=1,y=1),
        #             type=c("g","p","smooth"))
        
        # myctl <- trainControl(method = "cv",
        #                       number = 5,
        #                       savePredictions = "final",
        #                       allowParallel = TRUE,
        #                       classProbs = TRUE)
        
        mdl <- acml[1]
        for(mdl in acml){
          cat("\n",mdl,"-",dtype," ... ")
          t0 <- Sys.time()
          
          if(mdl=="tree"){
            loss_matr <- matrix(c(0, 1, 2.1, 0), nrow = 2, byrow = TRUE)
            
            objModel <- rpart(y ~ .,DTrainF ,
                              control=rpart.control(maxdepth=8),
                              method="class",
                              parms = list(loss = loss_matr))
            
            predictions_test <- predict(object = objModel, X_test, type="class")
            predictions_sep <- predict(object = objModel, X_Sep, type="class")
            
          }else if(mdl %in% c("logit","probit")){
            objModel <- glm(y ~ . ,data=DTrainF,
                            family = binomial(link = mdl))
            
            predictions_test_prob <- predict(object = objModel, X_test, type="response")
            predictions_test <- ifelse(predictions_test_prob > .5, "Poor", "NotPoor")
            predictions_sep_prob <- predict(object = objModel, X_Sep, type="response")
            predictions_sep <- ifelse(predictions_sep_prob > .5, "Poor", "NotPoor")
            
            
          }else if(mdl %in% c("bernoulli","adaboost","huberized")){
            fmla <- as.formula(paste("FinalPoor ~",paste(names(DTrainN)[-1],collapse = "+")))
            objModel <- gbm.fit(x = X_train,y = y_trainN ,distribution = mdl,verbose = FALSE)
            predictions_test_prob <- predict(object = objModel, X_test, type="response", n.trees = 3)
            predictions_test <- ifelse(predictions_test_prob > .5, "Poor", "NotPoor")
            predictions_sep_prob <- predict(object = objModel, X_Sep, type="response", n.trees = 3)
            predictions_sep <- ifelse(predictions_sep_prob > .5, "Poor", "NotPoor")
            # }else if(mdl=="nnet"){
            #   # objModel <- nnet(x = X_train,y = y_trainN,size = 2)
            #   # predict(object = objModel, X_test, type="raw")
            #   #
          }else{ #nnet
            
            objModel <- caret::train(X_train,y_trainF,
                                     method = mdl,
                                     trControl = myctl,
                                     #    metric = "ROC",
                                     preProcess = c("center","scale"),
                                     verbose=FALSE)
            
            predictions_test <- predict(object = objModel, X_test, type="raw")
            predictions_Sep <- predict(object = objModel, X_Sep, type="raw")
          }
          
          
          testresults <- data.frame(alg=y_testF,
                                    pred=predictions_test)
          
          ttab <- wtable(testresults,D[ttid][-splitIndex]$Weight*D[ttid][-splitIndex]$Size)
          #ttab
          Results <- rbind(Results,data.table(dtype,Cluster,mdl,
                                              Accuracy=sum(diag(ttab))/sum(ttab),
                                              Precision=sum(ttab[1,1])/sum(ttab[,1]),
                                              Recall=sum(ttab[1,1])/sum(ttab[1,]),
                                              FalsePositive=sum(ttab[2,1])/sum(ttab[2,]),
                                              DataHCR=sum(ttab[2,])/sum(ttab),
                                              PredHCR=sum(ttab[,2])/sum(ttab)))
          
          sepresults <- data.frame(alg=y_SepF,
                                   pred=predictions_sep)
          
          ttab <- wtable(sepresults,D[sepid]$Weight*D[sepid]$Size)
          SepRes <- rbind(SepRes,data.table(dtype,Cluster,mdl,
                                            Accuracy=sum(diag(ttab))/sum(ttab),
                                            Precision=sum(ttab[1,1])/sum(ttab[,1]),
                                            Recall=sum(ttab[1,1])/sum(ttab[1,]),
                                            FalsePositive=sum(ttab[2,1])/sum(ttab[2,]),
                                            DataHCR=sum(ttab[2,])/sum(ttab),
                                            PredHCR=sum(ttab[,2])/sum(ttab)))
          t1 <- Sys.time()
          cat(as.character(t1),"\t[",as.character(t1-t0),"]\n")
          
        }
      }
    }
  }
#}
Results[,DiffHCR:=DataHCR-PredHCR]
Results[,Diff:=(DataHCR/PredHCR-1)^2]
Results <- Results[!is.na(Diff)]
View(Results)
SepRes[,DiffHCR:=DataHCR-PredHCR]
SepRes[,Diff:=(DataHCR/PredHCR-1)^2]
SeSepRes <- SepRes[!is.na(Diff)]
View(SepRes)
#sink()
#View(ResultsTestsData)



D[cluster3==12,weighted.mean(Total_Exp_Month_Per_nondurable<PovertyLine*1.2826,Weight*Size),by=cluster3]
D[cluster3==13,weighted.mean(Total_Exp_Month_Per_nondurable<PovertyLine*1.532,Weight*Size),by=cluster3]
D[cluster3==7,weighted.mean(Total_Exp_Month_Per_nondurable<PovertyLine*1.292,Weight*Size),by=cluster3]

D[cluster3==1,weighted.mean(Total_Exp_Month_Per_nondurable<PovertyLine*.787,Weight*Size),by=cluster3]


DX <- cbind(D[sepid],sepresults)
S1 <- DX[alg=="Poor" & pred=="Poor",lapply(.SD, mean,na.rm=TRUE)]
S2 <- DX[alg=="Poor" & pred=="NotPoor",lapply(.SD, mean,na.rm=TRUE)]
S3 <- DX[alg=="NotPoor" & pred=="Poor",lapply(.SD, mean,na.rm=TRUE)]
S4 <- DX[alg=="NotPoor" & pred=="NotPoor",lapply(.SD, mean,na.rm=TRUE)]
#S5 <- DX[alg=="Poor" & pred=="NotPoor" & FoodProtein_Per>76,lapply(.SD, mean,na.rm=TRUE)]
View(rbind(S1,S2,S3,S4))
