

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

# upl <- c("adaptDA","CHAID","sparesdiscrim","elmNN","gpls","logicFS","FCNN4R","mxnet","vbmp")
# modellist <- getModelInfo()
# ns <- names(modellist)
# acml <- NULL
# acmpl <- NULL
# for(mname in ns){
#   mdl <- modellist[[mname]]
#   if("Classification" %in% mdl$type){
#     if(length(intersect(mdl$library,upl))){
#       cat("\nModel",mname,"unavailable as package",intersect(mdl$library,upl),"unavailable for R 4.0")
#     }else{
#       acml <- c(acml,mname)
#       acmpl <- c(acmpl,mdl$library)
#     }
#   }
# }
# dput(acml)
# acmpl <- unique(acmpl)
# dput(acmpl)
acml <- c("gbm","nnet","glm")#,"rknn","svmPoly","svmRadial","rf")
  c("ada", "AdaBag", 
          "AdaBoost.M1", "adaboost",
          "avNNet", "awnb",
          "awtan", "bag", "bagEarth", "bagEarthGCV", "bagFDA", "bagFDAGCV",
          "bam", "bartMachine", "bayesglm", "binda", "blackboost", "BstLm", 
          "bstSm", "bstTree", "C5.0", "C5.0Cost", "C5.0Rules", "C5.0Tree", 
          "cforest", "CSimca", "ctree", "ctree2", "dda", "deepboost", "dnn", 
          "dwdLinear", "dwdPoly", "dwdRadial", "earth", "evtree", "extraTrees", 
          "fda", "FH.GBML", "FRBCS.CHI", "FRBCS.W", "gam", "gamboost", 
          "gamLoess", "gamSpline", "gaussprLinear", "gaussprPoly", "gaussprRadial", 
          "gbm_h2o", "gbm", "gcvEarth", "glm", "glmboost", "glmnet_h2o", 
          "glmnet", "glmStepAIC", "hda", "hdda", "hdrda", "J48", "JRip", 
          "kernelpls", "kknn", "knn", "lda", "lda2", "Linda", "LMT", "loclda", 
          "LogitBoost", "logreg", "lssvmLinear", "lssvmPoly", "lssvmRadial", 
          "lvq", "manb", "mda", "Mlda", "mlp", "mlpKerasDecay", "mlpKerasDecayCost", 
          "mlpKerasDropout", "mlpKerasDropoutCost", "mlpML", "mlpWeightDecay", 
          "mlpWeightDecayML", "monmlp", "msaenet", "multinom", "naive_bayes", 
          "nb", "nbDiscrete", "nbSearch", "nnet", "nodeHarvest", "null", 
          "OneR", "ordinalNet", "ordinalRF", "ORFlog", "ORFpls", "ORFridge", 
          "ORFsvm", "ownn", "pam", "parRF", "PART", "partDSA", "pcaNNet", 
          "pda", "pda2", "PenalizedLDA", "plr", "pls", "plsRglm", "polr", 
          "PRIM", "protoclass", "qda", "QdaCov", "randomGLM", "ranger", 
          "rbf", "rbfDDA", "Rborist", "rda", "regLogistic", "rf", "rFerns", 
          "RFlda", "rfRules", "rlda", "rmda", "rocc", "rotationForest", 
          "rotationForestCp", "rpart", "rpart1SE", "rpart2", "rpartCost", 
          "rpartScore", "RRF", "RRFglobal", "rrlda", "RSimca", "sda", "sdwd", 
          "simpls", "SLAVE", "slda", "smda", "snn", "sparseLDA", "spls", 
          "stepLDA", "stepQDA", "svmBoundrangeString", "svmExpoString", 
          "svmLinear", "svmLinear2", "svmLinear3", "svmLinearWeights", 
          "svmLinearWeights2", "svmPoly", "svmRadial", "svmRadialCost", 
          "svmRadialSigma", "svmRadialWeights", "svmSpectrumString", "tan", 
          "tanSearch", "treebag", "vglmAdjCat", "vglmContRatio", "vglmCumulative", 
          "widekernelpls", "wsrf", "xgbDART", "xgbLinear", "xgbTree", "xyf"
)

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
              "Real_FoodExpenditure","Real_Durable_Exp","NewArea_Name","cluster3")
D <- D[,SIS := ifelse(NewArea_Name %in% c("Sh_Zahedan","Sistan"),1,0)]

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


registerDoParallel(6)
getDoParWorkers()


X_train <- trainD[,predictornames,with=FALSE]
X_train[is.na(X_train)] <- 0
y_train <- trainD[[outcomename]]

X_test <- testD[,predictornames,with=FALSE]
X_test[is.na(X_test)] <- 0
y_test <- testD[[outcomename]]

X_SIS <- SISD[,predictornames,with=FALSE]
X_SIS[is.na(X_SIS)] <- 0
y_SIS <- SISD[[outcomename]]



describe(X_train)
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
SISRes <- c()
for(mdl in acml){
  try({
    cat("\n",mdl," ... ")
    objModel <- caret::train(X_train,y_train,
                             method = mdl,
                             trControl = myctl,
                             family = "binomial",
                             #    metric = "ROC",
                             preProcess = c("center","scale"))
    predictions_test <- predict(object = objModel, X_test, type="raw")
    t1 <- Sys.time()
    cat(as.character(t1),"\t[",as.character(t1-t0),"]")
    Results[mdl] <- postResample(pred=as.factor(predictions_test), obs=as.factor(y_test))[1]
    cat("\tAccuracy:",Results[mdl])
    
    predictions_SIS <- predict(object = objModel, X_SIS, type="raw")
    cat(as.character(t1),"\t[",as.character(t1-t0),"]")
    SISRes[mdl] <- postResample(pred=as.factor(predictions_SIS), obs=as.factor(y_SIS))[1]
    
    t0 <- t1
  })
}

print(Results)
print(SISRes)