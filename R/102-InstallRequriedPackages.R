# 102-Install Required Packagees
#
# Copyright © 2016: Majid Einian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Install Required Packages =====================================\n")




pkglist <- c("yaml","RODBC","readxl","tools","foreign","data.table",
             "stringr","XLConnect","sm",#"spatstat",
             "ggplot2","haven",
             "rworldmap","rgdal","xlsx","Hmisc","reldist","dplyr","reshape2",
             "tcltk","janitor",
             "caret","caretEnsemble",
             "ada", "plyr", "adabag", "fastAdaboost",
             #"adaptDA", 
             "nnet", 
             "bnclassify", "caret", "earth", "mda", "mgcv", "bartMachine", 
             "arm", "binda", "party", "mboost", "partykit", "bst", "C50", 
             #"CHAID", 
             "rrcov", "rrcovHD",
             #"sparsediscrim", 
             "deepboost", "deepnet", 
             "kerndwd", "kernlab", 
             #"elmNN", 
             "evtree", "extraTrees", "frbs", 
             "import", "gam", "h2o", "gbm", "glmnet", "Matrix", "MASS", 
             #"gpls", 
             "hda", "HDclassif", "RWeka", "pls", "kknn", "klaR", 
             #"logicFS", 
             "caTools", "LogicReg", "class", "HiDimDA", "RSNNS", "keras", 
             #"FCNN4R", 
             "monmlp", "msaenet", 
             #"mxnet", 
             "naivebayes", "nodeHarvest", 
             "ordinalNet", "e1071", "ranger", "dplyr", "ordinalForest", "obliqueRF", 
             "snn", "pamr", "randomForest", "foreach", "partDSA", "penalizedLDA", 
             "stepPlr", "plsRglm", "supervisedPRIM", "proxy", "protoclass", 
             "randomGLM", "Rborist", "LiblineaR", "rFerns", "inTrees", "robustDA", 
             "rocc", "rotationForest", "rpart", "rpartScore", "RRF", "rrlda", 
             "sda", "sdwd", "ipred", "sparseLDA", "spls",
             #"vbmp",
             "VGAM", 
             "wsrf", "xgboost", "kohonen",
             "pROC","glmnet",
             "factoextra")

for(pkg in pkglist){
  if((eval(parse(text = paste0("require(",pkg,")")))==FALSE))
    install.packages(pkg)
}



endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)