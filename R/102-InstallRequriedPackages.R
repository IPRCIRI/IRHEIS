# 102-Install Required Packagees
# Builds the base data.table for households
#
# Copyright © 2016: Majid Einian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Install Required Packagees =====================================\n")




pkglist <- c("yaml","RODBC","readxl","tools","foreign","data.table",
             "stringr","XLConnect","sm","spatstat","ggplot2","haven",
             "rworldmap","rgdal","xlsx","Hmisc","reldist","dplyr","reshape2",
             "tcltk")

for(pkg in pkglist){
  if((eval(parse(text = paste0("require(",pkg,")")))==FALSE))
    install.packages(pkg)
}



endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)