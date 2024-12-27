# 102-Install Required Packagees
#
# Copyright © 2016: Majid Einian
# Copyright © 2016-2022: Majlis Research Center (The Research Center of Islamic Legislative Assembly)
# Licence: GPL-3
# For information on how to use and cite the results, see ResultsUsageLicence.md

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Install Required Packages =====================================\n")




pkglist <- c("yaml","RODBC","readxl","tools","foreign","data.table",
             "stringr","sm",#"XLConnect","spatstat",
             "ggplot2","haven",
             "rworldmap","rgdal","xlsx","Hmisc","reldist","dplyr","reshape2",
             "tcltk","janitor","isotone"
             )

for(pkg in pkglist){
  if((eval(parse(text = paste0("require(",pkg,")")))==FALSE))
    install.packages(pkg)
}



endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)