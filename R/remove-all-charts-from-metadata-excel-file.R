rm(list=ls())
library(xlsx)

library(yaml)
Settings <- yaml.load_file("Settings.yaml")


wb = loadWorkbook(Settings$MetaDataFilePath)
Sheets <- getSheets(wb)
n <- names(Sheets)
n2 <- n[grepl("Chart.",n)]
for(sname in n2)
  removeSheet(wb, sheetName = sname)
saveWorkbook(wb, Settings$MetaDataFilePath)
