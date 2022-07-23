# 101-BuildDirectoryStructure
# Builds the needed directory structure
#
# Copyright © 2016: Majid Einian
# Copyright © 2016-2022: Majlis Research Center (The Research Center of Islamic Legislative Assembly)
# Licence: GPL-3
# For information on how to use and cite the results, see ResultsUsageLicence.md

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ BuildDirectoryStructure =====================================\n")



library(yaml)
Settings <- yaml.load_file("Settings.yaml")

dir.create(Settings$HEISPath,showWarnings = FALSE)
dir.create(Settings$HEISCompressedPath,showWarnings = FALSE)
dir.create(Settings$HEISAccessPath,showWarnings = FALSE)
dir.create(Settings$HEISRawPath,showWarnings = FALSE)
dir.create(Settings$HEISProcessedPath,showWarnings = FALSE)
dir.create(Settings$HEISResultsPath,showWarnings = FALSE)


endtime <- proc.time()

cat("\n\n============================\nIt took ")
cat(endtime-starttime)