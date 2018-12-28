# 00-BuildDirectoryStructure
#
# Copyright © 2015: Majid Einian
# Licence: GPL-3

rm(list=ls())

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

dir.create(Settings$HEISPath,showWarnings = FALSE)
dir.create(Settings$HEISCompressedPath,showWarnings = FALSE)
dir.create(Settings$HEISAccessPath,showWarnings = FALSE)
dir.create(Settings$HEISRawPath,showWarnings = FALSE)
dir.create(Settings$HEISProcessedPath,showWarnings = FALSE)
dir.create(Settings$HEISResultsPath,showWarnings = FALSE)