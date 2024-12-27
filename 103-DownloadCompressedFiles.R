# 103-DownloaCompressedFiles.R
# downloads the RAR files that are not present in the folder 
# specified in Settings file
#
# Copyright © 2015: Majid Einian
# Copyright © 2016-2022: Majlis Research Center (The Research Center of Islamic Legislative Assembly)
# Licence: GPL-3
# For information on how to use and cite the results, see ResultsUsageLicence.md


################################################################################
################################################################################
###                                                                          ###
###                                                                          ###
###           THIS CODE DOES NOT WORK FOR NOW.                               ###
###           SCI PROVIDES DATA IN A MARKET.                                 ###
###           (FOR FREE NOW).                                                ###
###           I'VE NOT FOUND DIRECT DOWNLOAD LINKS YET.                      ###
###           PUT YOUR DOWNLAODED COMPRESSED (RAR) FILES                     ###
###           IN THE HEIS COMPRESSED FILES PATH                              ###
###           (SPECIFIED ON SETTINGS FILE)                                   ###
###                                                                          ###
###                                                                          ###
################################################################################
################################################################################



# rm(list=ls())
# 
# starttime <- proc.time()
# cat("\n\n================ DownloaCompressedFiles =====================================\n")
# 
# 
# library(yaml)
# library(readxl)
# 
# Settings <- yaml.load_file("Settings.yaml")
# 
# compressed_file_names_df <- read_excel(path = Settings$MetaDataFilePath,
#                                        sheet = Settings$MDS_CFN)
# 
# present_compressed_file_list <- list.files(Settings$HEISCompressedPath)
# x <- list.dirs(Settings$HEISCompressedPath, recursive = FALSE, full.names = FALSE)
# present_compressed_file_list <- setdiff(present_compressed_file_list, x)
# 
# years <- Settings$startyear:Settings$endyear
# 
# existing_file_list <- list.files(Settings$HEISCompressedPath)
# 
# ys <- compressed_file_names_df$Year %in% years
# needed_compressed_files_list <- compressed_file_names_df[ys,]$CompressedFileName
# 
# files_to_download <- setdiff(needed_compressed_files_list,existing_file_list)
# 
# if(length(files_to_download)>0){
#   urls <- paste0(Settings$RawDataWebAddress,files_to_download)
#   for(i in 1:length(files_to_download)){
#     cat(paste0("Downloading file ",i," / ",
#                length(files_to_download)," : ",files_to_download[i]),"\n")
#     try(download.file(urls[i], paste0(Settings$HEISCompressedPath, files_to_download[i]),mode="wb"))
#   }
# }else{
#     cat("All files in the range specified in Setting.yaml file are present, no need to download.")
# }
# 
# 
# endtime <- proc.time()
# 
# cat("\n\n============================\nIt took ")
# cat(endtime-starttime)
