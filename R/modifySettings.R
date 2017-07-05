library(yaml)
HEISPath           = "D:/HEIS/"
HEISCompressedPath = paste0(HEISPath,"DataCompressed/")
HEISAccessPath     = paste0(HEISPath,"DataAccess/")
HEISRawPath        = paste0(HEISPath,"DataRaw/")
HEISProcessedPath  = paste0(HEISPath,"DataProcessed/")
HEISResultsPath    = paste0(HEISPath,"DataResults/")
D80LinkDest        = paste0(HEISAccessPath,"D80Link.accdb")


Settings <- list(HEISPath          =HEISPath,
                 HEISCompressedPath=HEISCompressedPath,
                 HEISAccessPath    =HEISAccessPath,
                 HEISRawPath       =HEISRawPath,
                 HEISProcessedPath =HEISProcessedPath,
                 HEISResultsPath   =HEISResultsPath,
                 startyear=69,
                 endyear=94,
                 OS="windows",
                 RawDataWebAddress ="http://www.amar.org.ir/Portals/0/amarmozuii/hazinedaramad/",
                 MetaDataFilePath  = "../Data/MetaData.xlsx",
                 MDS_CFN           = "CompressedFileNames",              # MetaData Sheet: Compressed File Names
                 MDS_P1Cols        = "P1Cols",                           # MetaData Sheet: Columns of P1 Table
                 MDS_EC_A          = "EduCodes-A",
                 MDS_EC_B          = "EduCodes-B",
                 MDS_EC_C          = "EduCodes-C",
                 MDS_Food          = "FoodTables",
                 MDS_Loans         = "LoanTables",
                 MDS_House         = "HouseTables",
                 
                 MDS_Rough_Weights = "RegionWeights",
                 
                 weightsFile       ="../Data/AllWeights.rda",
                 D80LinkSource     ="../Data/D80Link.accdb",
                 D80LinkDest       =D80LinkDest
)
write(as.yaml(Settings),file = "Settings.yaml")