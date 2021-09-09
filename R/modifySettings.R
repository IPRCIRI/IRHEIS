library(yaml)
OS <- ifelse(version$os=="linux-gnu","Linux","Windows")
if(OS=="Linux"){
  HEISPath         = "/media/majid/Document/HEIS/"
}else{
 HEISPath           = "C:/HEIS/"
}
HEISCompressedPath = paste0(HEISPath,"DataCompressed/")
HEISAccessPath     = paste0(HEISPath,"DataAccess/")
HEISRawPath        = paste0(HEISPath,"DataRaw/")
HEISProcessedPath  = paste0(HEISPath,"DataProcessed/")
HEISResultsPath    = paste0(HEISPath,"DataResults/")
D80LinkDest        = paste0(HEISAccessPath,"D80Link.accdb")


nw <- c("OriginalFoodExpenditure","FoodOtherExpenditure", "Cigar_Exp",
        "Cloth_Exp","Amusement_Exp", "Communication_Exp", "House_Exp",
        "Energy_Exp", "Furniture_Exp", "Hotel_Exp","Restaurant_Exp",
        "Hygiene_Exp", "Transportation_Exp", "Other_Exp",
        "Add_to_NonDurable" ,"OwnedDurableItemsDepreciation")
w <- c(nw, "Medical_Exp",
       "Durable_NoDep","Durable_Emergency")

Settings <- list(HEISPath          =HEISPath,
                 HEISCompressedPath=HEISCompressedPath,
                 HEISAccessPath    =HEISAccessPath,
                 HEISRawPath       =HEISRawPath,
                 HEISProcessedPath =HEISProcessedPath,
                 HEISResultsPath   =HEISResultsPath,
                 HEISWeightsPath   ="../Data/SamplingWeights/",
                 HEISWeightFileName="HHWeights",
                 HEISCountyCodePath="../Data/CountyCodes8791/",
                 HEISCountyCodeFileName="ShCode",
                 startyear=99,
                 endyear=99,
                 baseBundleyear=95,
                 OS=OS,
                 # RawDataWebAddress ="http://www.amar.org.ir/Portals/0/amarmozuii/hazinedaramad/",
                 RawDataWebAddress ="https://www.amar.org.ir/Portals/0/amarmozuii/re_DataRaw_63_95_Info/",
                 MetaDataFilePath  = "../Data/MetaData.xlsx",
                 GeoInfoFilePath   = "../Data/GeoInfo.Shahrestan.xlsx",
                 InflationDataFilePath = "../Data/inflation.xlsx",
                 MDS_CFN           = "CompressedFileNames",              # MetaData Sheet: Compressed File Names
                 MDS_P1Cols        = "P1Cols",                           # MetaData Sheet: Columns of P1 Table
                 MDS_P2Cols        = "P2Cols",
                 MDS_Geo2          = "Geo2",
                 MDS_Geo4          = "Geo4",
                 MDS_GeoX          = "GeoX",   # ClusterInfo_Old
                 MDS_GeoX_New      = "GeoX_New",   # ClusterInfo_New
                 MDS_EC_A          = "EduCodes-A",
                 MDS_EC_B          = "EduCodes-B",
                 MDS_EC_C          = "EduCodes-C",
                 MDS_EC_D          = "EduCodes-D",
                 MDS_Food          = "FoodTables",
                 MDS_FoodGroups    = "FoodGroupTables",
                 MDS_InfantMilk    = "InfantMilkTables",
                 MDS_Cigar         = "CigarTables",
                 MDS_Cloth         = "ClothTables",
                 MDS_HouseandEnergy = "HouseandEnergyTables",
                 MDS_Energy        = "EnergyTables",
                 MDS_Furniture     = "FurnitureTables",
                 MDS_Transportation= "TransportationTables",
                 MDS_Communication = "CommunicationTables",
                 MDS_Amusement     = "AmusementTables",
                 MDS_Hotel         = "HotelTables",
                 MDS_Restaurant    = "RestaurantTables",
                 MDS_Other         = "OtherTables",
                 MDS_Durable       = "DurableTables",
                 MDS_DurableGroups = "DurableGroups",
                 MDS_DurableItems  = "DurableItems",
                 MDS_DurableItemsDepr= "DurableItemsDepr",
                 MDS_Finance       = "FinanceTables",
                 MDS_Investment    = "InvestmentTables",
                 MDS_Medical       = "MedicalTables",
                 MDS_Hygiene       = "HygieneTables",
                 MDS_Loans         = "LoanTables",
                 MDS_FinServices   = "FinServicesTables",
                 MDS_House         = "HouseTables",
                 MDS_Education     = "EducationTables",
                 MDS_Insurance     = "InsuranceTables",
                 MDS_ActivityState = "ActivityStateTables",
                 MDS_PubWage       = "PubWageTable",
                 MDS_PrvWage       = "PrvWageTable",
                 MDS_AgriInc       = "AgriIncTable",
                 MDS_BussInc       = "BussIncTable",
                 MDS_OtherInc     = "OthrIncTable",
                 MDS_Retirement    = "RetirementTables",
                 MDS_Interest      = "InterestTables",
                 MDS_Rent          = "RentTables",
                 MDS_Aid           = "AidTables",
                 MDS_Homemade      = "HomemadeTables",
                 MDS_Intra         = "IntraTables",
                 MDS_Subsidy       = "SubsidyTable",
                 MDS_Rough_Weights = "RegionWeights",
                 
                 OutFoodKCXShare      = 0.3, # Share of Kilo-Calory content in Expenses of outdoor food
                 OutFoodKCXShare12    = 0.4, # Share of Kilo-Calory content in Expenses of outdoor food
                 OutFoodKCXShare3456  = 0.2, # Share of Kilo-Calory content in Expenses of outdoor food
                 OutFoodKCXShare78910 = 0.1, # Share of Kilo-Calory content in Expenses of outdoor food
                 KCaloryNeed_Adult = 2100,
                 KCaloryNeed_Adult_WorldBank = 2100,
                 KCaloryNeed_Adult_NutritionInstitute = 2400,
                 KCaloryNeed_Child = 1800,
                 KCaloryNeed_B1 = 850,
                 KCaloryNeed_G1 = 780,
                 KCaloryNeed_B2 = 1250,
                 KCaloryNeed_G2 = 1190,
                 KCaloryNeed_B3 = 1430,
                 KCaloryNeed_G3 = 1330,
                 KCaloryNeed_B4 = 1560,
                 KCaloryNeed_G4 = 1440,
                 KCaloryNeed_B5 = 1690,
                 KCaloryNeed_G5 = 1540,
                 KCaloryNeed_B6 = 1980,
                 KCaloryNeed_G6 = 1730,
                 KCaloryNeed_B7 = 2370,
                 KCaloryNeed_G7 = 2040,
                 KCaloryNeed_B8 = 2700,
                 KCaloryNeed_G8 = 2120,
                 KCaloryNeed_B9 = 2460,
                 KCaloryNeed_G9 = 1990,
                 KCaloryNeed_B10 = 2010,
                 KCaloryNeed_G10 = 1780,
                 KCaloryNeed_NutInst_B1 = 705,
                 KCaloryNeed_NutInst_G1 = 643,
                 KCaloryNeed_NutInst_B2 = 948,
                 KCaloryNeed_NutInst_G2 = 865,
                 KCaloryNeed_NutInst_B3 = 1120,
                 KCaloryNeed_NutInst_G3 = 1047,
                 KCaloryNeed_NutInst_B4 = 1360,
                 KCaloryNeed_NutInst_G4 = 1242,
                 KCaloryNeed_NutInst_B5 = 1927,
                 KCaloryNeed_NutInst_G5 = 1782,
                 KCaloryNeed_NutInst_B6 = 3036,
                 KCaloryNeed_NutInst_G6 = 2434,
                 KCaloryNeed_NutInst_B7 = 3029,
                 KCaloryNeed_NutInst_G7 = 2300,
                 KCaloryNeed_NutInst_B8 = 2900,
                 KCaloryNeed_NutInst_G8 = 2200,
                 KCaloryNeed_NutInst_B9 = 2350,
                 KCaloryNeed_NutInst_G9 = 2000,
                 KCaloryNeed_lactating = 500,
                 nw=nw,
                 w=w,
                 InitialPoorPercentile= 1:35,
                 SectorsNumbers    = 1:5,
                 SectorsNames      = c("Public","Cooperative","Private","BussP","Agri"),
                 D80LinkSource     ="../Data/D80Link.accdb",
                 D80LinkDest       =D80LinkDest
)
write(as.yaml(Settings),file = "Settings.yaml")