# Data
This folder contains data that are needed to process Iranian Household Expenditures and Income Surveys data. Provided files are described below.

AllWeights.rda contains sampling weights of all Households in samples from 1376 to 1393. (I am trying to get the data for 1394). Weights data are extracted from "Summary" files provided by SCI. SCI provides "Summary" files with some price. The copyright status of these files and specially weights data are unknown.
Sampling weights for years before 1376 are not provided by SCI, a rough estimated can be based on rural and urban population of each province or rural and urban population of the country (these files do not include such estimates). 

D80Link.accdb provides linked tables to 1380 data with abnormal table names.

CompressedFileNames.csv provides compressed files names for each year. For now it is XX.rar for year XX,  but it is flexible to any SCI naming scheme.

You can download the AllWeights.rda without cloning the git repository from here:
https://cdn.rawgit.com/einian85/HouseHoldSurvey/df32dfb1/Data/AllWeights.rda
