# 190-InterDecileMovement.R
# The code identifies the movement of houselolds between deciles  
# and also over and under the Poverty and FoodPoverty Lines.
# 
# Copyright © 2018-2020:Salman Farajnia
# Licence: GPL-3

rm(list=ls())
starttime <- proc.time()
library(yaml)
Settings <- yaml.load_file("Settings.yaml")
library(readxl)
library(xlsx)
library(spatstat)
library(data.table)

library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(networkD3)

# This functions gets text and gives it back as a R Code.
ep <- function(t){
  return((parse(text=t)))
}

# This Function merges a public_sector column to data. public_sector is
# a dummy variable that is 1 if at least one member of the household works
# in public sector and 0 otherwise.
add_public_sector <- function(year, datatable){
  library(yaml)
  Settings <- yaml.load_file("Settings.yaml")
  library(haven)
  library(readxl)
  library(data.table)
  library(stringr)
  
  P1Cols <- data.table(read_excel(Settings$MetaDataFilePath, Settings$MDS_P1Cols))
  
  pub_wage_meta <- data.table(read_excel(Settings$MetaDataFilePath,Settings$MDS_PubWage))
  buss_wage_meta <- data.table(read_excel(Settings$MetaDataFilePath,Settings$MDS_BussInc))
  
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  load(file=paste0(Settings$HEISRawPath,"Y",year,"Raw.rda"))

  pub_wage_meta_y <-pub_wage_meta[Year==year]
  buss_wage_meta_y<- buss_wage_meta[Year==year]
  pubw_t_name <-pub_wage_meta_y$Table
  buss_t_name <- buss_wage_meta_y$Table
  Upubt <- Tables[[paste0("U",year,pubw_t_name)]]
  Rpubt <- Tables[[paste0("R",year,pubw_t_name)]]
  Ubusst <- Tables[[paste0("U",year,buss_t_name)]]
  Rbusst <- Tables[[paste0("R",year,buss_t_name)]]
  Tpubt <- rbind(Upubt, Rpubt)
  Tbusst <- rbind(Ubusst, Rbusst)
  #Twage
  for(n in names(Tpubt)){
    x <- which(pub_wage_meta_y==n)
    if(length(x)>0)
      setnames(Tpubt,n,names(pub_wage_meta_y)[x])
  }
  for(n in names(Tbusst)){
    x <- which(buss_wage_meta_y==n)
    if(length(x)>0)
      setnames(Tbusst,n,names(buss_wage_meta_y)[x])
  }
  Twage = rbind(Tpubt, Tbusst, fill=TRUE)
  # Twage[,HHID:=as.character(HHID)]
  Twage[, pub_sector:= fifelse(WageSector==1,1,0,na = 0)]
  Twage2 <- Twage[,c('HHID','pub_sector')]
  Twage3 <- Twage2[,.(public_sec= fifelse(sum(pub_sector)>0,1,0)), by='HHID']
  datatable <- merge(datatable, Twage3, by = "HHID", all.x=TRUE)
  return(datatable) 
}

draw_flow_rectangle <- function(DT,year){
  colnames(DT) <- c("source", "target", "value") 
  DT[,source:= paste0("Decile",source,"_",year)]
  DT[,target:= paste0("Decile",target,"_",year+1)]
  
  # create a node data frame: it lists every entities involved in the flow
  nodes <- data.frame(name=c(as.character(DT$source), as.character(DT$target)) %>% unique())
  
  # encode the nodes to numbers 
  DT$IDsource=match(DT$source, nodes$name)-1 
  DT$IDtarget=match(DT$target, nodes$name)-1
  # prepare colour scale
  ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'
  # Make the Network
  sn<- sankeyNetwork(Links = DT, Nodes = nodes,
                Source = "IDsource", Target = "IDtarget",
                Value = "value", NodeID = "name", 
                sinksRight=FALSE, colourScale=ColourScal, nodeWidth=40, fontSize=13, nodePadding=20)
  
  saveNetwork(sn, file = "C:/IRHEIS/decile movement/flow.html", selfcontained = TRUE)  
}


public_boolean = TRUE
# import yearly DataTables ,keep needed columns
# and append them to create total DataTable
years = Settings$startyear:Settings$endyear
years_test <- 90:91
data_total <- data.table()
wb_decile <- createWorkbook()
wb_poor <- createWorkbook()
wb_foodpoor <- createWorkbook()
wb_movement_ts <- createWorkbook()
movement_time_series = data.table(base_year=integer(),target_year=integer(),
                                  double_poor=numeric(), double_poor_p=numeric(),
                                  poor_rescue=numeric(),fall_to_poor = numeric(),
                                  doulble_fpoor=numeric(), doulble_fpoor_p=numeric(),
                                  fpoor_rescue=numeric(), fall_to_fpoor= numeric())
for(year in years){
  if (year %in% list(91,96,98)) next
  cat(paste0("\n------------------------------\nYear:",year,"-->",year+1,"\n"))
  #  load base year
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FinalPoor.rda"))
  base_year = MD[,c("HHID", "Year", "Decile", "Percentile", "FinalPoor", "FoodPoor",
               "Size", "Weight")]
  if (public_boolean){
    base_year = add_public_sector(year, base_year)
  }
  # load target year
  load(file=paste0(Settings$HEISProcessedPath,"Y",year+1,"FinalPoor.rda"))
  target_year = MD[,c("HHID", "Year", "Decile","Percentile", "FinalPoor", "FoodPoor",
                    "Size", "Weight")]
  if (public_boolean){
    target_year = add_public_sector(year+1, target_year)
  }
  data_total<-rbind(base_year,target_year)

  # duplicated HHIDs are the families who are in the dataset for two
  # consecutive year
  data_total[, duplicates := .N > 1, by = c("HHID")]
  data_total[, public_sec:=fifelse(is.na(public_sec),0,public_sec)]
  # adjust the weight of the duplicated families to represent 
  # the whole dateset.
  if (public_boolean){
    data_total[, weight_tot_panel := sum(Weight*Size*public_sec), by = c("Year", "duplicates")]
    data_total[, weight_tot := sum(Weight*Size*public_sec), by = c("Year")]
  } else{
    data_total[, weight_tot_panel := sum(Weight*Size), by = c("Year", "duplicates")]
    data_total[, weight_tot := sum(Weight*Size), by = c("Year")]
  }
  data_total[, Weight_panel:= Weight*weight_tot/weight_tot_panel]
  
  # remove families who were present only one-time in the datasets of
  # the two consecutive years
  data_total<-data_total[duplicates==TRUE][order(HHID, Year)]


  #reshape long panel to wide panel data
  data_wide<-reshape(data_total, direction = "wide", sep = "_",
                     v.names  = c("public_sec","Size","Decile","Percentile","FinalPoor",
                                  "FoodPoor","Weight_panel", "Weight"),
                     drop = c("weight_tot", "weight_tot_panel", "duplicates"),
                     timevar = "Year", idvar = "HHID")
  text <- paste0("data_wide <- data_wide[public_sec_",year,"==1 & public_sec_",year+1,"==1]")
  if (public_boolean){
    eval(ep(text)) 
  }
  # adjust weight and size of the households
  text <- paste0("Weight_panel:= (Weight_panel_",year,"+Weight_panel_",year+1,")/2")
  data_wide[,eval(ep(text))]
  
  text <- paste0("Size:= (Size_",year,"+Size_",year+1,")/2")
  data_wide[,eval(ep(text))]
  
  ######PART 1: Movement between DECILES########################################
  # create movement columns. {first column=status in base year}
  # {second column= status in target year} 
  # {third column= percentage of individuals in each part who change their status}
  text_base<-paste0("Decile_",year)
  text_target<-paste0("Decile_",year+1)
  data_wide[,pop:=sum(Weight_panel*Size),by=c(text_base)]
  movement_decile<-data_wide[,round(sum(Weight_panel*Size/pop*100), digits=1),by=c(text_base, text_target)]
  movement_decile<-movement_decile[order(eval(ep(text_base)), eval(ep(text_target)))]
  
  # draw_flow_rectangle(movement_decile,year)
  decile_matrix<-as.data.frame.matrix(xtabs(V1~., movement_decile))
  rownames(decile_matrix) = paste0(year,"_",rownames(decile_matrix))
  colnames(decile_matrix) = paste0(year+1,"_",colnames(decile_matrix))
  sheet_name =  paste0(year,"_",year+1)
  sheet = createSheet(wb_decile, sheetName = sheet_name)
  addDataFrame(decile_matrix,sheet = sheet)
  
  ######PART 2: Movement between Poor and non_Poor###################################
  # create movement columns. {first column=status in base year}
  # {second column= status in target year} 
  # {third column= percentage of individuals in each part who change their status}
  text_base<-paste0("FinalPoor_",year)
  text_target<-paste0("FinalPoor_",year+1)
  data_wide[,pop:=sum(Weight_panel*Size),by=c(text_base)]
  movement_poor<-data_wide[,round(sum(Weight_panel*Size), digits=1),by=c(text_base, text_target)]
  movement_poor<-movement_poor[order(eval(ep(text_base)), eval(ep(text_target)))]
  poor_matrix<-as.data.frame.matrix(xtabs(V1~., movement_poor))
  
  # add new columns to movement matrix
  poor_matrix["2"] = poor_matrix["0"]
  poor_matrix["3"] = poor_matrix["0"]
  poor_matrix["4"] = poor_matrix["0"]
  poor_matrix["5"] = poor_matrix["0"]
  poor_matrix["6"] = poor_matrix["0"]
  poor_matrix["7"] = poor_matrix["0"]
  
  # create a matrix that its elements are in percent 
  # and the sum of the columns is 100%
  poor_matrix[1,3] = poor_matrix[1,1]/(poor_matrix[1,1]+poor_matrix[2,1])*100
  poor_matrix[2,3] = poor_matrix[2,1]/(poor_matrix[1,1]+poor_matrix[2,1])*100
  poor_matrix[1,4] = poor_matrix[1,2]/(poor_matrix[1,2]+poor_matrix[2,2])*100
  poor_matrix[2,4] = poor_matrix[2,2]/(poor_matrix[1,2]+poor_matrix[2,2])*100

  # create a matrix that its elements are in percent 
  # and the sum of the rows is 100%
  poor_matrix[1,5] = poor_matrix[1,1]/(poor_matrix[1,1]+poor_matrix[1,2])*100
  poor_matrix[1,6] = poor_matrix[1,2]/(poor_matrix[1,1]+poor_matrix[1,2])*100
  poor_matrix[2,5] = poor_matrix[2,1]/(poor_matrix[2,1]+poor_matrix[2,2])*100
  poor_matrix[2,6] = poor_matrix[2,2]/(poor_matrix[2,1]+poor_matrix[2,2])*100

  
  # create a matrix that its elements are in percent 
  # and the sum of the all of its elements is 100%
  sum_pop = (poor_matrix[1,1]+poor_matrix[1,2]+
               poor_matrix[2,1]+poor_matrix[2,2])
  poor_matrix[1,7] = poor_matrix[1,1]/ sum_pop*100
  poor_matrix[1,8] = poor_matrix[1,2]/sum_pop*100
  poor_matrix[2,7] = poor_matrix[2,1]/sum_pop*100
  poor_matrix[2,8] = poor_matrix[2,2]/sum_pop*100  
  poor_matrix = round(poor_matrix, digits = 1)
  
  rownames(poor_matrix) = c("NotPoor","Poor")
  colnames(poor_matrix) = c("NotPoor","Poor","NotPoor_p1","Poor_p1",
                            "NotPoor_p2","Poor_p2","NotPoor_p3","Poor_p3")
  rownames(poor_matrix) = paste0(year,"_",rownames(poor_matrix))
  colnames(poor_matrix) = paste0(year+1,"_",colnames(poor_matrix))
  sheet_name =  paste0(year,"_",year+1)
  sheet = createSheet(wb_poor, sheetName = sheet_name)
  addDataFrame(poor_matrix,sheet = sheet)
  
  ######PART 3: Movement between FoodPoor and non_FoodPoor##########################
  # create movement columns. {first column=status in base year}
  # {second column= status in target year} 
  # {third column= percentage of individuals in each part who change their status}
  text_base<-paste0("FoodPoor_",year)
  text_target<-paste0("FoodPoor_",year+1)
  data_wide[,pop:=sum(Weight_panel*Size),by=c(text_base)]
  movement_foodpoor<-data_wide[,round(sum(Weight_panel*Size), digits=1),by=c(text_base, text_target)]
  movement_foodpoor<-movement_foodpoor[order(eval(ep(text_base)), eval(ep(text_target)))]
  
  foodpoor_matrix<-as.data.frame.matrix(xtabs(V1~., movement_foodpoor))
  
  # add new columns to movement matrix
  foodpoor_matrix["2"] = foodpoor_matrix["0"]
  foodpoor_matrix["3"] = foodpoor_matrix["0"]
  foodpoor_matrix["4"] = foodpoor_matrix["0"]
  foodpoor_matrix["5"] = foodpoor_matrix["0"]
  foodpoor_matrix["6"] = foodpoor_matrix["0"]
  foodpoor_matrix["7"] = foodpoor_matrix["0"]
  
  # create a matrix that its elements are in percent 
  # and the sum of the columns is 100%  
  foodpoor_matrix[1,3] = foodpoor_matrix[1,1]/(foodpoor_matrix[1,1]+foodpoor_matrix[2,1])*100
  foodpoor_matrix[2,3] = foodpoor_matrix[2,1]/(foodpoor_matrix[1,1]+foodpoor_matrix[2,1])*100
  foodpoor_matrix[1,4] = foodpoor_matrix[1,2]/(foodpoor_matrix[1,2]+foodpoor_matrix[2,2])*100
  foodpoor_matrix[2,4] = foodpoor_matrix[2,2]/(foodpoor_matrix[1,2]+foodpoor_matrix[2,2])*100

  # create a matrix that its elements are in percent 
  # and the sum of the rows is 100%  
  foodpoor_matrix[1,5] = foodpoor_matrix[1,1]/(foodpoor_matrix[1,1]+foodpoor_matrix[1,2])*100
  foodpoor_matrix[1,6] = foodpoor_matrix[1,2]/(foodpoor_matrix[1,1]+foodpoor_matrix[1,2])*100
  foodpoor_matrix[2,5] = foodpoor_matrix[2,1]/(foodpoor_matrix[2,1]+foodpoor_matrix[2,2])*100
  foodpoor_matrix[2,6] = foodpoor_matrix[2,2]/(foodpoor_matrix[2,1]+foodpoor_matrix[2,2])*100
  foodpoor_matrix = round(foodpoor_matrix, digits = 1)

  # create a matrix that its elements are in percent 
  # and the sum of the all of its elements is 100%
  sum_pop = (foodpoor_matrix[1,1]+foodpoor_matrix[1,2]+
               foodpoor_matrix[2,1]+foodpoor_matrix[2,2])
  foodpoor_matrix[1,7] = foodpoor_matrix[1,1]/ sum_pop*100
  foodpoor_matrix[1,8] = foodpoor_matrix[1,2]/sum_pop*100
  foodpoor_matrix[2,7] = foodpoor_matrix[2,1]/sum_pop*100
  foodpoor_matrix[2,8] = foodpoor_matrix[2,2]/sum_pop*100  
  foodpoor_matrix = round(foodpoor_matrix, digits = 1)  
  
  rownames(foodpoor_matrix) = c("NotFoodPoor","FoodPoor")
  colnames(foodpoor_matrix) = c("NotFoodPoor","FoodPoor","NotFoodPoor_p1","FoodPoor_p1",
                                "NotFoodPoor_p2","FoodPoor_p2","NotFoodPoor_p3","FoodPoor_p3")
  rownames(foodpoor_matrix) = paste0(year,"_",rownames(foodpoor_matrix))
  colnames(foodpoor_matrix) = paste0(year+1,"_",colnames(foodpoor_matrix))
  sheet_name =  paste0(year,"_",year+1)
  sheet = createSheet(wb_foodpoor, sheetName = sheet_name)
  addDataFrame(foodpoor_matrix,sheet = sheet)

  ######PART 4: Movement time series###################################
  # create Time-series of important cells in the movement matrices.
  double_poor = poor_matrix[2,2]
  double_poor_p = poor_matrix[2,8]
  poor_rescue = poor_matrix[2,5]
  fall_to_poor = poor_matrix [1,6]
  double_fpoor = foodpoor_matrix[2,2]
  double_fpoor_p = foodpoor_matrix[2,8]
  fpoor_rescue = foodpoor_matrix[2,5]
  fall_to_fpoor = foodpoor_matrix [1,6]
  movement_time_series<-rbind(movement_time_series,
                              list(year,year+1,double_poor,double_poor_p,poor_rescue,
                                   fall_to_poor,double_fpoor,double_fpoor_p,
                                   fpoor_rescue,fall_to_fpoor))

  
  # here I check the reliability of the data.
  #if the households which have the same ID in the two consecutive years
  # are really the same households, then their size may not change
  # considerably. so by drawing the histogram of the difference of the
  # size of a household in this two years I anticipate a distribution of mean Zero
  # which is concenterated near zero.
  base0 = paste0("Size_",year)
  base1 = paste0("Size_",year+1)
  data_wide$diff_size = data_wide[,..base0] - data_wide[,..base1]
  png(file=paste0(Settings$HEISResultsPath,year,"hist.png"))
  hist(data_wide$diff_size, freq=FALSE, ylim = c(0,0.9))
  dev.off()
}
saveWorkbook(wb_decile, paste0(Settings$HEISResultsPath,"decile_movement.xlsx"))
saveWorkbook(wb_poor, paste0(Settings$HEISResultsPath,"poor_movement.xlsx"))
saveWorkbook(wb_foodpoor, paste0(Settings$HEISResultsPath,"foodpoor_movement.xlsx"))
sheet = createSheet(wb_movement_ts, sheetName = 'movemetn time-series')
addDataFrame(movement_time_series, sheet = sheet)
saveWorkbook(wb_movement_ts, paste0(Settings$HEISResultsPath,"movement_time_series.xlsx"))





