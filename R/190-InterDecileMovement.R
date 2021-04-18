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

ep <- function(t){
  return((parse(text=t)))
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


# import yearly DataTables ,keep needed columns 
# and append them to create total DataTable
years = Settings$startyear:Settings$endyear
years = append(years, 89, after = 0)
years_test <- 89:90
data_total <- data.table()
wb_decile <- createWorkbook()
wb_poor <- createWorkbook()
wb_foodpoor <- createWorkbook()
for(year in years){
  if (year %in% list(91,96,98)) next
  cat(paste0("\n------------------------------\nYear:",year,"-->",year+1,"\n"))
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FoodPoor.rda"))
  base_year = MD[,c("HHID", "Year", "Decile", "Percentile", "InitialPoor", "FoodPoor",
               "Size", "Weight")]
  load(file=paste0(Settings$HEISProcessedPath,"Y",year+1,"FoodPoor.rda"))
  target_year = MD[,c("HHID", "Year", "Decile","Percentile", "InitialPoor", "FoodPoor",
                    "Size", "Weight")]
  data_total<-rbind(base_year,target_year)

  # duplicated HHIDs are the families who are in the dataset for two
  # consecutive year
  data_total[, duplicates := .N > 1, by = c("HHID")]
  
  # adjust the weight of the duplicated families to represent 
  # the whole dateset.
  data_total[, weight_tot_panel := sum(Weight), by = c("Year", "duplicates")]
  data_total[, weight_tot := sum(Weight), by = c("Year")]
  data_total[, Weight_panel:= Weight*weight_tot/weight_tot_panel]
  
  # remove families who were present only one-time in the dataset
  # in two consecutive years
  data_total<-data_total[duplicates==TRUE][order(HHID, Year)]
  
  #reshape long panel to wide panel data
  data_wide<-reshape(data_total, direction = "wide", sep = "_",
                     v.names  = c("Size","Decile","Percentile","InitialPoor",
                                  "FoodPoor","Weight_panel", "Weight"),
                     drop = c("weight_tot", "weight_tot_panel", "duplicates"),
                     timevar = "Year", idvar = "HHID")
  
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
  text_base<-paste0("InitialPoor_",year)
  text_target<-paste0("InitialPoor_",year+1)
  data_wide[,pop:=sum(Weight_panel*Size),by=c(text_base)]
  movement_poor<-data_wide[,round(sum(Weight_panel*Size/pop*100), digits=1),by=c(text_base, text_target)]
  movement_poor<-movement_poor[order(eval(ep(text_base)), eval(ep(text_target)))]
  
  poor_matrix<-as.data.frame.matrix(xtabs(V1~., movement_poor))
  rownames(poor_matrix) = c("NotPoor","Poor")
  colnames(poor_matrix) = c("NotPoor","Poor")
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
  movement_foodpoor<-data_wide[,round(sum(Weight_panel*Size/pop*100), digits=1),by=c(text_base, text_target)]
  movement_foodpoor<-movement_foodpoor[order(eval(ep(text_base)), eval(ep(text_target)))]
  
  foodpoor_matrix<-as.data.frame.matrix(xtabs(V1~., movement_foodpoor))
  rownames(foodpoor_matrix) = c("NotFoodPoor","FoodPoor")
  colnames(foodpoor_matrix) = c("NotFoodPoor","FoodPoor")
  rownames(foodpoor_matrix) = paste0(year,"_",rownames(foodpoor_matrix))
  colnames(foodpoor_matrix) = paste0(year+1,"_",colnames(foodpoor_matrix))
  sheet_name =  paste0(year,"_",year+1)
  sheet = createSheet(wb_foodpoor, sheetName = sheet_name)
  addDataFrame(foodpoor_matrix,sheet = sheet)
}
saveWorkbook(wb_decile, paste0(Settings$HEISResultsPath,"decile_movement.xlsx"))
saveWorkbook(wb_poor, paste0(Settings$HEISResultsPath,"poor_movement.xlsx"))
saveWorkbook(wb_foodpoor, paste0(Settings$HEISResultsPath,"foodpoor_movement.xlsx"))





