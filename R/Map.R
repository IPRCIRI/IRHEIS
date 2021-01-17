#Engle's Map
# 
# Copyright © 2019:Majid Einian & Arin Shahbazian
# Licence: GPL-3
rm(list=ls())

library(data.table)
library(readxl)
library(rworldmap)
library(rgdal)
library("viridis") 

Engle <- data.table(read_excel("Engle.xlsx","Province")) # Data to be plotted, see the excell file

tfs <- paste0("+proj=lcc +lat_1=30 +lat_2=36 +lat_0=24 +lon_0=54 ",
              "+x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ",
              "+ellps=WGS84 +towgs84=0,0,0")      # Transformation to make all shapefiles in one coordinate system

o <- capture.output(Seas <- readOGR("Shapefiles/1385",   
                                    layer = "Sea"))           # Maps of Caspian Sea, Persian Gulf, & Oman Sea
Seas <- spTransform(Seas,CRS(tfs))

o <- capture.output(Lakes <- readOGR("Shapefiles/1385",       # Maps of Lakes
                                     layer = "Lake"))
Lakes <- spTransform(Lakes,CRS(tfs))

o <- capture.output(ProvMap <- readOGR("Shapefiles/OSM",      # Maps of provinces based on latest admin. areas. 
                                       layer = "Iran_AL4"))       # extracted from openstreetmaps.org
ProvMap <- spTransform(ProvMap,CRS(tfs))

MapData <- rworldmap::joinData2Map(Engle,nameMap="ProvMap",                       # Joining the data to map
                                   nameJoinIDMap="ENNAME",  # province name in shapefile
                                   nameJoinColumnData="NameEn2",  # province name in data excel
                                   verbose = TRUE)
MapData2<-MapData                             

plot(Seas,col="lightblue", border="lightblue")                      # plot seas
plot(Lakes[c(15,25),],add=TRUE,col="lightblue", border="lightblue") # plot just Urmia and Qom lakes

#Urban Areas
mapUrban <- rworldmap::mapPolys(MapData, add = TRUE, nameColumnToPlot = "MPI",
                                 colourPalette = "heat",numCats = 30,
                                #catMethod ="pretty", 
                                #catMethod ="fixedWidth",
                                #catMethod =c(0.04,0.06,0.08,0.1,0.12,0.14,0.16,0.18,0.2,0.22), #A
                                #catMethod =c(0.2,0.23,0.26,0.29,0.32,0.36,0.39,0.42,0.45,0.48), #AB
                                catMethod =c(0.04,0.06,0.08,0.1,0.12,0.14,0.16,0.19,0.24), #DE
                                #white2Black black2White palette heat topo
                                #terrain rainbow negpos8 negpos9
                                 mapTitle = paste0("Engle Rate in Urban Areas"),
                                 addLegend = FALSE)             # Plot data
do.call( addMapLegend, c( mapUrban
                          , legendLabels = "all"             # add legend
                          , digits = 2
                          , legendWidth = 0.7
                          , legendShrink = 0.7
                          , horizontal = TRUE))


