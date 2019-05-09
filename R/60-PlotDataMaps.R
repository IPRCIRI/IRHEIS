#Poverty Map
# 
# Copyright © 2018:Majid Einian
# Licence: GPL-3
 rm(list=ls())

library(data.table)
library(readxl)
library(rworldmap)
library(rgdal)


Poverty <- data.table(read_excel("Data/Poverty.xlsx","Province")) # Data to be plotted, see the excell file

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

MapData <- rworldmap::joinData2Map(Poverty,                       # Joining the data to map
                                       nameMap="ProvMap",
                                       nameJoinIDMap="ENNAME",  # province name in shapefile
                                       nameJoinColumnData="NameEn2",  # province name in data excel
                                       verbose = TRUE)

plot(Seas,col="lightblue", border="lightblue")                      # plot seas
plot(Lakes[c(15,25),],add=TRUE,col="lightblue", border="lightblue") # plot just Urmia and Qom lakes

mapParams <- rworldmap::mapPolys(MapData, add = TRUE, nameColumnToPlot = "HCR95",
                                     colourPalette = "heat",
                                     mapTitle = paste0("Poverty Rate"),
                                     addLegend = FALSE)             # Plot data
do.call( addMapLegend, c( mapParams
                              , legendLabels = "all"             # add legend
                              , digits = 2
                              , legendWidth = 0.7
                              , legendShrink = .5
                              , horizontal = TRUE))
