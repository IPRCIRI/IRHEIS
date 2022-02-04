rm(list=ls())

library(yaml)
library(data.table)
library(readxl)
library(rworldmap)
library(rgdal)
Settings <- yaml.load_file("Settings.yaml")

GeP_kham <- data.table(read_excel("~/GitHub/IRHEIS/IranProvinceMap/Data/GeoData.xlsx","Province")) # Data to be plotted, see the excell file


GeP_kar2 <- data.table(read_excel("e:/dahak/decile_pro_non_real_inProvince.xlsx")) # Data to be plotted, see the excell file


GeP_Final <- merge(GeP_kham[,.(NameEn2,ProvinceName)],GeP_kar2[,.(ProvinceName,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)], by="ProvinceName",all=TRUE)
GeP_Final <- GeP_Final[,p_poor:=p1+p2+p3]
GeP_Final <- GeP_Final[,p_huge:=max(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10),by="ProvinceName"]
GeP_Final <- GeP_Final[,p_max:=ifelse(p_huge==p1,1,
                                      ifelse(p_huge==p2,2,
                                             ifelse(p_huge==p3,3,
                                                    ifelse(p_huge==p4,4,
                                                           ifelse(p_huge==p5,5,
                                                                  ifelse(p_huge==p6,6,
                                                                         ifelse(p_huge==p7,7,
                                                                                ifelse(p_huge==p8,8,
                                                                                       ifelse(p_huge==p9,9,10)))))))))]


GeP<- GeP_Final




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

MapData <- rworldmap::joinData2Map(GeP,                       # Joining the data to map
                                   nameMap="ProvMap",
                                   nameJoinIDMap="ENNAME",  # province name in shapefile
                                   nameJoinColumnData="NameEn2",  # province name in data excel
                                   verbose = TRUE)

jpeg("e:dahak/11.jpg", width = 500)
plot(Seas,col="lightblue", border="lightblue")                      # plot seas
plot(Lakes[c(15,25),],add=TRUE,col="lightblue", border="lightblue") # plot just Urmia and Qom lakes

mapParams <- rworldmap::mapPolys(MapData, add = TRUE, nameColumnToPlot = "p_poor",  #select variable
                                 colourPalette = "heat",
                                 mapTitle = paste0("3D national poors in province"),
                                 addLegend = FALSE)             # Plot data
do.call( addMapLegend, c( mapParams
                          , legendLabels = "all"             # add legend
                          , digits = 2
                          , legendWidth = 0.7
                          , legendShrink = .5
                          , horizontal = TRUE))
dev.off()
jpeg("e:dahak/12.jpg", width = 500)

plot(Seas,col="lightblue", border="lightblue")                      # plot seas
plot(Lakes[c(15,25),],add=TRUE,col="lightblue", border="lightblue") # plot just Urmia and Qom lakes


mapParams <- rworldmap::mapPolys(MapData, add = TRUE, nameColumnToPlot = "p10",  #select variable
                                 colourPalette = "heat",
                                 mapTitle = paste0("1D national rich in province"),      
                                 addLegend = FALSE)             # Plot data
do.call( addMapLegend, c( mapParams
                          , legendLabels = "all"             # add legend
                          , digits = 2
                          , legendWidth = 0.7
                          , legendShrink = .5
                          , horizontal = TRUE))
dev.off()


jpeg("e:dahak/13.jpg", width = 500)

plot(Seas,col="lightblue", border="lightblue")                      # plot seas
plot(Lakes[c(15,25),],add=TRUE,col="lightblue", border="lightblue") # plot just Urmia and Qom lakes


mapParams <- rworldmap::mapPolys(MapData, add = TRUE, nameColumnToPlot = "p_max",  #select variable
                                 colourPalette = "heat",
                                 mapTitle = paste0("huge D in province"),      
                                 addLegend = FALSE)             # Plot data
do.call( addMapLegend, c( mapParams
                          , legendLabels = "all"             # add legend
                          , digits = 2
                          , legendWidth = 0.7
                          , legendShrink = .5
                          , horizontal = TRUE))
dev.off()


GeP_kar1 <- data.table(read_excel("e:/dahak/decile_pro_non_real_inDecile.xlsx")) # Data to be plotted, see the excell file


GeP_Final <- merge(GeP_kham[,.(NameEn2,ProvinceName)],GeP_kar1[,.(ProvinceName,p,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)], by="ProvinceName",all=TRUE)
GeP_Final <- GeP_Final[,p_poor:=p1+p2+p3]

GeP<- GeP_Final


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

MapData <- rworldmap::joinData2Map(GeP,                       # Joining the data to map
                                   nameMap="ProvMap",
                                   nameJoinIDMap="ENNAME",  # province name in shapefile
                                   nameJoinColumnData="NameEn2",  # province name in data excel
                                   verbose = TRUE)

jpeg("e:dahak/21.jpg", width = 500)
plot(Seas,col="lightblue", border="lightblue")                      # plot seas
plot(Lakes[c(15,25),],add=TRUE,col="lightblue", border="lightblue") # plot just Urmia and Qom lakes

mapParams <- rworldmap::mapPolys(MapData, add = TRUE, nameColumnToPlot = "p_poor",  #select variable
                                 colourPalette = "heat",
                                 mapTitle = paste0("3D national poors in decile"),      
                                 addLegend = FALSE)             # Plot data
do.call( addMapLegend, c( mapParams
                          , legendLabels = "all"             # add legend
                          , digits = 2
                          , legendWidth = 0.7
                          , legendShrink = .5
                          , horizontal = TRUE))
dev.off()
jpeg("e:dahak/22.jpg", width = 500)

plot(Seas,col="lightblue", border="lightblue")                      # plot seas
plot(Lakes[c(15,25),],add=TRUE,col="lightblue", border="lightblue") # plot just Urmia and Qom lakes


mapParams <- rworldmap::mapPolys(MapData, add = TRUE, nameColumnToPlot = "p10",  #select variable
                                 colourPalette = "heat",
                                 mapTitle = paste0("1D national rich in decile"),      
                                 addLegend = FALSE)             # Plot data
do.call( addMapLegend, c( mapParams
                          , legendLabels = "all"             # add legend
                          , digits = 2
                          , legendWidth = 0.7
                          , legendShrink = .5
                          , horizontal = TRUE))
dev.off()