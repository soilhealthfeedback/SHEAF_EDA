#--SHEAF_NRI_state_map.R
#--loads some NRI dataset, combines with spatial data for visualization
#--author: Erich Seamon, University of Idaho
#--date: October 2018
#
#
#USAGE: 
#  SHEAF_NRI_state_map(2002, "Pastureland")

SHEAF_eqip_map <- function(year, NRIoption) {
  
  library(rgdal)
  library(leaflet)
  library(maptools)
  library(classInt)
  library(leaflet)
  library(dplyr)
  library(RColorBrewer)
  library(raster)
  
  
  
  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }
  
  
  #NRI---
  
  #eqip load via url - best if you are NOT on SESYNC rstudio server
  nri <- read.csv("https://nextcloud.sesync.org/index.php/s/p98ioxi2xZXZepg/download")
  
  #eqip load using csv - use if you ARE on SESYNC Rstudio server
  #setwd("/nfs/soilsesfeedback-data/data/eqip")
  #eqip <- read.csv("eqip.csv")
  
  #OR YOU MAY LOAD THE RDS FILE WHICH IS FASTER
  #setwd("/nfs/soilsesfeedback-data/data/eqip")
  #eqip <- readRDS("Eqip.rds")
  
  #----
  
  #SUPRESS WARNINGS FOR READSHAPEPOLY DEPRECATION---
  
  oldw <- getOption("warn")
  options(warn = -1)
  
  
  #LOAD SPATIAL COUNTY DATA FOR THE ENTIRE US from URL
  
  temp <- tempfile()
  download.file("https://nextcloud.sesync.org/index.php/s/ZgwtMn5cTnCocdR/download",temp)
  outDir<-"/tmp"
  unzip(temp,exdir=outDir)
  
  states <- readShapePoly('/tmp/states.shp',
                                  proj4string=CRS
                                  ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  states <- subset(states, STATE_NAME != "Alaska")
  states_conus <- subset(states, STATE_NAME != "Hawaii")
  
  options(warn = oldw)
  
  #---
  
  colnames(states_conus@data)[1] <- "State"
  
  nri <- subset(nri, Year == year)
  
  m <- merge(states_conus, nri, by=c("State"), duplicateGeoms = TRUE)
  

  #----
  
  #SET UP COLOR PALETTE---
  
  palz1 <- brewer.pal(9, "GnBu")
  
  palz <- colorRampPalette(palz1)
  
  #----
  
  #SET NA TO ZERO AND MAKE NUMERIC---
  
 get(paste("m$", NRIoption, sep=""))[is.na(get(paste("m$", NRIoption, sep="")))] <- 0
  
  m$Dollars_Paid[is.na(m$Pastureland)] <- 0 
  #m$Dollars_Paid <- as.numeric(m$Dollars_Paid)
  
  #----
  
  
  #SET UP THE INTERVALS FOR THE COLOR PALETTE USING A HIEARCHICAL CLUSTERING MECHANSIM TO DIVIDE THE VARIABLE THAT IS DISPLAYED---
  m <- subset(m, NRIoption != 0)
  palData <- classIntervals(eval(parse(text=paste("m$", NRIoption, sep=""))), style="jenks")
  colors <- findColours(palData, palz(100))
  
  #----
  
  #ASSIGN A COLOR USING THE PALETTE BY A RANGE---
  
  pal2 <- colorNumeric(rev(brewer.pal(9, "Spectral")), na.color = "#ffffff",
                       domain = eval(parse(text=paste("m$", NRIoption, sep=""))))
  
  #----
  
  
  #SET THE EXTENT OF THE MAP---
  
  exte <- as.vector(extent(states_conus))
  
  #----
  
  #SET UP LABELING SO IF WE HOVER WE SEE INFORMATION---
  
  label <- paste(sep = "<br/>", m$State, round(eval(parse(text=paste("m$", NRIoption, sep=""))), 0))
  markers <- data.frame(label)
  labs <- as.list(eval(parse(text=paste("m$", NRIoption, sep=""))))
  
  #----
  
  #NOW USE LEAFLET TO ACTUALLY DRAW THE MAP---
  
  leaflet(data = m) %>% addProviderTiles("Stamen.TonerLite") %>% fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(color = ~pal2(eval(parse(text=paste("m$", NRIoption, sep="")))), popup = markers$label,  weight = 1) %>%
    addLegend(pal = pal2, values = ~eval(parse(text=paste("m$", NRIoption, sep=""))), opacity = 1, title = NULL,
              position = "bottomright")
  
  #----
  
}




library(classInt)
library(maptools)

setwd("/dmine/data/states/")

states <- readShapePoly('states.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
#counties <- counties[grep(input$state2, counties@data$STATE_NAME),]

setwd("/dmine/data/soilses/data/")
files2 <- list.files(pattern = "nass2*")
myfiles2 = lapply(files2, read.csv, strip.white = TRUE, header = TRUE)
y <- do.call(rbind, myfiles2)
x <- as.data.frame(y)


nass1 <- read.csv("nass2_1982.csv", header = TRUE)
nass2 <- read.csv("nass2_1987.csv", header = TRUE)
nass3 <- read.csv("nass2_1992.csv", header = TRUE)
nass4 <- read.csv("nass2_1997.csv", header = TRUE)
nass5 <- read.csv("nass2_2002.csv", header = TRUE)
nass6 <- read.csv("nass2_2007.csv", header = TRUE)
nass7 <- read.csv("nass2_2012.csv", header = TRUE)

n1 <- rbind(nass1, nass2, nass3, nass4, nass5, nass6, nass7)

subset(n1, Year == "1982")


