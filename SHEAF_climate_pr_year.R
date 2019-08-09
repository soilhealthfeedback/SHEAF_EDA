#SHEAF_climate_pr_year.R 
#by Erich Seamon
#generates a data file of the number of extreme precip events from 2008-2012, aggregated by county
#original data is from ken kunkel @ noaa who provided extreme precip events per station.
#
#creates several maps of data aggregated by county
#1) average number of events per county based on all station data
#2) station with max number of events
#3) sd of station data by county
#4) number of stations per county
#5) coefficient of variation per county (sd of precip events / mean)

SHEAF_climate_pr_year <- function(year) {
  
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
  
  
  #stations from ghcnd
  stations <- read.csv("https://files.sesync.org/index.php/s/qWGCFkc3Q3mcfnr/download", strip.white = TRUE)
  
  #climate_pr_reoccuring <- read.csv("https://files.sesync.org/index.php/s/gFndoe7iwfLewWB/download")
  climate_pr_percentile <- read.csv2("https://files.sesync.org/index.php/s/gFndoe7iwfLewWB/download", strip.white = TRUE)

  
  climate_pr_perc_location <- climate_pr_percentile[seq(0, 16179, by=4),]
  climate_pr_perc_location <- data.frame(climate_pr_perc_location)
  climate_pr_perc_location <- climate_pr_perc_location[24:4044,]
  climate_pr_perc_location <- data.frame(climate_pr_perc_location)
  colnames(climate_pr_perc_location) <- "pr"
  climate_pr_perc_location$pr <- as.character(climate_pr_perc_location$pr)
  
  sbt <- strsplit(climate_pr_perc_location$pr, " ")  
 
  library(data.table)
  sbt <- t(as.data.table(sbt))
  sbt <- as.data.frame(sbt[,1])
  colnames(sbt) <- "pr"
  
  selectedRows <- stations$station %in% sbt$pr
  stations_pr <- stations[selectedRows,]
  stations_pr <- stations_pr[4:4020,]
  
#--loading number of days per year exceeding 99% percentile per station
  
  climate_pr_perc_days <- climate_pr_percentile[seq(3, 16179, by=4),]
  climate_pr_perc_days <- data.frame(climate_pr_perc_days)
  climate_pr_perc_days <- climate_pr_perc_days[25:4044,]
  climate_pr_perc_days <- data.frame(climate_pr_perc_days)
  colnames(climate_pr_perc_days) <- "pr"
  climate_pr_perc_days$pr <- as.character(climate_pr_perc_days$pr)
  

  sbt2 <- strsplit(climate_pr_perc_days$pr, " +")
  
  library(data.table)
  sbt2 <- t(as.data.table(sbt2))
  
  sbt2 <- cbind(sbt2[,1], sbt2[,6:36])
  colnames(sbt2) <- c("station", 1988:2018)
  
  sbt3 <- merge(stations_pr, sbt2, by=c("station"))
  
  
  
  #nri_combined <-  Reduce(function(x,y) merge(x = x, y = y, by = c("State", "County", "Year", "Fips"), all = TRUE), 
  #                        list(nri_tfact, nri_prime, nri_lcc, nri_irr, nri_eros, nri_dbl, nri_crpcov, nri_brd))
  
  #SUPRESS WARNINGS FOR READSHAPEPOLY DEPRECATION---
  
  oldw <- getOption("warn")
  options(warn = -1)
  
  
  #LOAD SPATIAL STATES DATA FOR THE ENTIRE US from URL
  
  temp <- tempfile()
  download.file("https://files.sesync.org/index.php/s/SDJ5P4R6DDmt4FF/download",temp)
  #download.file("https://nextcloud.sesync.org/index.php/s/c6PqRMiek4gDyGx/download",temp)
  outDir<-"/tmp"
  unzip(temp,exdir=outDir)
  
  counties_conus <- readShapePoly('/tmp/UScounties_conus.shp',
                                  proj4string=CRS
                                  ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  #states <- subset(states, STATE_NAME != "Alaska")
  #states_conus <- subset(states, STATE_NAME != "Hawaii")
  
  #---
  
  colnames(counties_conus@data)[2] <- "State"
  colnames(counties_conus@data)[1] <- "County"
  colnames(counties_conus@data)[5] <- "Fips"
  
  counties_conus$State <- as.character(counties_conus$State)
  counties_conus$County <- as.character(counties_conus$County)
  
  #nri_combined$Year <- as.numeric(nri_combined$Year)
  #nri_combined$State <- as.character(nri_combined$State)
  #nri_combined$County <- as.character(nri_combined$County)
  
  
  
  #nri_combined <- subset(nri_combined, Year == year)
  
  
  
  #nri_combined[is.na(nri_combined)] <- 0 
  
  
  #m <- merge(counties_conus, nri_combined, by=c("State", "County"))
  
  
  #----
  
  #SET UP COLOR PALETTE---
  
  palz1 <- brewer.pal(9, "GnBu")
  
  palz <- colorRampPalette(palz1)
  
  #----
  
  #SET NA TO ZERO AND MAKE NUMERIC---
  
  #m$Dollars_Paid[is.na(m$Dollars_Paid)] <- 0 
  #m$Dollars_Paid <- as.numeric(m$Dollars_Paid)
  
  #----
  
#  vari <- as.numeric(eval(parse(text=paste("m$", NRIoption, sep=""))))
#  vari[is.na(vari)] <- 0
#  assign(paste("m$", NRIoption, sep=""), vari)
  
  
  #SET UP THE INTERVALS FOR THE COLOR PALETTE USING A HIEARCHICAL CLUSTERING MECHANSIM TO DIVIDE THE VARIABLE THAT IS DISPLAYED---
#  m <- subset(m, NRIoption != 0)
#  palData <- classIntervals(as.numeric(eval(parse(text=paste("m$", NRIoption, sep="")))), style="jenks")
#  colors <- findColours(palData, palz(100))
  
  options(warn = oldw)
  
  
  #----
  
  #ASSIGN A COLOR USING THE PALETTE BY A RANGE---
  
  pal2 <- colorNumeric(rev(brewer.pal(9, "Spectral")), na.color = "#ffffff",
                       domain = as.numeric(eval(parse(text=paste("sbt3$`", year, "`", sep="")))))
  
  #----
  
  
  #SET THE EXTENT OF THE MAP---
  
  exte <- as.vector(extent(counties_conus))
  
  #----
  
  #SET UP LABELING SO IF WE HOVER WE SEE INFORMATION---
  
 # label <- paste(sep = "<br/>", m$State, round(as.numeric(eval(parse(text=paste("m$", NRIoption, sep="")))), 0))
#  markers <- data.frame(label)
#  labs <- as.list(as.numeric(eval(parse(text=paste("m$", NRIoption, sep="")))))
  
  #----
  
  #map of stations
  

  leaflet(data = counties_conus) %>% addProviderTiles("Stamen.TonerLite") %>% fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(fillColor = "white", color = "black", stroke = TRUE, weight = .5) %>% 
    addCircles(sbt3$long, sbt3$lat, popup=as.character(as.numeric(eval(parse(text=paste("sbt3$`", year, "`", sep=""))))), weight = 6, radius=800, 
                                                       color= ~pal2(as.numeric(eval(parse(text=paste("sbt3$`", year, "`", sep=""))))), stroke = TRUE, fillOpacity = 0.8) 
  
  
}


