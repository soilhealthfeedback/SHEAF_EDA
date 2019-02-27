#--SHEAF_TOTAL_map.R
#--loads some TOTAL dataset and merges, combines with spatial data for visualization
#--author: Erich Seamon, University of Idaho
#--date: October 2018
#
#-YEARS: 2004 thru 2018
#
#USAGE: 
#  SHEAF_TOTAL_map(2014, "dollars")

SHEAF_TOTAL_map <- function(year,unit_measure) {

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

 #temp <- tempfile()
 #download.file("https://nextcloud.sesync.org/index.php/s/c6PqRMiek4gDyGx/download",temp)
 #outDir<-"/tmp"
 #unzip(temp,exdir=outDir)
 
 states <- readShapePoly('/nfs/soilsesfeedback-data/data/states/states.shp',
                         proj4string=CRS
                         ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
 projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
 
 states <- subset(states, STATE_NAME != "Alaska")
 states <- subset(states, STATE_NAME != "Hawaii")
 
 
 
 
 
 
 #temp <- tempfile()
 #download.file("https://nextcloud.sesync.org/index.php/s/SDJ5P4R6DDmt4FF/download",temp)
 #outDir<-"/tmp"
 #unzip(temp,exdir=outDir)
 
 #setwd("/tmp/counties_conus")
 
 counties_conus <- readShapePoly('/nfs/soilsesfeedback-data/data/counties/UScounties_conus.shp',
                                 proj4string=CRS
                                 ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
 projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
 
 options(warn = oldw)
 
 counties_conus <- subset(counties_conus, STATE_NAME != "Alaska")
 counties_conus <- subset(counties_conus, STATE_NAME != "Hawaii")
 counties_conus <- subset(counties_conus, STATE_NAME != "Caribbean Region")
 counties <- subset(counties_conus, STATE_NAME != "Pacific Island Area")
 
 
 
 
 
 n1 <- read.csv("https://nextcloud.sesync.org/index.php/s/edf3GYTYJ6km9E9/download", header=TRUE)
 colnames(n1) <- c("year",          "state",             "ag.district",        "county",            "data.item",       "value",      "cv")
 
 
 n2 <- read.csv("https://nextcloud.sesync.org/index.php/s/sKMFKkm3oL8EXf8/download", header=TRUE)
 colnames(n2) <- c("year",          "state",             "ag.district",        "county",          "data.item",       "value",      "cv"   
                    )
 
 n1 <- rbind(n1, n2)
 
 
 
 
 
 xxx <- subset(n1, year == year)
 
 
 counties$NAME <- tolower(counties$NAME)
 counties$STATE_NAME<- tolower(counties$STATE_NAME)
 
 xxx$county <- tolower(xxx$county)
 #xxx$county <- capitalize(xxx$county)
 
 xxx$state <- tolower(xxx$state)
 #xxx$state <- capitalize(xxx$state)
 
 colnames(xxx)[4] <- "NAME"
 colnames(xxx)[2] <- "STATE_NAME"
 
 xxx$value <- as.numeric(xxx$value)
 #xxx$data.item <- as.numeric(xxx$data.item)
 
 levels(xxx$data.item)[levels(xxx$data.item)=="AG LAND, INCL BUILDINGS - ASSET VALUE, MEASURED IN $" ] <- "dollars"
 levels(xxx$data.item)[levels(xxx$data.item)=="AG LAND, INCL BUILDINGS - ASSET VALUE, MEASURED IN $ / ACRE" ] <- "dollars_per_acre"
 levels(xxx$data.item)[levels(xxx$data.item)=="AG LAND, INCL BUILDINGS - ASSET VALUE, MEASURED IN $ / OPERATION" ] <- "dollars_per_operation"
 levels(xxx$data.item)[levels(xxx$data.item)=="INCOME, FARM-RELATED, RENT, LAND & BUILDINGS - RECEIPTS, MEASURED IN $" ] <- "ag_income"
 
 
 xxx <- subset(xxx, data.item == unit_measure)
 xxx <- data.frame(xxx)
 #id = 1:15
 #xxx[id] = as.numeric(unlist(xxx[id]))
 
 
 m <- merge(counties, xxx, by=c("STATE_NAME", "NAME"))
 
 palz1 <- brewer.pal(9, "GnBu")
 
 palz <- colorRampPalette(palz1)
 
 m$value[is.na(m$value)] <- 0
 
 palData <- classIntervals(eval(parse(text=paste("m$", "value", sep=""))), style="hclust")
 colors <- findColours(palData, palz(100))
 
 
 #pal <- colorNumeric(palette = c("white", "orange", "darkorange", "red", "darkred"),
 #                    domain = eval(parse(text=paste("m$",input$agcensuscontrols , sep=""))))
 
 pal2 <- colorNumeric(brewer.pal(9, "GnBu"), na.color = "#ffffff",
                      domain = eval(parse(text=paste("m$", "value", sep=""))))
 
 
 
 exte <- as.vector(extent(states))
 
 label <- paste(sep = "<br/>", m$STATE_NAME, m$NAME, round(eval(parse(text=paste("m$", "value", sep=""))), 0))
 markers <- data.frame(label)
 labs <- as.list(eval(parse(text=paste("m$", "value", sep=""))))
 

 leaflet(data = m) %>% addProviderTiles("Stamen.TonerLite") %>% fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(color = ~pal2(eval(parse(text=paste("m$", "value", sep="")))), popup = markers$label,  weight = 1) %>%
   addLegend(pal = pal2, values = ~eval(parse(text=paste("m$", "value", sep=""))), opacity = 1, title = paste("TOTAL ", unit_measure, sep=""),
             position = "bottomright")
#----

}

