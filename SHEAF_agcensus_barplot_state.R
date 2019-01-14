#-SHEAF_agcensus_barplot_state.R
#-plots a map of a particular year of agcensus data - only works with 2012 right now
#-agcensus controls include:
#   
#"tile_farms"            "tile_acres"           "tile_acres_avgfarm"    "ditches_farms"         "ditches_acres"         
#"consease_farms"        "consease_acres"        "consease_avgfarm"      "notill_farms"          "notill_acres"         
#"notill_avgfarm"        "constill_farms"        "constill_acres"        "constill_avgfarm"      "convtill_farms"       
#"convtill_acres"        "convtill_acres.1"      "cc_farms"              "cc_acres"              "cc_avgfarm"  
#"ditches_acres_avgfarm"
#
#-example: SHEAF_agcensus_barplot_state(2012, "tile_farms")

SHEAF_agcensus_barplot_state <- function(year, agcensuscontrols) {


library(classInt)
library(leaflet)
library(dplyr)
library(RColorBrewer)
library(maptools)
library(raster)
  
  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }

  temp <- tempfile()
  download.file("https://nextcloud.sesync.org/index.php/s/c6PqRMiek4gDyGx/download",temp)
  outDir<-"/tmp"
  unzip(temp,exdir=outDir)
  
  states <- readShapePoly('/tmp/states.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  states <- subset(states, STATE_NAME != "Alaska")
  states <- subset(states, STATE_NAME != "Hawaii")
  




temp <- tempfile()
download.file("https://nextcloud.sesync.org/index.php/s/SDJ5P4R6DDmt4FF/download",temp)
outDir<-"/tmp"
unzip(temp,exdir=outDir)

setwd("/tmp/counties_conus")

counties <- readShapePoly('UScounties_conus.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

options(warn = oldw)

#--

n1 <- read.csv("https://nextcloud.sesync.org/index.php/s/SFiSow3f4aSTdCK/download", header=TRUE)
n1 <- n1[,1:25]


xxx <- subset(n1, year == year)

xxx$county <- tolower(xxx$county)
xxx$county <- sapply(xxx$county, simpleCap)

colnames(xxx)[4] <- "NAME"
colnames(xxx)[3] <- "STATE_NAME"

id = 5:25
xxx[id] = as.numeric(unlist(xxx[id]))


xxx_new <- aggregate(eval(parse(text=paste("xxx$", agcensuscontrols, sep=""))), by = list(xxx$STATE_NAME), FUN = "sum")
colnames(xxx_new) <- c("State", agcensuscontrols)


return(barplot(eval(parse(text=paste("xxx_new$", agcensuscontrols, sep=""))), width = 5, names.arg = xxx_new$State, las = 3, xlab = "", ylab = paste("Number of ", agcensuscontrols, sep=""), main = paste("Agcensus ", year, " for ", agcensuscontrols, sep="")) %>%
mtext(text = c("States"), side=1, line=9)
)

}