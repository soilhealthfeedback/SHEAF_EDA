#--SHEAF_NRI_barplot_county.R
#--loads some NRI dataset, combines with spatial data for visualization
#--author: Erich Seamon, University of Idaho
#--date: October 2018
#
#
#USAGE: 
#  SHEAF_NRI_barplot_county(State, County, NRIoption)
#
#  SHEAF_TOTAL_barplot_county("California", "dollars")

SHEAF_TOTAL_barplot_county <- function(state, unit_measure) {
  
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
  
  
  
  n1 <- read.csv("https://nextcloud.sesync.org/index.php/s/edf3GYTYJ6km9E9/download", header=TRUE)
  colnames(n1) <- c("year",          "state",             "ag.district",        "county",            "data.item",       "value",      "cv")
  
  
  n2 <- read.csv("https://nextcloud.sesync.org/index.php/s/sKMFKkm3oL8EXf8/download", header=TRUE)
  colnames(n2) <- c("year",          "state",             "ag.district",        "county",          "data.item",       "value",      "cv"   
  )
  
  n1 <- rbind(n1, n2)
  
  xxx <- n1
  
  
  
 # xxx <- subset(n1, year == year)
  
  
 # counties$NAME <- tolower(counties$NAME)
#  counties$STATE_NAME<- tolower(counties$STATE_NAME)
  
  colnames(xxx)[4] <- "County"
  colnames(xxx)[2] <- "State"
  
  xxx$County <- tolower(xxx$County)
  #xxx$County <- simpleCap(xxx$County)
  
  xxx$State <- tolower(xxx$State)
  #xxx$State <- simpleCap(xxx$State)
  

  
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
  #SUPRESS WARNINGS FOR READSHAPEPOLY DEPRECATION---
  
  oldw <- getOption("warn")
  options(warn = -1)
  
  state <- tolower(state)
  county <- tolower(county)
  
 xxx <- as.data.frame(xxx)
  
  total_barplot <- subset(xxx, State == state)
  
  total_barplot <- as.data.frame(total_barplot)
  bar <- barplot(as.numeric(eval(parse(text=paste("total_barplot$", "value", sep="")))), names.arg = unique(total_barplot$County), las = 3,  ylab = unit_measure, main = paste("TOTAL ", unit_measure, " by county \n", "for ", state, sep=""))
  return(mtext(text = state, side=1, line=7, at = bar[25]))
  
 
  
  #----
  
}
