#--SHEAF_eqip_barplot_practice.R
#--examine practice distribution by state
#--author: Erich Seamon, University of Idaho
#--date: October 2018
#
#--example:
# SHEAF_eqip_barplot_practice("Idaho")

SHEAF_eqip_barplot_practice <- function(state) {
  
  library(rgdal)
  library(leaflet)
  library(maptools)
  library(classInt)
  library(leaflet)
  library(dplyr)
  library(RColorBrewer)
  library(raster)
  
  
    
    options(scipen=999)
  
  
  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }
  
  
  #EQIP---
  
  #eqip load via url - best if you are NOT on SESYNC rstudio server
  eqip <- read.csv("https://nextcloud.sesync.org/index.php/s/os5ZxFXAAEgc2y4/download")
  
  #eqip load using csv - use if you ARE on SESYNC Rstudio server
  #setwd("/nfs/soilsesfeedback-data/data/eqip")
  #eqip <- read.csv("eqip.csv")
  
  #OR YOU MAY LOAD THE RDS FILE WHICH IS FASTER
  #setwd("/nfs/soilsesfeedback-data/data/eqip")
  #eqip <- readRDS("Eqip.rds")
  
  
  #print(unique(eqip$State))
  
  #x <- readline("What STATE?")
  
  #----
  
  
  #LOAD SPATIAL COUNTY DATA FOR THE ENTIRE US---
  
  temp <- tempfile()
  download.file("https://nextcloud.sesync.org/index.php/s/SDJ5P4R6DDmt4FF/download",temp)
  outDir<-"/tmp"
  unzip(temp,exdir=outDir)
  
  setwd("/tmp/counties_conus")
  
  oldw <- getOption("warn")
  options(warn = -1)
  
 
  counties_conus <- readShapePoly('UScounties_conus.shp',
                                  proj4string=CRS
                                  ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  

  options(warn = oldw)
  
  #----
  
  #AGGREGATING EQIP - THIS CODE AGGREGATES EQIP DATA BY STATE, COUNTY, PLANNED YEAR, AND PRACTICE NAME---
  
  eqip_aggregated <- aggregate(eqip$Dollars.Paid, by=list(eqip$State, eqip$County, eqip$planned_year, eqip$practice_name), FUN = "sum")
  colnames(eqip_aggregated) <- c("State", "County", "Year", "Practice_Name", "Dollars_Paid")
  
  #USING THE NEWLY AGGREGATED EQIP FILE, SUBSET BASED ON PRACTICE AND YEAR, AND THEN RE-AGGREGATE BY STATE AND COUNTY  
  #THIS EXAMPLE SUBSETS FOR 2010, FOR RESIDUE MANAGEMENT, NO TILL/STRIP TILL, AND CONSERVATION COVER
  
  eqip_practice <- subset(eqip_aggregated, State %in% c(state))
  eqip_practice <- aggregate(eqip_practice$Dollars_Paid, by = list(eqip_practice$State, eqip_practice$Practice_Name), FUN = "sum")
  colnames(eqip_practice) <- c("State", "Practice_Name", "Dollars_Paid")
  
  #--need to deal with units ft vs acres
  #eqip_ft <- subset(xx_eqip, units == "ft")
  #eqip_ft$units
  
  #----
  
  
  #DEAL WITH ALIGNING COUNTY NAMES EXACTLY AS THEY ARE IN THE SPATIAL FILE.  ALL LOWER EXCEPT FOR FIRST LETTER---
  
  #eqip_practice$County <- tolower(eqip_practice$County)
  #eqip_practice$County <- sapply(eqip_practice$County, simpleCap)
  par(mar=c(20,4,4,2))
  return(barplot(eqip_practice$Dollars_Paid, names.arg = eqip_practice$Practice_Name, las = 3))
  
  
}
