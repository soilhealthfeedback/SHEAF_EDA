#--SHEAF_eqip_barplot_year.R
#--examine practice distribution by year for a state
#--author: Erich Seamon, University of Idaho
#--date: October 2018

# PRACTICES
#Forage and Biomass Planting                  Integrated Pest Management (IPM)            
#Residue Management, No-Till/Strip Till       Terrace                                     
#Prescribed Grazing                           Conservation Crop Rotation                  
#Grassed Waterway                             Residue Management, Seasonal                
#Residue Management, Mulch Till               Riparian Forest Buffer                      
#Filter Strip                                 Mulching                                    
#Cover Crop                                   Conservation Cover                          
#Windbreak/Shelterbelt Establishment          Hedgerow Planting                           
#Stripcropping                                Stripcropping, Field                        
#Riparian Herbaceous Cover                    Contour Buffer Strips                       
#Residue Management, Ridge Till               Transition to Organic Production            
#Long Term No. Till                           Riparian Buffers - Vegetative               
#Vegetative Barrier                           Residue and Tillage Management, No-Till     
#Contour Orchard and Other Perennial Crops    Alley Cropping                              
#Silvopasture Establishment                   Herbaceous Wind Barriers                    
#Residue and Tillage Management, Ridge Till   Residue and Tillage Management, Reduced Till
#Multi-Story Cropping                         Strip - Intercropping                       
#Restoration of Compacted Soils           

#SHEAF_eqip_barplot_year("Idaho", "Vegetative Barrier")
  
SHEAF_eqip_barplot_year <- function(state, practice) {
  
  
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
  
  #temp <- tempfile()
  #download.file("https://nextcloud.sesync.org/index.php/s/SDJ5P4R6DDmt4FF/download",temp)
  #outDir<-"/tmp"
  #unzip(temp,exdir=outDir)
  
  #setwd("/tmp/counties_conus")  
  
  oldw <- getOption("warn")
  options(warn = -1)
  
 
  counties_conus <- readShapePoly('/nfs/soilsesfeedback-data/data/counties/UScounties_conus.shp',
                                  proj4string=CRS
                                  ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  


  
  #----
  
  #AGGREGATING EQIP - THIS CODE AGGREGATES EQIP DATA BY STATE, COUNTY, PLANNED YEAR, AND PRACTICE NAME---
  
  eqip_aggregated <- aggregate(eqip$Dollars.Paid, by=list(eqip$State, eqip$County, eqip$Applied.Year, eqip$practice_name), FUN = "sum")
  colnames(eqip_aggregated) <- c("State", "County", "Year", "Practice_Name", "Dollars_Paid")
  
  #USING THE NEWLY AGGREGATED EQIP FILE, SUBSET BASED ON PRACTICE AND YEAR, AND THEN RE-AGGREGATE BY STATE AND COUNTY  
  #THIS EXAMPLE SUBSETS FOR 2010, FOR RESIDUE MANAGEMENT, NO TILL/STRIP TILL, AND CONSERVATION COVER
  
  eqip_practice <- subset(eqip_aggregated, State %in% c(state))
  eqip_practice <- subset(eqip_practice, Practice_Name %in% c(practice))
  
  if(nrow(eqip_practice) == 0){ 
    print("selected practice has no dollars paid for chosen state")
    
  } else {
  
  try(eqip_practice <- aggregate(eqip_practice$Dollars_Paid, by = list(eqip_practice$State, eqip_practice$Year), FUN = "sum"), silent = TRUE)
  
 
  
  
  options(warn = oldw)
  
  
  
  
  colnames(eqip_practice) <- c("State", "Year", "Dollars_Paid")
  
  #--need to deal with units ft vs acres
  #eqip_ft <- subset(xx_eqip, units == "ft")
  #eqip_ft$units
  
  #----
  
  
  #DEAL WITH ALIGNING COUNTY NAMES EXACTLY AS THEY ARE IN THE SPATIAL FILE.  ALL LOWER EXCEPT FOR FIRST LETTER---
  
  #eqip_practice$County <- tolower(eqip_practice$County)
  #eqip_practice$County <- sapply(eqip_practice$County, simpleCap)
  
  return(barplot(eqip_practice$Dollars_Paid, names.arg = eqip_practice$Year, las = 3, xlab = "Years", ylab = "Dollars Paid", main = paste("Dollars Paid for \n", practice, "\n for ", state, sep="")))
  
  }
}
