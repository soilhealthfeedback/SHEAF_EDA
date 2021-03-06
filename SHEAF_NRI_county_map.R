#--SHEAF_NRI_county_map.R
#--loads some NRI dataset, combines with spatial data for visualization
#--author: Erich Seamon, University of Idaho
#--date: October 2018
#
#
#NRI options:
#
                                     
#"Tfact_County_Acres"                         
#"Tfact_Ave_tons_acre_weighted"               
#"Tfact_Ave_tons_acre_weighted.1"             
#"Prime_Cropland_Estimate"                    
#"Prime_Cropland_Mgn_Err"                     
#"Prime_CRP_Estimate"                         
#"PRIME_CRP_Mgn_Err"                          
#"Prime_Pastureland_Estimate"                 
#"Prime_Pastureland_Mgn_Err"                  
#"Prime_Rangeland_Estimate"                   
#"Prime_Rangeland_Mgn_Err"                    
#"Prime_Forestland_Estimate"                  
#"Prime_Forestland_Mgn_Err"                   
#"Prime_Other_Rural_Land_Estimate"            
#"Prime_Other_Rural_Land_Mgn_Err"             
#"Total_Prime_Farmland_Estimate"              
#"Total_Prime_Farmland_Mgn_Err"               
#"LCC_1_Estimate"                             
#"LCC_1_Mgn_Err"                              
#"LCC_2_Estimate"                             
#"LCC_2_Mgn_Err"                              
#"LCC_3_Estimate"                             
#"LCC_3_Mgn_Err"                              
#"LCC_4_Estimate"                             
#"LCC_4_Mgn.Err"                              
#"LCC_5_Estimate"                             
#"LCC_5_Mgn.Err"                              
#"LCC_6_Estimate"                             
#"LCC_6_Mgn.Err"                              
#"LCC_7Estimate"                              
#"LCC_7_Mgn_Err"                              
#"LCC_8_Estimate"                             
#"LCC_8_Mgn.Err"                              
#"LCC_total_NonFed_Ruraland_Estimate"         
#"LCC_total_NonFed_ruraland_Mgn_Err"          
#"Irr_Cropland_Estimate"                      
#"Irr_Cropland_Mgn_Err"                       
#"Irr_Irrigated_Cropland_Estimate"            
#"Irr_Irrigated_Cropland_Mgn_Err"             
#"Irr_Pastureland_Estimate"                   
#"Irr_Pastureland_Mgn_Err"                    
#"Irr_Irrigated_Pastureland_Estimate"         
#"Irr_Irrigated_Pastureland_Mgn_Err"          
#"Irr_Total_Cropland_Pastureland_Estimate"    
#"Irr_Total_Cropland_Pastureland_Mgn_Err"     
#"Irr_Total_irr_Cropland_Pastureland_Estimate"
#"Irr_Total_Irr_Cropland_Pastureland_Mgn_Err" 
#"Eros_Cropland_acres_Estimate"               
#"Eros_Cropland_acres_Mgn_Err"                
#"Eros_Tons_Watererosion_Estimate"            
#"Eros_Tons_Watererosion_Mgn_Err"             
#"Eros_Watererosion_rate_Estimate"            
#"Eros_Watererosion_rate_Mgn_Err"             
#"Eros_Tons_Winderosion_Estimate"             
#"Eros_Tons_Winderosion_Mgn_Err"              
#"Eros_Winderosion_rate_Estimate"             
#"Eros_Winderosion_rate_Mgn_Err"              
#"Eros_Tons_Water_Wind_erosion_Estimate"      
#"Eros_Tons_Water_Wind_erosion_Mgn_Err"       
#"Eros_Water_Wind_erosion_rate_Estimate"      
#"Eros_Water_Wind_erosion_rate_Mgn_Err"       
#"Dbl_Cultivated_Estimate"                    
#"Dbl_Cultivated_MgnNAErr"                    
#"Dbl_Doublecropped_Estimate"                 
#"Dbl_Doublecropped_MgnNAErr"                 
#"Crpcov_Grasses_Estimate"                    
#"Crpcov_Grasses_Mgn_Err"                     
#"Crpcov_Trees_Estimate"                      
#"Crpcov_Trees_Mgn_Err"                       
#"Crpcov_Wildlife_Estimate"                   
#"Crpcov_Wildlife_Mgn_Err"                    
#"Crpcov_Shallow_Water_Cover_Est"             
#"Crpcov_Shallow_Water_Cover_Mgn_Err"         
#"Crpcov_Native_Grasses_Estimate"             
#"Crpcov_Native_Grasses_Mgn_Err"              
#"Crpcov_Total_CRP_Estimate"                  
#"Brd_Cropland_Estimate"                      
#"Brd_Cropland_Mgn_Err"                       
#"Brd_Crp_Estimate"                           
#"Brd_Pastureland_Estimate"                   
#"Brd_Pastureland_Mgn.Err"                    
#"Brd_Rangeland_Estimate"                     
#"Brd_Rangeland_Mgn.Err"                      
#"Brd_Forestland_Estimate"                    
#"Brd_Forestland_Mgn_Err"                     
#"Brd_Ruraland_Estimate"                      
#"Brd_Ruraland_Mgn.Err"                       
#"Brd_Total_NonFed_Estimate"                  
#"Brd_Total_NonFed_Mgn_Err"                   
#"Brd_Developed_Estimate"                     
#"Brd_Developed_Mgn_Err"                      
#"Brd_WaterArea_Estimate"                     
#"Brd_Fed_Estimate"                           
#"Brd_Total_Surface_Estimate"    

#Year options:
# 1987 1992 1997 2002 2007 2012
#
#USAGE: 
#  SHEAF_NRI_county_map(2002, "Brd_Total_Surface_Estimate")

SHEAF_NRI_county_map <- function(year, NRIoption) {
  
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
  
  nri_tfact <- read.csv("https://nextcloud.sesync.org/index.php/s/ESranGDWaMcyDNj/download", strip.white=TRUE)
  nri_tfact$Year <- c("2015")
  
  nri_prime <- read.csv("https://nextcloud.sesync.org/index.php/s/YQCjJzwztpSfwpe/download", strip.white=TRUE)
  nri_lcc <- read.csv("https://nextcloud.sesync.org/index.php/s/RGb2eKkZtLpQ7X9/download", strip.white=TRUE)
  nri_irr <- read.csv("https://nextcloud.sesync.org/index.php/s/8EwQkxxsXa6XaRb/download", strip.white=TRUE)
  nri_eros <- read.csv("https://nextcloud.sesync.org/index.php/s/R8aASsxtMbiebYr/download", strip.white=TRUE)
  nri_dbl <- read.csv("https://nextcloud.sesync.org/index.php/s/tnge8GngoS2ozKg/download", strip.white=TRUE)
  nri_crpcov <- read.csv("https://nextcloud.sesync.org/index.php/s/GKroT2c8kRmHBPX/download", strip.white=TRUE)
  nri_brd <- read.csv("https://nextcloud.sesync.org/index.php/s/CedCm5X2PR6T37x/download", strip.white=TRUE)
  
  nri_combined <-  Reduce(function(x,y) merge(x = x, y = y, by = c("State", "County", "Year", "Fips"), all = TRUE), 
                          list(nri_tfact, nri_prime, nri_lcc, nri_irr, nri_eros, nri_dbl, nri_crpcov, nri_brd))
  
  #SUPRESS WARNINGS FOR READSHAPEPOLY DEPRECATION---
  
  oldw <- getOption("warn")
  options(warn = -1)
  
  
  #LOAD SPATIAL COUNTY DATA FOR THE ENTIRE US from URL
  
  temp <- tempfile()
  download.file("https://nextcloud.sesync.org/index.php/s/c6PqRMiek4gDyGx/download",temp)
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
  
  nri_combined$Year <- as.numeric(nri_combined$Year)
  nri_combined$State <- as.character(nri_combined$State)
  nri_combined$County <- as.character(nri_combined$County)
  
  
  
  nri_combined <- subset(nri_combined, Year == year)
  
  
  nri_combined[is.na(nri_combined)] <- 0 
  
  m <- merge(counties_conus, nri_combined, by=c("State", "County"))

  
  #----
  
  #SET UP COLOR PALETTE---
  
  palz1 <- brewer.pal(9, "GnBu")
  
  palz <- colorRampPalette(palz1)
  
  #----
  
  #SET NA TO ZERO AND MAKE NUMERIC---
 
  #m$Dollars_Paid[is.na(m$Dollars_Paid)] <- 0 
  #m$Dollars_Paid <- as.numeric(m$Dollars_Paid)
  
  #----
  
  vari <- as.numeric(eval(parse(text=paste("m$", NRIoption, sep=""))))
  vari[is.na(vari)] <- 0
  assign(paste("m$", NRIoption, sep=""), vari)
  
  
  #SET UP THE INTERVALS FOR THE COLOR PALETTE USING A HIEARCHICAL CLUSTERING MECHANSIM TO DIVIDE THE VARIABLE THAT IS DISPLAYED---
  m <- subset(m, NRIoption != 0)
  palData <- classIntervals(as.numeric(eval(parse(text=paste("m$", NRIoption, sep="")))), style="jenks")
  colors <- findColours(palData, palz(100))
  
  options(warn = oldw)
  
  
  #----
  
  #ASSIGN A COLOR USING THE PALETTE BY A RANGE---
  
  pal2 <- colorNumeric(rev(brewer.pal(9, "Spectral")), na.color = "#ffffff",
                       domain = as.numeric(eval(parse(text=paste("m$", NRIoption, sep="")))))
  
  #----
  
  
  #SET THE EXTENT OF THE MAP---
  
  exte <- as.vector(extent(counties_conus))
  
  #----
  
  #SET UP LABELING SO IF WE HOVER WE SEE INFORMATION---
  
  label <- paste(sep = "<br/>", m$State, round(as.numeric(eval(parse(text=paste("m$", NRIoption, sep="")))), 0))
  markers <- data.frame(label)
  labs <- as.list(as.numeric(eval(parse(text=paste("m$", NRIoption, sep="")))))
  
  #----
  
  #NOW USE LEAFLET TO ACTUALLY DRAW THE MAP---
  
  leaflet(data = m) %>% addProviderTiles("Stamen.TonerLite") %>% fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(color = ~pal2(as.numeric(eval(parse(text=paste("m$", NRIoption, sep=""))))), popup = markers$label,  weight = 1) %>%
    addLegend(pal = pal2, values = ~as.numeric(eval(parse(text=paste("m$", NRIoption, sep="")))), opacity = 1, title = NULL,
              position = "bottomright")
  
  #----
  
}
