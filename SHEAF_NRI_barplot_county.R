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
#"Prime_CRP_Mgn_Err"                          
#"Prime_Pastureland_Estimate"                 
#"Prime_Pastureland_Mgn_Err"                  
#"Prime_Rangeland_Estimate"                   
#"Prime_Rangeland_Mgn_Err"                    
#"Prime_Forestland_Estimate"                  
#"Prime_Forestland_Mgn_Err"                   
#"Prime_Other_Rural_Land_Estimate"            
#"Prime_Other_Rural_Land_Mgn_Err"             
#"Prime_Total_Farmland_Estimate"              
#"Prime_Total_Farmland_Mgn_Err"               
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
#  SHEAF_NRI_county_barplot(State, County, NRIoption)
#
#  SHEAF_NRI_county_barplot(California, Marin, "Brd_Total_Surface_Estimate")

SHEAF_NRI_county_barplot <- function(state, county, NRIoption) {
  
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
  
 nri_tfact <- read.csv("https://nextcloud.sesync.org/index.php/s/wgk5q4oeGLLWngs/download", strip.white=TRUE)
 nri_tfact$Year <- c("2015")
 
 nri_prime <- read.csv("https://nextcloud.sesync.org/index.php/s/5r2CSg8n2rLaBdt/download", strip.white=TRUE)
 nri_lcc <- read.csv("https://nextcloud.sesync.org/index.php/s/x5BQrzDBPMfgc26/download", strip.white=TRUE)
 nri_irr <- read.csv("https://nextcloud.sesync.org/index.php/s/dXLkJpMW52pBbTF/download", strip.white=TRUE)
 nri_eros <- read.csv("https://nextcloud.sesync.org/index.php/s/jyKfrceCRdSyqcP/download", strip.white=TRUE)
 nri_dbl <- read.csv("https://nextcloud.sesync.org/index.php/s/N2nZAD4KbbpRCDJ/download", strip.white=TRUE)
 nri_crpcov <- read.csv("https://nextcloud.sesync.org/index.php/s/GSdrizSNgLRf3s6/download", strip.white=TRUE)
 nri_brd <- read.csv("https://nextcloud.sesync.org/index.php/s/jMXn6ZHW6B46eKr/download", strip.white=TRUE)
 
nri_combined <-  Reduce(function(x,y) merge(x = x, y = y, by = c("State", "County", "Year", "Fips"), all = TRUE), 
        list(nri_tfact, nri_prime, nri_lcc, nri_irr, nri_eros, nri_dbl, nri_crpcov, nri_brd))
  
  #SUPRESS WARNINGS FOR READSHAPEPOLY DEPRECATION---
  
  oldw <- getOption("warn")
  options(warn = -1)
  
  nri_barplot <- subset(nri_combined, State == state & County == county)
  
  nri_barplot <- as.data.frame(nri_barplot)
  return(barplot(as.numeric(eval(parse(text=paste("nri_barplot$", NRIoption, sep="")))), names.arg = nri_barplot$Year, las = 3, xlab = "Years", ylab = NRIoption, main = paste(NRIoption, " for ", county, " county, ", state, sep="")))
  
 
  
  #----
  
}
