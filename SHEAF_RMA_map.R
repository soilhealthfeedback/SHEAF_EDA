#--SHEAF_RMA_map.R
#--loads some RMA dataset and merges, combines with spatial data for visualization
#--author: Erich Seamon, University of Idaho
#--date: October 2018
#
#-YEARS: 1989 thru 2015
#
# Crop Types:
#
#All Other Crops                          CORN                                    
#SOYBEANS                                 WHEAT                                   
#COTTON                                   POTATOES                                
#FRESH MARKET SWEET CORN                  GRAIN SORGHUM                           
#PEANUTS                                  PEACHES                                 
#OATS                                     TABLE GRAPES                            
#RICE                                     ALMONDS                                 
#WALNUTS                                  PRUNES                                  
#STONEFRUIT                               DRY BEANS                               
#TOMATOES                                 SUGAR BEETS                             
#BARLEY                                   SAFFLOWER                               
#FIGS                                     GRAPES                                  
#RAISINS                                  APPLES                                  
#SUNFLOWERS                               POPCORN                                 
#GREEN PEAS                               PROCESSING BEANS                        
#FRESH MARKET TOMATOES                    PEPPERS                                 
#CITRUS                                   TOBACCO                                 
#DRY PEAS                                 SWEET CORN                              
#HYBRID CORN SEED                         CRANBERRIES                             
#ONIONS                                   FLAX                                    
#COTTON EX LONG STAPLE                    FORAGE PRODUCTION                       
#FORAGE SEEDING                           RYE                                     
#SUGARCANE                                HYBRID SORGHUM SEED                     
#PEARS                                    FRESH PLUM                              
#CITRUS TREES                             NURSERY (CONTAINER)                     
#SPECIAL CITRUS                           GRP SOYBEANS                            
#GRP WHEAT                                GRP CORN                                
#GRP COTTON                               GRP FORAGE PRODUCTION                   
#GRP PEANUTS                              CANOLA                                  
#BLUEBERRIES                              GRP GRAIN SORGHUM                       
#INCOME PROTECTION COTTON                 MILLET                                  
#AVOCADO TREES                            ORANGE TREES                            
#INCOME PROTECTION CORN                   REVENUE COVERAGE CORN                   
#REVENUE COVERAGE SOYBEANS                INCOME PROTECTION WHEAT                 
#FLUE CURED TOBACCO                       GRAPEFRUIT                              
#MANDARINS                                ORANGES                                 
#TANGELOS                                 PROCESSING CLING PEACHES                
#FRESH NECTARINES                         FRESH FREESTONE PEACHES                 
#FRESH APRICOTS                           PROCESSING APRICOTS                     
#LEMONS                                   CIGAR BINDER TOBACCO                    
#CIGAR WRAPPER TOBACCO                    MANGO TREES                             
#BURLEY TOBACCO                           FIRE CURED TOBACCO                      
#DARK AIR TOBACCO                         MARYLAND TOBACCO                        
#MINNEOLA TANGELOS                        NAVEL ORANGES                           
#ORLANDO TANGELOS                         SWEET ORANGES                           
#VALENCIA ORANGES                         PLUMS                                   
#PROCESSING FREESTONE                     AVOCADOS                                
#LIME TREES                               CITRUS IV                               
#CITRUS VII                               CITRUS I                                
#PECANS                                   SWEETPOTATOES                           
#CIGAR FILLER TOBACCO                     EARLY & MIDSEASON ORANGES               
#RIO RED & STAR RUBY                      LATE ORANGES                            
#RUBY RED GRAPEFRUIT                      WATERMELONS                             
#NURSERY (FG&C)                           CULTIVATED WILD RICE                    
#CHERRIES                                 CITRUS V                                
#ADJUSTED GROSS REVENUE                   WINTER SQUASH                           
#RANGELAND                                CABBAGE                                 
#MUSTARD                                  CRAMBE                                  
#CHILE PEPPERS                            CLAMS                                   
#ALL OTHER CITRUS TREES                   GRAPEFRUIT TREES                        
#FRESH MARKET BEANS                       STRAWBERRIES                            
#CITRUS II                                MINT                                    
#PROCESSING CUCUMBERS                     CARAMBOLA TREES                         
#CITRUS III                               ALL OTHER GRAPEFRUIT                    
#ALFALFA SEED                             RASPBERRY AND BLACKBERRY                
#ADJUSTED GROSS REVENUE-LITE              CITRUS TREES IV                         
#SILAGE SORGHUM                           CITRUS VI                               
#LEMON TREES                              PASTURE,RANGELAND,FORAGE                
#PAPAYA                                   CITRUS TREES I                          
#CITRUS TREES II                          CITRUS VIII                             
#COFFEE                                   APICULTURE                              
#PUMPKINS                                 PASTURE,RANGELAND,FORAGE                
#MACADAMIA NUTS                           BUCKWHEAT                               
#CITRUS TREES III                         CITRUS TREES V                          
#SWEET POTATOES                           SESAME                                  
#Pistachios                               Olives                                  
#Strawberries                             BANANA                                  
#MACADAMIA TREES                          GRASS SEED                              
#TANGORS                                  PAPAYA TREE                             
#ANNUAL FORAGE                            CUCUMBERS                               
#MANDARINS/TANGERINES                     Whole Farm Revenue Protection   
#
# Damage Causes
#
#Drought                           Excess Moisture/Precip/Rain       Flood                            
#Freeze                            Insects                           Mycotoxin (Aflatoxin)            
#Plant Disease                     Heat                              Tornado                          
#Poor Drainage                     Cold Wet Weather                 
#Wind/Excess Wind                  Hail                              Wildlife                         
#Frost                             Hurricane/Tropical Depression     Other (Snow-Lightning-Etc.)      
#Cold Winter                       Failure Irrig Supply              Hot Wind                         
#Fruit Set Failure                 House Burn (Pole Burn)            Volcanic Eruption                
#Force Fire                        Fire                              Earthquake                       
#Excess Sun                        Insufficient Chilling Hours       Failure Irrig Equip              
#Cyclone                           Area Plan Crops Only              Decline in Price                 
#Aquaculture Disease               Oxygen Depletion                  Salinity                         
#Asiatic Citrus Canker             Storm Surge                       Tidal Wave                       
#Asian Soybean Rust                Other Causes                      Inability to Prepare Land for Irr
#Falling Numbers                   Federal or State Ordered Destruct
#
#USAGE: 
#  SHEAF_RMA_map(2014, "WHEAT", "Drought")

SHEAF_RMA_map <- function(year, crop, damagecause) {

library(rgdal)
library(leaflet)
library(maptools)
library(classInt)
library(leaflet)
library(dplyr)
library(RColorBrewer)
library(raster)
library(tidyr)


  
 simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


#RMA---

 #RMA COMMODITY
 
 #RMA by commodity and damage cause, includes claim counts
 commodity <- read.csv("https://nextcloud.sesync.org/index.php/s/niLjWBSwmCoxQyC/download")
 colnames(commodity) <- c("ID", "Year", "State", "County", "Commodity", "Loss_commodity", "Count_commodity")
 commodity$State <- state.name[match(commodity$State,state.abb)]
 
 #RMA COMMODITY FIX CAPITIALIZATION AND THEN SUBSET TO ONLY STUDY AREA
 #commodity <- subset(commodity, State == "Illinois" | State == "Indiana" | State == "Iowa" | State == "Michigan" | State == "Minnesota" | State == "Missouri" | State == "Ohio" | State == "Wisconsin")
 #colnames(commodity) <- c("ID", "Year", "State", "County", "Commodity", "Loss_commodity", "Count_commodity")
 
 #RMA DAMAGE
 
 #RMA by damage cause, includes claim counts
 damage <- read.csv("https://nextcloud.sesync.org/index.php/s/YErYqQYB9PAkmH9/download")
 colnames(damage) <- c("ID", "Year", "State", "County", "Commodity", "Damagecause", "Loss_damagecause", "Count_damagecause")
 damage$State <- state.name[match(damage$State,state.abb)]
 
 #RMA DAMAGE FIX CAPITIALIZATION AND THEN SUBSET TO ONLY STUDY AREA
 
 #damage <- subset(damage, State == "Illinois" | State == "Indiana" | State == "Iowa" | State == "Michigan" | State == "Minnesota" | State == "Missouri" | State == "Ohio" | State == "Wisconsin")
 #colnames(damage) <- c("ID", "Year", "State", "County", "Commodity", "Damagecause", "Loss_damagecause", "Count_damagecause")
 damage <- aggregate(damage$Loss_damagecause, by=list(damage$Commodity, damage$Damagecause, damage$County, damage$State, damage$Year), FUN = "sum")
 colnames(damage) <- c("Commodity", "Damagecause", "County", "State", "Year", "Loss_damagecause")
 
 
 #damage <- spread(damage, Damagecause, Loss_damagecause)
 
 damage <- subset(damage, Commodity == crop)
 
 damage <- subset(damage, Year == year)
 

 damage <- subset(damage, Damagecause == damagecause)
 
 if(nrow(damage) == 0){ 
   print("selected damage cause has no loss for the selected year and commodity")
   
 } else {
 
 damage <- try(aggregate(damage$Loss_damagecause, by = list(damage$State, damage$County), FUN = "sum"), silent = TRUE)
 

 
 
#----

#SUPRESS WARNINGS FOR READSHAPEPOLY DEPRECATION---

oldw <- getOption("warn")
options(warn = -1)


#LOAD SPATIAL COUNTY DATA FOR THE ENTIRE US from URL

temp <- tempfile()
download.file("https://nextcloud.sesync.org/index.php/s/paxKXxFGnZaHbbN/download",temp)
outDir<-"/tmp"
unzip(temp,exdir=outDir)

counties_conus <- readShapePoly('/tmp/UScounties_conus.shp',
                                proj4string=CRS
                                ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

options(warn = oldw)

#---

#RENAME STATE COLUMN TO MATCH WITH SPATIAL FILE---

colnames(damage)[2] <- "NAME"
colnames(damage)[1] <- "STATE_NAME"
colnames(damage)[3] <- "Loss"
#----

#NOW LETS MERGE THE FILES SO WE CAN PLOT MAPS AS NEEDED---

m <- merge(counties_conus, damage, by=c("STATE_NAME", "NAME"), duplicateGeoms = TRUE)

#----

#SET UP COLOR PALETTE---

palz1 <- brewer.pal(9, "GnBu")

palz <- colorRampPalette(palz1)

#----

#SET NA TO ZERO AND MAKE NUMERIC---

m$Dollars_Paid[is.na(m$Loss)] <- 0 
m$Dollars_Paid <- as.numeric(m$Loss)

#----


#SET UP THE INTERVALS FOR THE COLOR PALETTE USING A HIEARCHICAL CLUSTERING MECHANSIM TO DIVIDE THE VARIABLE THAT IS DISPLAYED---
m <- subset(m, Loss != 0)
palData <- classIntervals(eval(parse(text=paste("m$", "Loss", sep=""))), style="jenks")
colors <- findColours(palData, palz(100))

#----

#ASSIGN A COLOR USING THE PALETTE BY A RANGE---

pal2 <- colorNumeric(rev(brewer.pal(9, "Spectral")), na.color = "#ffffff",
                     domain = eval(parse(text=paste("m$", "Loss", sep=""))))

#----


#SET THE EXTENT OF THE MAP---

exte <- as.vector(extent(counties_conus))

#----

#SET UP LABELING SO IF WE HOVER WE SEE INFORMATION---

label <- paste(sep = "<br/>", m$STATE_NAME, round(eval(parse(text=paste("m$", "Loss", sep=""))), 0))
markers <- data.frame(label)
labs <- as.list(eval(parse(text=paste("m$", "Loss", sep=""))))

#----

#NOW USE LEAFLET TO ACTUALLY DRAW THE MAP---

leaflet(data = m) %>% addProviderTiles("Stamen.TonerLite") %>% fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(color = ~pal2(eval(parse(text=paste("m$", "Dollars_Paid", sep="")))), popup = markers$label,  weight = 1) %>%
  addLegend(pal = pal2, values = ~eval(parse(text=paste("m$", "Loss", sep=""))), opacity = 1, title = NULL,
            position = "bottomright")

#----

}

}

