#--SHEAF_RMA_barplot_damage.R
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
#  SHEAF_RMA_barplot_crop(2014, "Washington", "Drought")

SHEAF_RMA_barplot_crop <- function(year, damagecause, state) {

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
 commodity <- read.csv("https://nextcloud.sesync.org/index.php/s/TKKQqJQg2epBbsG/download")
 colnames(commodity) <- c("ID", "Year", "State", "County", "Commodity", "Loss_commodity", "Count_commodity")
 commodity$State <- state.name[match(commodity$State,state.abb)]
 
 #RMA COMMODITY FIX CAPITIALIZATION AND THEN SUBSET TO ONLY STUDY AREA
 #commodity <- subset(commodity, State == "Illinois" | State == "Indiana" | State == "Iowa" | State == "Michigan" | State == "Minnesota" | State == "Missouri" | State == "Ohio" | State == "Wisconsin")
 #colnames(commodity) <- c("ID", "Year", "State", "County", "Commodity", "Loss_commodity", "Count_commodity")
 
 #RMA DAMAGE
 
 #RMA by damage cause, includes claim counts
 damage <- read.csv("https://nextcloud.sesync.org/index.php/s/Qc9JERtaATStGZa/download")
 colnames(damage) <- c("ID", "Year", "State", "County", "Commodity", "Damagecause", "Loss_damagecause", "Count_damagecause")
 damage$State <- state.name[match(damage$State,state.abb)]
 
 #RMA DAMAGE FIX CAPITIALIZATION AND THEN SUBSET TO ONLY STUDY AREA
 
 #damage <- subset(damage, State == "Illinois" | State == "Indiana" | State == "Iowa" | State == "Michigan" | State == "Minnesota" | State == "Missouri" | State == "Ohio" | State == "Wisconsin")
 #colnames(damage) <- c("ID", "Year", "State", "County", "Commodity", "Damagecause", "Loss_damagecause", "Count_damagecause")
 damage <- aggregate(damage$Loss_damagecause, by=list(damage$Commodity, damage$Damagecause, damage$County, damage$State, damage$Year), FUN = "sum")
 colnames(damage) <- c("Commodity", "Damagecause", "County", "State", "Year", "Loss_damagecause")
 
 
 #damage <- spread(damage, Damagecause, Loss_damagecause)
 
 damage <- subset(damage, Damagecause == damagecause)
 
 damage <- subset(damage, Year == year)
 
 damage <- subset(damage, State == state)
 
 if(nrow(damage) == 0){ 
   print("selected damage cause has no loss for the selected year and commodity")
   
 } else {
 
   
damage <- aggregate(damage$Loss_damagecause, by = list(damage$Commodity), FUN = "sum")   
colnames(damage) <- c("Commodity", "Loss")

par(mar=c(18.1,4.1,4.1,2.1))
options(scipen = 999)

bar <- barplot(damage$Loss, names.arg = damage$Commodity, las = 3,  ylab = "Commodity Loss ($)", main = paste("RMA Commodities for ", year, ", ", state, sep=""))

return(mtext(text = c("Commodities"), side=1, line=8, at = bar[5]))
}}
 
 