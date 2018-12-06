#--SHEAF_eda_studyarea.R
#--loads some data, subsets for 8 state region of
#  Illinois, Indiana, Iowa, Michigan, Minnesota, Missouri, Ohio, and Wisconsin
#--author: Erich Seamon, University of Idaho
#--date: October 2018

library(rgdal)
library(leaflet)
library(maptools)
library(classInt)
library(leaflet)
library(dplyr)
library(Hmisc)
library(RColorBrewer)
library(raster)
library (RCurl)
library(maptools)

#AGCENSUS

#agcensus load via url - best if you are NOT on SESYNC rstudio server
agcensus <- read.csv("https://nextcloud.sesync.org/index.php/s/THpGDspGXFtLSGF/download")

#agcensus load using csv - use if you ARE on SESYNC Rstudio server
#setwd("/nfs/soilsesfeedback-data/data/agcensus")
#agcensus <- read.csv("Agcensus2012.csv")
#agcensus_metadata <- read.csv("Agcensus2012_metadata.csv")


#removes ancillary columns at the end of the agcensus
agcensus <- agcensus[,1:25]

#EQIP

#eqip load via url - best if you are NOT on SESYNC rstudio server
eqip <- read.csv("https://nextcloud.sesync.org/index.php/s/bgWSzqdqYDifJwz/download")

#eqip load using csv - use if you ARE on SESYNC Rstudio server
#setwd("/nfs/soilsesfeedback-data/data/eqip")
#eqip <- read.csv("eqip.csv")

#CENSUS

#census load - best if you are NOT on SESYNC rstudio server
census <- read.csv("https://nextcloud.sesync.org/index.php/s/C3jHtLfRToPkrJa/download")

#census load using csv - use if you ARE on SESYNC Rstudio server
#setwd("/nfs/soilsesfeedback-data/data/census")
#census <- read.csv("Census_States_CountyDem.csv")

#RMA

#RMA by commodity and damage cause, includes claim counts
commodity <- read.csv("https://nextcloud.sesync.org/index.php/s/niLjWBSwmCoxQyC/download")
damage <- read.csv("https://nextcloud.sesync.org/index.php/s/YErYqQYB9PAkmH9/download")

#SPATIAL DATA FOR US COUNTIES CONUS

temp <- tempfile()
download.file("http://dmine.io/waf/SHEAF/spatial_data/counties_conus/counties_conus.zip",temp)
outDir<-"/tmp"
unzip(temp,exdir=outDir)

setwd("/tmp/counties_conus")

counties_conus <- readShapePoly('UScounties_conus.shp',
                                proj4string=CRS
                                ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#AGGREGATING EQIP - THIS CODE AGGREGATES EQIP DATA BY STATE, COUNTY, PLANNED YEAR, AND PRACTICE NAME

xx_eqip2 <- aggregate(eqip$Dollars.Paid, by=list(eqip$State, eqip$County, eqip$planned_year, eqip$practice_name), FUN = "sum")
colnames(xx_eqip2) <- c("State", "County", "Year", "Practice_Name", "Dollars_Paid")
#xx_eqip3 <- subset(xx_eqip2, Practice_Name == "Residue Management, No-Till/Strip Till" & Planned_Year == "2010")

#AGGREGATING EQIP - THIS CODE AGGREGATES BY A PARTICULAR PRACTICE.  HERE WE ARE AGGREGATING BY THREE PRACTICE CODES COMBINED. 

xx_eqip3a <- subset(xx_eqip2, Practice_Name %in% c("Residue Management, No-Till/Strip Till", "Conservation Cover") & Year == 2010)
xx_eqip3 <- aggregate(xx_eqip3a$Dollars_Paid, by = list(xx_eqip3a$State, xx_eqip3a$County), FUN = "sum")
colnames(xx_eqip3) <- c("State", "County", "Dollars_Paid")

#--need to deal with units ft vs acres
#eqip_ft <- subset(xx_eqip, units == "ft")
#eqip_ft$units



xx_eqip3$County <- tolower(xx_eqip3$County)
xx_eqip3$County <- capitalize(xx_eqip3$County)

colnames(xx_eqip3)[2] <- "NAME"
colnames(xx_eqip3)[1] <- "STATE_NAME"

m <- merge(counties_conus, xx_eqip3, by=c("STATE_NAME", "NAME"))

palz1 <- brewer.pal(9, "GnBu")

palz <- colorRampPalette(palz1)

m$Dollars_Paid[is.na(m$Dollars_Paid)] <- 0 
m$Dollars_Paid <- as.numeric(m$Dollars_Paid)


palData <- classIntervals(eval(parse(text=paste("m$", "Dollars_Paid", sep=""))), style="hclust")
colors <- findColours(palData, palz(100))


pal2 <- colorNumeric(brewer.pal(9, "GnBu"), na.color = "#ffffff",
                     domain = eval(parse(text=paste("m$", "Dollars_Paid", sep=""))))



exte <- as.vector(extent(counties_conus))

label <- paste(sep = "<br/>", m$STATE_NAME, round(eval(parse(text=paste("m$", "Dollars_Paid", sep=""))), 0))
markers <- data.frame(label)
labs <- as.list(eval(parse(text=paste("m$", "Dollars_Paid", sep=""))))


leaflet(data = m) %>% addProviderTiles("Stamen.TonerLite") %>% fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(color = ~pal2(eval(parse(text=paste("m$", "Dollars_Paid", sep="")))), popup = markers$label,  weight = 1) %>%
  addLegend(pal = pal2, values = ~eval(parse(text=paste("m$", "Dollars_Paid", sep=""))), opacity = 1, title = NULL,
            position = "bottomright")




