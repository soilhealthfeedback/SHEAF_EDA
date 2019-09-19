#SHEAF_soils.R maps out modified soils data.  Must define the soils variable
#options:
# AW - Available Water, Db - Bulk Density, Erv - Erosion vulnerability, pH, SOM - Soil organic matter, AHT - A horizon thickness

soilvar <- "Erv"
#soils

soils <- read.csv("https://files.sesync.org/index.php/s/z3fZWwXHnBAD8TG/download")
colnames(soils)[2] <- "Fips"




soils <- merge(soils, countyFIPS, by = "Fips")

soils <- subset(soils, State == "Illinois" | State == "Indiana" | State == "Iowa" | State == "Michigan" | State == "Minnesota" | State == "Missouri" | State == "Ohio" | State == "Wisconsin" | State == "Nebraska"| State == "Kansas" 
                | State == "North Dakota" | State == "South Dakota")

soils <- soils[-2]
soils <- soils[-(8:10)]


#mapit

library(maptools)

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

setwd("/tmp")

counties <- readShapePoly('UScounties_conus.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

options(warn = oldw)

colnames(soils)[1] <- "Fips"

soils_counties <- merge(counties_conus, soils, by = c("Fips"))


#--map it



#----

#ASSIGN A COLOR USING THE PALETTE BY A RANGE---

pal2 <- colorNumeric(rev(brewer.pal(9, "Spectral")), na.color = "#ffffff",
                     domain = as.numeric(eval(parse(text=paste("soils_counties$", soilvar, sep="")))))

#----


#SET THE EXTENT OF THE MAP---

exte <- as.vector(extent(soils_counties))

#----

#SET UP LABELING SO IF WE HOVER WE SEE INFORMATION---

label <- paste(sep = "<br/>", paste("soils_counties$", soilvar, sep=""), round(as.numeric(eval(parse(text=paste("soils_counties$", soilvar, sep="")))), 0))
markers <- data.frame(label)
labs <- as.list(as.numeric(eval(parse(text=paste("soils_counties$", soilvar, sep="")))))

#----

#NOW USE LEAFLET TO ACTUALLY DRAW THE MAP---

leaflet(data = soils_counties) %>% addProviderTiles("Stamen.TonerLite") %>% fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(color = ~pal2(as.numeric(eval(parse(text=paste("soils_counties$", soilvar, sep=""))))), popup = markers$label,  weight = 1) %>%
  addLegend(pal = pal2, values = ~as.numeric(eval(parse(text=paste("soils_counties$", soilvar, sep="")))), opacity = 1, title = NULL,
            position = "bottomright")

#----



