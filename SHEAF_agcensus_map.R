#-SHEAF_agcensus_map.R
#County Level Conservation Practices
#-plots a map of a particular year of agcensus data - only works with 2012 right now
#-agcensus controls include:
#   
#"tile_farms"            "tile_acres"           "tile_acres_avgfarm"    "ditches_farms"         "ditches_acres"         
#"consease_farms"        "consease_acres"        "consease_avgfarm"      "notill_farms"          "notill_acres"         
#"notill_avgfarm"        "constill_farms"        "constill_acres"        "constill_avgfarm"      "convtill_farms"       
#"convtill_acres"        "convtill_acres.1"      "cc_farms"              "cc_acres"              "cc_avgfarm"  
#"ditches_acres_avgfarm"
#
#-example: SHEAF_agcensus_map(2012, "tile_farms")

SHEAF_agcensus_map <- function(year, agcensuscontrols) {


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
  download.file("https://nextcloud.sesync.org/index.php/s/ZgwtMn5cTnCocdR/download",temp)
  outDir<-"/tmp"
  unzip(temp,exdir=outDir)
  
  states <- readShapePoly('/tmp/states.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  states <- subset(states, STATE_NAME != "Alaska")
  states <- subset(states, STATE_NAME != "Hawaii")
  




temp <- tempfile()
download.file("https://nextcloud.sesync.org/index.php/s/paxKXxFGnZaHbbN/download",temp)
outDir<-"/tmp"
unzip(temp,exdir=outDir)

setwd("/tmp/counties_conus")

counties <- readShapePoly('UScounties_conus.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

options(warn = oldw)

#--

n1 <- read.csv("https://nextcloud.sesync.org/index.php/s/mdy8q3MWcZ9iLms/download", header=TRUE)
n1 <- n1[,1:25]


xxx <- subset(n1, year == year)

xxx$county <- tolower(xxx$county)
xxx$county <- sapply(xxx$county, simpleCap)

colnames(xxx)[4] <- "NAME"
colnames(xxx)[3] <- "STATE_NAME"

id = 5:25
xxx[id] = as.numeric(unlist(xxx[id]))


m <- merge(counties, xxx, by=c("STATE_NAME", "NAME"))
#m <- subset(m, tile_farms != 0)

m <- subset(m, eval(parse(text=paste("m$", agcensuscontrols, sep=""))) != 0)

palz1 <- rev(brewer.pal(9, "Spectral"))

palz <- colorRampPalette(palz1)

newone <- eval(parse(text=paste("m$", agcensuscontrols, sep="")))
newone[is.na(newone)] <- 0

palData <- classIntervals(newone, style="hclust")
colors <- findColours(palData, palz(100))


#pal <- colorNumeric(palette = c("white", "orange", "darkorange", "red", "darkred"),
#                    domain = eval(parse(text=paste("m$",input$agcensuscontrols , sep=""))))

pal2 <- colorNumeric(rev(brewer.pal(9, "Spectral")), na.color = "#ffffff",
                     domain = newone)



exte <- as.vector(extent(states))

label <- paste(sep = "<br/>", m$STATE_NAME, m$NAME, round(newone, 0))
markers <- data.frame(label)
labs <- as.list(newone)

title <- "TEST"

leaflet(data = m) %>% addProviderTiles("Stamen.TonerLite") %>% fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(color = ~pal2(newone), popup = markers$label,  weight = 1) %>%
  addLegend(pal = pal2, values = ~newone, opacity = 1, title = paste("Agcensus <br>", year, "<br>", "number of ", agcensuscontrols, sep=""),
            position = "bottomleft")

}