#cdl

SHEAF_cdl <- function(year) {

#cdl import
cdl <- read.csv("https://nextcloud.sesync.org/index.php/s/JwGgJAXfoEJN5m9/download")
cdl$fips <- sprintf("%05d",cdl$fips)
#remove

cdl <- subset(cdl, Category != 
                "Sod/Grass Seed" & Category !=
                "Open Water" & Category !=
                "Developed/Open Space"& Category !=
                "Developed/Low Intensity"& Category !=
                "Developed/Medium Intensity"& Category !=
                "Developed/High Intensity"& Category !=
                "Barren"& Category !=
                "Deciduous Forest"& Category !=
                "Evergreen Forest"& Category !=
                "Mixed Forest"& Category !=
                "Shrubland"& Category !=
                "Green/Pasture"& Category !=
                "Woody Wetlands"& Category !=
                "Herbaceous Wetlands"& Category !=
                "Christmans Trees"& Category !=
                "Other Tree Crops"& Category !=
                "Aquaculture"& Category !=
                "Perennial Ice/Snow"& Category !=
                "Grass/Pasture"& Category !=
                "Dbl Crop WinWht/Soybeans"& Category !=
                "Dbl Crop WinWht/Sorghum"& Category !=
                "Dbl Crop WinWht/Cotton"& Category !=
                "Dbl Crop Barley/Sorghum"& Category !=
                "Dbl Crop WinWht/Corn"& Category !=
                "Dbl Crop Soybeans/Cotton"& Category !=
                "Dbl Crop Soybeans/Oats"& Category !=
                "Dbl Crop Corn/Soybeans"& Category !=
                "Dbl Crop Oats/Corn"& Category !=
                "Dbl Crop Barley/Soybeans"& Category !=
                "Dbl Crop Barley/Corn"& Category !=
                "Dbl Crop Lettuce/Durum Wht"& Category !=
                "Dbl Crop Lettuce/Cantaloupe"& Category !=
                "Dbl Crop Durum Wht/Sorghum"& Category !=
                "Dbl Crop Lettuce/Barley"
)






#subset for barley, corn, cotton, hay, oats, rice, sorghum, soybeans, and wheat 
#cdl <- subset(cdl, Category == c("Barley", "Corn", "Hay", "Oats", "Rice", "Sorghum", "Soybeans", "Wheat"))


#aggregates cdl by year and fips
cdlsum <- aggregate(cdl$Acreage, by = list(cdl$year, cdl$fips), FUN = "sum")
colnames(cdlsum) <- c("year", "fips", "total_acreage")

#aggregating truncates FIPS.  this fixes that.
#FIPS_modified <- sprintf("%05d",cdlsum$FIPS)

#add fixed FIPS
#cdlsum2 <- cbind(cdlsum, FIPS_modified)

#remove the truncated FIPS
#cdlsum3 <- cdlsum2[,-2]

#rename the columns
#colnames(cdlsum3) <- c("year", "total_acreage", "fips")

#now merge total acreage back into the original cdl
cdlsum4 <- merge(cdl, cdlsum, by = c("fips", "year"))

cdlsum5 <- subset(cdlsum4, Category == "Barley" | Category ==  "Corn" | Category ==  "Hay" | Category ==  "Oats" | Category ==  "Rice" | Category ==  "Sorghum" | Category ==  "Soybeans" | Category ==  "Wheat" | Category == "Winter Wheat" | Category == "Spring Wheat" | Category == "Durum Wheat")

#

tototalcropland <- read.csv("https://files.sesync.org/index.php/s/Ly4TyC3RipXdkSG/download")

totalcropland$state_fips_code <- sprintf("%02d", totalcropland$state_fips_code)
totalcropland$county_code <- sprintf("%03d", totalcropland$county_code)
totalcropland$Fips <- paste(totalcropland$state_fips_code, totalcropland$county_code, sep="")

totalcropland <- cbind.data.frame(totalcropland[,13], totalcropland[,21], totalcropland[,41])
colnames(totalcropland) <- c("Year", "Cropland_Acres", "Fips")

totalcropland$Cropland_Acres <- as.numeric(gsub(",","",totalcropland$Cropland_Acres))
totalcropland_year <- subset(totalcropland, Year == 2012)
colnames(totalcropland_year)[3] <- "fips"


#--

cdlsum5 <- subset(cdlsum5, year == 2012)

cdlsum5a <- merge(cdlsum5, totalcropland_year, by=c("fips"))

#now calculate the percentage of each crop acreage by county
cdlsum5a$acreage_pct <- cdlsum5a$Acreage^2/cdlsum5a$Cropland_Acres^2

cdlsum6 <- aggregate(cdlsum5a$acreage_pct, by = list(cdlsum5a$fips), FUN = "sum")
cdlsum6$CDI <- 1-cdlsum6$x
colnames(cdlsum6) <- c("FIPS", "CDL_ratio", "CDI")

write.csv(cdlsum6, file = paste("/nfs/soilsesfeedback-data/raw_data/cdl/crop_diversity.csv", sep=""))


temp <- tempfile()
download.file("https://nextcloud.sesync.org/index.php/s/SDJ5P4R6DDmt4FF/download",temp)
outDir<-"/tmp"
unzip(temp,exdir=outDir)

setwd("/tmp")

counties <- readShapePoly('UScounties_conus.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


#write.csv(cdlsum6, file = "/nfs/soilsesfeedback-data/data/cdl/crop_diversity.csv")

cdlsum7 <- cdlsum6
colnames(cdlsum7[1]) <- c("FIPS")

m <- merge(counties, cdlsum7, by=c("FIPS"))


#pal <- colorNumeric(palette = c("white", "orange", "darkorange", "red", "darkred"),
#                    domain = eval(parse(text=paste("m$",input$agcensuscontrols , sep=""))))

pal2 <- colorNumeric(rev(brewer.pal(40, "Spectral")), na.color = "#ffffff",
                     domain = m$CDI)



exte <- as.vector(extent(counties))

label <- paste(sep = "<br/>", m$FIPS, m$CDI)
markers <- data.frame(label)
labs <- as.list(m$CDI)

map <- leaflet(data = m) %>% addProviderTiles("Stamen.TonerLite") %>% fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(color = ~pal2(m$CDI), popup = markers$label,  weight = 1) %>%
  addLegend(pal = pal2, values = ~m$CDI, opacity = 1, bins = 20, title = paste("Crop Diversity <br>", year, sep=""),
            position = "bottomleft")

return(map)

}