#------------------------------------------------------------------------#
# TITLE:        netcdf_access.R
#
# AUTHOR:       Erich Seamon
#
# INSTITUITON:  College of Natural Resources
#               University of Idaho
#
# DATE:         Feb 1, 2019
#
# STAGE:        netcdf access
#
# COMMENTS:     This script opens and displays netcdf data.  
#
#--Setting the working directory an d clearing the workspace-----------#


#netcdf_access(climatevar_short, climatevar, year )
#netcdf_access

netcdf_access <- function(climatevar_short, climatevar, year) {
  
  #library("ncdf")
  library("zoo")
  library("raster")
  library("sp")
  library("rgeos")
  library("rgdal")
  library("proj4")
  library("RNetCDF")
  library("ncdf4")
  library("RColorBrewer")
  library("raster")
  #library("rasterVis")
  library("latticeExtra")
  library("maptools")
  library("parallel")
  library("Evapotranspiration")
  library("plyr")
  library("data.table")
  library("sirad")
  library("rgdal")
  library("stringr")
  
  setwd("/nethome/erichs/counties/")
  
  counties <- readShapePoly('UScounties.shp', 
                            proj4string=CRS
                            ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  #subsets to CONUS
  counties <- subset(counties, STATE_NAME != "Alaska")
  counties <- subset(counties, STATE_NAME != "Hawaii")
  
  #--loop list for county by fip
  countyfiploop <- counties@data$FIPS
  
  #--data frame of county fip list
  countyfiplist <- data.frame(counties@data$FIPS)
  
  #--data frame of county names
  countynames <- data.frame(counties@data$NAME)
  statenames <- data.frame(counties@data$STATE_NAME)
  
  #combo of county names and fip for this list
  countylist <- cbind(statenames, countynames, countyfiplist)
  colnames(countylist) <- c("STATE_NAME", "NAME", "FIPS")
  
  #--number of rows in county list
  countylistrows <- nrow(countylist)
  
  #climatevar_short <- "pdsi"
  #climatevar <- "palmer_drought_severity_index"
  
  #nc <- nc_open(paste("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_", climatevar_short, "_1979_CurrentYear_CONUS.nc?lon[0:1:1385],lat[0:1:584],", climatevar, "[0:1:0][0:1:0][0:1:0],day[0:1:0]", sep=""))
  
  nc <- nc_open(paste("http://thredds.northwestknowledge.net:8080/thredds/dodsC/MET/pdsi/", climatevar_short, "_", year, ".nc?lon[0:1:1385],lat[0:1:584],", climatevar, "[0:1:0][0:1:0][0:1:0],day[0:1:0]", sep=""))
  
  #maxtemp <- nc_open('http://reacchpna.org/thredds/dodsC/agg_met_tmmx_1979_2014_WUSA.nc?lon[0:1:1385],lat[0:1:584],daily_maximum_temperature[0:1:0][0:1:0][0:1:0],day[0:1:10]')# Open a netcdf file 
  
  
  ##--
  
  # extract variable name, size and dimension
  v <- nc$var[[1]]
  size <- v$varsize
  dims <- v$ndims
  nt <- size[dims]              # length of time dimension
  lat <- nc$dim$lat$vals   # latitude position
  lon <- nc$dim$lon$vals  # longitude position
  
  # read sst variable
  r<-list()
  for (i in 1:nt) {
    start <- rep(1,dims)     # begin with start=(1,1,...,1)
    start[dims] <- i             # change to start=(1,1,...,i) to read    timestep i
    count <- size                # begin with count=(nx,ny,...,nt), reads entire var
    count[dims] <- 1             # change to count=(nx,ny,...,1) to read 1 tstep
    
    dt<-ncvar_get(nc, varid = 'palmer_drought_severity_index', start = start, count = count)
    
    # convert to raster
    r[i]<-raster(dt)
    r[i] <- rotate(r[[i]])
    extent(r[[i]]) <- c(25.0667, 49.4000, -124.7667, -67.0583)
    crs(r[[i]]) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  }
  
  r2 <- brick(r)
  
  r3 <- t(flip(r2, direction='x' ))
  
  r4 <- mean(r3)
  jj = 0
  newmatrix <- matrix(NA, nrow=countylistrows, ncol=2)
  for (l in countyfiploop) {
    jj = jj + 1
    subset_county <- counties[counties@data$FIPS == l,]
    e <- extract(r4, subset_county) 
    newmatrix[jj,1] <- mean(e[[1]])
    newmatrix[jj,2] <- l
    
  }  
  
  nm <- data.frame(NA, nrow=newmatrix, ncol=2)
  nm$pdsi <- as.numeric(as.character(newmatrix[,1]))
  nm$countyFIPS <- as.numeric(as.character(newmatrix[,2]))
  
  nm2 <- data.frame(nm$pdsi, nm$countyFIPS)
  colnames(nm2) <- c("pdsi", "FIPS")
  nm2$FIPS <- str_pad(nm2$FIPS, 5, pad = "0")
  
  pdsi <- merge(counties, nm2, by = "FIPS")
  
  
  
  #--map it
  
  pal <- colorNumeric(brewer.pal(9, "RdBu"), na.color = "#ffffff",
                      domain = pdsi$pdsi)
  
  
  exte <- as.vector(extent(counties))
  
  label <- paste(sep = "<br/>", pdsi$STATE_NAME, pdsi$pdsi)
  markers <- data.frame(label)
  labs <- as.list(pdsi$pdsi)
  
  
  leaflet(data = pdsi) %>% addProviderTiles("Stamen.TonerLite") %>% fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(color = pal(pdsi$pdsi), popup = markers$label,  weight = 1) %>%
    addLegend(pal = pal, values = pdsi$pdsi, bins = 3, opacity = 0.5, title = paste(years, " ", climatevar_short, sep=""),
              position = "bottomright")
  
}