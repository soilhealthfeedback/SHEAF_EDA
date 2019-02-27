#--Landuse acreage from cornbelt - totals per county

rented_land <- read.csv("https://nextcloud.sesync.org/index.php/s/Dtt6E5FDQtFgDit/download")


ag_land_corn <- read.csv("https://nextcloud.sesync.org/index.php/s/P92Df7gYgXKjYXa/download")
colnames(ag_land_corn) <- c("FIPS", "countycode", "row", "column", "type", "State", "Statecode", "label", "County", "cpubval", "Cropland_Acres", "Percent")

ag_land_corn$Cropland_Acres <- as.numeric(as.character(ag_land_corn$Cropland_Acres))

ag_land_corn2 <- subset(ag_land_corn, Cropland_Acres != "NA")


countyFIPS <- read.csv("https://nextcloud.sesync.org/index.php/s/wcFmKrSZW6Pr6D2/download")

countyFIPS$FIPS <- sprintf("%05d",countyFIPS$FIPS)

alc <- merge(ag_land_corn2, countyFIPS, by  = "FIPS")

alc2 <- aggregate(alc$Cropland_Acres, by = list(alc$STATE_NAME, alc$NAME, alc$FIPS), FUN = 'mean')
colnames(alc2) <- c("State", "County", "FIPS", "AGCENSUS_Cropland_Acres")
alc2$FIPS <- sprintf("%05d",alc2$FIPS)


#cash rent

agcode <- read.csv("https://nextcloud.sesync.org/index.php/s/ZpAQgjxg8Pbb6kL/download")
FIPS_county <- sprintf("%03d",agcode$X0.1)
FIPS_state <- sprintf("%02d",agcode$X1)
agcode$FIPS <- paste(FIPS_state, FIPS_county, sep="")
agcode2 <- subset(agcode, X1.1 != 2)
agcode2 <- subset(agcode2, X0.1 != 0)
agcode2 <- subset(agcode2, X0.1 != 999)
agcode2 <- subset(agcode2, X0.1 != 888)


agcode2 <- cbind(agcode2[,2], agcode2[,5])
colnames(agcode2) <- c("Ag.District.Code", "FIPS")
agcode2 <- as.data.frame(agcode2)
agcode2 <- join(countyFIPS, agcode2, by = "FIPS")


cashrent <- read.csv("https://nextcloud.sesync.org/index.php/s/asgFDLoZKGdaLHy/download")
cashrent_FIPS_county <- sprintf("%03d",cashrent$County.ANSI)
cashrent_FIPS_state <- sprintf("%02d",cashrent$State.ANSI)
cashrent$FIPS <- paste(cashrent_FIPS_state, cashrent_FIPS_county, sep="")

cashrent_NA <- cashrent[,c(2,6,8,9,10,19,21,23,25)]
colnames(cashrent_NA) <- c("Year", "State","Ag.District", "Ag.District.Code", "County", "Irrigated_Rent_Cropland", "NonIrrigated_Rent_Cropland", "Rent_Pastureland", "FIPS")
cashrent_other <- subset(cashrent_NA, County == "OTHER (COMBINED) COUNTIES")


cashrent2 <- merge(cashrent, agcode2, by = "FIPS", all = TRUE)
cashrent2 <- cashrent2[,c(1,3,7,9,10,11,20,22,24)]
colnames(cashrent2) <- c("FIPS", "Year", "State", "Ag.District", "Ag.District.Code", "County", "Irrigated_Rent_Cropland", "NonIrrigated_Rent_Cropland", "Rent_Pastureland")



#cashrent2$Irrigated_Rent_Cropland <- trimws(cashrent2$Irrigated_Rent_Cropland)
cashrent2$Irrigated_Rent_Cropland <- as.numeric(as.character(cashrent2$Irrigated_Rent_Cropland))
cashrent2$NonIrrigated_Rent_Cropland <- as.numeric(as.character(cashrent2$NonIrrigated_Rent_Cropland))

#cr <- subset(cashrent2, Year == 2012)


out = NULL
for (l in 2008:2017) {
  for (n in unique(cashrent2$State)) {
    cash <- subset(cashrent2, Year ==l & State == n)
    for (m in unique(cash$Ag.District.Code)) {
      
      Other <- subset(cash, Ag.District.Code == m)
      Other_Other <- subset(Other, County == "OTHER (COMBINED) COUNTIES")
      
      Other$Irrigated_Rent_Cropland[is.na(Other$Irrigated_Rent_Cropland)] <- Other_Other$Irrigated_Rent_Cropland[1]
      Other$Irrigated_Rent_Cropland[is.na(Other$Irrigated_Rent_Cropland)] <- mean(na.omit(Other$Irrigated_Rent_Cropland))
      
      out=rbind(out,Other)
      
      #df$baglocatie[empty] <- df$knmilocatie[empty]
      
    }}}



out2 = NULL
for (l in 2008:2017) {
  for (n in unique(cashrent2$State)) {
    cash <- subset(cashrent2, Year ==l & State == n)
    for (m in unique(cash$Ag.District.Code)) {
      
      Other <- subset(cash, Ag.District.Code == m)
      Other_Other <- subset(Other, County == "OTHER (COMBINED) COUNTIES")
      
      Other$NonIrrigated_Rent_Cropland[is.na(Other$NonIrrigated_Rent_Cropland)] <- Other_Other$NonIrrigated_Rent_Cropland[1]
      Other$NonIrrigated_Rent_Cropland[is.na(Other$NonIrrigated_Rent_Cropland)] <- mean(na.omit(Other$NonIrrigated_Rent_Cropland))
      
      out2=rbind(out2,Other)
      
      #df$baglocatie[empty] <- df$knmilocatie[empty]
      
    }}}

out3 = NULL
for (l in 2008:2017) {
  for (n in unique(cashrent2$State)) {
    cash <- subset(cashrent2, Year ==l & State == n)
    for (m in unique(cash$Ag.District.Code)) {
      
      Other <- subset(cash, Ag.District.Code == m)
      Other_Other <- subset(Other, County == "OTHER (COMBINED) COUNTIES")
      
      Other$Rent_Pastureland[is.na(Other$Rent_Pastureland)] <- Other_Other$Rent_Pastureland[1]
      Other$Rent_Pastureland[is.na(Other$Rent_Pastureland)] <- mean(na.omit(Other$Rent_Pastureland))
      
      out3=rbind(out3,Other)
      
      #df$baglocatie[empty] <- df$knmilocatie[empty]
      
    }}}

out4 <- cbind(out[,1:6], out$Irrigated_Rent_Cropland, out2$NonIrrigated_Rent_Cropland, out3$Rent_Pastureland)

out4$rent_average <- apply(out4[,7:9],1,function(x) mean(na.omit(x)))

cashrent3 <- out4

subset(cashrent3, Year == 2012)


alc_cashrent <- merge(cashrent3, alc2, by = "FIPS")
alc_cashrent2 <- alc_cashrent[,c(1,2,4,5,7,8,9,10,11,12,13)]



#alc_cashrent2 <- alc_cashrent[,c(1,2,3,4,6,12,23,25,27)]
colnames(alc_cashrent2) <- c("FIPS", "Year", "Ag.District", "Ag.District.Code", "Irrigated_Rent_Cropland", "NonIrrigated_Rent_Cropland", "Rent_Pastureland", "Rent_average", "State", "County", "Total_Cropland_Acres")
alc_cashrent2$Total_Rent <- alc_cashrent2$Rent_average * alc_cashrent2$Total_Cropland_Acres


write.csv(alc_cashrent2, file = "/nfs/soilsesfeedback-data/data/agcensus/total_rent.csv")

