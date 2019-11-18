library(tidyverse)
library(maps) ## for FIPS

## Create dataframe of county, state, fips to interface
#fips <- county.fips %>%
#  separate(polyname, into=c("long_state", "county"), sep=",") %>%
#  merge(., state.fips %>% select(abb, polyname) %>% rename(long_state=polyname), by=c("long_state"))

#revised fips
fips <- read.csv("https://nextcloud.sesync.org/index.php/s/wcFmKrSZW6Pr6D2/download")

fips$abb <- state.abb[match(fips$STATE_NAME,state.name)]

colnames(fips) <- c("ID", "long_state", "county", "fips", "abb")
fips$fips <- sprintf("%05d",fips$fips)

## read production expenses


tmp <- read.csv("https://files.sesync.org/index.php/s/YcfpyyFL4dsm46x/download", header=TRUE) %>%
merge(., fips, by=c("fips")) %>%
  ## remove GEO
select(-GEO) 

write.csv(tmp, file="/nfs/soilsesfeedback-data/raw_data/VarFix_NASS_FarmProductionExpenses_2007_2012_v20190801_WIDE_es1.csv", row.names=F)








