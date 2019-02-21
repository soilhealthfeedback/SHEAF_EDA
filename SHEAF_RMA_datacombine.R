#--SHEAF_RMA_datacombine.R
#--combine annual text RMA files into aggregated
#commodity and damage cause files
#--author: Erich Seamon, University of Idaho
#--date: October 2018

#currently added for just 2001-2015, given that 
#acreage is only available after 2000

rma2001 <- read.csv("/nfs/soilsesfeedback-data/data/RMA/RMA_textfiles2001_2015/2001.txt", sep = "|", header = FALSE, strip.white = TRUE)
rma2002 <- read.csv("/nfs/soilsesfeedback-data/data/RMA/RMA_textfiles2001_2015/2002.txt", sep = "|", header = FALSE, strip.white = TRUE)
rma2003 <- read.csv("/nfs/soilsesfeedback-data/data/RMA/RMA_textfiles2001_2015/2003.txt", sep = "|", header = FALSE, strip.white = TRUE)
rma2004 <- read.csv("/nfs/soilsesfeedback-data/data/RMA/RMA_textfiles2001_2015/2004.txt", sep = "|", header = FALSE, strip.white = TRUE)
rma2005 <- read.csv("/nfs/soilsesfeedback-data/data/RMA/RMA_textfiles2001_2015/2005.txt", sep = "|", header = FALSE, strip.white = TRUE)
rma2006 <- read.csv("/nfs/soilsesfeedback-data/data/RMA/RMA_textfiles2001_2015/2006.txt", sep = "|", header = FALSE, strip.white = TRUE)
rma2007 <- read.csv("/nfs/soilsesfeedback-data/data/RMA/RMA_textfiles2001_2015/2007.txt", sep = "|", header = FALSE, strip.white = TRUE)
rma2008 <- read.csv("/nfs/soilsesfeedback-data/data/RMA/RMA_textfiles2001_2015/2008.txt", sep = "|", header = FALSE, strip.white = TRUE)
rma2009 <- read.csv("/nfs/soilsesfeedback-data/data/RMA/RMA_textfiles2001_2015/2009.txt", sep = "|", header = FALSE, strip.white = TRUE)
rma2010 <- read.csv("/nfs/soilsesfeedback-data/data/RMA/RMA_textfiles2001_2015/2010.txt", sep = "|", header = FALSE, strip.white = TRUE)
rma2011 <- read.csv("/nfs/soilsesfeedback-data/data/RMA/RMA_textfiles2001_2015/2011.txt", sep = "|", header = FALSE, strip.white = TRUE)
rma2012 <- read.csv("/nfs/soilsesfeedback-data/data/RMA/RMA_textfiles2001_2015/2012.txt", sep = "|", header = FALSE, strip.white = TRUE)
rma2013 <- read.csv("/nfs/soilsesfeedback-data/data/RMA/RMA_textfiles2001_2015/2013.txt", sep = "|", header = FALSE, strip.white = TRUE)
rma2014 <- read.csv("/nfs/soilsesfeedback-data/data/RMA/RMA_textfiles2001_2015/2014.txt", sep = "|", header = FALSE, strip.white = TRUE)
rma2015 <- read.csv("/nfs/soilsesfeedback-data/data/RMA/RMA_textfiles2001_2015/2015.txt", sep = "|", header = FALSE, strip.white = TRUE)


RMA <- rbind(rma2001, rma2002, rma2003, rma2004, rma2005, rma2006, rma2007, rma2008, rma2009, rma2010, rma2011, rma2012, rma2013, rma2014, rma2015)
RMA <- data.frame(RMA[,1], RMA[,3], RMA[,5], RMA[,7], RMA[,12], RMA[,13], RMA[,14], RMA[,15], RMA[,16])
colnames(RMA) <- c("year", "state", "county", "commodity", "damagecause", "monthcode", "month", "acres", "loss")



RMA_commodity_loss <- aggregate(RMA$loss, by = list(RMA$year, RMA$state, RMA$county, RMA$commodity), FUN = sum)
colnames(RMA_commodity_loss) <- c("year", "state", "county", "commodity", "loss")
RMA_commodity_loss <- subset(RMA_commodity_loss, commodity != "ADJUSTED GROSS REVENUE")

RMA_commodity_count <- aggregate(RMA$loss, by = list(RMA$year, RMA$state, RMA$county, RMA$commodity), FUN = length)
colnames(RMA_commodity_count) <- c("year", "state", "county", "commodity", "count")
RMA_commodity_count <- subset(RMA_commodity_count, commodity != "ADJUSTED GROSS REVENUE")



RMA_commodity_acres <- aggregate(RMA$acres, by = list(RMA$year, RMA$state, RMA$county, RMA$commodity), FUN = sum)
colnames(RMA_commodity_acres) <- c("year", "state", "county", "commodity", "acres")
RMA_commodity_acres <- subset(RMA_commodity_acres, commodity != "ADJUSTED GROSS REVENUE")


RMA_damage_loss <- aggregate(RMA$loss, by = list(RMA$year, RMA$state, RMA$county, RMA$commodity, RMA$damagecause), FUN = sum)
colnames(RMA_damage_loss) <- c("year", "state", "county", "commodity", "damagecause", "loss")


RMA_damage_count <- aggregate(RMA$loss, by = list(RMA$year, RMA$state, RMA$county, RMA$commodity, RMA$damagecause), FUN = length)
colnames(RMA_damage_count) <- c("year", "state", "county", "commodity", "damagecause", "count")


RMA_damage_acres <- aggregate(RMA$acres, by = list(RMA$year, RMA$state, RMA$county, RMA$commodity, RMA$damagecause), FUN = sum)
colnames(RMA_damage_acres) <- c("year", "state", "county", "commodity", "damaecause", "acres")


RMA_commodity_combined <- cbind(RMA_commodity_loss, RMA_commodity_count$count, RMA_commodity_acres$acres)
colnames(RMA_commodity_combined) <- c("year", "state", "county", "commodity", "loss", "count", "acres")

RMA_commodity_combined$lossperacre <- RMA_commodity_combined$loss / RMA_commodity_combined$acres
RMA_commodity_combined$lossperclaim <- RMA_commodity_combined$loss / RMA_commodity_combined$count
RMA_commodity_combined$acresperclaim <- RMA_commodity_combined$acres / RMA_commodity_combined$count



RMA_damage_combined <- cbind(RMA_damage_loss, RMA_damage_count$count, RMA_damage_acres$acres)
colnames(RMA_damage_combined) <- c("year", "state", "county", "commodity", "damagecause", "loss", "count", "acres")
RMA_damage_combined$lossperacre <- RMA_damage_combined$loss / RMA_damage_combined$acres
RMA_damage_combined$lossperclaim <- RMA_damage_combined$loss / RMA_damage_combined$count
RMA_damage_combined$acresperclaim <- RMA_damage_combined$acres / RMA_damage_combined$count


write.csv(RMA_commodity_combined, file = paste("/nfs/soilsesfeedback-data/data/RMA/RMA_commodity_combined.csv", sep=""))
write.csv(RMA_damage_combined, file = paste("/nfs/soilsesfeedback-data/data/RMA/RMA_damage_combined.csv", sep=""))

