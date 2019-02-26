simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

census <- read.csv("https://nextcloud.sesync.org/index.php/s/iEpDeymRJG6oKFr/download")

colnames(census)[4] <- "State"
colnames(census)[8] <- "County"
census$County <- as.character(census$County)
census$State <- tolower(census$State)
census$State <- sapply(census$State, simpleCap)
census$County <- tolower(census$County)
census$County <- sapply(census$County, simpleCap)

countyFIPS <- read.csv("https://nextcloud.sesync.org/index.php/s/wcFmKrSZW6Pr6D2/download")


countyFIPS$FIPS <- sprintf("%05d",countyFIPS$FIPS)
colnames(countyFIPS) <- c("ID", "State", "County", "FIPS")

census2 <- plyr::join(census, countyFIPS, by  = c("State", "County"))

census3 <- census2 %>%
  select(Year, FIPS, Data.Item, Domain, Value) 

head(census3)

library(pheatmap)
colfunc <- colorRampPalette(c("deepskyblue4", "deepskyblue", "cyan"))
plot_mat <- function(census3)
  pheatmap(census3, colfunc(100), cluster_rows = FALSE, cluster_cols = FALSE)
col_l <- names(sort(colSums(values(census3)))) #order
row_l <- names(sort(rowSums(values(census3)), decreasing = TRUE))
plot_mat(values(census3)[row_l,col_l])
plot_mat(values(census3, norm = 'p')[row_l,col_l])
plot_mat(values(census3, norm = 'rca')[row_l,col_l])
plot_mat(values(census3, norm = 'rca', filter = 1)[row_l,col_l])
?pheatmap
