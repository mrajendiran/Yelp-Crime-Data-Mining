library(readr)
library(dplyr)

# read in raw crime data
urbana_data <- read_csv('Police_Incidents_Since_1988.csv')

# string split the mapping address to extract the Lat and Long
splt <-strsplit(urbana_data$`MAPPING ADDRESS`, "\n")

# cycle through to map split string into data frame
dftemp <- data.frame(matrix(ncol=1))
for (i in 1:3)
{
  temp <- as.data.frame(sapply(splt, function (x) x[i]))
  dftemp <- cbind(dftemp, temp)
}
colnames(dftemp) <- c("x", "Street Address", "City", "Lat and Long")
dftemp <- dftemp[2:4]

# remove "()" on Lat and Long
dftemp$`Lat and Long` <-sub("\\(", "", dftemp$`Lat and Long`)
dftemp$`Lat and Long` <-sub("\\)", "", dftemp$`Lat and Long`)

# split Lat and Long on ","
splt2 <- strsplit(as.character(dftemp$`Lat and Long`), ",")

# cycle through to map split string into data frame
dftemp2 <-data.frame(matrix(ncol=1))
for (i in 1:2)
{
  temp2 <- as.data.frame(sapply(splt2, function (x) x[i]))
  dftemp2 <- cbind(dftemp2, temp2)
}
colnames(dftemp2) <- c("x", "Latitude", "Longitude")
dftemp2 <- dftemp2[2:3]

# cbind data together
urbana_data <- cbind(urbana_data, dftemp, dftemp2)

# remove lines with "NA" in Lat and Long
urbana_data <- urbana_data[!(is.na(urbana_data$`Lat and Long`)),]

# filter to include only crimes after 2004
urbana_data <- filter(urbana_data, urbana_data$`YEAR OCCURRED` > 2004)

# write data to csv
write_csv(urbana_data, "UrbanaData.csv")
