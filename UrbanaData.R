library(readr)

urbana_data <- read_csv('Police_Incidents_Since_1988.csv')

splt <-strsplit(urbana_data$`MAPPING ADDRESS`, "\n")

dftemp <- data.frame(matrix(ncol=1))
for (i in 1:3)
{
  temp <- as.data.frame(sapply(splt, function (x) x[i]))
  dftemp <- cbind(dftemp, temp)
}
colnames(dftemp) <- c("x", "Street Address", "City", "Lat and Long")
dftemp <- dftemp[2:4]
dftemp$`Lat and Long` <-sub("\\(", "", dftemp$`Lat and Long`)
dftemp$`Lat and Long` <-sub("\\)", "", dftemp$`Lat and Long`)

splt2 <- strsplit(as.character(dftemp$`Lat and Long`), ",")

dftemp2 <-data.frame(matrix(ncol=1))
for (i in 1:2)
{
  temp2 <- as.data.frame(sapply(splt2, function (x) x[i]))
  dftemp2 <- cbind(dftemp2, temp2)
}
colnames(dftemp2) <- c("x", "Latitude", "Longitude")
dftemp2 <- dftemp2[2:3]

urbana_data <- cbind(urbana_data, dftemp, dftemp2)

write_csv(urbana_data, "UrbanaData.csv")
