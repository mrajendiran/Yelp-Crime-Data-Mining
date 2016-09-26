setwd("~/Documents/MSDS/Data Mining/Case Study 1")
library(jsonlite)
library(tibble)
library(dplyr)
library(rgdal)
library(tools)
library(dplyr)
library(ggplot2)
library(readr)
library(raster)
library(MASS)
library(RColorBrewer)
library(kedd)
library(ks)
library(ggmap)
library(ROCR)

yelp_dataset <- read.csv("yelp_dataset.csv")
yelp_review_dataset <- read.csv("yelp_dataset_review.csv")
yelp_businesses <- yelp_dataset
yelp_master <- read.csv("yelp_master.csv")

### .JSON to .CSV FOR YELP DATA ###

# # Convert .JSON to dataset with Urbana Business Data
# yelp <- stream_in(file("yelp_academic_dataset_business.json"))
# yelp_data <- flatten(yelp)
# str(yelp_data)
# yelp_data <- as_data_frame(yelp_data)
# yelp_dataset <- yelp_data %>% filter(city=="Urbana")
# yelp_dataset[,"categories"] <- NULL
# yelp_dataset[,"full_address"] <- NULL
# 
# # Create Backup
# yelp_dataset_backup <- yelp_dataset
# yelp_dataset <- yelp_dataset_backup
# 
# # Create CSV without categories included:
# yelp_dataset <- as.matrix(yelp_dataset)
# write.csv(yelp_dataset, file="yelp_dataset.csv")

# # Convert .JSON to dataset with Urbana Business Review Data
# yelp_review <- stream_in(file("yelp_academic_dataset_review.json"))
# yelp_review_data <- flatten(yelp_review)
# str(yelp_review_data)
# yelp_review_data <- as_data_frame(yelp_review_data)
# 
# # Create Backup
# yelp_review_dataset_backup <- yelp_review_data
# # use if backup needed
# yelp_review_data <- yelp_review_dataset_backup
# 
# # Filter Yelp Review Data
# business <- yelp_dataset[,"business_id"]
# business <- as.data.frame(unlist(business))
# yelp_review_dataset <- filter(yelp_review_data, yelp_review_data$business_id %in% business[,1])
# 
# # Create CSV for yelp reviews included:
# yelp_review_dataset <- as.matrix(yelp_review_dataset)
# write.csv(yelp_review_dataset, file="yelp_dataset_review.csv")

### CLEAN UP YELP DATA ###

# # Merge Yelp Review Data with Yelp Data and Clean up Yelp Master Data
# yelp_master <- merge(yelp_review_dataset, yelp_dataset, by="business_id")
# names(yelp_master)[c(4,7,20,22)] <- c("review_stars", "review_type", "business_stars", "business_type")
# yelp_master <- select(yelp_master, -starts_with("attributes.Dietary"), -starts_with("attributes.Hair"), -starts_with("attributes.Ambience"), -starts_with("neighborhoods"))
# # get rid of any NAs in the longitude/latitude 
# yelp_master <- yelp_master[complete.cases(yelp_master$longitude),]
# yelp_master <- yelp_master[complete.cases(yelp_master$latitude),]

# # Create CSV for master dataset of yelp reviews and businesses:
# yelp_master[,c("categories", "full_address")] <- NULL
# write.csv(yelp_master, file="yelp_master.csv")

### CLEAN UP CRIME DATA ###

# # read in raw crime data
# urbana_data <- read_csv('Police_Incidents_Since_1988.csv')
# 
# # string split the mapping address to extract the Lat and Long
# splt <-strsplit(urbana_data$`MAPPING ADDRESS`, "\n")
# 
# # cycle through to map split string into data frame
# dftemp <- data.frame(matrix(ncol=1))
# for (i in 1:3)
# {
#   temp <- as.data.frame(sapply(splt, function (x) x[i]))
#   dftemp <- cbind(dftemp, temp)
# }
# colnames(dftemp) <- c("x", "Street Address", "City", "Lat and Long")
# dftemp <- dftemp[2:4]
# 
# # remove "()" on Lat and Long
# dftemp$`Lat and Long` <-sub("\\(", "", dftemp$`Lat and Long`)
# dftemp$`Lat and Long` <-sub("\\)", "", dftemp$`Lat and Long`)
# 
# # split Lat and Long on ","
# splt2 <- strsplit(as.character(dftemp$`Lat and Long`), ",")
# 
# # cycle through to map split string into data frame
# dftemp2 <-data.frame(matrix(ncol=1))
# for (i in 1:2)
# {
#   temp2 <- as.data.frame(sapply(splt2, function (x) x[i]))
#   dftemp2 <- cbind(dftemp2, temp2)
# }
# colnames(dftemp2) <- c("x", "Latitude", "Longitude")
# dftemp2 <- dftemp2[2:3]
# 
# # cbind data together
# urbana_data <- cbind(urbana_data, dftemp, dftemp2)
# 
# # remove lines with "NA" in Lat and Long
# urbana_data <- urbana_data[!(is.na(urbana_data$`Lat and Long`)),]
# 
# # filter to include only crimes after 2004
# urbana_data <- filter(urbana_data, urbana_data$`YEAR OCCURRED` > 2004)
# 
# # write data to csv
# write.csv(urbana_data, "UrbanaData.csv")

## END CRIME CLEAN UP ##

# change long/lat to meters and bind to crime_data
yelp_master_lonlat = cbind(yelp_master$longitude, yelp_master$latitude)
yelp_master_meters = project(yelp_master_lonlat, proj="+init=epsg:26971")
head(yelp_master_meters)
yelp_master <- cbind(yelp_master, yelp_master_meters[,1], yelp_master_meters[,2])
names(yelp_master)[c(82,83)] <- c("longmeters", "latmeters")

# Load the crime data and clean up the crime data
# Filter for years 2005 - 2015
# Add 1 to all crimes
# Remove rows that have NAs in Longitude or Latitude
crime_data <- read.csv("UrbanaData.csv")
crime_data <- filter(crime_data, crime_data$YEAR.OCCURRED > 2004) #!!!
crime_data <- cbind(crime_data, 1)
names(crime_data)[c(37, 36, 35)] <- c("response", "long", "lat")
crime_data <- crime_data[complete.cases(crime_data$long),]
crime_data <- crime_data[complete.cases(crime_data$lat),]

# change long/lat to meters and bind to crime_data
crimes.locations.lonlat = cbind(crime_data$long, crime_data$lat)
crimes.locations.meters = project(crimes.locations.lonlat, proj="+init=epsg:26971")
head(crimes.locations.meters)
crime_data <- cbind(crime_data, crimes.locations.meters[,1], crimes.locations.meters[,2])
names(crime_data)[c(38,39)] <- c("longmeters", "latmeters")

# Clean Crime Data so error coordinates are not included 
crime_data <- crime_data[crime_data$lat < 41 & crime_data$lat > 39 & crime_data$long < -87 & crime_data$long > -89, ]

## URBANA CRIME HEAT MAP ##

# Download the base map
urbana <- get_map(location = "Urbana, Illinois", zoom = 14)
# Draw the heat map
map_total <- ggmap(urbana, extent = "device", darken=0.7) + geom_density2d(data = crime_data, aes(x = long, y = lat), size = 0.3) + 
  stat_density2d(data = crime_data, 
                 aes(x = long, y = lat, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE) +
  geom_point(aes(x=longitude, y=latitude), data=yelp_businesses, col="orange", size = 0.2, alpha=0.4)
map_total

### FILTER CRIME AND YELP DATA BY YEAR ###

crime_data05 <- filter(crime_data, crime_data$YEAR.OCCURRED == 2005)
yelp_data05 <- filter(yelp_master, grepl('2005', date))

crime_data06 <- filter(crime_data, crime_data$YEAR.OCCURRED == 2006)
yelp_data06 <- filter(yelp_master, grepl('2006', date))

crime_data07 <- filter(crime_data, crime_data$YEAR.OCCURRED == 2007)
yelp_data07 <- filter(yelp_master, grepl('2007', date))

crime_data08 <- filter(crime_data, crime_data$YEAR.OCCURRED == 2008)
yelp_data08 <- filter(yelp_master, grepl('2008', date))

crime_data09 <- filter(crime_data, crime_data$YEAR.OCCURRED == 2009)
yelp_data09 <- filter(yelp_master, grepl('2009', date))

crime_data10 <- filter(crime_data, crime_data$YEAR.OCCURRED == 2010)
yelp_data10 <- filter(yelp_master, grepl('2010', date))

crime_data11 <- filter(crime_data, crime_data$YEAR.OCCURRED == 2011)
yelp_data11 <- filter(yelp_master, grepl('2011', date))

crime_data12 <- filter(crime_data, crime_data$YEAR.OCCURRED == 2012)
yelp_data12 <- filter(yelp_master, grepl('2012', date))

crime_data13 <- filter(crime_data, crime_data$YEAR.OCCURRED == 2013)
yelp_data13 <- filter(yelp_master, grepl('2013', date))
# pare down data so each row is a unique business
yelp_data13 <- yelp_data13[!duplicated(yelp_data13[,2]),]

crime_data14 <- filter(crime_data, crime_data$YEAR.OCCURRED == 2014)
yelp_data14 <- filter(yelp_master, grepl('2014', date))
# pare down data so each row is a unique business
yelp_data14 <- yelp_data14[!duplicated(yelp_data14[,2]),]

crime_data15 <- filter(crime_data, crime_data$YEAR.OCCURRED == 2015)
yelp_data15 <- filter(yelp_master, grepl('2015', date))
# pare down data so each row is a unique business
yelp_data15 <- yelp_data15[!duplicated(yelp_data15[,2]),]