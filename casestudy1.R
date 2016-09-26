rm(list=ls())

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
library(ggmap)
library(ROCR)

# load the data
crime_data <- read.csv("UrbanaData.csv")
yelp_businesses <- read.csv("yelp_master.csv")
yelp_master <- read.csv("yelp_master.csv")

# Download the base map
urbana <- get_map(location = "Urbana, Illinois", zoom = 14)
# Draw the heat map
map_total <- ggmap(urbana, extent = "device", darken=0.7) + geom_density2d(data = crime_data, aes(x = Longitude, y = Latitude), size = 0.3) + 
  stat_density2d(data = crime_data, 
                 aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE) +
  geom_point(aes(x=longitude, y=latitude), data=yelp_businesses, col="orange", size = 0.2, alpha=0.4)
map_total
############
## UPDATE ##
############

# reproject lon/lat points to meters
crime.locations.lonlat = cbind(crime_data$Longitude, crime_data$Latitude)
crime.locations.meters = project(crime.locations.lonlat, proj = "+init=epsg:26971")
yelp.locations.lonlat = cbind(yelp_master$longitude, yelp_master$latitude)
yelp.locations.meters = project(yelp.locations.lonlat, proj="+init=epsg:26971")

# add x,y coordinates
crime_meters = as.data.frame(cbind(crime.locations.meters[,1],crime.locations.meters[,2]))
names(crime_meters) = c("x", "y")
write_csv(crime_data, "crime_data.csv")
crime_data = cbind(crime_data, crime_meters)
yelp_meters = as.data.frame(cbind(yelp.locations.meters[,1], yelp.locations.meters[,2]))
names(yelp_meters) = c("x", "y")
yelp_master = cbind(yelp_master, yelp_meters)

# Filter data by year for test sets (for crime and yelp data)
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

crime_data14 <- filter(crime_data, crime_data$YEAR.OCCURRED == 2014)
yelp_data14 <- filter(yelp_master, grepl('2014', date))

crime_data15 <- filter(crime_data, crime_data$YEAR.OCCURRED == 2015)
yelp_data15 <- filter(yelp_master, grepl('2015', date))

# Get non-crime data and combine with crime data
# Urbana City Bounds
# "northeast" : {
#   "lat" : 40.1574203,
#   "lng" : -88.1530727
# },
# "southwest" : {
#   "lat" : 40.0732478,
#   "lng" : -88.23302029999999
# }

# Create list of coordinates that span Urbana
long <- c(-88.23302, -88.15307)
lat <- c(40.07324, 40.15742)
bounds = (cbind(long, lat))
bounds.meters = project(bounds, proj ="+init=epsg:26971")
long <- seq(bounds.meters[1,1], bounds.meters[2,1], length.out = 10000 )
lat <- seq(bounds.meters[1,2], bounds.meters[2,2], length.out = 10000 )
urbana_full <- cbind(0, long, lat)
urbana_full <- as.data.frame(urbana_full)
names(urbana_full) <- c("response", "x", "y")

## TRAIN MODEL ON RESPONSES FROM 2014, USING PREDICTORS FROM 2013 ##

# Training Set for 2014 (Non-Crime coordinates and Yelp Data)
# remove crime coordinates from non-crime coordinates
urbana_full14 <- urbana_full[!(urbana_full$x %in% crime_data14$x),]
# bind the non-crime coordinates with the crime coordinates

##############
### UPDATE ###
##############
train_crime_data14 = cbind(1, crime_data14[,c("x", "y")])
colnames(train_crime_data14) = c("response", "x", "y")
train14 <- rbind(urbana_full14, train_crime_data14)

# run.spatial.kde function
run.spatial.kde = function(sample.points, est.points, sample.size)
{
  sample.points = sample.points[sample(nrow(sample.points), size=sample.size),]
  # compute optimal KDE bandwidth
  h = Hpi(sample.points, pilot = "dscalar")
  # run KDE
  est = kde(sample.points, H=h , eval.points = est.points)$estimate
  return(est)
}
# calculate crime density based on 2013
crime_density = run.spatial.kde(crime_data13[,c("x", "y")], train14[,c("x", "y")], 1000)

############################################
## need to use time stamped for parking?? ##
############################################

# remove lines with "NA" in attributes parking garage
yelp_parking_garage <- yelp_master[!(is.na(yelp_master$attributes.Parking.garage)),]
# find businesses with a parking garage
yelp_parking_garage <- yelp_parking_garage[yelp_parking_garage$attributes.Parking.garage == TRUE,]

# remove lines with "NA" in attributes parking street
yelp_parking_street <- yelp_master[!(is.na(yelp_master$attributes.Parking.street)),]
# find businesses with a street parking
yelp_parking_street <- yelp_parking_street[yelp_parking_street$attributes.Parking.street == TRUE,]

# remove lines with "NA" in attributes parking validated
yelp_parking_validated <- yelp_master[!(is.na(yelp_master$attributes.Parking.validated)),]
# find businesses with validated parking
yelp_parking_validated <- yelp_parking_validated[yelp_parking_validated$attributes.Parking.validated == TRUE,]

# remove lines with "NA" in attributes parking lot
yelp_parking_lot <- yelp_master[!(is.na(yelp_master$attributes.Parking.lot)),]
# find businesses with parking lot
yelp_parking_lot <- yelp_parking_lot[yelp_parking_lot$attributes.Parking.lot == TRUE,]

# remove lines with "NA" in attributes parking valet
yelp_parking_valet <- yelp_master[!(is.na(yelp_master$attributes.Parking.valet)),]
yelp_parking_valet <- yelp_parking_valet[yelp_parking_valet$attributes.Parking.valet == TRUE,]

# there are no businesses with valet parking or who validate parking- remove those from hypothesis

# functions for min distance
get.euc.distance = function(point.1, point.2)
{
  return (sqrt(sum((point.1 - point.2)^2)))
}

get.min.distances = function(points.1, points.2)
{
  return (apply(points.1, 
                1,
                function(point.1)
                {
                  distances = apply(points.2,
                                    1,
                                    function(point.2, point.1)
                                    {
                                      return (get.euc.distance(point.1, point.2))
                                    },
                                    point.1)
                  
                  return (min(distances))
                }))
}

# calculate distance to nearest business that has a parking garage from each training point
garage.min.dist = get.min.distances(train14[,c("x","y")], yelp_parking_garage[,c("x","y")])
# calculate distance to nearest business that has street parking from each training point
street.min.dist = get.min.distances(train14[,c("x","y")], yelp_parking_street[,c("x","y")])
# calculate distance to nearest business that has a parking lot from each training point
lot.min.dist = get.min.distances(train14[,c("x","y")], yelp_parking_lot[,c("x","y")])

# add predictor columns (density and distance to businesses with different parking attributes) to training data
train14 = cbind(train14, crime_density, garage.min.dist, street.min.dist, lot.min.dist)

# fit GLM - all variables
glm.fit.all = glm(response ~ . -x -y, data = train14, family = binomial)
# fit GLM - garage
glm.fit.garage = glm(response ~ garage.min.dist, data = train14, family = binomial)
# fit GLM - street
glm.fit.street = glm(response ~ street.min.dist, data = train14, family = binomial)
# fit GLM - lot
glm.fit.lot = glm(response ~ lot.min.dist, data = train14, family = binomial)
# fit GLM - garage and lot
glm.fit.garagelot = glm(response ~ garage.min.dist + lot.min.dist, data = train14, family = binomial)

# predict responses on 2015, using the GLM model and predictors from 2014

# build dataframe to predict, based on 2014 data
# Create list of coordinates that span Urbana
long <- c(-88.23302, -88.15307)
lat <- c(40.07324, 40.15742)
bounds = (cbind(long, lat))
bounds.meters = project(bounds, proj ="+init=epsg:26971")
long <- seq(bounds.meters[1,1], bounds.meters[2,1], length.out = 10000 )
lat <- seq(bounds.meters[1,2], bounds.meters[2,2], length.out = 10000 )
prediction.points <- cbind(long, lat)
prediction.points <- as.data.frame(prediction.points)
names(prediction.points) <- c("x", "y")
add.prediction.points <- crime_data15[37:38]
prediction.points <- rbind(prediction.points, add.prediction.points)

# calculate crime density based on 2014
crime_density = run.spatial.kde(crime_data14[,c("x", "y")], prediction.points, 1000)

# calculate distance to nearest business that has a parking garage from each prediction point
garage.min.dist = get.min.distances(prediction.points, yelp_parking_garage[,c("x","y")])
# calculate distance to nearest business that has street parking from each prediction point
street.min.dist = get.min.distances(prediction.points, yelp_parking_street[,c("x","y")])
# calculate distance to nearest business that has a parking lot from each prediction point
lot.min.dist = get.min.distances(prediction.points, yelp_parking_lot[,c("x","y")])

predict15 = as.data.frame(cbind(prediction.points, crime_density, garage.min.dist, street.min.dist, lot.min.dist))

# run prediction - on all variables
threats.all = predict(glm.fit.all, predict15, type = "response")
# run prediction - on garage
threats.garage = predict(glm.fit.garage, predict15, type = "response")
# run prediction - on street
threats.street = predict(glm.fit.street, predict15, type = "response")
# run prediction - on lot
threats.lot = predict(glm.fit.lot, predict15, type = "response")
# run prediction - garage and lot
threats.garagelot = predict(glm.fit.garagelot, predict15, type = "response")

# build prediction dataframe for evaluation- all
crime_predict15_all = cbind(prediction.points, threats.all, 0)
names(crime_predict15_all) = c("x", "y", "threat", "true")
crime_predict15_all[10001:21023,4] <- 1
# build prediction dataframe for evaluation- garage
crime_predict15_garage = cbind(prediction.points, threats.garage, 0)
names(crime_predict15_garage) = c("x", "y", "threat", "true")
crime_predict15_garage[10001:21023,4] <- 1
# build prediction dataframe for evaluation- street
crime_predict15_street = cbind(prediction.points, threats.street, 0)
names(crime_predict15_street) = c("x", "y", "threat", "true")
crime_predict15_street[10001:21023,4] <- 1
# build prediction dataframe for evaluation- lot
crime_predict15_lot = cbind(prediction.points, threats.lot, 0)
names(crime_predict15_lot) = c("x", "y", "threat", "true")
crime_predict15_lot[10001:21023,4] <- 1
# build prediction dataframe for evaluation- garage and lot
crime_predict15_garagelot = cbind(prediction.points, threats.garagelot, 0)
names(crime_predict15_garagelot) = c("x", "y", "threat", "true")
crime_predict15_garagelot[10001:21023,4] <- 1

# ROC Curve - all
pred <- prediction(crime_predict15_all$threat, crime_predict15_all$true)
pref <- performance(pred, "tpr", "fpr")
plot(pref, col = 'mediumvioletred')
abline(a=0, b=1)
# ROC Curve - garage
pred <- prediction(crime_predict15_garage$threat, crime_predict15_garage$true)
pref <- performance(pred, "tpr", "fpr")
plot(pref, col = 'deepskyblue', add = TRUE)
# ROC Curve - street
pred <- prediction(crime_predict15_street$threat, crime_predict15_street$true)
pref <- performance(pred, "tpr", "fpr")
plot(pref, col = 'yellow', add = TRUE)
# ROC Curve - lot
pred <- prediction(crime_predict15_lot$threat, crime_predict15_lot$true)
pref <- performance(pred, "tpr", "fpr")
plot(pref, col = 'orangered', add = TRUE)
# ROC Curve - garage and lot
pred <- prediction(crime_predict15_garagelot$threat, crime_predict15_garagelot$true)
pref <- performance(pred, "tpr", "fpr")
plot(pref, col = 'purple', add = TRUE)