source("YelpSource.R")

### CREATE TRAINING DATA SETS ### 

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
long <- seq(-88.23302, -88.15307, length.out = 20000)
lat <- seq(40.07324, 40.15742, length.out = 20000)
urbana_full <- cbind(0, long, lat, 0)
urbana_full <- as.data.frame(urbana_full)
names(urbana_full)[1] <- "response"
names(urbana_full)[4] <- "RESPONSE.TIME"

# change long/lat to meters and bind to urbana_full
urbana_longlat = cbind(urbana_full$long, urbana_full$lat)
urbana_longlat_meters = project(urbana_longlat, proj="+init=epsg:26971")
head(urbana_longlat_meters)
urbana_full <- cbind(urbana_full, urbana_longlat_meters[,1], urbana_longlat_meters[,2])
names(urbana_full)[c(5,6)] <- c("longmeters", "latmeters")

### TRAIN MODEL ON RESPONSES FROM 2014 ###

# Training Set for 2014 (Non-Crime coordinates and Yelp Data)
# remove crime coordinates from non-crime coordinates
urbana_full14 <- urbana_full[!(urbana_full$long %in% crime_data14$Longitude),]

# create a set of 20,000 points (including crime)
fill <- 20000 - nrow(crime_data14)
urbana_full14 <- urbana_full14[sample(nrow(urbana_full14), fill), ]

# bind the non-crime coordinates with the crime coordinates
head(crime_data14)
train14 <- rbind(urbana_full14, crime_data14[,c('response', 'long', 'lat', 'longmeters', 'latmeters', 'RESPONSE.TIME')])

## ADDING PREDICTOR: CRIME DENSITY FOR PREVIOUS YEAR ## 

# get crime densities for 2014 training point based on 2013s data points

h = Hpi(crime_data13[,c("longmeters","latmeters")], pilot="dscalar")
crime_density = kde(crime_data13[,c("longmeters","latmeters")], H=h, eval.points=train14[,c("longmeters","latmeters")])$estimate

# bind crime density as predictor to training data
train14 = cbind(train14, crime_density)
head(train14)

## ADDING PREDICTOR: 2014 % OF BUSINESSES ONLY TAKING CASH WITHIN A 1 MILE RADIUS ## 

# calculate business cash predictors
names(yelp_data14)
# reduce data down to business cash attributes
business_cash14 <- yelp_data14[,(c("business_id", "name", "longitude", "latitude", "attributes.Accepts.Credit.Cards", "longmeters", "latmeters"))]
# remove NAs from cash attribute column
business_cash14 <- business_cash14[complete.cases(business_cash14$attributes.Accepts.Credit.Cards),]
# calculate businesses in 1 mile radius to coordinate
str(business_cash14)
str(train14)

credit <- 0 
train14$cash <- 0
for (i in 1:nrow(train14)){
    distance <- sqrt((train14[i,"longmeters"]-business_cash14[,"longmeters"])^2 + (train14[i,"latmeters"]-business_cash14[,"latmeters"])^2)
    business_radius <- which(distance < 1609.34)
    
    for (j in 1:length(business_radius)) {
      credit <- c(credit, business_cash14[business_radius[j],"attributes.Accepts.Credit.Cards"])
    }
    
    credit_total <- sum(credit)/length(business_radius)
    
    if (is.na(credit_total)) {
      train14$cash[i] <- 0
      credit <- 0
    }
    else {
      train14$cash[i] <- ((1 - credit_total) * 100)
      credit <- 0
    }
}

## ADDING PREDICTOR: 2014 AVERAGE RATINGS OF BUSINESSES IN 1 MILE RADIUS ## 

# calculate business cash predictors

names(yelp_data14)
# reduce data down to business ratings attributes
business_rating14 <- yelp_data14[,(c("business_id", "name", "longitude", "latitude", "business_stars", "longmeters", "latmeters"))]
# remove NAs from ratings attribute column
business_rating14 <- business_rating14[complete.cases(business_rating14$business_stars),]

# calculate average rating of businesses in 250 m 
ratings <- 0 
train14$avg_rating <- 0

for (i in 1:nrow(train14)){
  distance <- sqrt((train14[i,"longmeters"]-business_rating14[,"longmeters"])^2 + (train14[i,"latmeters"]-business_rating14[,"latmeters"])^2)
  business_radius <- which(distance < 1609.34)
  
  for (j in 1:length(business_radius)) {
    ratings <- c(ratings, business_rating14[business_radius[j], "business_stars"])
  }
  
  if (is.na((sum(ratings)/length(business_radius)))) {
    train14$avg_rating[i] <- 0
    ratings <- 0
  }
  else {
    train14$avg_rating[i] <- (sum(ratings)/length(business_radius))
    ratings <- 0
  }
}


## ADDING PREDICTOR: 2014 PRICE RANGE OF BUSINESSES IN 1 MILE RADIUS ## 

# calculate business price range predictors
names(yelp_data14)
# reduce data down to business price range attributes
business_PriceRange14 <- yelp_data14[,(c("business_id", "name", "longitude", "latitude", "attributes.Price.Range", "longmeters", "latmeters"))] 
# remove NAs from price range column
business_PriceRange14 <- business_PriceRange14[complete.cases(business_PriceRange14$attributes.Price.Range),]
# calculate businesses in 1 mile radius to coordinate
str(business_PriceRange14)
str(train14)

# Calculate Average Price Range of Business in 1 Mile Radius
PriceRange <- 0 
train14$avg_PriceRange <- 0

for (i in 1:nrow(train14)){
  distance <- sqrt((train14[i,"longmeters"]-business_PriceRange14[,"longmeters"])^2 + (train14[i,"latmeters"]-business_PriceRange14[,"latmeters"])^2)
  business_radius <- which(distance < 1609.34)
  
  for (j in 1:length(business_radius)) {
    PriceRange <- c(PriceRange, business_PriceRange14[business_radius[j], "attributes.Price.Range"])
  }
  
  if (is.na((sum(PriceRange)/length(business_radius)))) {
    train14$avg_PriceRange[i] <- 0
    PriceRange <- 0
  }
  else {
    train14$avg_PriceRange[i] <- (sum(PriceRange)/length(business_radius))
    PriceRange <- 0
  }
}

## FIT LOGISTIC REGRESSION MODEL ##

## PREDICTORS: % OF CASH ONLY BUSINESSES WITHIN A 1 MILE RADIUS ##
## RESPONSE: LOCATION OF CRIME OR NOT (1 or 0 respectively) ##

str(train14)
# Cash + Crime Density as a Predictor
logm_14cash2 = glm(response ~ crime_density+cash, data = train14, family=binomial)
# Cash as a Predictor
logm_14cash = glm(response ~ cash, data = train14, family=binomial)

table(train14[,c('response', 'crime_density')])
hist(train14$response, breaks=20)


## PREDICTORS: AVERAGE BUSINESS RATING IN A 2 MILE RADIUS ##
## RESPONSE: LOCATION OF CRIME OR NOT (1 or 0 respectively) ##

# Avg Business Rating + Crime Density as a Predictor
logm_14ratings2 = glm(response ~ crime_density+avg_rating, data = train14, family=binomial)
# Avg Rating as a Predictor 
logm_14ratings = glm(response ~ avg_rating, data = train14, family=binomial)


## PREDICTORS: AVERAGE BUSINESS PRICE RANGE IN A 2 MILE RADIUS ##
## RESPONSE: Response time ##

# Avg Business Price Range + Crime Density as a Predictor
logm_14PriceRange2 = glm(RESPONSE.TIME ~ crime_density+avg_PriceRange, data = train14)

# Avg Business Price Range as a Predictor
logm_14PriceRange = glm(RESPONSE.TIME ~ avg_PriceRange, data = train14)

# Business Price Range with Crime Location
logm_14PriceRange3 = glm(response ~ crime_density+avg_PriceRange, data = train14)
logm_14PriceRange4 = glm(response ~ avg_PriceRange, data = train14)

# All Predictors, including crime density
logm_totaldensity = glm(response ~ crime_density+avg_PriceRange+avg_rating+cash, data = train14)
vif(logm_totaldensity)

# Previous Years Crime Density as a Predictor
logm_crimedensity = glm(response ~ crime_density, data = train14)

# All Predictors, not including crime density
logm_total = glm(response ~ avg_PriceRange+avg_rating+cash, data = train14)
vif(logm_total)

## LOGISTIC REGRESSION MODEL SUMMARIES ## 

# CRIME DENSITY + CASH AS PREDICTORS #

# Coefficients:
#                    Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)   -2.503e+00  4.128e-02  -60.63   <2e-16 ***
#   crime_density  3.994e+07  7.751e+05   51.53   <2e-16 ***
#   cash           1.300e-01  7.094e-03   18.33   <2e-16 ***

# CASH AS PREDICTOR #
# Coefficients:
#                    Estimate Std. Error z value Pr(>|z|)    
#   (Intercept) -1.020321   0.024827  -41.10   <2e-16 ***
#   cash         0.344693   0.005966   57.78   <2e-16 ***

# CRIME DENSITY + AVG_RATING AS PREDICTORS #
# Coefficients:
#                    Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)   -2.872e+00  8.976e-02  -31.99   <2e-16 ***
#   crime_density  4.330e+07  7.873e+05   55.00   <2e-16 ***
#   avg_rating     2.377e-01  2.539e-02    9.36   <2e-16 ***

# AVG_RATING AS PREDICTOR #
# Coefficients:
#                Estimate Std. Error z value Pr(>|z|)    
#   (Intercept) -1.17096    0.04533  -25.83   <2e-16 ***
#   avg_rating   0.41353    0.01307   31.63   <2e-16 ***

# CRIME DENSITY + AVG_PRICE_RANGE AS PREDICTORS #
# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     9.972e+00  5.789e+00   1.723    0.085 .  
# crime_density  -2.288e+08  8.768e+06 -26.092   <2e-16 ***
# avg_PriceRange -4.126e+00  3.659e+00  -1.128    0.259  

# AVG_PRICE_RANGE AS PREDICTOR #

# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       1.431      1.673   0.855    0.393    
# avg_PriceRange   -8.456      1.233  -6.857 7.22e-12 ***

# % OF BUSINESSES WITH CRIME DENSITY + CASH + AVG_RATINGS + AVG_PRICE_RANGE AS PREDICTORS #
# Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)     1.708e-02  4.883e-03   3.498 0.000469 ***
#   crime_density   3.329e+06  2.495e+04 133.422  < 2e-16 ***
#   avg_PriceRange  1.117e-01  6.857e-03  16.293  < 2e-16 ***
#   avg_rating     -2.889e-02  3.000e-03  -9.630  < 2e-16 ***
#   cash            1.680e-02  5.836e-04  28.795  < 2e-16 ***

# % OF BUSINESSES WITH CASH + AVG_RATINGS + AVG_PRICE_RANGE AS PREDICTORS #
# Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)     0.1139162  0.0066385   17.16   <2e-16 ***
#   avg_PriceRange  0.2788213  0.0092685   30.08   <2e-16 ***
#   avg_rating     -0.0918782  0.0040736  -22.55   <2e-16 ***
#   cash            0.0395265  0.0007674   51.51   <2e-16 ***
  
# PREVIOUS YEARS CRIME DENSITY AS PREDICTOR #
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)   9.517e-02  2.511e-03    37.9   <2e-16 ***
#   crime_density 3.696e+06  2.398e+04   154.1   <2e-16 ***


### PREDICT RESPONSES ON 2015 DATA, USING FITTED MODEL AND PREDICTORS FROM 2014 ###

# build prediction data for 2015 data
# crime density from 2014
# % of businesses cash only within 1 miles for 2015
# avg ratings for businesses within 1 miles for 2015

# get coordinates from urbana city
urbana_full15 <- urbana_full[,c(2,3,5,6)]
names(urbana_full15)
# create a set of 20,000 points (including crime)
fill2 <- 20000 - nrow(crime_data15)
set.seed(20)
urbana_full15 <- urbana_full15[sample(nrow(urbana_full15), fill2), ]

# include 2015 crime coordinates (without responses)
predict15 <- rbind(urbana_full15, crime_data15[,c('long', 'lat', 'longmeters', 'latmeters')])

## ADDING PREDICTOR: CRIME DENSITY FOR PREVIOUS YEAR ## 

# get crime densities for 2015 prediction points based on 2014s data points

hp = Hpi(crime_data14[,c("longmeters","latmeters")], pilot="dscalar")
crime_density14 = kde(crime_data14[,c("longmeters","latmeters")], H=hp, eval.points=predict15[,c("longmeters","latmeters")])$estimate

# bind crime density as predictor to training data
predict15 = cbind(predict15, crime_density14)
head(predict15)

## ADDING PREDICTOR: % OF BUSINESSES ONLY TAKING CASH WITHIN A 1 MILE RADIUS ## 

# calculate business cash predictors
names(yelp_data15)
# pare down data so each row is a unique business
yelp_data15 <- yelp_data15[!duplicated(yelp_data15[,2]),]
# reduce data down to business cash attributes
business_cash15 <- yelp_data15[,(c("business_id", "name", "longitude", "latitude", "attributes.Accepts.Credit.Cards", "longmeters", "latmeters"))]
# remove NAs from cash attribute column
business_cash15 <- business_cash15[complete.cases(business_cash15$attributes.Accepts.Credit.Cards),]
# calculate businesses in 1 mile radius to coordinate
str(business_cash15)


credit <- 0 
predict15$cash <- 0
for (i in 1:nrow(predict15)){
  distance <- sqrt((predict15[i,"longmeters"]-business_cash15[,"longmeters"])^2 + (predict15[i,"latmeters"]-business_cash15[,"latmeters"])^2)
  business_radius <- which(distance < 1609.34)
  for (j in 1:length(business_radius)) {
    credit <- c(credit, business_cash15[business_radius[j],"attributes.Accepts.Credit.Cards"])
  }
  credit_total <- sum(credit)/length(business_radius)
  
  if (is.na(credit_total)) {
    predict15$cash[i] <- 0
    credit <- 0
  }
  else {
    predict15$cash[i] <- ((1 - credit_total) * 100)
    credit <- 0
  }
}
head(predict15)

## ADDING PREDICTOR: AVERAGE RATINGS OF BUSINESSES IN 1 MILE RADIUS ## 

# calculate business cash predictors

names(yelp_data15)
# reduce data down to business ratings attributes
business_rating15 <- yelp_data15[,(c("business_id", "name", "longitude", "latitude", "business_stars", "longmeters", "latmeters"))]
# remove NAs from ratings attribute column
business_rating15 <- business_rating15[complete.cases(business_rating15$business_stars),]

# calculate average rating of businesses in 1 mile radius
ratings <- 0 
predict15$avg_rating <- 0
for (i in 1:nrow(predict15)){
  distance <- sqrt((predict15[i,"longmeters"]-business_rating15[,"longmeters"])^2 + (predict15[i,"latmeters"]-business_rating15[,"latmeters"])^2)
  business_radius <- which(distance < 1609.34)
  for (j in 1:length(business_radius)) {
    ratings <- c(ratings, business_rating15[business_radius[j], "business_stars"])
  }
  if (is.na((sum(ratings)/length(business_radius)))) {
    predict15$avg_rating[i] <- 0
    ratings <- 0
  }
  else {
    predict15$avg_rating[i] <- (sum(ratings)/length(business_radius))
    ratings <- 0
  }
}
head(predict15)

## ADDING PREDICTOR: AVERAGE PRICE RANGE OF BUSINESSES IN 1 MILE RADIUS ## 

# reduce data down to price range attributes
PriceRange_15 <- yelp_data15[,(c("business_id", "name", "longitude", "latitude", "attributes.Price.Range", "longmeters", "latmeters"))]
# remove NAs from price range attribute column
PriceRange_15 <- PriceRange_15[complete.cases(PriceRange_15$attributes.Price.Range),]

# calculate average price range of businesses in 1 mile radius
PriceRange <- 0 
predict15$avg_PriceRange <- 0

for (i in 1:nrow(predict15)){
  distance <- sqrt((predict15[i,"longmeters"]-PriceRange_15[,"longmeters"])^2 + (predict15[i,"latmeters"]-PriceRange_15[,"latmeters"])^2)
  business_radius <- which(distance < 1609.34)
  for (j in 1:length(business_radius)) {
    PriceRange <- c(PriceRange, PriceRange_15[business_radius[j], "attributes.Price.Range"])
  }
  if (is.na((sum(PriceRange)/length(business_radius)))) {
    predict15$avg_PriceRange[i]  <- 0
    PriceRange <- 0
  }
  else {
    predict15$avg_PriceRange[i] <- (sum(PriceRange)/length(business_radius))
    PriceRange <- 0
  }
}

### PREDICTION ###

# run prediction for cash + crime density
crime_predict_cash2 <- predict(logm_14cash2, predict15, type="response")
# run prediction for cash
crime_predict_cash <- predict(logm_14cash, predict15, type="response")

# run prediction for avg_ratings + crime density
names(predict15)
crime_predict_ratings2 <- predict(logm_14ratings2, predict15, type="response")
# run prediction for avg_ratings
crime_predict_ratings <- predict(logm_14ratings, predict15, type="response")

# run prediction for price_range + crime density
crime_predict_responsetime2 = predict(logm_14PriceRange2, predict15, type="response")
# run prediction for price_range
crime_predict_responsetime = predict(logm_14PriceRange, predict15, type="response")

# run prediction for price_range + crime density (using crime points)
crime_predict_responsetime3 = predict(logm_14PriceRange3, predict15, type="response")
# run prediction for price_range (using crime points)
crime_predict_responsetime4 = predict(logm_14PriceRange4, predict15, type="response")

# run prediction crime density
crime_predict_density <- predict(logm_crimedensity, predict15, type="response")

# run prediction for all predictors, including crime density
crime_predict_totalcrime <- predict(logm_totaldensity, predict15, type="response")

# run prediction for all predictors, not including crime density
crime_predict_total <- predict(logm_total, predict15, type="response")

## EVALUATE PREDICTIONS ##
urbana_full15 <- urbana_full

# create a set of 20,000 points (including crime)
fill2 <- 20000 - nrow(crime_data15)
urbana_full15 <- urbana_full15[sample(nrow(urbana_full15), fill2), ]

# create true points for 2015 crime
true15 <- rbind(urbana_full15, crime_data15[,c('response','long', 'lat', 'longmeters', 'latmeters', 'RESPONSE.TIME')])

# cash + crime density predictions vs. true crime responses
pr_cashcrime <- prediction(crime_predict_cash2, true15$response)
prf_cashcrime <- performance(pr_cashcrime, measure = "tpr", x.measure = "fpr")

# cash predictions vs. true crime responses
pr_cash <- prediction(crime_predict_cash, true15$response)
prf_cash <- performance(pr_cash, measure = "tpr", x.measure = "fpr")

# avg business rating + crime density predictions vs. true crime responses
pr_ratingscrime <- prediction(crime_predict_ratings2, true15$response)
prf_ratingscrime <- performance(pr_ratingscrime, measure = "tpr", x.measure = "fpr")

# avg business rating predictions vs. true crime responses
pr_ratings <- prediction(crime_predict_ratings, true15$response)
prf_ratings <- performance(pr_ratings, measure = "tpr", x.measure = "fpr")

# avg business price range + crime density predictions vs. true crime responses
pr_pricecrime <- prediction(crime_predict_responsetime3, true15$response)
prf_pricecrime <- performance(pr_pricecrime, measure = "tpr", x.measure = "fpr")

# avg business price range predictions vs. true crime responses
pr_price <- prediction(crime_predict_responsetime4, true15$response)
prf_price <- performance(pr_price, measure = "tpr", x.measure = "fpr")

# all predictors predictions w/ crime density vs. true crime responses
pr_predcrime <- prediction(crime_predict_totalcrime, true15$response)
prf_predcrime <- performance(pr_predcrime, measure = "tpr", x.measure = "fpr")

# all predictors predictions w/o crime density vs. true crime responses
pr_pred <- prediction(crime_predict_total, true15$response)
prf_pred <- performance(pr_pred, measure = "tpr", x.measure = "fpr")

# crime density predictions vs. true crime responses
pr_crime <- prediction(crime_predict_density, true15$response)
prf_crime <- performance(pr_crime, measure = "tpr", x.measure = "fpr")


performance(pr_pred,"f")

# plot ROC curve for each prediction
plot(prf_cashcrime, col='mediumvioletred', main="ROC Curves")
plot(prf_cash, col='blue', main="ROC Curves")
plot(prf_ratingscrime, col='red', main="ROC Curves")
plot(prf_price, col='red', main="ROC Curves")
plot(prf_pricecrime, col='red', main="ROC - Price Range + Crime Prediction")
plot(prf_predcrime, col='red', main="ROC Curves")
plot(prf_pred, col='red', main="ROC Curves")
plot(prf_crime, col='red', main="ROC Curves")

# auc for cash+crime
auc_cashcrime <- performance(pr_cashcrime, measure = "auc")
auc_cashcrime <- auc_cashcrime@y.values[[1]]
auc_cashcrime
# [1] 0.8536818

# auc for cash
auc_cash <- performance(pr_cash, measure = "auc")
auc_cash <- auc_cash@y.values[[1]]
auc_cash
# [1] 0.5553616

# auc for rating+crime
auc_ratingcrime <- performance(pr_ratingscrime, measure = "auc")
auc_ratingcrime <- auc_ratingcrime@y.values[[1]]
auc_ratingcrime
# [1] 0.9545163

# auc for rating
auc_rating <- performance(pr_ratings, measure = "auc")
auc_rating <- auc_rating@y.values[[1]]
auc_rating
# [1] 0.5303663

# auc for price+crime
auc_pricecrime <- performance(pr_pricecrime, measure = "auc")
auc_pricecrime <- auc_pricecrime@y.values[[1]]
auc_pricecrime
# [1] 0.959677

# auc for price
auc_price <- performance(pr_price, measure = "auc")
auc_price <- auc_price@y.values[[1]]
auc_price
# [1] 0.707199

# auc for all predictors w/o crime density
auc_pred <- performance(pr_pred, measure = "auc")
auc_pred <- auc_pred@y.values[[1]]
auc_pred
# [1] 0.6083573

# auc for all predictors w/ crime density
auc_predcrime <- performance(pr_predcrime, measure = "auc")
auc_predcrime <- auc_predcrime@y.values[[1]]
auc_predcrime
# [1] 0.8271655

# auc for crime density
auc_crime <- performance(pr_crime, measure = "auc")
auc_crime <- auc_crime@y.values[[1]]
auc_crime
# [1] 0.9559881

# Find Classification Rate (https://www.r-bloggers.com/evaluating-logistic-regression-models/)

# Classification Rate for Price Range
accuracy <- table(crime_predict_responsetime, true15[,"RESPONSE.TIME"])
sum(diag(accuracy))/sum(accuracy)
# [1] 5e-04
names(predict15)

# Classification Rate for Crime Density + Price Range
accuracy2 <- table(crime_predict_responsetime2, true15[,"RESPONSE.TIME"])
sum(diag(accuracy))/sum(accuracy)
# [1] 5e-04

# Check for Multicollinearity
# > vif(logm_totaldensity)
# crime_density avg_PriceRange     avg_rating           cash 
# 1.167852       4.535433       4.341561       1.264062 
# > vif(logm_total)
# avg_PriceRange     avg_rating           cash 
# 4.384114       4.234081       1.156394
