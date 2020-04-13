Install.packages("MASS")
Install.packages("car")

CarPrice<- read.csv('CarPrice_Assignment.csv')
head(CarPrice)
set.seed(2017)
split <- sample(seq_len(nrow(CarPrice)), size = floor(0.75 * nrow(CarPrice)))
train <- CarPrice[split, ]
test <- CarPrice[-split, ]
dim(train)
#hypothesize the following subset of the variables as potential predicators
#Construct a new data fram consisting solely of these variables
train <- subset(train, select=c(price,car_ID,fueltype,aspiration,doornumber,carbody,drivewheel,enginelocation,wheelbase,carlength,carwidth,carheight,curbweight,enginetype,cylindernumber,enginesize,fuelsystem,boreratio,stroke,compressionratio,horsepower,peakrpm,citympg,highwaympg))
#checking the training data
head(train)
#checking training data for null values
sapply(train, function(x) sum(is.na(x)))
summary(train)

library(psych)
par(mar=c(1,1,1,1))
pairs.panels(train, fueltype="diesel")
#data analysis formula
fit <-lm(price~car_ID +  fueltype + aspiration+doornumber+carbody+drivewheel+enginelocation+wheelbase+carlength+carwidth+carwidth+carheight+curbweight+enginetype+cylindernumber+enginesize+fuelsystem+boreratio+stroke+compressionratio+horsepower+peakrpm+citympg+highwaympg,data=train)
summary(fit)
#confidence level 
confint(fit, conf.level=0.95)

attach(train)

#checking correlation of data
cor(compressionratio,horsepower, method='pearson')
cor(peakrpm,horsepower, method='pearson')
cor(highwaympg,horsepower, method='pearson')
cor(boreratio,stroke, method='pearson')
cor(carlength,carwidth, method='pearson')
cor(citympg,highwaympg,method='pearson')
#Create a confidence interval for the model coefficients
confint(fit, conf.level=0.95)
#Check the diagnostic plots for the model
plot(fit)

test<- subset(test, select=c(price,car_ID,aspiration,carbody,drivewheel,enginelocation,wheelbase,carwidth,carheight,curbweight,enginetype,cylindernumber,enginesize,boreratio,stroke,horsepower,peakrpm,citympg,highwaympg))
prediction <- predict(fit, newdata = test)
#predicting prices
head(prediction)
head(test$price)
SSE <- sum((test$price - prediction) ^ 2)
SST <- sum((test$price - mean(test$price)) ^ 2)
1 - SSE/SST
