Install.packages("MASS")
Install.packages("car")
Install.packages("MASS")
Install.packages("car")

CarPrice<- read.csv('CarPrice_Assignment.csv',stringsAsFactors = FALSE)
#head(CarPrice)
#View(CarPrice)
#summary(CarPrice)
#str(CarPrice)
#evaluating the categorical variables
#factor(CarPrice$carbody)
#factor(CarPrice$fueltype)
#factor(CarPrice$fuelsystem)
#factor(CarPrice$drivewheel)
#factor(CarPrice$aspiration)
#factor(CarPrice$doornumber)
#factor(CarPrice$enginelocation)
#factor(CarPrice$enginetype)
#factor(CarPrice$cylindernumber)




#splitting the Carname into Carbrand
CarPrice$CarName <- gsub( " .*$", "",CarPrice$CarName)
#write.table("Brand",file="CarPrice_Assignment.csv",sep=",",append=TRUE,row.names=FALSE,col.names=FALSE)

#fixing 4fwd in drivewheel
library(plyr)
CarPrice$drivewheel<-revalue(CarPrice$drivewheel, c("4wd"="fwd"))
CarPrice$cylindernumber<-revalue(CarPrice$cylindernumber, c("two"="2","three"="3","four"="4","five"="5","six"="6","eight"="8","twelve"="12"))
CarPrice$cylindernumber<- as.numeric(levels(CarPrice$cylindernumber))[CarPrice$cylindernumber]
View(CarPrice)

#creating dummy variables for categorical variables
str(CarPrice$fueltype)
summary(factor(CarPrice$fueltype))
levels(CarPrice$fueltype)<-c(1,0)
summary(CarPrice$fueltype)
CarPrice$fueltype<- as.numeric(levels(CarPrice$fueltype))[CarPrice$fueltype]
summary(CarPrice$fueltype)
# Check the summary of mainroad variable
summary(CarPrice$fueltype)

# Do the same for other such categorical variables
levels(CarPrice$aspiration)
CarPrice$aspiration<- as.numeric(levels(CarPrice$aspiration))[CarPrice$aspiration]

levels(CarPrice$doornumber)<-c(1,0)
CarPrice$doornumber <- as.numeric(levels(CarPrice$doornumber))[CarPrice$doornumber]

levels(CarPrice$drivewheel)<-c(1,0) 
CarPrice$drivewheel <- as.numeric(levels(CarPrice$drivewheel))[CarPrice$drivewheel]

levels(CarPrice$enginetype)<-c(1,0)
CarPrice$enginetype <- as.numeric(levels(CarPrice$enginetype))[CarPrice$enginetype]


# Now we come across variables having more than 3 levels. 
summary(factor(CarPrice$symboling))

#Converting "symboling" into dummies . 
dummy_1 <- data.frame(model.matrix( ~CarPrice$enginetype, data = CarPrice))

#check the dummy_1 data frame.


#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "symboling". 
dummy_1 <- dummy_1[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "symboling" column
CarPrice_1 <- cbind(CarPrice[,-2], dummy_1)


##############
#Converting "CarName" into dummies . 
dummy_2 <- data.frame(model.matrix( ~CarPrice_1$CarName, data = CarPrice))

#check the dummy_1 data frame.


#This column should be removed from the newly created dummy_2 dataframe containing the dummy values for the variable "CarName"
dummy_2 <- dummy_2[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "symboling" column
CarPrice_2 <- cbind(CarPrice_1[,-2], dummy_2)


##############
#Converting "Carbody" into dummies . 
dummy_3 <- data.frame(model.matrix( ~CarPrice_2$carbody, data = CarPrice))

#check the dummy_1 data frame.


#This column should be removed from the newly created dummy_2 dataframe containing the dummy values for the variable "CarName"
dummy_3 <- dummy_3[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "Carname" column
CarPrice_3 <- cbind(CarPrice_2[,-5], dummy_3)

################################################################33
#Converting "Enginetype" into dummies . 
dummy_4 <- data.frame(model.matrix( ~CarPrice_1$enginetype, data = CarPrice))


#This column should be removed from the newly created dummy_2 dataframe containing the dummy values for the variable "Enginetype"
dummy_4 <- dummy_4[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "Enginetype" column
CarPrice_4 <- cbind(CarPrice_3[,-12], dummy_4)
View(CarPrice_4)

##############
#Converting "fuelsystem" into dummies . 
dummy_5 <- data.frame(model.matrix( ~CarPrice_1$fuelsystem, data = CarPrice))


#This column should be removed from the newly created dummy_2 dataframe containing the dummy values for the variable "fuelsystem"
dummy_5 <- dummy_5[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "fuelsystem" column
CarPrice_5 <- cbind(CarPrice_4[,-14], dummy_5)
View(CarPrice_5)
#############################################



# Divide into training and test data set
#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(CarPrice_5), 0.7*nrow(Carprice_5))
# generate the train data set
train = Carprice_5[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = Carprice_5[-trainindices,]





#Execute the first model_1 multilinear model in the training set. 
model_1 <-lm(price~.,data=train)

# Check the summary of model. 
summary(model_1)

##############

# Lets load the library in which stepAIC function exists
install.packages("MASS")
library(MASS)

# We have a total of 15 variables considered into the model 
#Now let;s run the code. 

step <- stepAIC(model_1, direction="both")

# Check if the correlation matrix givessome insight.
corrs = cor(CarPrice_5)
View(corrs)


##############
#For the calculation of VIF you need to install the package "car", 
# First let's install this package in your R system
install.packages("car")

# load the package
library("car")

# Pass the model_1 in the vif function
vif(model_1)

# Look at summary of the model again to see the P values
summary(model_1)

# remove bbratio variable based on High VIF and insignificance (p>0.05)
# Make a new model without bbratio variable

model_2 <- lm(formula = price ~ + bedrooms +aspiration + stories + mainroad + guestroom + 
                basement + hotwaterheating + airconditioning + parking + prefarea + 
                furnishingstatusunfurnished + furnishingstatussemi.furnished + areaperbedroom, data = train)

car_ID,,,doornumber,carbody,drivewheel,enginelocation,wheelbase,carlength,carwidth,carheight,curbweight,enginetype,cylindernumber,enginesize
,fuelsystem,boreratio,stroke,compressionratio,horsepower,peakrpm,citympg,highwaympg
# check the accuracy of this model
summary(model_2)

# Repeat the process of vif calculation of model_2. 
vif(model_2)


# Remove the areaperbedroom variable based on high VIF and insignificance (p>0.05)
# Make a model without areaperbedroom variable
model_3 <- lm(formula = price ~ area + bedrooms + bathrooms + stories + mainroad + guestroom + 
                basement + hotwaterheating + airconditioning + parking + prefarea + 
                furnishingstatusunfurnished + furnishingstatussemi.furnished , data = train)

#Check the accuracy of this model
summary(model_3)

# Calculate the vif for model_3. 
vif(model_3)
# all VIFs are below 2, thus Multicollinearity is not a problem anymore


# Remove furnishingstatussemi.furnishingstatus based on insignificance. It has has the highest p value of 0.53 among the remaining variables
# Make a model without areaperbedroom variable

model_4 <- lm(formula = price ~ area + bedrooms + bathrooms + stories + mainroad + guestroom + 
                basement + hotwaterheating + airconditioning + parking + prefarea + 
                furnishingstatusunfurnished , data = train)

# Check the accuracy and p-values again
summary(model_4)


# bedrooms variable has a p value of 0.22, thus is insignificant
## Make a new model after removing bedrooms variable 

model_5 <- lm(formula = price ~ area + bathrooms + stories + mainroad + guestroom + 
                basement + hotwaterheating + airconditioning + parking + prefarea + 
                furnishingstatusunfurnished , data = train)

### Check the accuracy and p-values again
summary(model_5)


# basement variable is having the highest p-value (lowest significance) among the remaining variables.
# Make a new model after removing basement variable 

model_6 <- lm(formula = price ~ area + bathrooms + stories + mainroad + guestroom + 
                hotwaterheating + airconditioning + parking + prefarea + 
                furnishingstatusunfurnished , data = train)

### Check the accuracy and p-values again
summary(model_6)


# mainroad variable has the highest p-value among the other variables in the model
# Make a new model after removing mainroad variable 

model_7 <- lm(formula = price ~ area + bathrooms + stories + guestroom + 
                hotwaterheating + airconditioning + parking + prefarea + 
                furnishingstatusunfurnished , data = train)

# Check the adjusted R-squared and p-values again
summary(model_7)


# hotwater heating variable is having the highest p-value among the other variables in the model, 
# Make a new model after removing hotwaterheating variable 

model_8 <- lm(formula = price ~ area + bathrooms + stories + guestroom + 
                airconditioning + parking + prefarea + 
                furnishingstatusunfurnished , data = train)

#Check the accuracy
summary(model_8)


# Predict the house prices in the testing dataset
Predict_1 <- predict(model_8,test[,-1])
test$test_price <- Predict_1

# Accuracy of the predictions
# Calculate correlation
r <- cor(test$price,test$test_price)
# calculate R squared by squaring correlation
rsquared <- cor(test$price,test$test_price)^2

# check R-squared
rsquared