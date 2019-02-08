# Author - Omkar Dixit
# Dataset - California Housing Prices

library(tidyverse)
library(reshape2)
library(plotly)
library(IRdisplay)
library(MASS)
library(ISLR)
require(corrplot)

# Read the data 
housing = read.csv('./Data/housing.csv')
nrow(housing)
# Making Sure that the data isn't weird
head(housing)

# Lets check the summary of the data
summary(housing)

par(mfrow=c(1,1))
hist(housing$longitude, main = "Longitude", xlab = "Longitude", ylab = "Count", col = "darkgray")
hist(housing$latitude, main = "latitude", xlab = "latitude", ylab = "Count", col = "darkgray")
hist(housing$housing_median_age, main = "housing_median_age", xlab = "housing_median_age", ylab = "Count", col = "darkgray")
hist(housing$total_rooms, main = "total_rooms", xlab = "total_rooms", ylab = "Count", col = "darkgray")
hist(housing$total_bedrooms, main = "total_bedrooms", xlab = "total_bedrooms", ylab = "Count", col = "darkgray")
hist(housing$population, main = "population", xlab = "population", ylab = "Count", col = "darkgray")
hist(housing$households, main = "households", xlab = "households", ylab = "Count", col = "darkgray")
hist(housing$median_income, main = "median_income", xlab = "median_income", ylab = "Count", col = "darkgray")
hist(housing$median_house_value, main = "median_house_value", xlab = "median_house_value", ylab = "Count", col = "darkgray")

# Now from the histograms we can see that:
# There are some houses with old age homes in them
# We need to standardize the scale of the data since the range is very weird some for 0-10 and some range to 500,000

# Since we saw that the total_bedrooms had a lot of NA's, we will fill them with median because using mean would be volatile because mean is affected more by the outliers
housing$total_bedrooms[is.na(housing$total_bedrooms)] <- median(housing$total_bedrooms, na.rm=TRUE)

# Rather than having total_bedrooms and total_rooms separate we can have something like an avg of them 
housing$avg_bedrooms <- housing$total_bedrooms/housing$households
housing$avg_rooms <- housing$total_rooms/housing$households

# Lets just check once 
head(housing)

# All good ! lets drop the total_bedrooms and total_rooms
housing <- housing[, !(names(housing) %in% c('total_bedrooms', 'total_rooms'))]

# Lets now split the ocean_proximity into seperate boolean columns 
# First we get all the categories
categories <- unique(housing$ocean_proximity)

# Now we need to add new columns to the housing DF
for(c in categories){
  housing[,c] <- rep(0, times=nrow(housing))
}

# Lets fill the new columns
for (i in 1:nrow(housing)){
  c <- as.character(housing$ocean_proximity[i])
  housing[, c][i] <- 1
} 

# Now we dont need ocean proximity anymore
# So we drop it 
housing$ocean_proximity <- NULL

colnames(housing)
# Lets use scatter plot to check what predicting variables have a linear relationship with median_house_value
par(mfrow=c(3,3))
plot(x=housing$longitude, y=housing$median_house_value, xlab="Longitude", ylab="Median House Value", main="Longitude VS Median House Value")
plot(x=housing$latitude, y=housing$median_house_value, xlab="Latitude", ylab="Median House Value", main="Longitude VS Median House Value")
plot(x=housing$housing_median_age, y=housing$median_house_value, xlab="Housing Median Age", ylab="Median House Value", main="Housing Median Age VS Median House Value")
plot(x=housing$population, y=housing$median_house_value, xlab="Population", ylab="Median House Value", main="Population VS Median House Value")
plot(x=housing$households, y=housing$median_house_value, xlab="Households", ylab="Median House Value", main="Households VS Median House Value")
plot(x=housing$median_income, y=housing$median_house_value, xlab="Median Income", ylab="Median House Value", main="Median Income VS Median House Value")
plot(x=housing$median_house_value, y=housing$median_house_value, xlab="Median House Value", ylab="Median House Value", main="Median House Value VS Median House Value")
plot(x=housing$avg_bedrooms, y=housing$median_house_value, xlab="Average Bedrooms", ylab="Median House Value", main="Average Bedrooms VS Median House Value")
plot(x=housing$`NEAR BAY`, y=housing$median_house_value, xlab="Ocean Proximity - NEAR BAY", ylab="Median House Value", main="Ocean Proximity - NEAR BAY VS Median House Value")
plot(x=housing$INLAND, y=housing$median_house_value, xlab="Ocean Proximity - INLAND", ylab="Median House Value", main="Ocean Proximity - INLAND VS Median House Value")
plot(x=housing$`NEAR OCEAN`, y=housing$median_house_value, xlab="Ocean Proximity - NEAR OCEAN", ylab="Median House Value", main="Ocean Proximity - NEAR OCEAN VS Median House Value")
plot(x=housing$ISLAND, y=housing$median_house_value, xlab="Ocean Proximity - ISLAND", ylab="Median House Value", main="Ocean Proximity - ISLAND VS Median House Value")
plot(x=housing$`<1H OCEAN`, y=housing$median_house_value, xlab="Ocean Proximity - <1H OCEAN", ylab="Median House Value", main="Ocean Proximity - <1H OCEAN VS Median House Value")

# Looks like Median Income is the only feature with a linear relationship with Median House Value

# Lets use boxplot to check if there are any outliers 
par(mfrow=c(1,1))
boxplot(housing$longitude, xlab = "Boxplot (Longitude)")
boxplot(housing$latitude, xlab = "Boxplot (Latitude)")
boxplot(housing$housing_median_age, xlab = "Boxplot (Housing Median Age)")
boxplot(housing$population, xlab = "Boxplot (Population)")
boxplot(housing$households, xlab = "Boxplot (Households)")
boxplot(housing$median_income, xlab = "Boxplot (Median Income)")
boxplot(housing$median_house_value, xlab = "Boxplot (Median House Value)")
boxplot(housing$avg_bedrooms, xlab="Boxplot (Average Bedrooms)")
boxplot(housing$`NEAR BAY`, xlab="Boxplot (Ocean Proximity - Near Bay)")
boxplot(housing$INLAND, xlab="Boxplot (Ocean Proximity - INLAND)")
boxplot(housing$`NEAR OCEAN`, xlab="Boxplot (Ocean Proximity - NEAR OCEAN)")
boxplot(housing$`<1H OCEAN`, xlab="Boxplot (Ocean Proximity - <1 Ocean)")
boxplot(housing$ISLAND, xlab="Boxplot (Ocean Proximity - Island)")

# The data has a lot of outliers 

# as we can see that there are NA values in total_bedrooms
# we can also split the ocean_proximity column into separate columns with boolean data 
# here we can replace the na values in total bedrooms with the median as it wont be affected by the outliers

# Now it is very useful to find correlation between variables and see how they are correlated to each other and to the output variable
# We take correlation plot to do the heavy lifting for use i.e correlation of one with all other
# The color here will indicate the correlation
corrplot(cor(housing),method="circle")
corMat <- as.data.frame(corrplot(cor(housing),method = "number"))

# Lets check which one has correlation more than 50% with median_house_value
# why median_house_value ? because thats what we are going to predict
row.names(corMat)[abs(corMat$median_house_value) > 0.50]

# Lets try to predict the median_house_value as a function of other variables using the variable that has the maximum correlation it
lm1.fit <- lm(median_house_value~median_income, data=housing)
lm1.fit
summary(lm1.fit)
names(lm1.fit)
confint(lm1.fit)
plot(housing$median_income, housing$median_house_value)
abline(lm1.fit, lwd=3, col="red")

# This will be used as to calculate sample size what is 75% for now to use it as training Data and rest as testing Data
sample.size <- floor(0.75 * nrow(housing))

# Setting seed will make sure you get some random numbers generated
set.seed(123)

# Stores Random rownumbers in trainIndices
trainIndices <- sample(seq_len(nrow(housing)), size=sample.size)

# Creates training dataset with row numbers stored in trainIndices
trainData <- housing[trainIndices,]

# All the ones excluding the ones in trainIndices are stored as testing Data
testData <- housing[-trainIndices, ]

lm2.fit <- lm(median_house_value ~ median_income, data=trainData)
lm2.fit
predict(lm2.fit, testData)
plot(testData$median_income, testData$median_house_value)
abline(lm2.fit, lwd=3, col="red")
