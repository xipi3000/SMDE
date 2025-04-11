##########################################
#A
##########################################
airplanes<-read.csv2("airplane_price_dataset.csv",sep=",")
library(dplyr)
airplanes <- airplanes %>%
  mutate(
    Capacity = as.numeric(Capacity),
    Range.km. = as.numeric(Range.km.),
    FuelConsumption.L.h. = as.numeric(FuelConsumption.L.h.),
    HourlyMaintenance... = as.numeric(HourlyMaintenance...),
    NumberofEngines = as.numeric(NumberofEngines),
    ProductionYear = as.numeric(ProductionYear),
    Age = as.numeric(Age),
    Price... = as.numeric(Price...),
  )


#Plot the price distribution
hist(airplanes$Price...)
#The data is skewd, we will try to fix it with a transformation

#Transform with logarithm
airplanes$log10_price <- log10(airplanes$Price +1)

#Price becomes normal data, but is segmented
hist(airplanes$log10_price)
fan <- airplanes[airplanes$EngineType == "Turbofan", ]
piston <-  airplanes[airplanes$EngineType == "Piston", ]

#Data is normal
hist(piston$log10_price)


#Data still segmented
hist(fan$log10_price)

#Finally got rid of segmentation
fan_low <- fan[fan$Model == "Bombardier CRJ200", ]
fan_mid <- fan[fan$Model %in% c("Airbus A320", "Boeing 737"), ]
fan_high <- fan[fan$Model %in% c("Airbus A350","Boeing 777")  ,]
hist(fan_low$log10_price)
hist(fan_mid$log10_price)
hist(fan_high$log10_price)


# Create simple linear regression for each numerical variable
indep_vars <- c("ProductionYear", "NumberofEngines", "Capacity", "Range.km.", "FuelConsumption.L.h.", "HourlyMaintenance...", "Age")
models <- list()
for(var in indep_vars) {
  formula <- as.formula(paste("log10_price ~ ", var))
  models[[var]] <- formula
}


#The only good ones are production year and age
op<-par(mfrow=c(3,3))
plot(models[["ProductionYear"]],data = piston)
plot(models[["NumberofEngines"]],data = piston)
plot(models[["Capacity"]],data = piston)
plot(models[["Range.km."]],data = piston)
plot(models[["FuelConsumption.L.h."]],data = piston)
plot(models[["HourlyMaintenance..."]],data = piston)
plot(models[["Age"]],data = piston)
par(op)


#Same graphics for each data set, seems quite similar for every variable
plot(models[["ProductionYear"]],data = fan_low)
plot(models[["Age"]],data = fan_low)

plot(models[["ProductionYear"]],data = fan_mid)
plot(models[["Age"]],data = fan_mid)

plot(models[["ProductionYear"]],data = fan_high)
plot(models[["Age"]],data = fan_high)


#Both have the same correlation, either of them can be chosen
cor.test(piston$log10_price,piston$ProductionYear)
cor.test(piston$log10_price,piston$Age)

cor.test(fan_low$log10_price,fan_low$ProductionYear)
cor.test(fan_low$log10_price,fan_low$Age)

cor.test(fan_mid$log10_price,fan_mid$ProductionYear)
cor.test(fan_mid$log10_price,fan_mid$Age)

cor.test(fan_high$log10_price,fan_high$ProductionYear)
cor.test(fan_high$log10_price,fan_high$Age)

#We choose age, and as it is parabolic we try to increase R2 with the exponential
models[["Age2"]] <-log10_price ~ (Age+I(Age^2) )
piston_1<-lm(models[["Age"]],data = piston)
piston_2<-lm(models[["Age2"]],data = piston)
summary(piston_1)
summary(piston_2)

fan_low_1<-lm(models[["Age"]],data = fan_low)
fan_low_2<-lm(models[["Age2"]],data = fan_low)
summary(fan_low_1)
summary(fan_low_2)

fan_mid_1<-lm(models[["Age"]],data = fan_mid)
fan_mid_2<-lm(models[["Age2"]],data = fan_mid)
summary(fan_mid_1)
summary(fan_mid_2)

fan_high_1<-lm(models[["Age"]],data = fan_high)
fan_high_2<-lm(models[["Age2"]],data = fan_high)
summary(fan_high_1)
summary(fan_high_2)


##########################################
#B)
##########################################
# Create our multivariable model with production year and age
multivar_model <- lm(log10_price ~( ProductionYear+I(ProductionYear^2)) + (Age+I(Age^2)), data = piston)
summary(multivar_model)
summary(piston_2)

###1. Normality###
#Shapiro Wilks Test
shapiro.test(residuals(multivar_model))
shapiro.test(residuals(piston_2))
# Using Histogram
hist(residuals(multivar_model))
hist(residuals(piston_2))
### 2. Homogenity of Variance ###
# Residual Analysis #
plot(residuals(multivar_model))
plot(residuals(piston_2))
##Breusch Pagan Test
library(lmtest)
bptest(multivar_model)
bptest(piston_2)
### 3. The independence of errors ### 
dwtest(multivar_model, alternative = "two.sided")
#NaNs appear, but we can already see that they are the same
dwtest(piston_2, alternative = "two.sided")

#After all the tests we see that they have exacly the same resutls because age and production year are the same, one just has negative slope and the other positive
#So the multivariable model is the same as the single one


##########################################
#C) 
##########################################

#Create new column for the distinction of model
fan_high$Category <- ifelse(grepl("Airbus", fan_high$Model), "Airbus",
                            ifelse(grepl("Boeing", fan_high$Model), "Boeing", "Other"))

# Convert it to a factor
fan_high$Category <- factor(fan_high$Category)

#New model is created
models[["Age2_Category"]] <-log10_price ~ (Age+I(Age^2) +Category )
factor_fan_high_reg <- lm(models[["Age2_Category"]], data = fan_high)

#Comparing it to the one without the category
fan_high_reg <- lm(models[["Age2"]] , data = fan_high)
summary(factor_fan_high_reg) 
summary(fan_high_reg)


###Normality
#Shapiro Wilks Test
shapiro.test(residuals(factor_fan_high_reg))
shapiro.test(residuals(fan_high_reg))
###Homogenity of Variance
#Breusch Pagan Test
library(lmtest)
bptest(factor_fan_high_reg)
bptest(fan_high_reg)
###The independence of errors
dwtest(factor_fan_high_reg, alternative = "two.sided")
dwtest(fan_high_reg, alternative = "two.sided")
#The factor has better results


#The same with the other dataset
fan_mid$Category <- ifelse(grepl("Airbus", fan_mid$Model), "Airbus",
                            ifelse(grepl("Boeing", fan_mid$Model), "Boeing", "Other"))
fan_mid$Category <- factor(fan_mid$Category)


factor_fan_mid_reg <- lm(models[["Age2_Category"]], data = fan_mid)
fan_mid_reg <- lm(models[["Age2"]], data = fan_mid)

summary(factor_fan_mid_reg) #Better
summary(fan_mid_reg)

###Normality
#Shapiro Wilks Test
shapiro.test(residuals(factor_fan_mid_reg))
shapiro.test(residuals(fan_mid_reg))
###Homogenity of Variance
#Breusch Pagan Test
library(lmtest)
bptest(factor_fan_mid_reg)
bptest(fan_mid_reg)
###The independence of errors
dwtest(factor_fan_mid_reg, alternative = "two.sided")
dwtest(fan_mid_reg, alternative = "two.sided")
#The factor has better results

##########################################
#D)
##########################################
#Piston
n <- nrow(piston)
train.sample1 <- sample(1:n, round(0.67*n))
train.set1 <- piston[train.sample1, ] 
test.set1 <- piston[-train.sample1, ] 
train.model1 <- lm(models[["Age2"]] , data = train.set1)
yhat<-predict(train.model1, test.set1, interval="prediction")
y<-test.set1$log10_price
error<-cbind(yhat[,1,drop=FALSE],y,(y-yhat[,1])^2)
sqr_err<-error[,3]
mse<-mean(sqr_err)

### Root Mean Square Error ###
RMSE1<-sqrt(mse/(nrow(test.set1)))
RMSE1
RMSE_train1<- sqrt(mean((train.model1$residuals)^2)/nrow(train.set1))
RMSE_train1


#Low tier
n <- nrow(fan_low)
train.sample1 <- sample(1:n, round(0.67*n))
train.set1 <- fan_low[train.sample1, ] 
test.set1 <- fan_low[-train.sample1, ] 
train.model1 <- lm(models[["Age2"]] , data = train.set1)
yhat<-predict(train.model1, test.set1, interval="prediction")
y<-test.set1$log10_price
error<-cbind(yhat[,1,drop=FALSE],y,(y-yhat[,1])^2)
sqr_err<-error[,3]
mse<-mean(sqr_err)

### Root Mean Square Error ###
RMSE1<-sqrt(mse/(nrow(test.set1)))
RMSE1
RMSE_train1<- sqrt(mean((train.model1$residuals)^2)/nrow(train.set1))
RMSE_train1



#Mid tier
n <- nrow(fan_mid)
train.sample1 <- sample(1:n, round(0.67*n))
train.set1 <- fan_mid[train.sample1, ] 
test.set1 <- fan_mid[-train.sample1, ] 
train.model1 <- lm(models[["Age2_Category"]] , data = train.set1)
yhat<-predict(train.model1, test.set1, interval="prediction")
y<-test.set1$log10_price
error<-cbind(yhat[,1,drop=FALSE],y,(y-yhat[,1])^2)
sqr_err<-error[,3]
mse<-mean(sqr_err)

### Root Mean Square Error ###
RMSE1<-sqrt(mse/(nrow(test.set1)))
RMSE1
RMSE_train1<- sqrt(mean((train.model1$residuals)^2)/nrow(train.set1))
RMSE_train1


#High tier
n <- nrow(fan_high)
train.sample1 <- sample(1:n, round(0.67*n))
train.set1 <- fan_high[train.sample1, ] 
test.set1 <- fan_high[-train.sample1, ] 
train.model1 <- lm(models[["Age2_Category"]] , data = train.set1)
yhat<-predict(train.model1, test.set1, interval="prediction")
y<-test.set1$log10_price
error<-cbind(yhat[,1,drop=FALSE],y,(y-yhat[,1])^2)
sqr_err<-error[,3]
mse<-mean(sqr_err)

### Root Mean Square Error ###
RMSE1<-sqrt(mse/(nrow(test.set1)))
RMSE1
RMSE_train1<- sqrt(mean((train.model1$residuals)^2)/nrow(train.set1))
RMSE_train1


