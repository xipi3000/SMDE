#Loading data set
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

#Plot the price
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


fan_low <- fan[fan$Model == "Bombardier CRJ200", ]
fan_mid <- fan[fan$Model %in% c("Airbus A320", "Boeing 737"), ]
fan_high <- fan[fan$Model %in% c("Airbus A350","Boeing 777")  ,]

#Finally got rid of segmentation
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

op<-par(mfrow=c(3,3))
#The only good ones are production year and age
plot(models[["ProductionYear"]],data = piston)
plot(models[["NumberofEngines"]],data = piston)
plot(models[["Capacity"]],data = piston)
plot(models[["Range.km."]],data = piston)
plot(models[["FuelConsumption.L.h."]],data = piston)
plot(models[["HourlyMaintenance..."]],data = piston)
plot(models[["Age"]],data = piston)
par(op)
#Both have the same correlation, either of them can be chosen
cor.test(piston$log10_price,piston$ProductionYear)
cor.test(piston$log10_price,piston$Age)


#We choose age, and as it is parabolic we try to increase R2 with the exponential
models[["Age2"]] <-log10_price ~ (Age+I(Age^2) )
piston_1<-lm(models[["Age"]],data = piston)
piston_2<-lm(models[["Age2"]],data = piston)
summary(piston_1)
summary(piston_2)


#Same procedings to the other segmentations
plot(models[["ProductionYear"]],data = fan_low)
plot(models[["Age"]],data = fan_low)


x2_2<-(fan_low$Age)^2
fan_low_2<-lm(fan_low$log10_price~fan_low$Age+x2_2)
summary(fan_low_2)


plot(models[["ProductionYear"]],data = fan_mid)
plot(models[["Age"]],data = fan_mid)

plot(models[["ProductionYear"]],data = fan_high)
plot(models[["Age"]],data = fan_high)


#B) Create our multivariable model with production year and age
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
dwtest(piston_2, alternative = "two.sided")

#After all the tests we see that they have exacly the same resutls because age and production year are the same, one just has negative slope and the other positive
#So the multivariable model is the same as the single one



#C) Create new column for the distinction of model
fan_high$Category <- ifelse(grepl("Airbus", fan_high$Model), "Airbus",
                            ifelse(grepl("Boeing", fan_high$Model), "Boeing", "Other"))

# Convert it to a factor
fan_high$Category <- factor(fan_high$Category)

#New model is created


models[["Age2_Category"]] <-log10_price ~ (Age+I(Age^2) +Category )

factor_fan_high_reg <- lm(models[["Age2_Category"]], data = fan_high)

fan_high_reg <- lm(models[["Age2"]] , data = fan_high)

#Comparing it to the one without the category
summary(factor_fan_high_reg) 
summary(fan_high_reg)


###1. Normality###
#Shapiro Wilks Test
shapiro.test(residuals(factor_fan_high_reg))
shapiro.test(residuals(fan_high_reg))
### 2. Homogenity of Variance ###
##Breusch Pagan Test
library(lmtest)
bptest(factor_fan_high_reg)
bptest(fan_high_reg)
### 3. The independence of errors ### 
dwtest(factor_fan_high_reg, alternative = "two.sided")
dwtest(fan_high_reg, alternative = "two.sided")

#The one with category has better results


#The same with the other dataset
fan_mid$Category <- ifelse(grepl("Airbus", fan_mid$Model), "Airbus",
                            ifelse(grepl("Boeing", fan_mid$Model), "Boeing", "Other"))
fan_mid$Category <- factor(fan_mid$Category)


factor_fan_mid_reg <- lm(models[["Age2_Category"]], data = fan_mid)
fan_mid_reg <- lm(models[["Age2"]], data = fan_mid)

summary(factor_fan_mid_reg) #Better
summary(fan_mid_reg)

###1. Normality###
#Shapiro Wilks Test
shapiro.test(residuals(factor_fan_mid_reg))
shapiro.test(residuals(fan_mid_reg))
### 2. Homogenity of Variance ###
##Breusch Pagan Test
library(lmtest)
bptest(factor_fan_mid_reg)
bptest(fan_mid_reg)
## 3. The independence of errors ###
dwtest(factor_fan_mid_reg, alternative = "two.sided")
dwtest(fan_mid_reg, alternative = "two.sided")

#d)

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


