airplanes<-read.csv2("airplane_price_dataset.csv",sep=",")
# Model = Cat, Year = Cat, NumOfEng= Cat, EngType= Cat, Capacity = Quant, Range = Quant, FuelCons = Quant, HourMantain = Quant, Age = Cat, SalesReg= Cat, PRice = QUant
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

hist(airplanes$Price...)

hist(airplanes$Price...)


# Create log10-transformed price (handles zeros/negative prices if they exist)
airplanes$log10_price <- log10(airplanes$Price +1) #+ 1)  # +1 avoids -Inf if Price=0
hist(airplanes$log10_price)
fan <- airplanes[airplanes$EngineType == "Turbofan", ]
piston <-  airplanes[airplanes$EngineType == "Piston", ]

hist(piston$log10_price)
hist(fan$log10_price)
# 
# fan$price_tier <- cut(fan$log10_price,
#                       breaks = c(0, 7.6, 8.3, Inf),  # Adjust break points as needed
#                       labels = c("Low", "Mid", "High"))

# 
# fan_low <- fan[fan$price_tier == "Low", ]
# fan_mid <- fan[fan$price_tier == "Mid", ]
# fan_high <- fan[fan$price_tier == "High" ,]

fan_low <- fan[fan$Model == "Bombardier CRJ200", ]
fan_mid <- fan[fan$Model %in% c("Airbus A320", "Boeing 737"), ]
fan_high <- fan[fan$Model %in% c("Airbus A350","Boeing 777")  ,]

hist(fan_low$log10_price)
hist(fan_mid$log10_price)
hist(fan_high$log10_price)

indep_vars <- c("ProductionYear", "NumberofEngines", "Capacity", "Range.km.", "FuelConsumption.L.h.", "HourlyMaintenance...", "Age")




models <- list()
# Create simple linear regression for each numerical variable
for(var in indep_vars) {
  formula <- as.formula(paste("log10_price ~ ", var))
  models[[var]] <- formula
}



regresions <- list()
for(var in indep_vars) {
  formula <- as.formula(paste("log10_price ~ ", var))
  regresions[[var]] <- lm(formula,data = piston)
}



# If p-value is > 0.05 we cannot say that alternative hypothesis is true
plot(models[["ProductionYear"]],data = piston)
plot(models[["NumberofEngines"]],data = piston)
plot(models[["Capacity"]],data = piston)
plot(models[["Range.km."]],data = piston)
plot(models[["FuelConsumption.L.h."]],data = piston)
plot(models[["HourlyMaintenance..."]],data = piston)
plot(models[["Age"]],data = piston)


cor.test(piston$log10_price,piston$ProductionYear)
cor.test(piston$log10_price,piston$Age)


summary(lm(piston$log10_price~piston$ProductionYear))

# x2_2<-(piston$ProductionYear)^2
# reg2_2<-lm(piston$log10_price~piston$ProductionYear+x2_2)
# summary(reg2_2)

x2_2<-(piston$ProductionYear)^2
piston_2<-lm(piston$log10_price~piston$ProductionYear+x2_2)
summary(piston_2)

# 
# young_piston <- piston[piston$Age < 25, ]
# 
# cor.test(young_piston$log10_price,young_piston$Age)
# plot(young_piston$log10_price~young_piston$Age)
# summary(lm(young_piston$log10_price~young_piston$Age))



plot(models[["ProductionYear"]],data = fan_low)
plot(models[["Age"]],data = fan_low)
# x2_2<-(fan_low$ProductionYear)^2
# reg2_2<-lm(fan_low$log10_price~fan_low$ProductionYear+x2_2)
# summary(reg2_2)

x2_2<-(fan_low$Age)^2
fan_low_2<-lm(fan_low$log10_price~fan_low$Age+x2_2)
summary(fan_low_2)


plot(models[["ProductionYear"]],data = fan_mid)
plot(models[["Age"]],data = fan_mid)

plot(models[["ProductionYear"]],data = fan_high)
plot(models[["Age"]],data = fan_high)


library(ggplot2)
# Check linearity of each predictor
p1 <- ggplot(piston, aes(ProductionYear, log10_price)) + 
  geom_point() + geom_smooth(method = "loess")
p2 <- ggplot(piston, aes(Age, log10_price)) + 
  geom_point() + geom_smooth(method = "loess")
gridExtra::grid.arrange(p1, p2, ncol = 2)

multivar_model <- lm(log10_price ~( ProductionYear+I(ProductionYear^2)) + (Age+I(Age^2)), data = piston)
#multivar_model <- lm(log10_price ~( ProductionYear) + (Age), data = piston)

summary(multivar_model)
plot(residuals(multivar_model))

vif(multivar_model)

summary(multivar_model)
summary(piston_2)

###1. Normality###
#Shapiro Wilks Test
shapiro.test(residuals(multivar_model))
shapiro.test(residuals(piston_2))
shapiro.test(residuals(fan_low_2))
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


fan_high$Category <- ifelse(grepl("Airbus", fan_high$Model), "Airbus",
                            ifelse(grepl("Boeing", fan_high$Model), "Boeing", "Other"))

# Convert it to a factor
fan_high$Category <- factor(fan_high$Category)

x2_2<-(piston$Age)^2
factor_fan_high_reg <- lm(log10_price ~ (Age+I(Age^2)  )+ Category, data = fan_high)

fan_high_reg <- lm(log10_price ~ (Age+I(Age^2)  ), data = fan_high)

summary(reg2_3)
hist(residuals(factor_fan_high_reg))
hist(residuals(fan_high_reg))
anova(fan_high_reg, factor_fan_high_reg)