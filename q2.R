library(dplyr)
library(ggplot2)
library(car)
library(lmtest)  
library(emmeans)
library(tidyr)

airplanes <- read.csv2("airplane_price_dataset.csv", sep=",")
airplanes <- airplanes %>%
  mutate(
    Capacity = as.numeric(Capacity),
    Range.km. = as.numeric(Range.km.),
    FuelConsumption.L.h. = as.numeric(FuelConsumption.L.h.),
    HourlyMaintenance... = as.numeric(HourlyMaintenance...),
    NumberofEngines = as.character(NumberofEngines),
    ProductionYear = as.character(ProductionYear),
    Age = as.character(Age),
    Price = as.numeric(Price...),
  )

#a)
new_data <- airplanes %>%
  filter(Model %in% c("Airbus A320", "Airbus A350", "Boeing 737", "Boeing 777"))

new_data <- new_data %>%
  mutate(Log_Price = log(Price))

ggplot(new_data, aes(x = Log_Price)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Log(Price) for Selected Airplane Models",
       x = "Log(Price)", y = "Count")


ggplot(new_data, aes(x = Model, y = Log_Price, fill = Model)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Boxplot of Log(Price) by Airplane Model",
       x = "Model", y = "Log(Price)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#b)

model_range <- aov(Range.km. ~ Model, data = new_data)
model_capacity <- aov(Capacity ~ Model, data = new_data)
model_fuel <- aov(FuelConsumption.L.h. ~ Model, data = new_data)
model_maintenance <- aov(HourlyMaintenance... ~ Model, data = new_data)
model_price <- aov(Log_Price ~ Model, data = new_data)


#The populations from which the samples are selected must be normal:
hist(new_data$Capacity)
hist(new_data$Range.km.)
hist(new_data$FuelConsumption.L.h.)
hist(new_data$HourlyMaintenance...)
hist(new_data$Log_Price)

boxplot(Range.km. ~ Model, data = new_data)
boxplot(Capacity ~ Model, data = new_data)
boxplot(FuelConsumption.L.h. ~ Model, data = new_data)
boxplot(HourlyMaintenance... ~ Model, data = new_data)
boxplot(Log_Price ~ Model, data = new_data)

ks.test(residuals(model_range), "pnorm", mean(residuals(model_range)), sd(residuals(model_range)))
ks.test(residuals(model_capacity), "pnorm", mean(residuals(model_capacity)), sd(residuals(model_capacity)))
ks.test(residuals(model_fuel), "pnorm", mean(residuals(model_fuel)), sd(residuals(model_fuel)))
ks.test(residuals(model_maintenance), "pnorm", mean(residuals(model_maintenance)), sd(residuals(model_maintenance)))
ks.test(residuals(model_price), "pnorm", mean(residuals(model_price)), sd(residuals(model_price)))

#none is normally distributted. (fuel y maintencance are close)


#The observations within each sample must be independent:

# Create linear models for each variable (required for dwtest)
capacity_lm <- lm(Capacity ~ Model, data = new_data)
range_lm <- lm(Range.km. ~ Model, data = new_data)
fuel_lm <- lm(FuelConsumption.L.h. ~ Model, data = new_data)
maintenance_lm <- lm(HourlyMaintenance... ~ Model, data = new_data)
price_lm <- lm(Log_Price ~ Model, data = new_data)

# Perform Durbin-Watson test for independence of observations

# For capacity
dw_test_capacity <- dwtest(capacity_lm, alternative ="two.sided")
print("Durbin-Watson Test for Capacity:")
print(dw_test_capacity)

# For range
dw_test_range <- dwtest(range_lm, alternative ="two.sided")
print("Durbin-Watson Test for Range:")
print(dw_test_range)

# For fuel consumption
dw_test_fuel <- dwtest(fuel_lm, alternative ="two.sided")
print("Durbin-Watson Test for Fuel Consumption:")
print(dw_test_fuel)

# For maintenance
dw_test_maintenance <- dwtest(maintenance_lm, alternative ="two.sided")
print("Durbin-Watson Test for Hourly Maintenance:")
print(dw_test_maintenance)

# For price
dw_test_price <- dwtest(price_lm, alternative ="two.sided")
print("Durbin-Watson Test for Price:")
print(dw_test_price)

#OBSERVATONS: CAPACITY AND RANGE ARE FIXED FOR A GIVEN MODEL, AND, THUS, ARE NOT INDEPENDENT

#The populations from which the samples are selected must have equal variances (homogeneity of variance):
#levene test / Breusch Pagan test

# Levene's Test for Homogeneity of Variance
print("Levene Test for Capacity:")
print(leveneTest(Capacity ~ Model, data = new_data))

print("Levene Test for Range:")
print(leveneTest(Range.km. ~ Model, data = new_data))

print("Levene Test for Fuel Consumption:")
print(leveneTest(FuelConsumption.L.h. ~ Model, data = new_data))

print("Levene Test for Hourly Maintenance:")
print(leveneTest(HourlyMaintenance... ~ Model, data = new_data))

print("Levene Test for Log(Price):")
print(leveneTest(Log_Price ~ Model, data = new_data))


# Breusch-Pagan Test for Heteroscedasticity (on linear models)
print("Breusch-Pagan Test for Capacity:")
print(bptest(capacity_lm))

print("Breusch-Pagan Test for Range:")
print(bptest(range_lm))

print("Breusch-Pagan Test for Fuel Consumption:")
print(bptest(fuel_lm))

print("Breusch-Pagan Test for Hourly Maintenance:")
print(bptest(maintenance_lm))

print("Breusch-Pagan Test for Log(Price):")
print(bptest(price_lm))

#capacity and range are fixed, so no variance


summary(model_fuel) #not affected
summary(model_maintenance) #not affected
summary(model_range) # range is fixed for model, anova  not valid
summary(model_capacity) #capacity is fixed for model, so anova is not valid
summary(model_price) # affected

TukeyHSD(model_price) # Airbus a350 and boeing777 are more expensive than airbus a320 and boeing 737, the least expensive

#c)
two_way_anova <- aov(Log_Price ~ SalesRegion * Model, data = new_data)


# Boxplot to visualize the effect of SalesRegion on Log_Price
ggplot(new_data, aes(x = SalesRegion, y = Log_Price, fill = SalesRegion)) +
  geom_boxplot() +
  labs(title = "Effect of SalesRegion on Log_Price",
       x = "SalesRegion",
       y = "Log_Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot to visualize the effect of SalesRegion and Model on Log_Price
ggplot(new_data, aes(x = SalesRegion, y = Log_Price, fill = Model)) +
  geom_boxplot() +
  labs(title = "Effect of SalesRegion and Model on Log_Price",
       x = "SalesRegion",
       y = "Log_Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Check for Durbin-Watson test for autocorrelation in residuals
dwtest(anova_model) #no significant autocorrelation

summary(two_way_anova)
# both model and salesRegion impact on price, each one independently

# Post-hoc test
emmeans(anova_model, pairwise ~ SalesRegion * Model)



#d)
# Add the new variable 'year_cat' based on 'ProductionYear'
new_data <- new_data %>%
  mutate(
    year_cat = ifelse(as.numeric(ProductionYear) >= 2000, "Newer", "Older")
  )

#e)

# Boxplot for Log_Price by year_cat
ggplot(new_data, aes(x = year_cat, y = Log_Price, fill = year_cat)) +
  geom_boxplot() +
  labs(title = "Log_Price Distribution by Year Category", x = "Year Category", y = "Log_Price")

# Boxplot for Log_Price by Model and year_cat interaction
ggplot(new_data, aes(x = interaction(Model, year_cat), y = Log_Price, fill = Model)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Log_Price Distribution by Model and Year Category Interaction", 
    x = "Model and Year Category Interaction", 
    y = "Log_Price"
  )


# two-way ANOVA
anova_result <- aov(Log_Price ~ Model * year_cat, data = new_data)

#Check normality
# Q-Q plot of residuals
qqnorm(residuals(anova_result))
qqline(residuals(anova_result), col = "red")  #follows the diagonal, so normality


# Levene's test for homogeneity of variances
leveneTest(Log_Price ~ Model * year_cat, data = new_data) #doesn't pass, but continue anyway

#Independence
dwtest(anova_result)  #Independent

# Checking the results of the ANOVA
summary(anova_result) #both model and year category affect price, but they don't interact with each other


