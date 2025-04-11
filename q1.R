library(dplyr)
library(ggplot2)
#a)

airplanes <- read.csv2("airplane_price_dataset.csv",sep=",")
# Model = Cat, Year = Cat, NumOfEng= Cat, EngType= Cat, Capacity = Quant, Range = Quant, FuelCons = Quant, HourMantain = Quant, Age = Quant, SalesReg = Cat, PRice = Quant (log)

airplanes <- airplanes %>%
  mutate(
    Capacity = as.numeric(Capacity),
    Range.km. = as.numeric(Range.km.),
    FuelConsumption.L.h. = as.numeric(FuelConsumption.L.h.),
    HourlyMaintenance... = as.numeric(HourlyMaintenance...),
    NumberofEngines = as.character(NumberofEngines),
    ProductionYear = as.character(ProductionYear),
    Age = as.numeric(Age),
    Price... = log10(as.numeric(Price...))
  )

#b)
summary(airplanes$Price...)
summary(airplanes$FuelConsumption.L.h.)

#for (engineType in unique(airplanes$EngineType)) {
#  airplanes_s <- airplanes[airplanes$EngineType == engineType,]
#  
#  cat("===== ", engineType, " =====\nPrice:\n")
#  print(summary(airplanes_s$Price...))
#  cat("Fuel consumption:\n")
#  print(summary(airplanes_s$FuelConsumption.L.h.))
#}

airplanes %>%
  group_by(EngineType) %>%
  summarise(n = n(), price_mean = mean(Price...), price_sd = sd(Price...), fuel_mean = mean(FuelConsumption.L.h.), fuel_sd = sd(FuelConsumption.L.h.))


#c)

boxplot(airplanes$FuelConsumption.L.h.~airplanes$EngineType)

#d)

for (engineType in unique(airplanes$EngineType)) {
 airplanes_s <- airplanes[airplanes$EngineType == engineType,]

 cat("===== ", engineType, " =====\n")
 print(round(t.test(airplanes_s$FuelConsumption.L.h.)$conf.int[1:2], 2))
}


#e)
tab <- table(airplanes$Model,airplanes$SalesRegion)
chisq.test(tab)


# f)
filtered_airplanes <- airplanes %>%
  filter(Model %in% c("Bombardier CRJ200", "Cessna 172"))

#g)
ggplot(filtered_airplanes, aes(x = Price..., fill = EngineType, color = EngineType)) +
  geom_density(alpha = 0.4) +  # overlapping distributions
  labs(title = "Density Plot of Price by Engine Type",
       x = "Log10(Price)", 
       y = "Density") +
  theme_minimal()

ggplot(filtered_airplanes, aes(x = EngineType, y = Price..., fill = EngineType)) +
  geom_boxplot() +
  labs(title = "Boxplot of Log-Transformed Price by Engine Type",
       x = "Engine Type",
       y = "Log10(Price)") +
  theme_minimal()

#h)
median_price <- median(filtered_airplanes$Price..., na.rm = TRUE)

filtered_airplanes <- filtered_airplanes %>%
  mutate(PriceCategory = ifelse(Price... <= median_price, "Low", "High"))

print(filtered_airplanes)

#i)
cross_table <- table(filtered_airplanes$Model, filtered_airplanes$PriceCategory)
print(cross_table)

conditional_probs <- prop.table(cross_table, margin = 1)  # Normalize by row (Model)
print(conditional_probs)

#j)
chi_test <- chisq.test(cross_table)
print(chi_test)

#K)
cross_table2 <- table(filtered_airplanes$Model, filtered_airplanes$SalesRegion)
print(cross_table2)

prop.table(table(filtered_airplanes$SalesRegion))
prop.table(table(filtered_airplanes$Model))
prop.table(cross_table2, 1)
prop.table(cross_table2, 2)


chi_test2 <- chisq.test(cross_table2)
print(chi_test2)

#Visualization of Data
ggplot(filtered_airplanes, aes(x = Model, fill = SalesRegion)) +
  geom_bar(position = "fill") + 
  labs(title = "Proportion of Sales by Airplane Model",
       x = "Airplane Model", 
       y = "Proportion",
       fill = "Sales Region") +
  theme_minimal()

