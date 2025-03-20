#a)

airplanes<-read.csv2("airplane_price_dataset.csv",sep=",")
# Model = Cat, Year = Cat, NumOfEng= Cat, EngType= Cat, Capacity = Quant, Range = Quant, FuelCons = Quant, HourMantain = Quant, Age = Cat, SalesReg= Cat, PRice = QUant



airplanes <- airplanes %>%
  mutate(
    Capacity = as.numeric(Capacity),
    Range.km. = as.numeric(Range.km.),
    FuelConsumption.L.h. = as.numeric(FuelConsumption.L.h.),
    HourlyMaintenance... = as.numeric(HourlyMaintenance...),
    NumberofEngines = as.character(NumberofEngines),
    ProductionYear = as.character(ProductionYear),
    Age = as.character(Age),
  )

#b)
summary(airplanes)
library(dplyr)

summary_by_engine <- airplanes %>%
  group_by(EngineType) %>%
  summarise(across(where(is.numeric), list(
    mean = mean
  ), na.rm = TRUE))


#c)

boxplot(airplanes$FuelConsumption.L.h.~airplanes$EngineType)

#d)

round(t.test(airplanes$Capacity)$conf.int, 2)


#f)
filtered_airplanes <- airplanes %>%
  filter(Model %in% c("Bombardier CRJ200", "Cessna 172"))

#g)
# Transformation of price to logarithmic scale
filtered_airplanes <- filtered_airplanes %>%
  mutate(Log_Price = log(Price))


ggplot(filtered_airplanes, aes(x = Log_Price, fill = EngineType, color = EngineType)) +
  geom_density(alpha = 0.4) +  # overlapping distributions
  labs(title = "Density Plot of Price by Engine Type",
       x = "Price", 
       y = "Density") +
  theme_minimal()

ggplot(filtered_airplanes, aes(x = EngineType, y = Log_Price, fill = EngineType)) +
  geom_boxplot() +
  labs(title = "Boxplot of Log-Transformed Price by Engine Type",
       x = "Engine Type",
       y = "Log(Price)") +
  theme_minimal()

#h)
median_price <- median(filtered_airplanes$Price, na.rm = TRUE)

filtered_airplanes <- filtered_airplanes %>%
  mutate(PriceCategory = ifelse(Price <= median_price, "Low", "High"))

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
