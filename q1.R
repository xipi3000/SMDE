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

#e)
tab <- table(airplanes$Model,airplanes$SalesRegion)
prop.table(tab)
