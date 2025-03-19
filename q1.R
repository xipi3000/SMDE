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

summary(airplanes)
library(dplyr)

summary_by_engine <- airplanes %>%
  group_by(EngineType) %>%
  summarise(across(where(is.numeric), list(
    mean = mean
  ), na.rm = TRUE))



boxplot(airplanes$FuelConsumption.L.h.~airplanes$EngineType)

