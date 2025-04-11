library(dplyr)
library(tidyr)
library(ggplot2)
install.packages("HSAUR")
library(HSAUR)
library(FactoMineR)
library(lmtest)

# Load and reinterpret observations
airplanes <- read.csv2("airplane_price_dataset.csv",sep=",")
# Model = Cat, Year = Cat, NumOfEng= Cat, EngType= Cat, Capacity = Quant, Range = Quant, FuelCons = Quant, HourMantain = Quant, Age = Quant, SalesReg = Cat, PRice = Quant (log)

airplanes <- airplanes %>%
  mutate(
    Capacity = as.numeric(Capacity),
    Range.km. = as.numeric(Range.km.),
    FuelConsumption.L.h. = as.numeric(FuelConsumption.L.h.),
    HourlyMaintenance... = as.numeric(HourlyMaintenance...),
    NumberofEngines = as.numeric(NumberofEngines),
    ProductionYear = as.numeric(ProductionYear),
    Age = as.numeric(Age),
    Price... = log10(as.numeric(Price...))
  )
summary(airplanes)

# a)

ggplot(airplanes, aes(x = SalesRegion, y = Price..., fill = SalesRegion)) +
  geom_boxplot() +
  labs(title = "Boxplot of Log-Transformed Price by Sales Region",
       x = "Sales Region",
       y = "Log10(Price)") +
  theme_minimal()
ggplot(airplanes, aes(x = Price..., fill = SalesRegion, color = SalesRegion)) +
  geom_density(alpha = 0.4) +  # overlapping distributions
  labs(title = "Density Plot of Price by Engine Type",
       x = "Log10(Price)", 
       y = "Density") +
  theme_minimal()

# Sales Region
breaks <- c(5.5, 6, 7, 7.25, 7.5, 7.75, 8, 8.25, 8.5, 8.75, 9) # Bins for the prices. Group low-price models, then equal width bins for higher value models
pricetb <- table(cut(airplanes$Price..., breaks), airplanes$SalesRegion)
pricetb
chisq.test(pricetb)

### Assumptions and observations ###
airplanes_filtered <- airplanes %>%
  mutate(
    EngineType = as.numeric(EngineType == "Piston"),
    Model = NULL,
    SalesRegion = NULL,
  )
afp <- airplanes_filtered[airplanes_filtered$EngineType == "Piston",]
aft <- airplanes_filtered[airplanes_filtered$EngineType == "Turbofan",]

# Perfect correlation between the engine type and the number of engines
table(airplanes$EngineType, airplanes$NumberofEngines)

# Perfect correlation between the year of production and the age
cor.test(airplanes_filtered$ProductionYear,airplanes_filtered$Age)

# Almost perfect correlation between the capacity and range
table(airplanes_filtered$Capacity,airplanes_filtered$Range.km.)
cor.test(airplanes_filtered$Capacity,airplanes_filtered$Range.km.)

# Histograms
airplanes_filtered %>% 
  pivot_longer(names(.)) %>%   # pivots to long form 
  ggplot() +                     
  aes(value, fill = name) +          # by default, pivot_longer will produce columns "name" and "value" 
  geom_histogram() +
  facet_wrap(~name, scales = 'free_x')

# Price ranges and data distribution for capacity / range
airplanes_filtered[between(airplanes_filtered$Price..., 0, 6),] %>% 
  pivot_longer(names(.)) %>%   # pivots to long form 
  ggplot() +                     
  aes(value, fill = name) +          # by default, pivot_longer will produce columns "name" and "value" 
  geom_histogram() +
  facet_wrap(~name, scales = 'free_x')

# Finding correlation between variables
cor(airplanes_filtered)

###### Kaiser-Meyer-Olkin (KMO) Test ###
## We can use kmo function written by Prof. Shigenobu Aok.
### (http://minato.sip21c.org/swtips/factor-in-R.pdf)
kmo <- function(x)
{
  x <- subset(x, complete.cases(x))       # Omit missing values
  r <- cor(x)                             # Correlation matrix
  r2 <- r^2                               # Squared correlation coefficients
  i <- solve(r)                           # Inverse matrix of correlation matrix
  d <- diag(i)                            # Diagonal elements of inverse matrix
  p2 <- (-i/sqrt(outer(d, d)))^2          # Squared partial correlation coefficients
  diag(r2) <- diag(p2) <- 0               # Delete diagonal elements
  KMO <- sum(r2)/(sum(r2)+sum(p2))
  MSA <- colSums(r2)/(colSums(r2)+colSums(p2))
  return(list(KMO=KMO, MSA=MSA))
}

# Remove low-correlation factors until KMO is high enough
kmo(airplanes_filtered %>% mutate(ProductionYear = NULL, EngineType = NULL, Age = NULL, HourlyMaintenance... = NULL))

# Remove low-correlation variables
airplanes_f <- airplanes_filtered %>%
  mutate(ProductionYear = NULL, EngineType = NULL, Age = NULL, HourlyMaintenance... = NULL)
p <- PCA(airplanes_f)
summary(p)

# Scree plot
plot(p$eig[,3], type="o", xlab="Dimension", ylab="Cumulative percentage of variance")

### b) ###

# Principal components dimension reduction
p$var$coord[,1:2]
airplanes_f$PC1 <- p$ind$coord[,1]
airplanes_f$PC2 <- p$ind$coord[,2]
cor(airplanes_f[,6:7]) # Orthogonality test

# Linear regression
reg <- lm(Price... ~ PC1 + PC2, data=airplanes_f)
summary(reg)

# 1. Normality
hist(residuals(reg))

# 2. Homogeneity of variance
plot(residuals(reg))
bptest(reg) # Fails

# 3. Independence of errors
dwtest(reg, alternative="two.sided")

# Predictions
pred <- predict(reg, airplanes_f)

plot(airplanes_f$Price..., pred, xlab = "Actual price", ylab = "Predicted price")
abline(0, 1, lty = 4)

plot(fitted(reg),
     residuals(reg) / airplanes_f$Price... * 100,
     xlab = "Fitted Values",
     ylab = "Residuals (in %)")
abline(h = 0, lty = 4)
