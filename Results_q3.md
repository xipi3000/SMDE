# Results q3
## a)
Price is skewed, so we need to transform, log solves skewness. Now we have a segmented dataset. 
Two big groups are separated by EngineType, after this inside the Turbofans, price data is also segmented.
There are three normal distributions which are caused by the models of the airplanes.
Bombardier is in the low tier, Airbus A320 and Boeing 737 on the mid tier, and Airbus A350 and Boeing 777 on the high tier.

So we divided the models for each of this ranks, are they clearly behave totally different for each group of models.
The only indepentent variables that have some relationship with the price is the year of production and the age. 
Both of them create a parabolic regression, which cannot be linearized with any transformation, but could be improved its value with the R2^2.
Then after testing we see that even with different slope directinon, the values were the same, as age and year of production is the same variable.

The best can be both.

##b) 
As age and year of production are really the same variable then doing a multivariable doesn't change anything to the model, as both are the same variable.

##c)
This it only serves in the mid and high tiers on the turbofans, because of our initial data segmentation, as the only data sets that have different models are this two, with only Airbus and Boeing.
 