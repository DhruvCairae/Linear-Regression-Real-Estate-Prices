# import the data into R and summary
library(readr)
homes <- read_csv("dataset.csv")

#Converting Date of sale into number of days for numerical analysis
homes$sale_days<-Sys.time()-homes$date
homes$sale_days<- as.numeric(homes$sale_days, units="days")

#Creating a Catagorical Variable on whether home was renovated
homes$whether_renovated<- ifelse(homes$yr_renovated==0, 0, 1) 


#Creating two new varibales for number of years from sale and number of years from renovation
install.packages("lubridate")
library(lubridate)
homes$year_sold<-year(x = homes$date)
homes$years_built<-homes$year_sold-homes$yr_built
homes$years_renovated<-ifelse(homes$yr_renovated==0, 0, homes$year_sold-homes$yr_renovated)

#Transforming Price into Log
homes$log_price<-log10(homes$price)


#Creating a new Subset excluding redundant variables deemed unnecessay to analysis
firstcut_homes<-subset(homes, select = c(log_price,bedrooms,bathrooms,sqft_living,sqft_lot,floors,waterfront,view,condition,grade,sqft_above,sqft_basement,zipcode,sqft_living15,sqft_lot15,sale_days,whether_renovated,years_built,years_renovated))
summary(firstcut_homes)
cor(firstcut_homes)
library(corrplot)
cor.vis <- round(cor(firstcut_homes),2)
corrplot(cor.vis, method = "number")


#Final Dataframe for further analysis
dfh<-subset(firstcut_homes, select = c(log_price,bedrooms, bathrooms, sqft_living,view, grade, sqft_above, sqft_basement, sqft_living15,floors))
cor(dfh)
head(dfh)

#Correlation Matrix
library(corrplot)
cor.vis <- round(cor(dfh),2)
corrplot(cor.vis, method = "number")

#Scatter Plot Matrix
pairs(dfh)

#Creating Models
model1 <- lm(log_price ~bedrooms+bathrooms + sqft_living+floors+view+grade+sqft_above+sqft_basement+sqft_living15,data=dfh)
summary(model1)
model2<- lm(log_price ~bathrooms + sqft_living+floors+view+grade+sqft_above+sqft_living15,data=dfh)
summary(model2)

#Residual Plots
library(car)
residualPlot(model2)

#Test for constant error variance
library(lmtest)
bptest(model2)

#QQ Plot for Normality
library(car)
qqPlot(model2)

#KS test for Normality
residuals_1 <- residuals(model2)
residuals_1 <- scale(residuals_1, center = TRUE, scale = TRUE)
ks.test(residuals_1, "pnorm")

#Residual plots
library(car)
residualPlots(model2)

#VIF
vif(model2)

#Transform Variables and new models
dfh$logsqft_living<-log10(dfh$sqft_living)
dfh$logsqft_living15<-log10(dfh$sqft_living15)


model3<- lm(log_price ~bathrooms + logsqft_living+floors+view+grade+ logsqft_living15,data=dfh)
summary(model3)
residualPlots(model3)
vif(model3)


model4<-lm(log_price ~ logsqft_living+view+grade+ logsqft_living15,data=dfh)
summary(model4)
residualPlot(model4)
residualPlots(model4)
bptest(model4)
qqPlot(model4)
residuals_2<- residuals(model4)
residuals_2<- scale(residuals_2,center = TRUE, scale = TRUE)
ks.test(residuals_2, "pnorm")
vif(model4)

#AIC for model 1

library(MASS)
stepAIC(model1)
stepAIC(model1, direction = "both")

library(MASS)
stepAIC(model1) 
stepAIC(model1, direction = "both",k = log(nrow(firstcut_homes))) 



install.packages("leaps")
library(leaps)
reg.subset <- regsubsets(log_price ~bedrooms+bathrooms + sqft_living+floors+view+grade+sqft_above+sqft_basement+sqft_living15,data=dfh)
plot(reg.subset) 
plot(reg.subset, scale = "adjr2") 
plot(reg.subset, scale = "r2") 







