library(dplyr)
library(tidyverse)
library(ggplot2)
install.packages("olsrr")
library("olsrr")
devtools::install_github("rsquaredacademy/olsrr")
library(devtools)

# Install development version from GitHub
# install.packages("devtools")


setwd("D:/introcasestudy")

#Reading data from csv file
df <- as.data.frame(read.csv('ImmoDataNRW.csv'))

attach(df)

# Data Types-- Summary of data
unique(energyEfficiencyClass)4

sum(is.na(df))
colSums(is.na(df))
x##Task 1.1.1 Select only observations for the city of Dortmund

dortmund <- subset(df,regio2=="Dortmund")
sum(is.na(dortmund))

factor(energyEfficiencyClass)
##Task 1.1.2 Remove the variable that has the highest number of missing observations (NA).
colSums(is.na(dortmund))
#Column noParkSpaces has 420 missing observations
removed_column_data = within(dortmund,rm(noParkSpaces))

##Task 1.1.3 remove all rows that contain at least one NA. Do not remove rows that contain "NO_INFORMATION"
removed_rows_data  = removed_column_data %>% drop_na()

Filter(function(u) any(c('NO_INFORMATION') %in% u), removed_rows_data)

##Task 1.2 Compute the rental price per square meter (sqmPrice). 
#This will be the dependent variable for the regression analysis.

data_regression = mutate(removed_rows_data,sqmPrice = totalRent /livingSpace)

##Task 1.3  Group the values of the variable typeOfFlat

data_regression$typeOfFlat[which(data_regression$typeOfFlat %in% c('loft', 'maisonette', 'penthouse', 'terraced_flat','other'))] <- 'luxurious_artistic_other'
data_regression$typeOfFlat[which(data_regression$typeOfFlat %in% c('ground_floor','raised_ground_floor'))] <- 'r_ground_floor'
data_regression$typeOfFlat[which(data_regression$typeOfFlat %in% c('roof_storey','half_basement'))] <- 'roof_half_basement'
data_prepare<-data_regression
summary(data_prepare)
summary(data_prepare$floor)
summary(data_prepare$yearConstructed)
sd(data_prepare$yearConstructed)
sd(data_prepare$floor) 

table(data_regression$typeOfFlat)
table(data_regression$yearConstructed)
table(data_regression$condition)
table(data_regression$energyEfficiencyClass)
table(data_regression$lastRefurbish)
table(data_regression$floor)
table(data_regression$yearConstructed)
## Tasks 2.1.1- Find the "best" predictors for sqmPrice using Best Subset Selection.


#Starting with null model

lmod=NULL

lmod<-lm(sqmPrice~balcony + yearConstructed + newlyConst + hasKitchen + lift + garden + typeOfFlat + 
           floor + condition + lastRefurbish + energyEfficiencyClass, data=data_prepare)

z<-ols_step_all_possible(lmod)

# total number of models
nrow(z)

## Tasks 2.1.2 compare using AIC and BIC as selection criteria

#see which models were chosen as best by both methods
idx_AIC = which.min(z$aic) ## 1981
idx_BIC = which.min(z$sbic) ## 2036

AIC<-min(z$aic)##1933.924
BIC<-min(z$sbc)##598.1692
variables_in_aic<-z[idx_AIC,3]
#"balcony yearConstructed newlyConst hasKitchen lift typeOfFlat floor condition energyEfficiencyClass"
variables_in_bic<-z[idx_BIC,3]
#"balcony yearConstructed newlyConst hasKitchen lift typeOfFlat floor condition lastRefurbish energyEfficiencyClass"


## Tasks 2.2.1 . Estimate the "best" linear model for sqmPrice using the AIC from 1

lm_aic = lm(sqmPrice ~ yearConstructed + newlyConst +condition+ hasKitchen +lift+ typeOfFlat+ floor + balcony + energyEfficiencyClass, data = data_prepare)

## Task 2.2.2- estimate coefficients and statistical significance

summary(lm_aic)

#We can decide whether there is any significant relationship between x and y by 
#testing the null hypothesis that ?? =0 ?

#As the p-value is much less than 0.05 in case of variable newlyConstTRUE, conditiongood, yearConstructed , 
#livingSpace, energyEfficiencyClassNO_INFORMATION,liftTrue,typeOfFlatr_ground_floor, hasKitchenTrue,
#we reject the null hypothesis that ?? = 0. 
#Hence there is a significant relationship between the variables in the linear regression model of the data 
#set faithful.

# But variables typeOfFlatluxurious_artistic_other, conditionNo_Information has p value much greater than 0.05, and we reject null hypothesis
# in their case, and therefore these variables not dependent on sqpPrice

##F-statistic is a good indicator of whether there is a relationship between our predictor and the response variables. The further the F-statistic is from 1 the better it is
##13.77
#-----------------------------------------
## tasks 2.2.3 provide confidence intervals for the regression parameters and evaluate the goodness of fit.

confint(lm_aic,level = 0.95)
# true vs fitted

##Checking assumptions
ggplot(data_prepare, aes(y=sqmPrice, x=lm_aic$fitted.values)) +
  geom_point() + ylab("True values") + xlab("Fitted values") + ggtitle("Linear Regression Model")+
  theme(plot.title = element_text(
    size=25,hjust = 0.5,face = "bold"),text = element_text(size=25)) +
  geom_abline(intercept = 0, slope = 1, col="red")

ggplot(data_prepare, aes(y=lm_aic$residuals, x=lm_aic$fitted.values)) +
  geom_point() + ylab("Residuals") + xlab("Fitted values") + ggtitle("Residual vs Fitted Plot")+
  theme(plot.title = element_text(
    size=25,hjust = 0.5,face = "bold"),text = element_text(size=25)) +
  geom_abline(intercept = 0, slope = 0, col="red")

ggplot(data_prepare, aes(y=rstandard(lm_aic), x=lm_aic$fitted.values)) +
  geom_point() + ylab("Standardized Residuals") + xlab("Fitted values") + ggtitle("Scale-Location Plot")+
  theme(plot.title = element_text(
    size=25,hjust = 0.5,face = "bold"),text = element_text(size=25)) +
  geom_abline(intercept = 0, slope = 0, col="red")


ggplot(data = lm_aic, aes(sample = lm_aic$residuals)) +  ggtitle("Q-Q plot")+
  geom_qq(color = "dark blue") +
  geom_qq_line(color = "orange") +
  labs(y = "Residuals(AIC model)", x = "Theoratical Quantiles(AIC model)", element_text()) +theme_bw()+
  theme(
    axis.title.x = element_text(size=25, face="bold"),
    axis.title.y = element_text( size=25, face="bold"),
    axis.text = element_text(size = 25, face="bold"),
    plot.title = element_text(size=25,hjust = 0.5,face = "bold")
  )
