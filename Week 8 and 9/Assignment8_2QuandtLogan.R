library(plyr)
library(dplyr)
library(purrr)
library(readxl)
library(stringr)
library(tidyverse)

setwd('/Users/logan/Documents/GitHub/DSC520LQ')
housing_df <- read_excel('/Users/logan/Documents/GitHub/DSC520LQ/week-6-housing.xlsx')
var_names <- colnames(housing_df)
var_names

# I.) Explain any transformations or modifications you are making to the dataset
## I will be removing some variables that are not useful in my analysis or that had missing data.
## I ended up renaming Sale Price due to errors with variables in second equation as well.

refined_cols <- c('Sale Date', 'Sale Price', 'zip5', 'bedrooms', 'sq_ft_lot', 'square_feet_total_living', 'prop_type','year_built','bath_full_count','bath_half_count','bath_3qtr_count')
housing_df_final <- housing_df[refined_cols]
names(housing_df_final)[names(housing_df_final) == 'Sale Price'] <- 'sale_price'

# II.) Create two variables one that will contain the variables Sale Price and Square Foot of Lot (same variables used from previous assignment on simple regression) 
# and one that will contain Sale Price and several additional predictors of your choice. Explain the basis for your additional predictor selections.

price_lot_lm <- lm(sale_price ~ sq_ft_lot, housing_df_final)
multiple_pred_lm <- lm(sale_price ~ square_feet_total_living + sq_ft_lot + bedrooms + year_built, housing_df_final)

## Chose predictors zip code, square feet total living and year built.
## These are common factors that go into pricing a house such as price/sq ft total living, new construction vs old and zip code.

# III.) Execute a summary() function on two variables defined in the previous step to compare the model results.
summary(price_lot_lm)
summary(multiple_pred_lm)

## The R2 for the simple regression for Sale Price/Sq_ft_lot were
## R2 = 0.01435, Adjusted R2 = 0.01428
## The R2 for the multiple predictors were
## R2 = 0.2202, Adjusted R2 = 0.22
## As R2 is a measure of fit for the model we can see neither model is a great fit for the data.
## However, the additional predictor model performed much better (0.014 vs 0.2202) 
## likely due to the predictors (sq_foot_total_living, sq_ft_lot, bedrooms and year_built) being more predictive of sale price than sq_ft_lot alone.

# IV.) Considering the parameters of the multiple regression model you have created. 
# What are the standardized betas for each parameter and what do the values indicate?

library(lm.beta)
lm.beta(price_lot_lm)
lm.beta(multiple_pred_lm)

## These standarized betas for the simple regression indicate that for every standard deviation change in sq_ft_lot there is a 0.11 st dev change in Sale Price.
## The standardized betas for the multiple predicors indicate that for every std dev change in the predictors there is the corresponding st dev change in sales price.
## square_feet_total_living = 0.4203, sq_ft_lot = .038, year_built = 0.119, bedrooms = -0.19
## This indicates that square_feet_total_living drives the biggest increase in Sales Price.

# V.) Calculate the confidence intervals for the parameters in your model and explain what the results indicate
confint.lm(price_lot_lm, parameter=0.95)
confint.lm(multiple_pred_lm, parameter=0.95)
## The results indicate that with 95% confidence the results would be within these two bounds for the parameters.
## These results also confirm that sq ft total living is the best predictor due to the smallest margin of error

# VI.) Assess the improvement of the new model compared to your original model (simple regression model) by testing 
# whether this change is significant by performing an analysis of variance.
anova(price_lot_lm,multiple_pred_lm)
# The F score of 1131.6 and a p value under 0.05 (2.2e-16) indicate that the changes were statistically significant.

# VII.) Perform casewise diagnostics to identify outliers and/or influential cases, 
# storing each function's output in a dataframe assigned to a unique variable name

residuals <- resid(multiple_pred_lm)
stdresiduals <- rstandard(multiple_pred_lm)
stdntresiduals <- rstudent(multiple_pred_lm)
cookscases <- cooks.distance(multiple_pred_lm)
dfbetacases <- dfbeta(multiple_pred_lm)
dffitcases <- dffits(multiple_pred_lm)
leveragecases <- hatvalues(multiple_pred_lm)
covratiocases <- covratio(multiple_pred_lm)

casewise_df <- data.frame(residuals, stdresiduals, stdntresiduals, cookscases, dfbetacases, dffitcases, leveragecases,covratiocases)

# VIII.) Calculate the standardized residuals using the appropriate command, specifying those that are +-2, 
# storing the results of large residuals in a variable you create.

casewise_df$largeres <- casewise_df$stdresiduals > 2 | casewise_df$stdresiduals < -2

# IX.) Use the appropriate function to show the sum of large residuals
sum(casewise_df$largeres_df)

# X.) Which specific variables have large residuals (only cases that evaluate as TRUE)
colnames(casewise_df)
large_residuals_var <- casewise_df$largeres
sum(large_residuals_var)
## There are 335 variables that have large residuals. 

# XI.) Investigate further by calculating the leverage, cooks distance, and covariance rations. 
# Comment on all cases that are problematics.

problematics <- casewise_df[casewise_df$largeres, c("cookscases", "leveragecases",'covratiocases')]
problematics

# XII.) Perform the necessary calculations to assess the assumption of independence and 
# state if the condition is met or not

dwt(multiple_pred_lm)

## The statistic is less than 1 which means the assumption has not been met.

# XIII.) Perform the necessary calculations to assess the assumption of no multicollinearity and 
# state if the condition is met or not

vif(multiple_pred_lm)
1/vif(multiple_pred_lm)
mean(vif(multiple_pred_lm))

## The VIFs are all below 10 and the tolerance statistics are all above 0.2 and the average is only slightly above 1. This suggests there is no significant collinearity.

# XIV.) Visually check the assumptions related to the residuals using the plot() and hist() functions. 
# Summarize what each graph is informing you of and if any anomalies are present.
plot(multiple_pred_lm)
hist(casewise_df$residuals)
hist(casewise_df$stdresiduals)
hist(casewise_df$stdntresiduals)

# XV.) Overall, is this regression model unbiased? 
# If an unbiased regression model, what does this tell us about the sample vs. the entire population model?

## Overall I would say the regression model is biased as there are 335 outliers and although not by much the average VIF is slightly over 1. 