library(readxl)
library(plyr)
library(pastecs)
library(ggplot2)
library(psych)
library(dplyr)

setwd('/Users/logan/Documents/GitHub/DSC520LQ')
housing_df <- read_excel('/Users/logan/Documents/GitHub/DSC520LQ/week-6-housing.xlsx')
housing_df
#Use the apply function on a variable in your dataset
med_house_size <- apply(housing_df['square_feet_total_living'],2, median)
#Use the aggregate function on a variable in your dataset
aggregate(square_feet_total_living ~ year_built, housing_df, mean)
#Use the plyr function on a variable in your dataset - more specifically, I want to see you split some data, perform a modification to the data, and then bring it back together
housing_df_modification <- ddply(housing_df, ~ sq_ft_lot, summarize, Pos_Expansion_Sq_Ft = sq_ft_lot + square_feet_total_living)
housing_df_modification
#Check distributions of the data
gliving_ft <- ggplot(housing_df, aes(square_feet_total_living))
glot_ft <- ggplot(housing_df, aes(sq_ft_lot))
gliving_ft + geom_histogram(bins=10)
glot_ft + geom_histogram()
glot_ft + geom_boxplot()
gliving_ft + geom_boxplot()
#Identify if there are any outliers
## For square feet total living area and sq ft lot sizes there are outliers as seen in the box plots.

#Create at least 2 new variables
housing_df$Renovated <- with(housing_df,year_renovated != 0)
housing_df$Renovated
housing_df$Yearly_Inflation <- with(housing_df,housing_df$`Sale Price` * 1.10)
housing_df$Yearly_Inflation

