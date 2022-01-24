library(plyr)
library(dplyr)
library(purrr)
library(readxl)
library(purrr)

setwd('/Users/logan/Documents/GitHub/DSC520LQ')
housing_df <- read_excel('/Users/logan/Documents/GitHub/DSC520LQ/week-6-housing.xlsx')
housing_df
#group_by and summarize functions in dplyr
housing_df %>% group_by(zip5) %>% summarize(AvgHomeSize=mean(square_feet_total_living))
#select function in dplyr
select(housing_df,'Sale Price',square_feet_total_living)
#filter function in dplyr
housing_df %>% filter(square_feet_total_living == 2000)
#slice function in dplyr
housing_df %>% slice(10:20)
#mutate function in dplyr
housing_df %>% mutate(`Sale Price`/square_feet_total_living)

#keep function in purrr
keep(housing_df$`Sale Price`,function(x)x>1000000)
#discard function in purrr
discard(housing_df$bedrooms,function(x)x<3)

#cbind
housing_vec_one <- c('Sale Price','square_feet_total_living')
housing_vec_two <- c('bedrooms','prop_type')
housing_bind_one <- housing_df[housing_vec_one]
housing_bind_two <- housing_df[housing_vec_two]
housing_cbound <- cbind(housing_bind_one, housing_bind_two)
housing_cbound

#rbind
housing_row_one <- housing_df[1:5,]
housing_row_two <- housing_df[6:10,]
housing_row_bind <- rbind(housing_row_one,housing_row_two)
housing_row_bind

#split a string and concatenate it back together
library(stringr)
housing_df$addr_full
housing_add_one <- pluck(housing_df$addr_full,7,1)
housing_add_one
housing_add_two <- strsplit(housing_add_one," ")
housing_add_two
housing_add_concatenate <- paste0(housing_df$addr_full %>% pluck(7,1), collapse = " ")
housing_add_concatenate
