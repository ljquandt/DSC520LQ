#load 
library(ggplot2)
library(qqplotr)
setwd('/Users/Logan/Documents/Github/DSC520LQ')
#read csv file
acs_df <- read.csv('/Users/Logan/Documents/Github/DSC520LQ/acs-14-1yr-s0201.csv')
#Data Types
colnames(acs_df)
#use str(); nrow(); ncol()
str(acs_df)
nrow(acs_df)
ncol(acs_df)
#Create a Histogram of the HSDegree variable using the ggplot2 package
hist <- ggplot(acs_df,aes(HSDegree))+geom_histogram(aes(y=..density..), binwidth=2)+ggtitle('High School Degrees by US county') + xlab('% of population in County w/ HS Degree') + ylab('# of counties')
hist
##Is it unimodal?
  #Yes it only has 1 peak

##Is it approximately symmetrical?
   #It is not symmetrical, it is skewed

##Is it approximately bell-shaped?
  #It is approximately bell-shaped with a negative skew.

##Is it approximately normal?
  #It is not normal

##If not normal, is the distribution skewed? If so, in which direction?
  #The distribution has a negative skew as the tail is longer to the left.

##Include a normal curve to the Histogram that you plotted
hist2 <- hist + stat_function(fun = dnorm, args = list(mean = mean(acs_df$HSDegree, na.rm = TRUE), sd = sd(acs_df$HSDegree, na.rm=TRUE)), color='black', size=1)

##Explain whether a normal distribution can accurately be used as a model for this data
 # A normal distribution can not accurately be used for this data due to the negative skew.

#Create a Probability Plot of the HSDegree variable.
ggplot(acs_df, aes(sample = HSDegree)) + stat_qq()


##Based on what you see in this probability plot, is the distribution approximately normal? Explain how you know
 #The distribution is not normal as a normal distribution on a probablity plot is a straight line

##If not normal, is the distribution skewed? If so, in which direction? Explain how you know.
 # The distribution is skewed. It is skewed to the right as there is a tail on the left.

library(pastecs)

#Now that you have looked at this data visually for normality, you will now quantify normality with numbers using the stat.desc() function. Include a screen capture of the results produced
stat.desc(acs_df$HSDegree,norm=TRUE)

##In the analysis done we are able to see that the % of population with high school degree by county is skewed negatively based off the histogram data. 
## The data is platykurtic as the tails are very thin as seen in the Histogram.
## A larger sample size is likely to smooth the data into more closely resembling a normal distribution



