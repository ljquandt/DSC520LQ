library(ggplot2)
library(pastecs)
library(psych)
library(dplyr)
setwd('/Users/Logan/Documents/Github/dsc520clone')
scores <- read.csv('data/scores.csv')
describe(scores)
str(scores)
colnames(scores)
scores

# 1.) What are the observational units in this study
## The observational units are the scores and the counts of the students that received each score in both sections.

# 2.) Identify the variables mentioned in the narrative paragraph and determine which are categorical and quantitative?
## The variables are Count, Score and Section. Count and Score are quantitative while Section is categorical.

# 3.) Create one variable to hold a subset of your data set that contains only the Regular Section and one variable for the Sports Section
section_names <- unique(scores['Section'])
regular_section <- filter(scores,scores$Section ==section_names[2,1])
sports_section <- filter(scores,scores$Section == section_names[1,1])


#4.) Use the Plot function to plot each Sections scores and the number of students achieving that score. 
# Use additional Plot Arguments to label the graph and give each axis an appropriate label. Once you have produced your Plots answer the following questions

regular_section_xaxis <- regular_section$Score
regular_section_yaxis <- regular_section$Count
sports_section_xaxis <- sports_section$Score
sports_section_yaxis <- sports_section$Count
sports_title <- "# of Students who received each Score in the Sports Section"
regular_title <- "# of Studend who received each Score in the Regular Section"
x_label <- 'Score'
y_label <- '# of Students who received each score'

par(mfrow=c(1,2))
plot(sports_section_xaxis,sports_section_yaxis,xlab=x_label, ylabel=y_label, type ='p', main = sports_title)
plot(regular_section_xaxis, regular_section_yaxis, xlab=x_label,ylab=y_label, type='p', main = regular_title)
ggplot(scores,aes(y=Count,x=Score, color=Section, size=Count)) + geom_point()

## 4-1 Comparing and contrasting the point distributions between the two section, looking at both tendency and consistency: Can you say that one section tended to score more points than the other? Justify and explain your answer.
sports_students <- sum(sports_section$Count)
regular_students <- sum(regular_section$Count)
sports_total_scores <- sum(sports_section$Count * sports_section$Score)
regular_total_scores <- sum(regular_section$Count * regular_section$Score)
regular_mean <- regular_total_scores / regular_students
sports_mean <- sports_total_scores / sports_students
regular_median <- median(regular_section$Score)
sports_median <- median(sports_section$Score)
regular_var <- var(regular_section$Score)
sports_var <- var(sports_section$Score)

## 4-1 cont The regular section had a higher mean than the sports section (335 and 307 respectively) and has more students scoring above 300 compared to the sports section.

## 4-2 There is not a section where every student scores higher than the other section. However, the regular section has a higher mean, median and a smaller variance which implies the regular section is more likely to score higher.

## 4-3 A variable that could also explain the distribution differences is whether one section is taught online and the other in-person. An online class may have less access to help than a in-person course.



