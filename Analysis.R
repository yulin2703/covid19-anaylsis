rm(list=ls()) # removing all variables stored previously
library(Hmisc) # import Hmisc library

data <- read.csv("C:/Users/yulin/covid19_R/COVID19_line_list_data.csv")
describe(data) # Hmisc command

# cleaning up death column
data$death_dummy <- as.integer(data$death != 0)
# death rate
sum(data$death_dummy) / nrow(data)

# AGE ANALYSIS
# Our claim: people who die are older.
dead = subset(data, death_dummy == 1)
alive = subset(data, death_dummy == 0)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)
# Is this statistically significant?
t.test(alive$age, dead$age, alternative="two.sided", conf.level = 0.99)
# If p-value < 0.05, we reject null hypothesis, but here the p-value ~ 0, so we reject the null hypothesis. 
# We conclude that it's statistically significant.

# GENDER ANALYSIS
# Our claim: gender has no effect.
men = subset(data, gender == "male")
women = subset(data, gender == "female")
mean(men$death_dummy, na.rm = TRUE) #8.5%!
mean(women$death_dummy, na.rm = TRUE) #3.7%
# Is this statistically significant?
t.test(men$death_dummy, women$death_dummy, alternative="two.sided", conf.level = 0.99)
# 99% confidence: Men have from 0.8% to 8.8% higher chance of dying.
# p-value = 0.002 < 0.05, so this is statistically significant.
