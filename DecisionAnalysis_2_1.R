#Section1####

#Section2----

library(tidyverse)
library(readr)
library(dplyr)
library(tidyr) 
library(ggplot2)

urlfile = "https://raw.githubusercontent.com/CWWhitney/teaching_R/master/participants_data.csv"
?read.csv
participants_data <- read_csv(url(urlfile))
#read.csv("participants_data.csv")
names(participants_data)
str(participants_data)
participants_data
head(participants_data, 
     n = 4)
view(participants_data)

# Change the variable to gender
participants_data$gender


# Change the selection to batch and age
select(participants_data, 
       batch,
       age)

# Change the selection 
# without batch and age
select(participants_data,
       -academic_parents,
       -working_hours_per_day)
#use filter function
# Change the selection to 
# those who work more than 5 hours a day
filter(participants_data, 
       working_hours_per_day >5)
#Create a subset of the data with multiple options in the filter function:
# Change the filter to those who 
# work more than 5 hours a day and 
# names are longer than three letters
filter(participants_data, 
       working_hours_per_day >5 & 
         letters_in_first_name >3)
# Rename the variable km_home_to_office as commute
rename(participants_data, 
       commute = km_home_to_office)

# Mutate a new column named age_mean 
# that is a function of the age multiplied
# by the mean of all ages in the group
mutate(participants_data, 
       age_mean = age*
         mean(age))
# Mutate new column named response_speed 
# populated by 'slow' if it took you 
# more than a day to answer my email and 
# 'fast' for others
mutate(participants_data, 
       response_speed = 
         ifelse(days_to_email_response > 1, 
                "slow", "fast"))
# Create a summary of the participants_mutate data 
# with the mean number of siblings 
# and median years of study
summarize(participants_data,
          mean(number_of_siblings),
          median(years_of_study))
# Use the magrittr pipe to summarize 
# the mean days to email response, 
# median letters in first name, 
# and maximum years of study by gender
participants_data %>% 
#  group_by(research_continent) %>% 
  summarize(mean(days_to_email_response), 
            median(letters_in_first_name), 
            max(years_of_study))
# Use the magrittr pipe to create a new column 
# called commute, where those who travel 
# more than 10 km to get to the office 
# are called "commuter" and others are "local". 
# Summarize the mean days to email response, 
# median letters in first name, 
# and maximum years of study. 
participants_data %>% 
  mutate(commute = ifelse(
    km_home_to_office > 10, 
    "commuter", "local")) %>% 
 # group_by(response_speed) %>% 
  summarize(mean(days_to_email_response), 
            median(letters_in_first_name), 
            max(years_of_study))
# Split the data frame by batch, 
# fit a linear model formula 
# (days to email response as dependent(first)
# and working hours as independent (second)) 
# to each batch, compute the summary, 
# then extract the R^2.
participants_data %>%
  split(.$batch) %>% 
  map(~ 
        lm(days_to_email_response ~ 
             working_hours_per_day, 
           data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")



