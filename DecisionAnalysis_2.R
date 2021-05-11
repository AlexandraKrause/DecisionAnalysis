#tasks####

#select: carat and price
#filter: only where carat is > 0.5
#rename: rename price as cost
#mutate: create a variable with expensive, 
#if greater than mean of cost and cheap otherwise

#group_by: split into cheap and expensive
#summarize: give some summary statistics of your choice


library(tidyverse)
library(readr)
library(dplyr)
library(tidyr) 
library(ggplot2)

str(diamonds)
view(diamonds)

head(diamonds,n=4)
names(diamonds)
str(diamonds)
#select: carat and price####
select(diamonds,
       -carat,
       -price)
#filter: only where carat is > 0.5####
filter(diamonds, 
       carat >0.05)
#rename: rename price as cost####
diamonds<-rename(diamonds, 
       cost = price)

#mutate: create a variable with expensive
#if greater than mean of cost and cheap otherwise####

#cost_mean<-mean(diamonds$cost, trim=0, na.rm=FALSE)
#output would be: 3932.8
#max(diamonds$cost)
#output:18823
#one value seems to be very high so that the mean is very high as well. i use trim

cost_mean<-mean(diamonds$cost, trim=1, na.rm=FALSE)
cost_mean
diamonds<-diamonds %>%
mutate(diamonds, 
       commute = 
         ifelse(cost > cost_mean, 
                "expensive", "cheap"))
head(diamonds,n=4)
view(diamonds)
#group_by: split into cheap and expensive####
diamonds %>%
group_by(commute) %>%
 
view(diamonds)
#summarize: give some summary statistics of your choice####
#mean cost, max carat
diamonds %>% 
summarize(mean(cost), 
            max(carat))



