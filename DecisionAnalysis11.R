library(readr)
library(decisionSupport)
library (DiagrammeR)
library(tidyverse)
library(ggplot2)
library(plyr)
library(dplyr)

####Lecture 11: Communicating Decision Support####

#Welcome to lecture 11 of Decision Analysis and Forecasting for
#Agricultural Development. Feel free to bring up any questions or concerns
#in the Slack or to Dr. Cory Whitney or the course tutor.
#Communicating results of Decision Analysis models

#The results of a Monte Carlo model are not always easy to interpret and to
#communicate about. One important step in communicating these often very large
#data sets is with good visualization. In previous lectures and seminars we 
#have covered the basics of plotting the distributions of model results for 
#comparisons between decision options. In this lecture we will build on what 
#we learned in the Decision Models lecture and the Model programming seminar to
#learn more about how to generate useful plots for communicating model results.

#In the Model programming seminar we generated a model called 
#example_mc_simulation. As a quick refresher change the plot of the results
#of that model from hist_simple_overlay to boxplot_density by using the method
#argument in the plot_distributions function.

plot_distributions(mcSimulation_object = example_mc_simulation,
                   vars = "final_result",
                   method = "boxplot_density",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")


####Plot many results together####

#We can use the compound_figure function to create a simple compound figure of
#model results and analyses of a binary decision (do or do not do). The figure
#includes the distribution of the expected outcome, the expected cashflow, as
#well as the variable importance and the value of information.
#Create the estimate object:

cost_benefit_table <- data.frame(label = c("Revenue", "Costs"),
                                 variable = c("revenue", "costs"),
                                 distribution = c("norm", "norm"),
                                 lower = c(100,  500),
                                 median = c(NA, NA),
                                 upper = c(10000, 5000))

# (a) Define the model function without name for the return value:

profit1 <- function() {
  Decision <- revenue - costs
  cashflow <- rnorm(rep(revenue, 20))
  return(list(Revenues = revenue,
              Costs = costs, 
              cashflow = cashflow, 
              Decision = Decision))
}

compound_figure(model = profit1, 
                input_table = cost_benefit_table, 
                decision_var_name = "Decision",
                cashflow_var_name = "cashflow",
                model_runs = 1e2, 
                distribution_method = 'smooth_simple_overlay')

####Other visualization options####

#Here we demonstrate a few more various graphical options
#to visualize uncertainty intervals of outcomes of Monte Carlo simulations.
#We create a data set of yield distributions of three different farming 
#practices and use the function mcmc_intervals() from the bayesplot library 
#to plot the data set (Gabry and Mahr 2021).
test <- data.frame("practice 1" = rnorm(n = 1000, mean = 8, sd = 1.5), 
                   "practice 2" = rnorm(n = 1000, mean = 7, sd = 1), 
                   "practice 3" = rnorm(n = 1000, mean = 5, sd = 0.5))


color_scheme_set("red")
mcmc_intervals(test,prob = 0.5,prob_outer = 0.9,point_est = "median")

#Do the same with the with mcmc_areas() function from 
#the bayesplot library (Gabry and Mahr 2021).
test <- data.frame("practice 1" = rnorm(n = 1000, mean = 8, sd = 1.5), 
                   "practice 2" = rnorm(n = 1000, mean = 7, sd = 1), 
                   "practice 3" = rnorm(n = 1000, mean = 5, sd = 0.5))

color_scheme_set("blue")

mcmc_areas(test,prob = 0.9,point_est = "median")

####Comparative density curves####

#We can also use geom_density()in ggplot2 
#to compare the spread of different distributions (Wickham et al. 2020):
test <- data.frame("practice 1" = rnorm(n = 1000, mean = 8, sd = 1.5), 
                   "practice 2" = rnorm(n = 1000, mean = 7, sd = 1), 
                   "practice 3" = rnorm(n = 1000, mean = 5, sd = 0.5))

stacked_test <- stack(test)

ggplot(stacked_test, 
       aes(x=values,group=ind,fill=ind )) +
  geom_density(colour=NA,alpha=.5) +
  ylab("Probability density") +
  xlab("Yield")

#####Comparative histogram####

#Use ggplot2 geom_histogram()function to show the histogram of the
#data in comparison:
test <- data.frame("practice 1" = rnorm(n = 1000, mean = 8, sd = 1.5), 
                   "practice 2" = rnorm(n = 1000, mean = 7, sd = 1), 
                   "practice 3" = rnorm(n = 1000, mean = 5, sd = 0.5))

stacked_test <- stack(test)

ggplot(stacked_test,aes(x=values))+ 
  geom_histogram(data=subset(stacked_test,ind =='practice.1'),
                 aes(fill = ind), alpha = 0.5, bins = 150) + 
  geom_histogram(data=subset(stacked_test,ind == 'practice.2'),
                 aes(fill = ind), alpha = 0.5, bins = 150) +
  geom_histogram(data=subset(stacked_test,ind == 'practice.3'),
                 aes(fill = ind), alpha = 0.5, bins = 150) 


####Plot cashflow####

#The plot_cashflow function from the decisionSupport package
#(Luedeling et al. 2021) creates a cashflow plot of the returned 
#list of related outputs from the mcSimulation function 
#using ggplot2 (Wickham et al. 2020). The function 

#### Plotting the cashflow:####

#### Create the estimate object (for multiple options):####

variable = c("revenue_option1", "costs_option1", "n_years", 
             "revenue_option2", "costs_option2")
distribution = c("norm", "norm", "const", "norm", "norm")
lower = c(10000,  5000, 10, 8000,  500)
upper = c(100000, 50000, 10, 80000,  30000)

costBenefitEstimate <- as.estimate(variable, distribution, lower, upper)

#### Define the model function without name for the return value:####

profit1 <- function(x) {
  
 cashflow_option1 <- vv(revenue_option1 - costs_option1,
                        n = n_years, var_CV = 100)
 cashflow_option2 <- vv(revenue_option2 - costs_option2,
                        n = n_years, var_CV = 100)
  
  return(list(Revenues_option1 = revenue_option1,
              Revenues_option2 = revenue_option2,
              Costs_option1 = costs_option1,
              Costs_option2 = costs_option2,
              Cashflow_option_one = cashflow_option1,
              Cashflow_option_two = cashflow_option2))
}

#### Perform the Monte Carlo simulation:####

predictionProfit1 <- mcSimulation(estimate = costBenefitEstimate,
                                  model_function = profit1,
                                  numberOfModelRuns = 10000,
                                  functionSyntax = "plainNames")


##### Plot the cashflow distribution over time####

plot_cashflow(mcSimulation_object = predictionProfit1,
              cashflow_var_name = "Cashflow_option_one",
              x_axis_name = "Years with intervention",
              y_axis_name = "Annual cashflow in USD",
              color_25_75 = "green4", color_5_95 = "green1",
              color_median = "red")

####Plot the cashflow####
#with panels to compare the cashflow distribution over time for
#multiple decision options:
variable = c("revenue_option1", "costs_option1", "n_years", 
             "revenue_option2", "costs_option2")
distribution = c("norm", "norm", "const", "norm", "norm")
lower = c(10000,  5000, 10, 8000,  500)
upper = c(100000, 50000, 10, 80000,  30000)

costBenefitEstimate <- as.estimate(variable, distribution, lower, upper)

#### Define the model function without name for the return value:####

profit1 <- function(x) {
  
  cashflow_option1 <- vv(revenue_option1 - costs_option1,
                         n = n_years, var_CV = 100)
  cashflow_option2 <- vv(revenue_option2 - costs_option2,
                         n = n_years, var_CV = 100)
  
  return(list(Revenues_option1 = revenue_option1,
              Revenues_option2 = revenue_option2,
              Costs_option1 = costs_option1,
              Costs_option2 = costs_option2,
              Cashflow_option_one = cashflow_option1,
              Cashflow_option_two = cashflow_option2))
}

#### Perform the Monte Carlo simulation:####

predictionProfit1 <- mcSimulation(estimate = costBenefitEstimate,
                                  model_function = profit1,
                                  numberOfModelRuns = 10000,
                                  functionSyntax = "plainNames")

plot_cashflow(mcSimulation_object = predictionProfit1, 
              cashflow_var_name = c("Cashflow_option_one",
                                    "Cashflow_option_two"),
              x_axis_name = "Years with intervention",
              y_axis_name = "Annual cashflow in USD",
              color_25_75 = "green4", color_5_95 = "green1",
              color_median = "red", 
              facet_labels = c("Option 1", "Option 2"))


####Bonus, More plotting options####
#Violin & box plot overlays#

#Here we use R’s built in OrchardSprays data
#to run the example from the tidyverse Violin plot examples (Wickham 2021).
ggplot(OrchardSprays, aes(y = decrease, x = treatment, fill = treatment))+
  geom_violin() +
  geom_boxplot(width = 0.1) +
  theme(legend.position = "none")

####Ridge line plot####

#A variation on the example from edav using the ggridges library (Wilke 2021).
ggplot(OrchardSprays, 
       aes(x=decrease,y = treatment,fill = treatment)) +
  geom_density_ridges_gradient(scale=2) + 
  theme_ridges() +
  theme(legend.position = "none")
#automatically defines quantiles (5 to 95% and 25 to 75%)
#as well as a value for the median.

#More examples on the rdrr.io CRAN website.

#To see more options for plotting high
#dimensional data visit the High Domensional Data vignette.