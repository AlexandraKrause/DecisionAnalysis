library(readr)
#install.packages("decisionSupport")
library(decisionSupport)
#install.packages("DiagrammeR")
library (DiagrammeR)
library(tidyverse)
library(ggplot2)
library(plyr)
library(dplyr)

####second step:get data####


#input_table_gender <-read.delim("./input_table_gender.txt", header=T)
#names(input_table_gender)

input_table_gender <-read.csv2("./input_table_gender.csv")
names(input_table_gender)

####perform a monte carlo simulation####

#Using the model function above,
#we can perform a Monte Carlo simulation with the mcSimulation() 
#function from decisionSupport. This function generates 
#distributions of all variables in the input table as well 
#as the specified model outputs (see return() function above)
#by calculating random draws in our defined example_decision_function().
#Make sure that all the variables in the input table are included
#in the model (erroneous variables listed there can cause issues
#with some of the post-hoc analyses).

#The numberOf ModelRuns argument is an integer indicating 
#the number of model runs for the Monte Carlo simulation. 
#Unless the model function is very complex, 
#10,000 runs is a reasonable choice 
#(for complex models,10,000 model runs can take a while, 
#so especially when the model is still under development, 
#it often makes sense to use a lower number).



mcSimulation_results <- decisionSupport::mcSimulation(
  estimate = decisionSupport::as.estimate(input_table_gender),
  model_function = input_table_gender,
  numberOfModelRuns = 200,
  functionSyntax = "plainNames"
)

# Run the Monte Carlo simulation using the model function
chile_mc_simulation <- mcSimulation(estimate = as.estimate(input_table_gender),
                                    model_function = input_table_gender,
                                    numberOfModelRuns = 800,
                                    functionSyntax = "plainNames")

####perform a monte carlo simulation####

#Using the model function above,
#we can perform a Monte Carlo simulation with the mcSimulation() 
#function from decisionSupport. This function generates 
#distributions of all variables in the input table as well 
#as the specified model outputs (see return() function above)
#by calculating random draws in our defined example_decision_function().
#Make sure that all the variables in the input table are included
#in the model (erroneous variables listed there can cause issues
#with some of the post-hoc analyses).

#The numberOf ModelRuns argument is an integer indicating 
#the number of model runs for the Monte Carlo simulation. 
#Unless the model function is very complex, 
#10,000 runs is a reasonable choice 
#(for complex models,10,000 model runs can take a while, 
#so especially when the model is still under development, 
#it often makes sense to use a lower number).


####model assessment####

#Plot Net Present Value (NPV) distributions

#We can use the plot_distributions() function to produce 
#one of the several plotting options for distribution outputs.
#This shows us an overlay of the full results of the 
#Monte Carlo model of the decision options, 
#i.e. the expected NPV if we choose to do the
#intervention Interv_NPV or not do the intervention NO_Interv_NPV.

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("Interv_NPV", "NO_Interv_NPV"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)

# boxplots

#We can use the same function to show the distributions of the
#‘do’ Interv_NPV and ‘do not do’ NO_Interv_NPV decision scenarios
#as boxplots. This can be useful when comparing multiple outputs
#by illustrating the spread of the data resulting from the 
#decision model. Boxplots show the median (central line), 
#the 25th and 75th percentiles (sides of boxes) and any outliers 
#(light circles outside of boxes).

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("Interv_NPV",
                                             "NO_Interv_NPV"),
                                    method = 'boxplot')

#distribution

#We can use the same function for the value of the decision 
#(difference in NPV between do and do not do). 
#This can be quite helpful for us since it shows us the outcome 
#distribution of the decision itself.
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = "NPV_decision_do",
                                    method = 'boxplot_density')
####Cashflow analysis####

#Here we plot the distribution of annual cashflow over the 
#entire simulated period for the intervention (n_years). 
#For this we use the plot_cashflow() function which uses the 
#specified cashflow outputs from the mcSimulation() function 
#(in our case Cashflow_decision_do) to show cashflow over time.

plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_do")

####Projection to Latent Structures (PLS) analysis####

#We apply a post-hoc analysis to the mcSimulation() outputs

#with plsr.mcSimulation() to determine the Variable 
#Importance in the Projection (VIP) score and coefficients of 
#a Projection to Latent Structures (PLS) regression model. 
#This function uses the outputs of the mcSimulation() selecting
#all the input variables from the decision analysis function 
#in the parameter object and then runs a PLS regression with an 
#outcome variable defined in the parameter resultName. 
#We use the code names(mcSimulation_results$y)[3] to select the
#outcome variable NPV_decision_do, which is the third element of
#the list y in our mcSimulation_results outputs
#(this must be a character element).

pls_result <- plsr.mcSimulation(object = mcSimulation_results,
                                resultName = names(mcSimulation_results$y)[3], ncomp = 1)
#We run the plot_pls() on the results from plsr.mcSimulation() with a number of standard settings. The length of the bars is equal to VIP with a vertical line at ‘1’ on the x-axis indicating a standard cut-off for VIP used for variable selection. The overall plot only shows those variables with a VIP > 0.8, which is the common threshold for variable selection. The colors of the bars represent the positive or negative coefficient of the given input variable with the output variable.
#Here we import the input table again to replace the labels
# for the variables on the y-axis. The input table can 
#include a label and variable column. The standard 
#(from the variable column) are usually computer readable and 
#not very nice for a plot. The plot_pls() function uses the text
#in the label column as replacement for the default text in the 
#variable column.
plot_pls(pls_result, input_table = input_table, threshold = 0)


####Value of Information (VoI) analysis####

#We calculate Value of Information (VoI) analysis with 
#the Expected Value of Perfect Information (EVPI). 
#As we learned in Lecture 8 on forecasts, EVPI measures the
#expected opportunity loss that is incurred when the 
#decision-maker does not have perfect information about a 
#particular variable. EVPI is determined by examining the 
#influence of that variable on the output value of a decision model.

#We use the function data.frame() to transform the x and y 
#outputs of the mcSimulation() function for EVPI calculation. 
#We use the multi_EVPI() function to calculate the EVPI for
#multiple independent variables. For the first_out_var argument 
#we choose intervention_mngmt_audit_cost
#from the input table since this is the first variable after 
#the NPV and cashflow model outputs, which we would like to
#exclude from the EVPI analysis.


#here we subset the outputs from the mcSimulation function (y) 
#by selecting the correct variables
#choose this carefully and be sure to run the multi_EVPI 
#only on the variables that the you want

mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[1:3])

evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "Interv_NPV")

#We use the function plot_evpi() on the results from multi_EVPI() to plot the Expected Value of Perfect Information (EVPI). Here we show the results with the standard settings. 
#The length of the bars is equal to EVPI.
plot_evpi(evpi, decision_vars = "NPV_decision_do")

#Finally, we can use the compound_figure() function to provide
#a single figure for a quick assessment. 
#The can be used to run the full decision assessment for 
#a simple binary decision (do or not do).
compound_figure(mcSimulation_object = mcSimulation_results, 
                input_table = input_table, plsrResults = pls_result, 
                EVPIresults = evpi, decision_var_name = "NPV_decision_do", 
                cashflow_var_name = "Cashflow_decision_do", 
                base_size = 7)
