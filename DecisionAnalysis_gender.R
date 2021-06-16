library(readr)
library(decisionSupport)
library (DiagrammeR)
library(tidyverse)
library(ggplot2)
library(plyr)
library(dplyr)


  
####first step:get data####

#input_table_gender <-read.delim("./input_table_gender.txt", header=T)
#names(input_table_gender)
example <- read.csv("./example_input_table.csv")

input_table_gender <-read.csv2("./input_table_gender.csv", dec = ",")
names(input_table_gender)
str(input_table_gender)

test <- na.omit(input_table_gender)
####function####

#First we generate a model as a function.
#We use the decisionSupport functions vv() to produce 
#time series with variation from a pre-defined mean and 
#coefficient of variation,chance_event() to simulate whether events 
#occur and discount() to discount values along a time series.
#discount: diskontierung: auf uhrsprungsjahr zur?ckrechnen

decision_function <- function(x, varnames){

####vvs####  
  # use vv() to add variability to the 
  # random draws of own business branch and of 
  #state insurance, private insurance, ETF
  # over a 65 year simulation 
  #  # over a 82 year simulation (estimated death)
  # 40 years insurance, paying 12 month a year =480
  # 82 years alive after paying 480 euro/ month:insurance (82-25-40=17)
  # ETF: 82 x 12= 984
  # 17 years, 12 month:204
  # Age of retirenment: 65
  # marriage age 25
  

  Own_business_branch <- vv(var_mean = Own_branch, 
               var_CV_40 = var_CV_40, 
               n = 396)
  
  
  Off-Farm_job <- vv(var_mean = Off-Farm_job, 
                     var_CV_40 = var_CV_40, 
                     n = 396)
  
  Family_money <- vv(var_mean = Family_money, 
                    var_CV_40 = var_CV_40, 
                    n = 396)
  
  
  Farm_job_payed <- vv(var_mean = Farm_job_payed, 
                    var_CV_40 = var_CV_40, 
                    n = 396)
  
  ETF <- vv(var_mean = ETF, 
               var_CV_82 = var_CV_82, 
               n = 984)    
  
  Private_insurance <- vv(var_mean = Private_insurance, 
               var_CV_40 = var_CV_40, 
               n = 396)

  Private_insurance_inv <- vv(var_mean = Private_insurance_inv, 
                              var_CV_17 = var_CV_17, 
                              n = 204)
  
  State_insurance <- vv(var_mean = State_insurance, 
                           var_CV_17 = var_CV_17, 
                           n = 204)
  
  State_insurance_inv <- vv(var_mean = State_insurance_inv, 
                                  var_CV_17 = var_CV_17, 
                                  n = 204)

#### calculate ex-ante risks ####
    Husband_risk <-
    chance_event(Husband_risk, 1, 0, n = 1)

    Divorce_risk <-
    chance_event(Divorce_risk, 1, 0, n = 1)

    Man_Death_risk <-
    chance_event(Man_Death_risk, 1, 0, n = 1)
    
    Bancruptcy_risk <-
    chance_event(Man_Death_risk, 1, 0, n = 1)
    
    Late_transfer_risk_obstacle <-
      chance_event(Man_Death_risk, 1, 0, n = 1)
    
    Child_Elderly_risk_obstacle <-
      chance_event(Man_Death_risk, 1, 0, n = 1)
    

####add variability####
    
    
####start with own business branch####
# adjust branches for probability that other branches before were not "taken"
#65 jahre rente mal 12 Monate=780
    
# use chance_event() 
# assuming  0 Own_business_branch  at all in the event of no child care option
Costs_for_child_care_adjusted_Own_business_branch <- chance_event(chance = Costs_for_child_care, 
                                                                  value_if = 0,
                                                                  value_if_not = Own_branch,
                                                                  n = var_CV_40)
# use chance_event() 
# assuming  0 Own_business_branch  at all in the event of no elderly care option
Costs_for_elderly_care_adjusted_Own_business_branch <- chance_event(chance = Costs_for_elderly_care, 
                                                                    value_if = 0,
                                                                    value_if_not = Own_branch,
                                                                    n = var_CV_40)
    

    
# calculate pension without own business branch
profit_without_Own_business_branch <- Default_option3 + Default_option2
profit_with_Own_business_branch <- Own_branch
    
    
# use 'discount' to calculate net present value 
# 'discount_rate' is expressed in percent
NPV_no_branch <- discount( profit_without_Own_business_branch, discount_rate = 5, calculate_NPV = TRUE)    
NPV_branch <- discount(profit_with_Own_business_branch, discount_rate = 5, calculate_NPV = TRUE)
    
# calculate the overall NPV of the decision (do - don't do)
NPV_decision <- NPV_branch-NPV_no_branch
    
return(list(NPV_no_branch =  NPV_no_branch,
            NPV_branch =  NPV_branch, 
            NPV_decision = NPV_decision))

    #### then private insurance####
    
# calculate pension without private insurance
profit_without_private_insurance <- Default_option3 + Default_option2
    
# calculate pension with private insurance
profit_with_private_insurance <- Private_insurance
    
# use 'discount' to calculate net present value 
# 'discount_rate' is expressed in percent
NPV_no_pi <- discount(profit_without_private_insurance, discount_rate = 5, calculate_NPV = TRUE)
NPV_pi <- discount(profit_with_private_insurance, discount_rate = 5, calculate_NPV = TRUE)
    
# calculate the overall NPV of the decision (do - don't do)
NPV_decision <- NPV_pi-NPV_no_pi
    
return(list(NPV_no_pi =  NPV_no_pi,
            NPV_pi =  NPV_pi, 
            NPV_decision = NPV_decision))


#### then state insurance####
    
# calculate pension without state insurance
profit_without_State_insurance <- Default_option3 + Default_option2

# calculate pension with state insurance
profit_with_State_insurance <- State_insurance

# use 'discount' to calculate net present value 
# 'discount_rate' is expressed in percent
NPV_no_si <- discount(profit_without_State_insurance, discount_rate = 5, calculate_NPV = TRUE)
NPV_si <- discount(profit_with_State_insurance, discount_rate = 5, calculate_NPV = TRUE)

# calculate the overall NPV of the decision (do - don't do)
NPV_decision <- NPV_si-NPV_no_si

return(list(NPV_no_si =  NPV_no_si,
            NPV_si =  NPV_si, 
            NPV_decision = NPV_decision))


#### then ETF ####

# calculate pension without ETF
profit_without_ETF <- Default_option3 + Default_option2

# calculate pension with ETF
profit_with_ETF <- ETF

# use 'discount' to calculate net present value 
# 'discount_rate' is expressed in percent
NPV_no_ETF <- discount(profit_without_ETF, discount_rate = 5, calculate_NPV = TRUE)
NPV_ETF <- discount(profit_with_ETF, discount_rate = 5, calculate_NPV = TRUE)

# calculate the overall NPV of the decision (do - don't do)
NPV_decision <- NPV_ETF-NPV_no_ETFv

return(list(NPV_no_ETF =  NPV_no_ETF,
            NPV_ETF =  NPV_ETF, 
            NPV_decision = NPV_decision))
}

####Model branches####
# Estimate the pension without plan
#pension <- Husbands_or_family_money1 * Agriculatural_insurance1
#no cost

      
    # Benefits from plan ####
    # The following allows considering that intervention strips may
    # restrict access to the reservoir for livestock.
    
#    if (intervention_strips)
#      TLU_intervention <-
#      TLU * (1 + change_TLU_intervention_perc / 100)
#    else
#      TLU_intervention <- TLU
    
#    if (decision_intervention_strips){
#      livestock_benefits <- TLU_intervention * TLU_profit
#      total_benefits <- crop_production + livestock_benefits
#      net_benefits <- total_benefits - intervention_cost
#      result_interv <- net_benefits}
    
#    
#    if (!decision_intervention_strips){
#      livestock_benefits <- TLU_no_intervention * TLU_profit
#      total_benefits <- livestock_benefits
#      net_benefits <- total_benefits - intervention_cost
#      result_n_interv <- net_benefits}
    
#} #close intervention loop bracket

#NPV_interv <-
#discount(result_interv, discount_rate, calculate_NPV = TRUE)

#NPV_n_interv <-
#  discount(result_n_interv, discount_rate, calculate_NPV = TRUE)

    
    
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
  model_function = decision_function,
  numberOfModelRuns = 200,
  functionSyntax = "plainNames"
)

mcSimulation_results

#Run the Monte Carlo simulation using the model function
#chile_mc_simulation <- mcSimulation(estimate = as.estimate(input_table_gender),
#                                    model_function = input_table_gender,
#                                    numberOfModelRuns = 800,
#                                   functionSyntax = "plainNames")

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

#Here we show the results of a Monte Carlo simulation 
#(200 model runs) for 
#estimating the comparative profits with and without hail nets.

#Here we show the results of a Monte Carlo simulation (200 model runs) for
#estimating the comparative profits with and without hail nets.
plot_distributions(mcSimulation_object = mcSimulation_results, 
                   vars = c("NPV_no_pi", "NPV_npi","NPV_no_pi", "NPV_no_branch", "NPV_branch"),
                   method = 'smooth_simple_overlay', 
                   base_size = 7)

#decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
#                                    vars = c("Interv_NPV", "NO_Interv_NPV"),
#                                    method = 'smooth_simple_overlay', 
#                                    base_size = 7)

# boxplots

#We can use the same function to show the distributions of the
#‘do’ Interv_NPV and ‘do not do’ NO_Interv_NPV decision scenarios
#as boxplots. This can be useful when comparing multiple outputs
#by illustrating the spread of the data resulting from the 
#decision model. Boxplots show the median (central line), 
#the 25th and 75th percentiles (sides of boxes) and any outliers 
#(light circles outside of boxes).

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_no_pi",
                                             "NPV_npi",
                                             "NPV_no_pi", 
                                             "NPV_no_branch",
                                             "NPV_branch"),
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
