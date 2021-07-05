library(readr)
library(decisionSupport)
library (DiagrammeR)
library(tidyverse)
library(ggplot2)
library(plyr)
library(dplyr)



#install.packages (pls_result)


#thinking about cost problem:
#nicht vv funktion für einzelne jahre, but einen profit für lower boundary und einen für upper. diesen variieren über vv frü anzahl der jahre variieren lassen
# diese diskontieren und dann zeitreihe von diskontierten werten bekommen


#kosten prolongieren bis renetneintritt, dann diskontieren für bestimmten zeitpunkt: alter 82 zb
# die indikatoren prolongieren zb 40 jahre 


#durchschnittliche rentenkosten prolongieren: was rente pro jahr kostet wenn mit 25
#einzahlen.the pension we get minus this mean prolongierte rentenkosten aus den 40 jahren zuvor
#4 zahlen: revenue, ertrag den ich aus rente bekomme. und einen kostenwert, der vorher prolongiert werden muss.
#prolongierter lower und upper boundary. abziehen, damit 1 profit. von vv überletzte 17 jahre prolongieren







#notes. job on farm no child and elderly care? i mean, money from family for retirement is s.th. else.
#note!!!!!pls with post hoc might not make sense for us!############################################

####first step:get data####

#input_table_gender <-read.csv2("./input_table_gender.csv", dec = ",")
#input_table_gender <-read.csv2("./input_table_gender_4th.csv", dec = ",")
input_table_gender <-read.csv2("./input_table_gender_final.csv", dec = ",")


input_table_gender <- input_table_gender %>% 
  mutate(Description = as.character(Description),
         label = as.character(label),
         variable = as.character(variable),
         distribution = as.character(distribution),
         lower = as.numeric(lower),
         median = as.numeric(median),
         upper = as.numeric(upper))

str(input_table_gender)

####function####


# Way 1 = common and own branch
# Way 2 = common and Job away from farm 
# Way 3 = common and Job away from farm + 50 Euro mehr insurance + 20 Euro paying more
# Way 4 = common and family money  
# Way 5 = common and Payment of wife (on farm job)
# Way 6 = common and Own branch + all other thing (ETF,Private insurance and Mix)

Way <- 1


decision_function <- function(x, varnames){
  
  Default_option <- vv(var_mean = Default_option,
                         var_CV = var_cv_40, 
                         n = 480 )

  #Own_business_brach:
  
  Own_branch <- vv(var_mean = Own_branch, 
                            var_CV = var_cv_40, 
                            n = 480)
  
  Off_Farm_job <- vv(var_mean = Off_Farm_job, 
                     var_CV = var_cv_40, 
                     n = 480)
  
  
  Costs_for_child_care <- vv(var_mean = Costs_for_child_care, 
                             var_CV = var_cv_6, 
                             n = 72)
  
  Costs_for_elderly_care <- vv(var_mean = Costs_for_elderly_care, 
                               var_CV = var_cv_10, 
                               n = 120)
  
  State_insurance <- vv(var_mean = State_insurance, 
                        var_CV = var_cv_17, 
                        n = 184)
  
  State_insurance_inv <- vv(var_mean = State_insurance_inv, 
                              var_CV = var_cv_40, 
                              n = 480)
  
  Family_money <- vv(var_mean = Family_money, 
                     var_CV = var_cv_40, 
                     n = 480)
  
  Off_Farm_job<- vv(var_mean = Off_Farm_job, 
                       var_CV = var_cv_40, 
                       n = 480)
  
  
  Agri_insurance <- vv(var_mean = Agri_insurance, 
                       var_CV = var_cv_17, 
                       n = 184)
  
  Agri_insurance_inv <-  vv(var_mean = Agri_insurance_inv, 
                              var_CV = var_cv_40, 
                              n = 480)
  
  
  ETF <- vv(var_mean = ETF, 
            var_CV = var_cv_17, 
            n = 184)
  
  ETF_inv <- vv(var_mean = ETF_inv, 
                  var_CV = var_cv_40, 
                  n = 480)
  
  Mix <- vv(var_mean = Mix, 
            var_CV = var_cv_17, 
            n = 17)
  
  Mix_inv <- vv(var_mean = Mix_inv, 
                  var_CV = var_cv_40, 
                  n = 480)
  
  #### calculate ex-ante risks ####
  Husband_risk <-
    chance_event(Husband_risk, 1, 0, n = 1)
  
  Divorce_risk <-
    chance_event(Divorce_risk, 1, 0, n = 1)
  
  Man_Death_risk <-
    chance_event(Man_Death_risk, 1, 0, n = 1)
  
  Bancruptcy_risk <-
    chance_event(Man_Death_risk, 1, 0, n = 1)
  
  Child_Elderly_risk_obstacle <-
    chance_event(Man_Death_risk, 1, 0, n = 1)
  
  
  # Way 1 = common and own branch
  
  #man death auch risk?

  if(Way == 1){
    
    profit_Default <- ((Agri_insurance - Agri_insurance_inv) + Default_option)* (1-Man_Death_risk * Divorce_risk * Bancruptcy_risk)
    
    profit_with_Own_business_branch <- (Own_branch2 + Own_branch3 + Agri_insurance - Agri_insurance_inv - (Costs_for_child_care - Costs_for_elderly_care)) * (1- Husband_risk  * Bancruptcy_risk * Divorce_risk)
    
    
    NPV_no_branch <- discount(profit_Default,
                              discount_rate = 5, calculate_NPV = TRUE)  
    
    NPV_branch <- discount(profit_with_Own_business_branch,
                           discount_rate = 5, calculate_NPV = TRUE)
    
    NPV_decision <- NPV_branch - NPV_no_branch
    
    return(list(NPV_no_branch =  NPV_no_branch,
                NPV_branch =  NPV_branch, 
                NPV_decision = NPV_decision,
                Cashflow_decision_gender =  profit_with_Own_business_branch  - profit_Default))
  }

  
  if(Way == 2){
    
    profit_Default <- ((Agri_insurance - Agri_insurance_inv) + Default_option)* (1-Man_Death_risk * Divorce_risk * Bancruptcy_risk)
    
    profit_with_Own_business_branch <- (Own_branch + State_insurance - State_insurance_inv - ((Costs_for_child_care - Costs_for_elderly_care) * (1-Child_Elderly_risk_obstacle))) * (1- Husband_risk  * Bancruptcy_risk * Divorce_risk)
    
    
    NPV_no_branch <- discount(profit_Default,
                              discount_rate = 5, calculate_NPV = TRUE)  
    
    NPV_branch <- discount(profit_with_Own_business_branch,
                           discount_rate = 5, calculate_NPV = TRUE)
    
    NPV_decision <- NPV_branch - NPV_no_branch
    
    return(list(NPV_no_branch =  NPV_no_branch,
                NPV_branch =  NPV_branch, 
                NPV_decision = NPV_decision,
                Cashflow_decision_gender =  profit_with_Own_business_branch  - profit_Default))
  }
  
  # Way 2 = common and Job away from farm 
  #child care risk zusätzlich zu den child care kosten..?

#  if(Way == 2){
    
#    profit_Default <- ((Agri_insurance - Agri_insurance_inv) + Default_option)* (1-Man_Death_risk * Divorce_risk * Bancruptcy_risk)
#    
#    profit_with_Job_away_of_farm <- ((Off_Farm_job + State_insurance - State_insurance_inv - ((Costs_for_child_care + Costs_for_elderly_care) * (1- Child_Elderly_risk_obstacle)))* 1- Husband_risk)
#    NPV_no_branch <- discount(profit_Default,
#                              discount_rate = 5, calculate_NPV = TRUE)  
#    
#    NPV_branch <- discount(profit_with_Job_away_of_farm,
#                           discount_rate = 5, calculate_NPV = TRUE)
#    
#    NPV_decision <- NPV_branch - NPV_no_branch
#    
 #   return(list(NPV_no_branch =  NPV_no_branch,
 #               NPV_branch =  NPV_branch, 
 #               NPV_decision = NPV_decision,
 #               Cashflow_decision_gender =  profit_with_Job_away_of_farm  - profit_Default))
 # }
  
  # Way 3 = common and Job away from farm + 50 Euro mehr insurance + 20 Euro paying more

  if(Way == 3){
    
    profit_Default <- ((Agri_insurance - Agri_insurance_inv) + Default_option)* (1-Man_Death_risk * Divorce_risk * Bancruptcy_risk)
    
    profit_with_Job_away_of_farm <- ((Off_Farm_job + (State_insurance + 50) - (State_insurance_inv + 20) - ((Costs_for_child_care + Costs_for_elderly_care) * 1- Child_Elderly_risk_obstacle)) * 1- Husband_risk)
    
    NPV_no_branch <- discount(profit_Default,
                              discount_rate = 5, calculate_NPV = TRUE)  
    
    NPV_branch <- discount(profit_with_Job_away_of_farm,
                           discount_rate = 5, calculate_NPV = TRUE)
    
    NPV_decision <- NPV_branch - NPV_no_branch
    
    return(list(NPV_no_branch =  NPV_no_branch,
                NPV_branch =  NPV_branch, 
                NPV_decision = NPV_decision,
                Cashflow_decision_gender =  profit_with_Job_away_of_farm  - profit_Default))
  }
  
  # Way 4 = common and family money  
  
  if(Way == 4){
    
    profit_Default <- ((Agri_insurance - Agri_insurance_inv) + Default_option)* (1-Man_Death_risk * Divorce_risk * Bancruptcy_risk)
    
    profit_with_Family_money <- (Family_money + Agri_insurance - Agri_insurance_inv)  * (1-Bancruptcy_risk * Man_Death_risk * Divorce_risk * Husband_risk)
    
    NPV_no_branch <- discount(profit_Default,
                              discount_rate = 5, calculate_NPV = TRUE)  
    
    NPV_branch <- discount(profit_with_Family_money,
                           discount_rate = 5, calculate_NPV = TRUE)
    
    NPV_decision <- NPV_branch - NPV_no_branch
    
    return(list(NPV_no_branch =  NPV_no_branch,
                NPV_branch =  NPV_branch, 
                NPV_decision = NPV_decision,
                Cashflow_decision_gender =  profit_with_Family_money  - profit_Default))
  }
  


# Way 5 = common and Payment of wife (on farm job)
  
if(Way == 5){
  
  profit_Default <- ((Agri_insurance - Agri_insurance_inv) + Default_option)* (1-Man_Death_risk * Divorce_risk * Bancruptcy_risk)
  
  profit_with_On_Farm_Job <- ((Farm_job_payed + State_insurance - State_insurance_inv) * 1- Husband_risk)
  
  NPV_no_branch <- discount(profit_Default,
                            discount_rate = 5, calculate_NPV = TRUE)  
  
  NPV_branch <- discount(profit_with_On_Farm_Job,
                         discount_rate = 5, calculate_NPV = TRUE)
  
  NPV_decision <- NPV_branch - NPV_no_branch
  
  return(list(NPV_no_branch =  NPV_no_branch,
              NPV_branch =  NPV_branch, 
              NPV_decision = NPV_decision,
              Cashflow_decision_gender =  profit_with_On_Farm_Job  - profit_Default))
  
  
  plot_cashflow(mcSimulation_object_way1 = predictionProfit1,
                cashflow_var_name = "Cashflow_decision_gender",
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
}
}  

mcSimulation_results_way1 <- decisionSupport::mcSimulation(
  estimate = decisionSupport::as.estimate(input_table_gender),
  model_function = decision_function,
  numberOfModelRuns = 200,
  functionSyntax = "plainNames"
)

mcSimulation_results_way2 <- decisionSupport::mcSimulation(
  estimate = decisionSupport::as.estimate(input_table_gender),
  model_function = decision_function,
  numberOfModelRuns = 200,
  functionSyntax = "plainNames"
)

mcSimulation_results_way3 <- decisionSupport::mcSimulation(
  estimate = decisionSupport::as.estimate(input_table_gender),
  model_function = decision_function,
  numberOfModelRuns = 200,
  functionSyntax = "plainNames"
)

mcSimulation_results_way4 <- decisionSupport::mcSimulation(
  estimate = decisionSupport::as.estimate(input_table_gender),
  model_function = decision_function,
  numberOfModelRuns = 200,
  functionSyntax = "plainNames"
)

mcSimulation_results_way5 <- decisionSupport::mcSimulation(
  estimate = decisionSupport::as.estimate(input_table_gender),
  model_function = decision_function,
  numberOfModelRuns = 200,
  functionSyntax = "plainNames"
)

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

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results_way1, 
                                    vars = c("NPV_no_branch", "NPV_branch"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)
mcSimulation_results_way1

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results_way2, 
                                    vars = c("NPV_no_branch", "NPV_branch"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results_way3, 
                                    vars = c("NPV_no_branch", "NPV_branch"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results_way4, 
                                    vars = c("NPV_no_branch", "NPV_branch"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)
#############################################################################################


# boxplots

#We can use the same function to show the distributions of the
#‘do’ Interv_NPV and ‘do not do’ NO_Interv_NPV decision scenarios
#as boxplots. This can be useful when comparing multiple outputs
#by illustrating the spread of the data resulting from the 
#decision model. Boxplots show the median (central line), 
#the 25th and 75th percentiles (sides of boxes) and any outliers 
#(light circles outside of boxes).

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results_way1, 
                                    vars = c("NPV_no_branch",
                                             "NPV_branch"),
                                    method = 'boxplot')
#distribution

#We can use the same function for the value of the decision 
#(difference in NPV between do and do not do). 
#This can be quite helpful for us since it shows us the outcome 
#distribution of the decision itself.
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results_way1, 
                                    vars = c("NPV_no_branch",
                                    "NPV_branch"),
                                    method = 'boxplot_density')

####Cashflow analysis####

#Here we plot the distribution of annual cashflow over the 
#entire simulated period for the intervention (n_years). 
#For this we use the plot_cashflow() function which uses the 
#specified cashflow outputs from the mcSimulation() function 
#(in our case Cashflow_decision_do) to show cashflow over time.

Cashflow <- plot_cashflow(mcSimulation_object = mcSimulation_results_way1, cashflow_var_name = "Cashflow_decision_gender")


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


pls_result <- plsr.mcSimulation(object = mcSimulation_results_way1,
                                resultName = names(mcSimulation_results_way1$y)[3], ncomp = 1)
#We run the plot_pls() on the results from plsr.mcSimulation() with a number of standard settings.
#The length of the bars is equal to VIP with a vertical line at ‘1’
#on the x-axis indicating a standard cut-off for VIP used for variable selection. 
#The overall plot only shows those variables with a VIP > 0.8, which is the 
#common threshold for variable selection. The colors of the bars represent the 
#positive or negative coefficient of the given input variable with the output variable.
#Here we import the input table again to replace the labels
# for the variables on the y-axis. The input table can 
#include a label and variable column. The standard 
#(from the variable column) are usually computer readable and 
#not very nice for a plot. The plot_pls() function uses the text
#in the label column as replacement for the default text in the 
#variable column.
plot_pls(pls_result, input_table = "./input_table_gender_final.csv", threshold = 0)


# We calculate Value of Information (VoI) analysis with the Expected Value of Perfect Information (EVPI). 
#As we learned in Lecture 8 on forecasts, EVPI measures 
# the expected opportunity loss that is incurred when the decision-maker 
# does not have perfect information about a particular variable. 
# EVPI is determined by examining the influence of that variable on the output value of a decision model.
mcSimulation_table <- data.frame(mcSimulation_results_way1$x, mcSimulation_results_way1$y[1:3])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_decision")
plot_evpi(evpi, decision_vars = "NPV_decision")


## in the compound figure, we are forced to use the wrong input table as an input, therefore we get bad results for some plots.
compound_figure(mcSimulation_object = mcSimulation_results_way1, 
                input_table = input_table_gender, plsrResults = pls_result, 
                EVPIresults = evpi, decision_var_name = "NPV_decision", 
                cashflow_var_name = "Cashflow_decision_gender", 
                base_size = 7)

#way 2
plot_cashflow(mcSimulation_object = mcSimulation_results_way2, cashflow_var_name = "Cashflow_decision_gender")
pls_result <- plsr.mcSimulation(object = mcSimulation_results_way2,
                                resultName = names(mcSimulation_results_way2$y)[3], ncomp = 1)
plot_pls(pls_result, threshold = 0, input_table = input_table_gender)
mcSimulation_table <- data.frame(mcSimulation_results_way2$x, mcSimulation_results_way2$y[1:3])
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_decision")
plot_evpi(evpi, decision_vars = "NPV_decision")
compound_figure(mcSimulation_object = mcSimulation_results_way2, 
                input_table = input_table_gender, plsrResults = pls_result, 
                EVPIresults = evpi, decision_var_name = "NPV_decision", 
                cashflow_var_name = "Cashflow_decision_gender", 
                base_size = 7)

