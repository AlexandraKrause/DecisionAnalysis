library(readr)
library(decisionSupport)
library (DiagrammeR)
library(tidyverse)
library(ggplot2)
library(plyr)
library(dplyr)


####1.:get data####


input_table_show <-read.csv2("./input_table_show.csv", dec = ",")
names(input_table_show)
str(input_table_show)
test <- na.omit(input_table_show)
####2.:function####

#generate a model as a function
#with the decisionSupport functions vv()
#produce time series with variation from a pre-defined mean and 
#coefficient of variation,chance_event() to simulate whether events occur
#and discount() to discount values along a time series.
#discount: 

decision_function1 <- function(x, varnames){
  
  ####vvs####  
  # use vv() to add variability to the 
  # random draws of own business branch and of private insurance
  # over a 65 year simulation 
  # 40 years insurance, paying 12 month a year =480
  # 82 years alive after paying 480 euro/ month:insurance
  # 33 years, 12 month:396
  Own_business_branch <- vv(var_mean = Own_branch, 
                            var_CV = var_CV, 
                            n = 480)
  

  Private_insurance <- vv(var_mean = Private_insurance, 
                          var_CV = var_CV, 
                          n = 396)
  
  Private_insurance_inv <- vv(var_mean = Private_insurance_inv, 
                          var_CV = var_CV, 
                          n = 480)
  
  #### calculate ex-ante risks ####
  Husband_risk <-
    chance_event(Husband_risk, 1, 0, n = 1)
  
  Divorce_risk <-
    chance_event(Divorce_risk, 1, 0, n = 1)
  
  Man_Death_risk <-
    chance_event(Man_Death_risk, 1, 0, n = 1)
  
  Bancruptcy_risk<-
    chance_event(Man_Death_risk, 1, 0, n = 1)
  
  Late_transfer_risk_obstacle<-
    chance_event(Man_Death_risk, 1, 0, n = 1)
  
  Child_Elderly_risk_obstacle<-
    chance_event(Man_Death_risk, 1, 0, n = 1)
  
  
  ####add variability####
#calculate NpV with discount rate
#Net present value (NPV) is the difference between the present value 
#of cash inflows and the present value of cash outflows over a period of time
  
  ####start with own business branch####
  # adjust branches for probability that other branches before: child and elderly care
  # were not "taken"

  
  # use chance_event() 
  # assuming  0 Own_business_branch  at all in the event of no child care option
  Costs_for_child_care_adjusted_Own_business_branch <- chance_event(chance = Costs_for_child_care, 
                                                                    value_if = 0,
                                                                    value_if_not = Own_branch,
                                                                    n = 480)
  
  
  # use chance_event() 
  # assuming  0 Own_business_branch  at all in the event of no elderly care option
  Costs_for_elderly_care_adjusted_Own_business_branch <- chance_event(chance = Costs_for_elderly_care, 
                                                                      value_if = 0,
                                                                      value_if_not = Own_branch,
                                                                      n = 480)
  
  # calculate pension without own business branch
  profit_without_Own_business_branch <- Default_option3 + Default_option2
  profit_with_Own_business_branch <- Own_branch
  
  # calculate pension with own business branch
  #    profit_wit_Own_business_branch<- Agricultural_insurance
  
  
  # use 'discount' to calculate net present value 
  # 'discount_rate' is expressed in percent
  NPV_no_branch <- discount( profit_without_Own_business_branch, discount_rate = 5, calculate_NPV = TRUE)    
  NPV_branch <- discount(profit_with_Own_business_branch, discount_rate = 5, calculate_NPV = TRUE)
  
  # calculate the overall NPV of the decision (do - don't do)
  NPV_decision <- NPV_branch-NPV_no_branch
  
  return(list(NPV_no_branch =  NPV_no_branch,
              NPV_branch =  NPV_branch, 
              NPV_decision = NPV_decision))
  
  
  # calculate pension without private insurance
  profit_without_private_insurance <- Default_option3 + Default_option2
  
  # calculate pension with private insurance
  profit_with_private_insurance <- Private_insurance-Private_insurance_inv
  
  # use 'discount' to calculate net present value 
  # 'discount_rate' is expressed in percent
  NPV_no_pi <- discount(profit_without_private_insurance, discount_rate = 5, calculate_NPV = TRUE)
  NPV_pi <- discount(profit_with_private_insurance, discount_rate = 5, calculate_NPV = TRUE)
  
  # calculate the overall NPV of the decision (do - don't do)
  NPV_decision <- NPVpi-NPV_no_pi
  
  return(list(NPV_no_pi =  NPV_no_pi,
              NPV_pi =  NPV_pi, 
              NPV_decision = NPV_decision))
}



####perform a monte carlo simulation####

#Using the model function above
#use mcSimulation() function from decisionSupport.
#This function generates distributions of all variables in the input table
#as well as the specified model outputs
#by calculating random draws in our defined decision function.

#Unless the model function is very complex, 
#10,000 runs is a reasonable choice 

mcSimulation1_results <- decisionSupport::mcSimulation(
  estimate = decisionSupport::as.estimate(input_table_show),
  model_function = decision_function1,
  numberOfModelRuns = 10000,
  functionSyntax = "plainNames"
)

mcSimulation1_results

