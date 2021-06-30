library(readr)
library(decisionSupport)
library (DiagrammeR)
library(tidyverse)
library(ggplot2)
library(plyr)
library(dplyr)



####first step:get data####

#input_table_gender <-read.csv2("./input_table_gender.csv", dec = ",")
input_table_gender <-read.csv2("./input_table_gender_4th.csv", dec = ",")


input_table_gender <- input_table_gender %>% 
  mutate(Description = as.character(Description),
         label = as.character(label),
         variable = as.character(variable),
         distribution = as.character(distribution),
         lower = as.numeric(lower),
         median = as.numeric(median),
         upper = as.numeric(upper))

str(input_table_gender)
########

#Reminder about the make_variables function
make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}#Then call:
  make_variables(as.estimate(input_table_gender))

####function####


# Way 1 = common and own branch
# Way 2 = common and Job away from farm 
# Way 3 = common and Job away from farm + 50 Euro mehr insurance + 20 Euro mehr bezahlt
# Way 4 = common and Family money to buy pension options
# Way 5 = common and Payment of Wife for farm work
# Way 6 = common and Own branch + all other thing

#Way 5 and 6 are still missing

Way <- 3


decision_function <- function(x, varnames){
  
#  Default_option_2 <- vv(var_mean = Default_option_2,
#                         var_CV = var_cv_40, 
#                         n = 40 )
  
#  Default_option_3 <- vv(var_mean = Default_option_3,
#                         var_CV = var_cv_40, 
#                         n = 40)
  
#  Default_option_3_costs <- vv(var_mean = Default_option_3_costs, 
#                               var_CV = var_cv_40, 
#                               n = 40)
  
  
#  Own_business_branch <- vv(var_mean = Own_branch, 
#                            var_CV = var_cv_40, 
#                            n = 40)
  
#  Off_Farm_job <- vv(var_mean = Off_Farm_job, 
#                     var_CV = var_cv_40, 
#                     n = 40)
  
  
#  Costs_for_child_care <- vv(var_mean = Costs_for_child_care, 
#                             var_CV = var_cv_40, 
#                             n = 40)
  
#  Costs_for_elderly_care <- vv(var_mean = Costs_for_elderly_care, 
#                               var_CV = var_cv_40, 
#                               n = 40)
  
#  State_insurance <- vv(var_mean = State_insurance, 
#                        var_CV = var_cv_17, 
#                        n = 40)
  
#  State_insurance_costs <- vv(var_mean = State_insurance_inv, 
#                             var_CV = var_cv_40, 
#                              n = 40)
  
#  Family_money <- vv(var_mean = Family_money, 
#                     var_CV = var_cv_40, 
#                     n = 40)
  
#  Farm_job_payed <- vv(var_mean = Farm_job_payed, 
#                       var_CV = var_cv_40, 
#                       n = 40)
  
  
#  Agri_insurance <- vv(var_mean = Agri_insurance, 
#                       var_CV = var_cv_17, 
                       n = 40)
  
#  Agri_insurance_costs <-  vv(var_mean = Agri_insurance_inv, 
#                              var_CV = var_cv_40, 
#                              n = 40)
  
  
  ETF <- vv(var_mean = ETF, 
            var_CV = var_cv_17, 
            n = 40)
  
  ETF_costs <- vv(var_mean = ETF_costs, 
                  var_CV = var_cv_40, 
                  n = 40)
  
  Mix <- vv(var_mean = ETF, 
            var_CV = var_cv_17, 
            n = 40)
  
  Mix_costs <- vv(var_mean = Mix_costs, 
                  var_CV = var_cv_40, 
                  n = 40)
  
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
  
  
  # use chance_event() 
  # assuming  0 Own_business_branch  at all in the event of no child care option
  Costs_for_child_care_adjusted_Own_business_branch <- chance_event(chance = 
                                                                      Costs_for_child_care, 
                                                                    value_if = 0,
                                                                    value_if_not = Own_branch,
                                                                    n = var_cv_40)
  # use chance_event() 
  # assuming  0 Own_business_branch  at all in the event of no elderly care option
  Costs_for_elderly_care_adjusted_Own_business_branch <- chance_event(chance = 
                                                                        Costs_for_elderly_care, 
                                                                      value_if = 0,
                                                                      value_if_not = Own_branch,
                                                                      n = var_cv_40)
  
  
  if(Way == 1){
    
    profit_without_Own_business_branch <- (Default_option_3 - Default_option_3_costs) + Default_option_2
    
    profit_with_Own_business_branch <- (Own_business_branch + Agri_insurance - Agri_insurance_costs - Costs_for_child_care - Costs_for_elderly_care)
    
    
    NPV_no_branch <- discount(profit_without_Own_business_branch,
                              discount_rate = 5, calculate_NPV = TRUE)  
    
    NPV_branch <- discount(profit_with_Own_business_branch,
                           discount_rate = 5, calculate_NPV = TRUE)
    
    NPV_decision <- NPV_branch - NPV_no_branch
    
    return(list(NPV_no_branch =  NPV_no_branch,
                NPV_branch =  NPV_branch, 
                NPV_decision = NPV_decision))
  }
  
  if(Way == 2){
    
    profit_without_Job_away_of_farm <- (Default_option_3 - Default_option_3_costs) + Default_option_2
    
    profit_with_Job_away_of_farm <- (Off_Farm_job + State_insurance - State_insurance_costs - Costs_for_child_care - Costs_for_elderly_care)
    
    NPV_no_branch <- discount(profit_without_Job_away_of_farm,
                              discount_rate = 5, calculate_NPV = TRUE)  
    
    NPV_branch <- discount(profit_with_Job_away_of_farm,
                           discount_rate = 5, calculate_NPV = TRUE)
    
    NPV_decision <- NPV_branch - NPV_no_branch
    
    return(list(NPV_no_branch =  NPV_no_branch,
                NPV_branch =  NPV_branch, 
                NPV_decision = NPV_decision))
  }
  
  if(Way == 3){
    
    profit_without_Own_business_branch <- (Default_option_3 - Default_option_3_costs) + Default_option_2
    
    profit_with_Job_away_of_farm <- (Off_Farm_job + (State_insurance + 50) - (State_insurance_costs + 20) - Costs_for_child_care - Costs_for_elderly_care)
    
    NPV_no_branch <- discount(profit_without_Own_business_branch,
                              discount_rate = 5, calculate_NPV = TRUE)  
    
    NPV_branch <- discount(profit_with_Job_away_of_farm,
                           discount_rate = 5, calculate_NPV = TRUE)
    
    NPV_decision <- NPV_branch - NPV_no_branch
    
    return(list(NPV_no_branch =  NPV_no_branch,
                NPV_branch =  NPV_branch, 
                NPV_decision = NPV_decision))
  }
  
  if(Way == 4){
    
    profit_without_Own_business_branch <- (Default_option_3 - Default_option_3_costs) + Default_option_2
    
    profit_with_Family_money <- (Family_money + Agri_insurance - Agri_insurance_costs)
    
    NPV_no_branch <- discount(profit_without_Own_business_branch,
                              discount_rate = 5, calculate_NPV = TRUE)  
    
    NPV_branch <- discount(profit_with_Family_money,
                           discount_rate = 5, calculate_NPV = TRUE)
    
    NPV_decision <- NPV_branch - NPV_no_branch
    
    return(list(NPV_no_branch =  NPV_no_branch,
                NPV_branch =  NPV_branch, 
                NPV_decision = NPV_decision, 
                risk))
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
  numberOfModelRuns = 10000,
  functionSyntax = "plainNames"
)

mcSimulation_results_way4 <- decisionSupport::mcSimulation(
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

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results_way1, 
                                    vars = c("NPV_no_branch", "NPV_branch"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results_way4, 
                                    vars = c("NPV_no_branch", "NPV_branch"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)
