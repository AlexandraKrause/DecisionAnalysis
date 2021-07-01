library(readr)
library(decisionSupport)
library (DiagrammeR)
library(tidyverse)
library(ggplot2)
library(plyr)
library(dplyr)


#### Building the model ####


input_estimates <- data.frame(variable = c("Market_price_BigFarmer",
                                           "Market_price_SmallFarmer", 
                                           "Substrate_cost", "Peat_cost"),
                              lower = c(0.7, 0.9, 0.34, 0.4),
                              median = NA,
                              upper = c(0.9, 1.4, 0.34, 0.4),
                              distribution = c("posnorm", "posnorm", 
                                               "const","const"),
                              label = c("Price (EU/Salat)", "Price (EU/Salat)", 
                                        "Cost for food waste/Salat",
                                        "Cost for substrate/Salat"),
                              Description = c("Price in Euro per Salat",
                                              "Price in Euro per Salat",
                                              "Cost for food waste/Salat",
                                              "Cost for substrate/Salat"))

input_estimates <- input_estimates %>%
  mutate(variable = as.character(variable),
         distribution = as.character(distribution),
         label = as.character(label),
         Description = as.character(Description))


model_function <- function(){

  # Food waste 
  SmallFarmer <- Market_price_SmallFarmer - Substrate_cost


  # Peat
  BigFarmer <- Market_price_BigFarmer - Peat_cost


### Estimate the final results from the model ####
  final_result <-  SmallFarmer - BigFarmer

  return(list(SmallFarmer =  SmallFarmer,
              BigFarmer =  BigFarmer,
              final_result = final_result))
}



#### Run the Monte Carlo simulation using the model function ####
chile_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                    model_function = model_function,
                                    numberOfModelRuns = 800,
                                    functionSyntax = "plainNames")

chile_mc_simulation


decisionSupport::plot_distributions(mcSimulation_object = chile_mc_simulation,
                                    vars = c("SmallFarmer", "BigFarmer"),
                                    method = 'smooth_simple_overlay',
                                    base_size = 7)
