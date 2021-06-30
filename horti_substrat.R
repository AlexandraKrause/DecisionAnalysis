library (DiagrammeR)


# Building the model


input_estimates <- data.frame(variable = c("Yield", "Market_price", "Substrate_Cost", "Peat_cost"),
                              lower = c(200, 0.5,0.32,0.45),
                              median = NA,
                              upper = c(350, 0.8,0.34,0.4),
                              distribution = c("posnorm", "posnorm", "const","const"),
                              label = c("Yield (g/Salat)", "Price (EU/gramm)", "Cost for food waste/Salat", "cost for substrate/Salat"),
                              Description = c("Yield in kg per Salat",
                                              "Price in Euro per Salat",
                                              "Cost for food waste/Salat", 
                                              "cost for substrate/Salat"))

input_estimates

model_function <- function(){
  
  # food waste
  decision1<- (Yield * Market_price) - Substrate_Cost
  


  
  # peat
  no_decision <- (Yield * Market_price)- Peat_Cost

  
  # Estimate the final results from the model
  final_result <-  decision1 - no_decision
  
  return(list(no_decision =  no_decision,
              decision1 =  decision1, 
              final_result = final_result, 
              risk))
}



# Run the Monte Carlo simulation using the model function
chile_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                    model_function = model_function,
                                    numberOfModelRuns = 800,
                                    functionSyntax = "plainNames")

chile_mc_simulation


decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results_way1, 
                                    vars = c("no_decision", "decision"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)



?plot_distributions
plot_distributions(mcSimulation_object = chile_mc_simulation,
                   vars = "final_result",
                   method = "hist_simple_overlay",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")
