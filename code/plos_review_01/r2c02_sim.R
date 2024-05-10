library(here)
library(magrittr)

r2c02_sim = function(data,
                     input_params,
                     tau,
                     model,
                     beta_weights){
  
  # data = data.table::fread(file.path(here::here(), 'data', '09RI1ZH_exp_data.tsv'),
  #                          sep = '\t', na.strings = 'n/a')
  # model = 'uncertainty_seplr'
  # input_params = c(0.1, 0.5, 0.7, NA)
  # tau = 0.2
  # beta_weights = c(1, -0.5, 0.5, -0.01, 0.01)
  
  # Source self-written functions
  source(file.path(here::here(), 'code', 'simulation', 'Simulate.R'))
  source(file.path(here::here(), 'code', 'utils', 'Add_comp.R'))
  
  # Simulate behavior given model and parameters
  # Examples:
  #   model = 'rw', input_params = c(0.1, NA, NA, NA) => RW model => alpha = 0.1
  #   model = 'uncertainty', input_params = c(0.1, 0.5, NA, NA) => Uncertainty model => alpha = 0.1, pi = 0.5
  #   model = 'seplr', input_params = c(0.2, 0.4, NA, NA) => SEPLR model => alpha_pos = 0.2, alpha_neg = 0.4
  #   model = 'uncertainty_seplr', input_params = c(0.2, 0.4, 0.9, NA) => Uncertainty+SEPLR model => alpha_pos = 0.2, alpha_neg = 0.4, pi = 0.9
  #   model = 'surprise', input_params = c(0.1, 0.5, 0.7, NA) => Surprise model => l = 0.1, u = 0.5, s = 0.7 
  #   model = 'uncertainty_surprise', input_params = c(0.1, 0.5, 0.7, 0.5) => Uncertainty+Surprise model => l = 0.1, u = 0.5, s = 0.7, pi = 0.5
  sim = Simulate(data = data,
                 x = input_params,
                 tau = tau, 
                 model = model,
                 beta_weights = beta_weights)
  
  # Replace participants choices with choices of model
  # Choices
  sim$chosen_bandit = sim$model_choice_option
  # Chosen side ('left' vs 'right')
  sim$model_choice_side_rl = rep('left', length(sim$model_choice_side))
  sim$model_choice_side_rl[sim$model_choice_side == 2] = 'right'
  sim$choice = sim$model_choice_side_rl
  # Outcomes (based on choices)
  sim$outcome = sim$model_outcome
  
  # Get correct choices
  # Find correct choice
  sim$correct = c('left', 'right')[(sim$option_right > sim$option_left)*1+1]
  # See if choice was correct
  sim$correct_choice = sim$correct == sim$choice
  # Get correct choices for forced choices
  sim$correct_choice[sim$forced_left == 1] = sim$choice[sim$forced_left == 1] == 'left'
  sim$correct_choice[sim$forced_right == 1] = sim$choice[sim$forced_right == 1] == 'right'
  # Get bandit comparison for each trial of simulation
  sim = Add_comp(sim)
  # Delete 'v' in middle of comparison
  sim = sim %>%
    .[, bandit := paste(substr(comp, 1, 1), substr(comp, 3,3), sep = '')]
  
  # Return results table
  return(sim)
  
}