library(here)
library(magrittr)

Param_recov = function(data,
                       input_params,
                       algorithm,
                       xtol_rel,
                       maxeval,
                       x0,
                       lb,
                       ub,
                       temperature,
                       tau,
                       model){
  
  # data = data.table::fread(file.path(here::here(), 'data', '09RI1ZH_exp_data.tsv'),
  #                          sep = '\t', na.strings = 'n/a')
  # model = 'uncertainty_seplr'
  # input_params = c(0.1, 0.5, 0.7, NA)
  # tau = 0.2
  # temperature = 7
  
  # Source self-written functions
  source(file.path(here::here(), 'code', 'simulation', 'Simulate.R'))
  source(file.path(here::here(), 'code', 'model_fitting', 'Fit_models_new.R'))
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
                 temperature = temperature,
                 tau = tau,
                 model)
  
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
  
  # Delete NA from parameter vectors
  x0 = x0[!is.na(x0)]
  lb = lb[!is.na(lb)]
  ub = ub[!is.na(ub)]

  # Fit model to simulated data
  recov = Fit_models_new(data = sim,
                         algorithm = algorithm,
                         xtol_rel = xtol_rel,
                         maxeval = maxeval,
                         x0 = x0,
                         lb = lb,
                         ub = ub,
                         param_recov = TRUE,
                         recov_model = model)
    
  # Get fitting result
  res = recov$fitting_out %>%
    .[variable != 'LRs',]
  # Create data frame filled with values of input parameters that were used for simulation
  add = res[variable == 'x0']
  add$variable = 'input_params'
  add$value = input_params[!is.na(input_params)]
  # Fuse input_params to results
  res = rbind(res, add)
  
  # Return results table
  return(res)
  
}