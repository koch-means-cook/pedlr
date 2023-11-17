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
  source(file.path(here::here(), 'code', 'model_fitting', 'Fit_models_new.R'))
  source(file.path(here::here(), 'code', 'utils', 'Add_comp.R'))
  source(file.path(here::here(), 'code', 'utils', 'Get_svs.R'))
  
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
  
  # Saved na-padded x0 for later when generating output of model recovery
  x0_pad = x0
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
  # (regression model betas based on non-normalized predictors + learning model parameters)
  add = res[variable == 'coefs']
  add$variable = 'input_params'
  # The simulation makes trial-by-trial predictions based on the regression
  # models beta values. We can therefore not simulate data using the beta
  # weights based on z-scored predictors (e.g. "z_V1") because z-scoring requires
  # all values encountered in the entire experiment (full distribution).
  # Recovery therefore needs to be evaluated based on beta weights stemming from
  # non-normalized predictors
  add$value = c(rep(NA, length(beta_weights[!is.na(beta_weights)])),
                beta_weights[!is.na(beta_weights)],
                input_params[!is.na(input_params)])
  # Fuse input_params to results
  res = rbind(res, add)
  
  # Add recovered parameters to model data, with NA padding to max of 4 parameters
  recov_params = res[variable == 'coefs'] %>%
    # ADD c(x1,x2,x3,x4) and c(b0,b1,b2,b3,b4)
    # Get values of recovered parameters
    .[!x %in% c('z_(Intercept)', 'z_V1', 'z_V2', 'z_V1u', 'z_V2u',
                '(Intercept)', 'V1', 'V2', 'V1u', 'V2u'), ]
  add_params = rep(NA, 4)
  add_params[1:nrow(recov_params)] = recov_params$value
  add_params = as.numeric(add_params)
  # Add recovered beta weights, with NA padding to max of 5 betas
  recov_betas = res[variable == 'coefs'] %>%
    # Get values of recovered betas
    .[x %in% c('(Intercept)', 'V1', 'V2', 'V1u', 'V2u'), ]
  add_betas = rep(NA, 5)
  add_betas[1:nrow(recov_betas)] = recov_betas$value
  add_betas = as.numeric(add_betas)
  
  # Get experiment data of model with recovered parameters & recovered beta weights
  model_data = recov$model_data %>%
    .[, ':='(recov_x1 = add_params[1],
             recov_x2 = add_params[2],
             recov_x3 = add_params[3],
             recov_x4 = add_params[4],
             recov_b0 = add_betas[1],
             recov_b1 = add_betas[2],
             recov_b2 = add_betas[3],
             recov_b3 = add_betas[4],
             recov_b4 = add_betas[5])]
  
  ### Full model recovery
  # Get model to be recovered
  generating_model = model
  # Set random starting values for full model recovery
  model_recov_svs = Get_svs(starting_values = 'random')
  # Fit all models to simulated data
  model_recov = Fit_models_new(data = sim,
                               algorithm = algorithm,
                               xtol_rel = xtol_rel,
                               maxeval = maxeval,
                               x0 = model_recov_svs$x0,
                               lb = model_recov_svs$lb,
                               ub = model_recov_svs$ub,
                               param_recov = FALSE)
  # Add constant variables to model recovery
  model_recov_res = model_recov$fitting_out %>%
    # Remove LR information
    .[variable != 'LRs',] %>%
    # Add info on generating process (model and parameters)
    .[, generating_model := generating_model] %>%
    .[, ':='(generating_x1 = input_params[1],
             generating_x2 = input_params[2],
             generating_x3 = input_params[3],
             generating_x4 = input_params[4],
             generating_b0 = beta_weights[1],
             generating_b1 = beta_weights[2],
             generating_b2 = beta_weights[3],
             generating_b3 = beta_weights[4],
             generating_b4 = beta_weights[5])] %>%
    # Information that starting values for model recovery were random
    .[, model_recovery_svs := 'random']
  
  # Form named list of outputs
  out = list(recovery = res,
             recovery_data = model_data,
             model_recovery = model_recov_res)
  
  # Return results table
  return(out)
  
}