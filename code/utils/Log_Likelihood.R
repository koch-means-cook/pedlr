library(here)
library(data.table)


# Load models
source(file.path(here::here(), 'code', 'model_fitting', 'Fit_Rw.R',
                 fsep = .Platform$file.sep))
source(file.path(here::here(), 'code', 'model_fitting', 'Fit_Pedlr_interdep.R',
                 fsep = .Platform$file.sep))
source(file.path(here::here(), 'code', 'model_fitting', 'Fit_Pedlr.R',
                 fsep = .Platform$file.sep))
source(file.path(here::here(), 'code', 'model_fitting', 'Fit_Pedlr_step.R',
                 fsep = .Platform$file.sep))
source(file.path(here::here(), 'code', 'model_fitting', 'Fit_Pedlr_fixdep.R',
                 fsep = .Platform$file.sep))
source(file.path(here::here(), 'code', 'model_fitting', 'Fit_Pedlr_simple.R',
                 fsep = .Platform$file.sep))
source(file.path(here::here(), 'code', 'model_fitting', 'Fit_Pedlr_simple_const.R',
                 fsep = .Platform$file.sep))



# LL function
Log_Likelihood = function(x,
                          design_run1,
                          data_run1,
                          design_run2,
                          data_run2,
                          model){
  
  # Allocate vector to store LL for each run
  ll_vector = c()
  
  # Loop across both runs
  for(run in seq(2)){
    
    # Select data for specific run
    if(run == 1){
      design = design_run1
      data = data_run1
    } else if(run == 2){
      design = design_run2
      data = data_run2
    }
    
    # --- Start run-specific likelihood calc ---
    
    # Get choices of participant
    choices_participant = data$choice
    
    # params.alpha0 = 0.2
    # params.alpha1 = 0.7
    # params.interdep = 0.5
    # params.temperature = 7
    
    # Interdependent Pedlr
    if(model == 'Pedlr_interdep'){
      # Set vector entries to reflect parameters
      params.alpha0 = x[1]
      params.alpha1 = x[2]
      params.interdep = x[3]
      params.temperature = x[4]
      
      # Call model to obtain model values
      model_data = Fit_Pedlr_interdep(data = data,
                                      params.alpha0 = params.alpha0,
                                      params.alpha1 = params.alpha1,
                                      params.interdep = params.interdep,
                                      params.temperature = params.temperature,
                                      params.reward_space_ub = 100,
                                      choice_policy = 'softmax',
                                      init_values = c(50, 50, 50))
      
      # Pedlr_fixdep model
    } else if(model == 'Pedlr_fixdep'){
      # Set vector entries to reflect parameters
      params.alpha0 = x[1]
      params.alpha1 = x[2]
      params.temperature = x[3]
      
      # Call model to obtain model values
      model_data = Fit_Pedlr_fixdep(data = data,
                                    params.alpha0 = params.alpha0,
                                    params.alpha1 = params.alpha1,
                                    params.temperature = params.temperature,
                                    params.reward_space_ub = 100,
                                    choice_policy = 'softmax',
                                    init_values = c(50, 50, 50))
      
      
    } else if(model == 'Pedlr'){ 
      params.alpha0 = x[1]
      params.alpha1 = x[2]
      params.temperature = x[3]
      
      # Call model to obtain model values
      model_data = Fit_Pedlr(data = data,
                             params.alpha0 = params.alpha0,
                             params.alpha1 = params.alpha1,
                             params.temperature = params.temperature,
                             params.reward_space_ub = 100,
                             choice_policy = 'softmax',
                             init_values = c(50, 50, 50))
      
      # Pedlr with step function
    } else if(model == 'Pedlr_step'){
      
      # Get PEs of Rw model for this participant and run
      id = unique(data$participant_id)
      run_id = unique(data$run)
      rw_file = file.path(here::here(),
                          'derivatives',
                          'model_fitting',
                          paste(id,
                                '-fit-Rw.tsv',
                                sep = ''))
      rw_fit = data.table::fread(rw_file,
                                 sep = '\t',
                                 na.strings = 'n/a')
      # Get optimized parameters for RW model
      rw_params_alpha = rw_fit[run == run_id & file == 1 & iter == 1 & para == 'alpha']$second_solution
      rw_params_temp = rw_fit[run == run_id & file == 1 & iter == 1 & para == 'temperature']$second_solution
      # Recreate data based on optimzed parameters
      rw = Fit_Rw(data = data,
                  params.alpha = rw_params_alpha,
                  params.temperature = rw_params_temp,
                  params.reward_space_ub = 100,
                  choice_policy = 'softmax',
                  init_values = c(50,50,50))
      # # Get PE distribution (of absolute PE)
      # pes = c(rw$PE$stim_1, rw$PE$stim_2, rw$PE$stim_3)
      # pes = abs(pes[!is.na(pes)])
      # Get highest 5% of PEs to set "rare" boundary
      pe_boundary_abs = rw$pe_boundary_abs
      
      params.alpha0 = x[1]
      params.alpha1 = x[2]
      params.temperature = x[3]
      
      # Call model to obtain model values
      model_data = Fit_Pedlr_step(data = data,
                                  params.alpha0 = params.alpha0,
                                  params.alpha1 = params.alpha1,
                                  params.temperature = params.temperature,
                                  params.reward_space_ub = 100,
                                  choice_policy = 'softmax',
                                  init_values = c(50, 50, 50),
                                  pe_boundary_abs = pe_boundary_abs)
      
      # Pedlr model with constant alpha0 
    } else if(model == 'Pedlr_simple_const'){ 
      params.alpha1 = x[1]
      params.temperature = x[2]
      
      # Call model to obtain model values
      model_data = Fit_Pedlr_simple_const(data = data,
                                          params.alpha1 = params.alpha1,
                                          params.temperature = params.temperature,
                                          params.reward_space_ub = 100,
                                          choice_policy = 'softmax',
                                          init_values = c(50, 50, 50))
      
      # Pedlr model without LR decomposition
    }else if(model == 'Pedlr_simple'){ 
      params.alpha1 = x[1]
      params.temperature = x[2]
      
      # Call model to obtain model values
      model_data = Fit_Pedlr_simple(data = data,
                                    params.alpha1 = params.alpha1,
                                    params.temperature = params.temperature,
                                    params.reward_space_ub = 100,
                                    choice_policy = 'softmax',
                                    init_values = c(50, 50, 50))
      
      # Standard Rescorla-Wagner model
    }else if(model == 'Rw'){
      params.alpha = x[1]
      params.temperature = x[2]
      
      # Call model to obtain model values
      model_data = Fit_Rw(data = data,
                          params.alpha = params.alpha,
                          params.temperature = params.temperature,
                          params.reward_space_ub = 100,
                          choice_policy = 'softmax',
                          init_values = c(50, 50, 50))
    }
    
    
    # Get choices and probability of choices of model
    choices_model = model_data$choices$choice
    choices_prob_model = model_data$choices$choice_prob
    
    # Get forced choices to exclude from likelihood calculation (since here there is no
    # choice probability and in turn no likelihood of choice)
    index_fc = which((design$trial_type == 'forced'))
    
    # Get time out trials (outcome = NA) to exclude
    index_to = which(is.na(design$outcome))
    
    # Exclude forced choices and time outs
    index_excl = unique(sort(c(index_fc, index_to)))
    choices_prob_model = choices_prob_model[-index_excl]
    
    # Calculate likelihood (Probability model decides as participant)
    likelihood = choices_prob_model
    if(any(is.na(likelihood))){
      stop('NA in likelihood vector')
    }
    # Log likelihood
    likelihood = log(likelihood)
    # Sum up log likelihood over all choices
    likelihood = sum(likelihood)
    
    # Add run-specific summed-up LL to across-run vector
    ll_vector = c(ll_vector, likelihood)
    
  }
  
  # Sum up likelihood of both runs to optimize across runs with the same set of 
  # parameters
  across_run_likelihood = sum(ll_vector)
  
  # Negative log likelihood (to use minimization algorithms)
  across_run_likelihood = -across_run_likelihood
  
  # Return negative log likelihood
  return(across_run_likelihood)
}
  