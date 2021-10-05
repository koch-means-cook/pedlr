# 
# data = Load_data()
# data = Prepare_data_for_fit(data)
# data = data[participant_id == 'G5RTD96' & task_version == 1]

library(optimr)

Fit_model = function(data){

  # Load models
  source_path = file.path(base_path, 'code', 'models',
                          fsep = .Platform$file.sep)
  source_files = list.files(source_path, pattern = "[.][rR]$",
                            full.names = TRUE, recursive = TRUE)
  invisible(lapply(source_files, function(x) source(x)))
  
  # LL function
  Log_Likelihood = function(x,
                            design,
                            data,
                            model){
    
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
      
    # Pedlr model
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
      
    # Standard Rescorla-Wagner model
    } else if(model == 'Rw'){
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
    
    # Exclude forced choices (since here there is no choice probability and in turn no likelihood of choice)
    choices_prob_model = choices_prob_model[design$trial_type == 'choice']
    
    # Exclude time out trials (outcome = NA)
    choices_participant = choices_participant[!is.na(design$outcome)]
    choices_model = choices_model[!is.na(design$outcome)]
    choices_prob_model = choices_model[!is.na(design$outcome)]
    
    # Calculate likelihood (Probability model decides as participant)
    likelihood = choices_prob_model
    if(any(is.na(likelihood))){
      stop('NA in likelihood vector')
    }
    # Log likelihood
    likelihood = log(likelihood)
    # Sum up log likelihood over all choices
    likelihood = sum(likelihood)
    # Negative log likelihood
    likelihood = -likelihood
    
    # Return negative log likelihood
    return(likelihood)
  }
  
  # Pedlr_interdep
  # Randomly sampled initial values
  x = c(runif(1,0,1), runif(1,0,1), runif(1,0,1), runif(1,0.5,10))
  # Define algorithm
  opts = list('algorithm'='NLOPT_LN_COBYLA', 'xtol_rel'=1.0e-8)
  # Solve function for minimum
  opt = nloptr::nloptr(x0=x,
                       eval_f=Log_Likelihood,
                       lb=c(0,0,0,0.5),
                       ub=c(1,1,1,10),
                       opts=opts,
                       data=data,
                       design=data,
                       model = 'Pedlr_interdep')
  opt$solution
  
  # Pedlr
  x = c(runif(1,0,1), runif(1,0,1), runif(1,0.5,10))
  opts = list('algorithm'='NLOPT_LN_COBYLA', 'xtol_rel'=1.0e-8)
  opt = nloptr::nloptr(x0=x,
                       eval_f=Log_Likelihood,
                       lb=c(0,0,0.5),
                       ub=c(1,1,10),
                       opts=opts,
                       data=data,
                       design=data,
                       model = 'Pedlr')
  opt$solution
  
  # Rw
  x = c(runif(1,0,1), runif(1,0.5,10))
  opts = list('algorithm'='NLOPT_LN_COBYLA', 'xtol_rel'=1.0e-8)
  opt = nloptr::nloptr(x0=x,
                       eval_f=Log_Likelihood,
                       lb=c(0,0.5),
                       ub=c(1,10),
                       opts=opts,
                       data=data,
                       design=data,
                       model = 'Rw')
  opt$solution
  
}