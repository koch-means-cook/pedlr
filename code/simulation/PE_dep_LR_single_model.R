

# Model that has to decide between two presented stimuli and picks the one with the highest expected reward
PE_dep_LR_single_model = function(design, params.alpha0, params.alpha1, params.temperature){
  
  # Get number of trials
  params.ntrials = nrow(design)
  # DF to store model values in (has to be one entry longer since model will also update value in last trial)
  # Initialized with 50 (since default for rewards between 0 and 100)
  df_values = data.frame(matrix(50,params.ntrials,1))
  colnames(df_values) = c('value')
  # DF to store PE
  df_PE = data.frame(matrix(0,params.ntrials,1))
  colnames(df_PE) = c('pe')
  # DF for function of PE
  df_fPE = data.frame(matrix(0,params.ntrials,1))
  colnames(df_fPE) = c('fpe')
  
  # Loop over every trial
  for(trial_count in c(1:params.ntrials)){
    
    # Get model value
    curr_value = df_values[trial_count,1]

    # Calculate PE based on discrepancy between estimated reward and actual reward given
    outcome = design$reward[trial_count]
    PE = outcome - curr_value
    f_PE = params.alpha0 + (1-params.alpha0) * params.alpha1 * (abs(PE)/reward_space_ub)
    
    #     Nicos model:
    #     alpha[trial] =  alpha_0 + (1-alpha_0)*alpha_1*(abs(PE[trial]/outmax))
    # 		#alpha[trial] =  alpha_0 + (1-alpha_0)*alpha_1*(PE[trial]/outmax)^2
    # 		V[trial+1] = V[trial] + alpha[trial]*PE[trial]
    
    # Update value for next iteration with PE dependent learning rule
    df_values[(trial_count+1):nrow(df_values),1] = curr_value + f_PE * PE
    
    # Safe PE in data frame
    df_PE[trial_count, 1] = PE
    df_fPE[trial_count, 1] = f_PE
    
    #print(trial_count)
    
  }
  
  # Form list of returns (and cut off extra line of model values after last choice)
  model_data <- list('values' = df_values,
                     'PE' = df_PE,
                     'fPE' = df_fPE)
  return(model_data)
  
}