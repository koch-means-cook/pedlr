

# Model that has to decide between two presented stimuli and picks the one with the highest expected reward
PE_dep_LR_choice_model = function(design, params.alpha0, params.alpha1, params.temperature){
  
  # Get number of trials
  params.ntrials = nrow(design)
  # DF to store model values in (has to be one entry longer since model will also update value in last trial)
  # Initialized with 50 (since default for rewards between 0 and 100)
  df_values = data.frame(matrix(50,params.ntrials,3))
  colnames(df_values) = c('v_stim_1', 'v_stim_2', 'v_stim_3')
  # DF to store PE
  df_PE = data.frame(matrix(0,params.ntrials,3))
  colnames(df_PE) = c('pe_stim_1', 'pe_stim_2', 'pe_stim_3')
  # DF for function of PE
  df_fPE = data.frame(matrix(0,params.ntrials,3))
  colnames(df_fPE) = c('fpe_stim_1', 'fpe_stim_2', 'fpe_stim_3')
  # DF for choice, L/R response, and choice probabilities of both choices
  df_choice = data.frame(matrix(0,params.ntrials, 5))
  colnames(df_choice) = c('choice',
                          'response',
                          'choice_prob_stim_1',
                          'choice_prob_stim_2',
                          'choice_prob_response')
  
  # Loop over every trial
  for(trial_count in c(1:params.ntrials)){
    
    # Vector to hold compared model values for choice
    curr_value = c(0,0)
    # Vector to hold stimulus index (i.e. which distribution it was drawn from) so only the correct
    # distribution gets updated
    stim_id = c(0,0)
    # Log choice-probability for each trial
    choice_prob = c(0,0)
    
    # Get model value for first stimulus
    stim_1 = design$option_left[trial_count]
    stim_id[1] = grep(as.character(stim_1), colnames(df_values))
    curr_value[1] = df_values[trial_count, stim_id[1]]
    # And for second stimulus
    stim_2 = design$option_right[trial_count]
    stim_id[2] = grep(as.character(stim_2), colnames(df_values))
    curr_value[2] = df_values[trial_count, stim_id[2]]
    
    # Probabilistic model choice with choice probabilities controlled by soft-max function
    # Softmax function depends on temperature parameter (sigmoid steepness)
    choice_prob[1] = (exp(curr_value[1]/temperature) /
                        (exp(curr_value[1]/temperature) + exp(curr_value[2]/temperature))
    )
    choice_prob[2] = 1- choice_prob[1]
    # Make choice based on soft-max probability
    choice = sample(c(1,2), 1, replace=TRUE, prob=c(choice_prob[1], choice_prob[2]))
    
    # Update value of chosen stimulus
    # Get current reward estimation
    curr_value = curr_value[choice]
    # Calculate PE based on discrepancy between estimated reward and actual reward given
    outcome = c(design$reward_stim_1[trial_count], design$reward_stim_2[trial_count])
    PE = outcome[choice] - curr_value
    f_PE = params.alpha0 + (1-params.alpha0) * params.alpha1 * (abs(PE)/reward_space_ub)
    
    #     Nicos model:
    #     alpha[trial] =  alpha_0 + (1-alpha_0)*alpha_1*(abs(PE[trial]/outmax))
    # 		#alpha[trial] =  alpha_0 + (1-alpha_0)*alpha_1*(PE[trial]/outmax)^2
    # 		V[trial+1] = V[trial] + alpha[trial]*PE[trial]
    
    # Update value for next iteration with PE dependent learning rule
    df_values[(trial_count+1):nrow(df_values), stim_id[choice]] = curr_value + f_PE*PE
    
    # Safe PE in data frame
    df_PE[trial_count, stim_id[choice]] = PE
    df_fPE[trial_count, stim_id[choice]] = f_PE
    
    # Safe choice and softmax choice probability in data frame
    df_choice$choice[trial_count] = c(design$option_left[trial_count],
                                      design$option_right[trial_count])[choice]
    df_choice$response[trial_count] = choice
    df_choice$choice_prob_stim_1[trial_count] = choice_prob[1]
    df_choice$choice_prob_stim_2[trial_count] = choice_prob[2]
    df_choice$choice_prob_response[trial_count] = choice_prob[choice]
    
    #print(trial_count)
    
  }
  
  # Form list of returns (and cut off extra line of model values after last choice)
  model_data <- list('choice' = df_choice,
                     'values' = df_values[1:params.ntrials,],
                     'PE' = df_PE,
                     'fPE' = df_fPE)
  return(model_data)
  
}

