
# Function to make a choice between two presented stimuli

# - Updating of values only for chosen option
# - Choice policy based on softmax

Rw = function(design,
              params.alpha,
              params.temperature,
              params.reward_space_ub,
              choice_policy,
              init_values = c(50,50,50)){
  
  # Get other parameters from design
  # Number of trials
  params.ntrials = nrow(design)
  # Number of different stimuli with associated distributions
  params.ndist = max(c(design$option_left, design$option_right))
  
  # Data frames to store choices, model values, prediction errors, and updating
  df_choices = data.frame(matrix(NA, params.ntrials, 3))
  colnames(df_choices) = c('choice', 'choice_prob', 'forced_choice')
  # Initialize stimuli with provided values
  df_values = data.frame(matrix(rep(init_values, each=params.ntrials), params.ntrials, params.ndist))
  # Initialize prediction errors and updates with NA
  df_pe = df_fpe = data.frame(matrix(NA, params.ntrials, params.ndist))
  # Set colnames for each stimulus
  colnames(df_values) = colnames(df_pe) = colnames(df_fpe) = paste('stim_',
                                                                   as.character(c(1:params.ndist)),
                                                                   sep='')
  
  # Loop over trials
  for(trial_count in 1:params.ntrials){
    
    # Get current comparison (choice options as 2 entry vector)
    comp_stim = c(design$option_left[trial_count], design$option_right[trial_count])
    comp_value = c(df_values[trial_count, comp_stim[1]], df_values[trial_count, comp_stim[2]])
    comp_reward = c(design$reward_stim_1[trial_count], design$reward_stim_2[trial_count])
    
    # In case of free choice trial
    if(design$forced_left[trial_count] == 0 & design$forced_right[trial_count] == 0){
      # Not a forced choice trial
      forced_choice = 0
      # Select between softmax and greedy choice based on input
      # Softmax
      if(choice_policy == 'softmax'){
        # Make softmax choice based on model values (returns index of choice in 2 entry vector)
        sm = Softmax_choice(comp_value[1], comp_value[2], params.temperature)
        choice = sm$choice
        choice_prob = sm$choice_prob
        # Greedy
      } else if(choice_policy == 'greedy'){
        # Make greedy choice
        choice = which(comp_value == max(comp_value))
        choice_prob = 1
        # In case of same value take random choice
        if(length(choice) > 1){
          choice = choice[sample(choice)[1]]
        }
      }
      # In case of forced choice left, chose left with nearly 100% probability
    } else if(design$forced_left[trial_count] == 1){
      choice = 1
      choice_prob = 99999/100000
      forced_choice = 1
      # In case of forced choice right, chose right with 100% probability
    } else if(design$forced_right[trial_count] == 1){
      choice = 2
      choice_prob = 99999/100000
      forced_choice = 1
      # In case forced choice of both alternatives, raise error (broken design)
    } else if(design$forced_left[trial_count] == 1 & design$forced_right[trial_count] == 1){
      stop(paste("Simultaneous forced choice of left and right option in trial ",
                 trial_count,
                 ".",
                 sep=''))
    }
    
    # Get chosen stimulus, value, and reward
    choice_stim = comp_stim[choice]
    choice_value = comp_value[choice]
    choice_reward = comp_reward[choice]
    
    
    # Calculate updating according to model
    pe = choice_reward - choice_value
    fpe = pe
    updated_value = choice_value + params.alpha * pe
    
    # Update entries
    # choice, pe, and fpe are updated in current trial
    df_choices$choice[trial_count] = choice_stim
    df_choices$choice_prob[trial_count] = choice_prob
    df_choices$forced_choice[trial_count] = forced_choice
    df_pe[trial_count, choice_stim] = pe
    df_fpe[trial_count, choice_stim] = fpe
    # Value is updated for all following trials (exception of last trial)
    if(trial_count != params.ntrials){
      df_values[(trial_count + 1):nrow(df_values), choice_stim] = updated_value 
    }
    
  }
  
  # Form list of returns (and cut off extra line of model values after last choice)
  model_data <- list('choices' = df_choices,
                     'values' = df_values[1:params.ntrials,],
                     'PE' = df_pe,
                     'fPE' = df_fpe)
  return(model_data)
  
}
