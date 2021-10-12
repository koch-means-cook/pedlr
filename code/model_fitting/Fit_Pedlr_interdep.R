library(here)

# Load functions
source_path = file.path(here::here(), 'code', 'models',
                        fsep = .Platform$file.sep)
source_files = list.files(source_path, pattern = "[.][rR]$",
                          full.names = TRUE, recursive = TRUE)
invisible(lapply(source_files, function(x) source(x)))


Fit_Pedlr_interdep = function(data,
                              params.alpha0,
                              params.alpha1,
                              params.interdep,
                              params.temperature,
                              params.reward_space_ub,
                              choice_policy,
                              init_values = c(50,50,50)){
  
  # Get other parameters from design
  # Number of trials
  params.ntrials = nrow(data)
  # Number of different stimuli with associated distributions
  params.ndist = max(c(data$option_left, data$option_right))
  
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
    comp_stim = c(data$option_left[trial_count], data$option_right[trial_count])
    comp_value = c(df_values[trial_count, comp_stim[1]], df_values[trial_count, comp_stim[2]])
    comp_reward = c(data$reward_stim_1[trial_count], data$reward_stim_2[trial_count])
    # Get index of choice the participant made
    subj_choice = which(comp_stim == data$choice[trial_count])
    
    # Skip NA choice trials (time outs of participant)
    if(is.na(data$choice[trial_count])){
      if(data$forced_left[trial_count] == 0 & data$forced_right[trial_count] == 0){
        
        # Not a forced choice trial
        forced_choice = 0
        model_choice = NA
        choice_prob = NA
        
        # In case of forced choice left, chose left with 100% probability
      } else if(data$forced_left[trial_count] == 1){
        forced_choice = 1
        model_choice = NA
        choice_prob = NA
        # In case of forced choice right, chose right with 100% probability
      } else if(data$forced_right[trial_count] == 1){
        forced_choice = 1
        model_choice = NA
        choice_prob = NA
        # In case forced choice of both alternatives, raise error (broken design)
      } else if(data$forced_left[trial_count] == 1 & data$forced_right[trial_count] == 1){
        stop(paste("Simultaneous forced choice of left and right option in trial ",
                   trial_count,
                   ".",
                   sep=''))
      }
      
      # Normal trials (no time out): Choice and updating of model
    } else{
      # In case of free choice trial
      if(data$forced_left[trial_count] == 0 & data$forced_right[trial_count] == 0){
        # Not a forced choice trial
        forced_choice = 0
        
        # Select between softmax and greedy choice based on input
        # Softmax
        if(choice_policy == 'softmax'){
          softmax = Softmax_choice(comp_value[1], comp_value[2], params.temperature)
          # Get index of choice model would make
          model_choice = comp_stim[softmax$choice]
          # Get probability of model's choice given participant's previous choices
          if(model_choice == subj_choice){
            # Softmax choice always gives probability of choice model made
            choice_prob = softmax$choice_prob
          } else{
            # If model and participant dont agree we need to take counter probability
            choice_prob = 1 - softmax$choice_prob
          }
          
          # Greedy
        } else if(choice_policy == 'greedy'){
          # Make greedy choice
          model_choice = which(comp_value == max(comp_value))
          # In case of same value take random choice
          if(length(choice) > 1){
            model_choice = sample(c(1,2), 1)
          }
          # Get choice probability based on subjects choice (avoid 0 since log would produce inf)
          if(model_choice == subj_choice){
            choice_prob = 99999/100000
          } else{
            choice_prob = 1/100000
          }
        }
        # In case of forced choice left, choose left with 100% probability
      } else if(data$forced_left[trial_count] == 1){
        forced_choice = 1
        model_choice = 1
        # Get choice probability based on subjects choice (avoid 0 since log would produce inf)
        if(model_choice == subj_choice){
          choice_prob = 99999/100000
        } else{
          choice_prob = 1/100000
        }
        # In case of forced choice right, choose right with 100% probability
      } else if(data$forced_right[trial_count] == 1){
        forced_choice = 1
        model_choice = 2
        # Get choice probability based on subjects choice (avoid 0 since log would produce inf)
        if(model_choice == subj_choice){
          choice_prob = 99999/100000
        } else{
          choice_prob = 1/100000
        }
        # In case forced choice of both alternatives, raise error (broken design)
      } else if(data$forced_left[trial_count] == 1 & data$forced_right[trial_count] == 1){
        stop(paste("Simultaneous forced choice of left and right option in trial ",
                   trial_count,
                   ".",
                   sep=''))
      }
      
      # Stimulus and value of option chosen by participant
      choice_stim = comp_stim[subj_choice]
      choice_value = comp_value[subj_choice]
      
      # Reward chosen by participant
      # No reward in case of wrong choice in forced choice trials
      if(forced_choice == 1 & model_choice != subj_choice){
        choice_reward = NA
        
        # Otherwise get reward from choice
      } else {
        choice_reward = comp_reward[subj_choice]
      }
      
      
      # Calculate prediction error according to subjects choice
      # Not if false choice
      if(is.na(choice_reward)){
        pe = NA
        fpe = NA
        
        # Otherwise update
      } else {
        pe = choice_reward - choice_value
        fpe = params.interdep * params.alpha0 + (1 - params.interdep) * params.alpha1 * (abs(pe)/params.reward_space_ub)
        updated_value = choice_value + fpe * pe 
      }
      
      
      # Update entries
      # choice, pe, and fpe are updated in current trial
      df_choices$choice[trial_count] = choice_stim
      df_choices$choice_prob[trial_count] = choice_prob
      df_choices$forced_choice[trial_count] = forced_choice
      df_pe[trial_count, choice_stim] = pe
      df_fpe[trial_count, choice_stim] = fpe
      # Value is updated for all following trials (exception of last trial and false forced choices)
      if(trial_count != params.ntrials & !is.na(pe)){
        df_values[(trial_count + 1):nrow(df_values), choice_stim] = updated_value 
      }
    }
  }
  
  # Form list of returns (and cut off extra line of model values after last choice)
  model_data <- list('choices' = df_choices,
                     'values' = df_values[1:params.ntrials,],
                     'PE' = df_pe,
                     'fPE' = df_fpe)
  return(model_data)
  
}