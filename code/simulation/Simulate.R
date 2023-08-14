library(data.table)
library(magrittr)

Simulate = function(data,
                    x,
                    temperature,
                    tau,
                    model){
  
  source(file.path(here::here(), 'code', 'simulation', 'Make_choice.R'))
  source(file.path(here::here(), 'code', 'simulation', 'Compute_value_per_trial.R'))
  source(file.path(here::here(), 'code', 'model_fitting', 'LRfunction.R'))

  # data = data.table::fread(file.path(here::here(), 'data', '09RI1ZH_exp_data.tsv'),
  #                          sep = '\t', na.strings = 'n/a')
  # x = c(0.175, 0.505, NA, NA)
  # temperature = 7
  # tau = 0.2
  # model = 'seplr'

  
  # Select relevant columns
  cols = c('option_left', 'option_right', 'pic_left', 'pic_right', 'reward_stim_1',
           'reward_stim_2', 'comp_number', 'trial_type', 'free_choice', 'forced_left',
           'forced_right', 'with_rating', 'run', 'block_n', 'is_rare', 'bad_forced',
           'participant_id', 'name_design_r1', 'name_design_r2', 'forced', 'choice',
           'outcome', 'est_1_reward', 'est_1_range', 'est_1_rt', 'est_2_reward',
           'est_2_range', 'est_2_rt', 'est_3_reward', 'est_3_range', 'est_3_rt',
           'option_choice', 'timeout', 'error')
  data = data %>%
    .[, ..cols] %>%
    # Add trial counter
    .[, trial := seq(.N)] %>%
    # Initialize value for each bandit
    .[, ':='(val_b_1 = 50,
             val_b_2 = 50,
             val_b_3 = 50,
             # Initialize PE for each bandit
             pe_b_1 = 0,
             pe_b_2 = 0,
             pe_b_3 = 0,
             # Initialize LR of trial
             lr = 0,
             # Initialize trailing surprise for each bandit
             s_b_1 = 0,
             s_b_2 = 0,
             s_b_3 = 0)]
  
  # Initialize data frame to hold model choices
  model_information = data.table::data.table()
  
  # Loop over trials (model "plays" task)
  for(t in seq(nrow(data))){
    
    # Reset values for second block (new stimuli and reward distributions)
    if(t == 241){
      data = data[t:nrow(data), ':='(val_b_1 = 50,
                                     val_b_2 = 50,
                                     val_b_3 = 50,
                                     s_b_1 = 0,
                                     s_b_2 = 0,
                                     s_b_3 = 0)]
    }
    
    # Get data of trial
    trial_data = data[t,]
    # Isolate value both bandits presented for choice
    left_value = c(trial_data$val_b_1,
                   trial_data$val_b_2,
                   trial_data$val_b_3)[trial_data$option_left]
    right_value = c(trial_data$val_b_1,
                    trial_data$val_b_2,
                    trial_data$val_b_3)[trial_data$option_right]
    
    # If choice trial
    if(trial_data$trial_type == 'choice'){
      # Simulate model choice for trial (based on value of left and right option, softmax-based)
      model_choice = Make_choice(value_1 = left_value,
                                 value_2 = right_value,
                                 temperature = temperature)
      # Get model choice (left vs. right; probability of choice; and chosen bandit)
      model_choice_side = model_choice$choice
      model_choice_prob = model_choice$choice_prob
      model_choice_option = c(trial_data$option_left, trial_data$option_right)[model_choice_side]
    } else{
      # If forced trial
      # Assume no errors in forced choice trials!
      # Forced choice: left
      if(trial_data$forced_left == 1){
        model_choice_side = 1
        model_choice_prob = 1
        model_choice_option = c(trial_data$option_left, trial_data$option_right)[1]
        # Forced choice: right
      } else{
        model_choice_side = 2
        model_choice_prob = 1
        model_choice_option = c(trial_data$option_left, trial_data$option_right)[2]
      }
    }
    
    
    # Updating
    # Value before updating
    value = c(trial_data$val_b_1,
              trial_data$val_b_2,
              trial_data$val_b_3)[model_choice_option]
    # Outcome of model's choice
    reward = c(trial_data$reward_stim_1,
               trial_data$reward_stim_2)[model_choice_side]
    # Trailing surprise
    surprise = c(trial_data$s_b_1,
                 trial_data$s_b_2,
                 trial_data$s_b_3)[model_choice_option]
    
    # Use current value and reward to update values for coming trials
    update = Compute_value_per_trial(V = value,
                                     R = reward,
                                     S = surprise,
                                     x = x,
                                     tau = 0.2,
                                     model = model)
    # Record PE of current trial
    pe_bandit = paste0('pe_b_', model_choice_option)
    data[t, pe_bandit] = update$pe
    # Record LR of current trial
    data[t, 'lr'] = update$lr
    
    # Update value of updated bandit for all consecutive trials
    # No value/surprise prediction in last trial
    if(t < 480){
      value_bandit = paste0('val_b_', model_choice_option)
      data[(t+1):nrow(data), value_bandit] = update$v_updated
      # Add trailing surprise of updated bandit for all consecutive trials
      s_bandit = paste0('s_b_', model_choice_option)
      data[(t+1):nrow(data), s_bandit] = update$s_updated
    }
    
    # Add model information
    model_information_temp = data.table::data.table()
    model_information_temp$left_value = left_value
    model_information_temp$right_value = right_value
    model_information_temp$model_choice_side = model_choice_side
    model_information_temp$model_choice_prob = model_choice_prob
    model_information_temp$model_choice_option = model_choice_option
    model_information_temp$model_outcome = reward
    
    # Fuse for each trial
    model_information = rbind(model_information,
                              model_information_temp)
    
    
  }

  # Add constant types of information to output
  model_information$model = model
  model_information$x1 = x[1]
  model_information$x2 = x[2]
  model_information$x3 = x[3]
  model_information$x4 = x[4]
  model_information$tau = tau
  model_information$temperature = temperature
  
  # Fuse data with model behavior
  simulated_data = cbind(data, model_information)
  
  # Set surprise to NA if uncertainty not relevant for model (RW, surprise, & seplr)
  if(model %in% c('rw', 'surprise', 'seplr')){
    simulated_data$s_b_1 = NA
    simulated_data$s_b_2 = NA
    simulated_data$s_b_3 = NA
  }
  
  return(simulated_data)
  
}
