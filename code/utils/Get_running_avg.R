Get_running_avg = function(choice_option,
                           choice_outcome,
                           stim){
  # Initialize array keeping running average for all trials
  running_avg = c()
  
  # Loop over trials
  for(i in seq(length(choice_option))){
    # Find all choices of option so far
    idx = choice_option[1:i] == stim
    # Create mean over all stim choices so far
    trialwise_mean = mean(choice_outcome[1:i][idx],
                          na.rm = TRUE)
    # Append running average for each trial
    running_avg = c(running_avg, trialwise_mean)
  }
  
  # Set all trials in which option has not been chosen so far to NA
  running_avg[is.nan(running_avg)] = NA
  return(running_avg)
}