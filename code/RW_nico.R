RW_nico = function(data, params) {

  ntrials = nrow(data)
  # Number of different stimuli with associated distributions
  ndist = max(c(data$option_left, data$option_right))

  # Data frames to store choices, model values, prediction errors, and updating
  values = matrix(params$init_values, ntrials, 3)
  df = data.frame(matrix(NA, ntrials, 7))
  colnames(df) = c('choice', 'choice_prob', 'forced_choice', 'value1', 'value2', 'value3', 'PE')

  for(trial_count in 1:ntrials){

    # Get current comparison (choice options as 2 entry vector)
    V = values[trial_count, data$choice[trial_count]]
    R = data$outcome[trial_count]

    comp_stim = c(data$option_left[trial_count], data$option_right[trial_count])
    comp_value = values[trial_count, comp_stim]

    # Get if forced choice was an error
    error = data$error[trial_count]

    df$choice_prob[trial_count] = exp(V/params$temp) / sum(exp(comp_value/params$temp))
    if (!is.na(R)) {
      PE = R - V
      values[min(trial_count + 1, ntrials):ntrials, data$choice[trial_count]] = V + params$alpha * PE
      df$PE[trial_count] = PE
    }
  }
  df$value1 = values[,1]
  df$value2 = values[,2]
  df$value3 = values[,3]
  df$choice = data$choice
  df$forced_choice = data$free_choice*-1 + 1
  return(df)
}
