PEDLR_nico = function(data, params, init_values = 50) {
  
  # params = list()
  # params$alpha0 = 0.2
  # params$alpha1 = 0.7
  # params$temp = 5
  # params$init_values = 50
  
  ntrials = nrow(data)
  # Number of different stimuli with associated distributions
  ndist = max(c(data$option_left, data$option_right))

  # Data frames to store choices, model values, prediction errors, and updating
  values = matrix(params$init_values, ntrials, 3)
  df = data.frame(matrix(NA, ntrials, 8))
  colnames(df) = c('choice', 'choice_prob', 'forced_choice', 'value1', 'value2', 'value3', 'PE', 'LR')

  for(trial_count in 1:ntrials){

    # Get current comparison (choice options as 2 entry vector)
    V = values[trial_count, data$choice[trial_count]]
    R = data$outcome[trial_count]

    comp_stim = c(data$option_left[trial_count], data$option_right[trial_count])
    #comp_stim = comp_stim[which(comp_stim != data$choice[trial_count])]
    comp_value = values[trial_count, comp_stim]

    # Get if forced choice was an error
    error = data$error[trial_count]

    df$choice_prob[trial_count] = exp(V/params$temp) / sum(exp(comp_value/params$temp))
    
    # alt_choice_prob = Softmax_choice_prob(value_1 = comp_value[1],
    #                                       value_2 = comp_value[2],
    #                                       choice = which(comp_stim == data$choice[trial_count]),
    #                                       params$temp)$choice_prob
    
    #message(paste('Nico: ', df$choice_prob[trial_count], ' | Chris: ', alt_choice_prob, sep = ''))
    
    
    if (!is.na(R)) {
      PE = R - V
      df$PE[trial_count] = PE
      #LR = params$alpha0 + (1-params$alpha0)*params$alpha1 * (abs(PE)/max(abs(df$PE[1:trial_count]), na.rm = TRUE))
      
      # Nico's approach: Nico normalized the PE term by the max of the PEs so far (stronger updating in beginning)
      #LR = params$alpha0 + (1-params$alpha0)*params$alpha1 * (abs(PE)/max(abs(df$PE[1:trial_count]), na.rm = TRUE))
      
      # My approach (constant normalization of PE term with 100, the highest point range)
      LR = params$alpha0 + (1-params$alpha0)*params$alpha1 * (abs(PE)/100)
      
      
      df$LR[trial_count] = LR
      values[min(trial_count + 1, ntrials):ntrials, data$choice[trial_count]] = V + LR * PE
    }
  }
  df$value1 = values[,1]
  df$value2 = values[,2]
  df$value3 = values[,3]
  df$choice = data$choice
  df$forced_choice = data$free_choice*-1 + 1
  return(df)
}
