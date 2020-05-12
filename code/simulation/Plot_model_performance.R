
Plot_value = function(results,
                                  average_subjects,
                                  mean_value_calculation){
  
  
  
  # Arguments
  #   results: data frame of model output (result from 'Apply_model' function)
  #   average_subjects: TRUE/FALSE, Determines if plot should be averaged over all subjects or each
  #                     subjects should be plotted individually.
  #   mean_value_calculation: Determines calculation of mean value for each stimulus. Available options:
  #           - 'all_trials': Calculating the average value for a stimulus uses all trials, even the value
  #                           estimates in wich this stimulus was not chosen and is not updated ('value 
  #                           plateaus')
  #           - 'chosen_trials':  Average value for stimulus is only calculated over outcomes of choices
  #                               so 'value plateaus' do not influence the mean value
  
  # Check inputs
  # Argument options for mean_value_valculation
  if(!(mean_value_calculation == 'all_trials' | 
       mean_value_calculation == 'chosen_trials')){
    stop(paste("Option for mean_value_calculation ('",
               mean_value_calculation,
               ") invalid. Available options are:\n",
               "-'all_trials'\n",
               "-'chosen_trials'",
               sep=''))
  }
  
  # Set data format to data table for easier preperation
  results = setDT(results)
  
  # Deselect PE and function of PE for plotting
  results = results %>%
    select(-pe_stim_1,
           -pe_stim_2,
           -pe_stim_3,
           -fpe_stim_1,
           -fpe_stim_2,
           -fpe_stim_3)
  
  # Prepare data table for plotting
  results = melt(results,
                 id.vars=c('sub_id',
                           'trial',
                           'stim_1',
                           'stim_2',
                           'reward_stim_1',
                           'reward_stim_2',
                           'choice',
                           'choice_prob',
                           'comp_number',
                           'task_version'))
  
  # Plot for averaged subjects
  if(average_subjects){
    # Average over subjects
    results = ddply(results,
                    .(trial, task_version, variable),
                    summarize,
                    mean_value = mean(value),
                    sd_value = sd(value))
    
    # Plot average value for each trial
    plot = ggplot(results,
                  aes(x = trial,
                  y = mean_value,
                  group = variable,
                  color = variable)) +
      geom_line() +
      facet_wrap(~task_version)
  } else if(average_subjects == FALSE){
    # Plot average value for each trial for each subject
    plot = ggplot(results,
                  aes(x = trial,
                      y = value,
                      group = variable,
                      color = variable)) +
      geom_line() +
      facet_wrap(~task_version + sub_id)
  }
  
  # Add lines and distribution plot
  
  
  
  
}






## Plot results of choice model

# Cut down data frame for plotting
data_plot = data_model[,1:11]
data_plot = melt(data_plot, id.vars=c('sub_id',
                                      'trial',
                                      'stim_1',
                                      'stim_2',
                                      'reward_stim_1',
                                      'reward_stim_2',
                                      'choice',
                                      'comp_number'))

# Average over all subjects
data_plot = ddply(data_plot,
                  .(trial, variable),
                  summarize,
                  mean_value = mean(value),
                  sd_value = sd(value))

# Plot model values
p_choice = ggplot(data=data_plot, aes(x=trial, y=mean_value, group=variable, color=variable)) +
  geom_hline(yintercept=c(1/3*100, 50, 2/3*100), linetype='dashed') +
  geom_hline(yintercept=c(mean(subset(data_plot, variable == 'v_stim_1')$mean_value),
                          mean(subset(data_plot, variable == 'v_stim_2')$mean_value),
                          mean(subset(data_plot, variable == 'v_stim_3')$mean_value)),
             color = c(color$gain, color$gaussian, color$loss)) +
  geom_line(size=1, alpha=0.8) +
  scale_color_manual(values = c(color$gain, color$gaussian, color$loss)) +
  labs(title=paste('Choice model performance (α0=', alpha0, ', α1=', alpha1, ', τ=', temperature, ')', sep=''),
       x='Trial',
       y='Estimated outcome',
       color='Stimulus')
ggplotly(p_choice)





# Only chosen option

data_plot = data_model[,1:11]
data_plot = melt(data_plot, id.vars=c('sub_id',
                                      'trial',
                                      'stim_1',
                                      'stim_2',
                                      'reward_stim_1',
                                      'reward_stim_2',
                                      'choice',
                                      'comp_number'))
# Select only choices of stim 1 to calculate mean value (leaving out plateaus)
data_1 = subset(data_plot, choice == 1 & variable == 'v_stim_1')
# Calculate mean value of dist 1 WHEN CHOSEN
mean_1 = mean(data_1$value, na.rm = TRUE)

# Same for 2
data_2 = subset(data_plot, choice == 2 & variable == 'v_stim_2')
mean_2 = mean(data_2$value, na.rm = TRUE)

# Same for 3
data_3 = subset(data_plot, choice == 3 & variable == 'v_stim_3')
mean_3 = mean(data_3$value, na.rm = TRUE)

# Average over all subjects
data_plot = ddply(data_plot,
                  .(trial, variable),
                  summarize,
                  mean_value = mean(value),
                  sd_value = sd(value))

# Plot model values
p = ggplot(data=data_plot, aes(x=trial, y=mean_value, group=variable, color=variable)) +
  geom_hline(yintercept=c(1/3*100, 50, 2/3*100), linetype='dashed') +
  geom_hline(yintercept=c(mean_1,
                          mean_2,
                          mean_3),
             color=c(color$gain, color$gaussian, color$loss)) +
  geom_line(size=1, alpha=0.8) +
  scale_color_manual(values = c(color$gain, color$gaussian, color$loss)) +
  labs(title=paste('Choice model performance (α0=', alpha0, ', α1=', alpha1, ', τ=', temperature, ')', sep=''),
       x='Trial',
       y='Estimated outcome',
       color='Stimulus')
p = ggplotly(p)

p




# Single participants

# Cut down data frame for plotting
data_plot = data_model[,1:11]
data_plot = melt(data_plot, id.vars=c('sub_id',
                                      'trial',
                                      'stim_1',
                                      'stim_2',
                                      'reward_stim_1',
                                      'reward_stim_2',
                                      'choice',
                                      'comp_number'))

# Select single subject
#data_plot = subset(data_plot, sub_id == 1)

# Plot model values
p_choice = ggplot(data=data_plot, aes(x=trial, y=value, group=variable, color=variable)) +
  theme_bw() +
  geom_hline(yintercept=c(1/3 *100, 50, 2/3*100), linetype='dashed',
             size=0.1) +
  # geom_hline(yintercept=c(mean(subset(data_plot, variable == 'v_stim_1')$mean_value),
  #                         mean(subset(data_plot, variable == 'v_stim_2')$mean_value),
  #                         mean(subset(data_plot, variable == 'v_stim_3')$mean_value)),
  #            color = c(color$gain, color$gaussian, color$loss)) +
  geom_line(size=0.1, alpha=0.8) +
  scale_color_manual(values=c(color$gain, color$gaussian, color$loss)) +
  labs(title=paste('Choice model performance (alpha0=', alpha0, ', alpha1=', alpha1, ', temp=', temperature, ')', sep=''),
       x='Trial',
       y='Estimated outcome',
       color='Stimulus') +
  facet_wrap( ~ sub_id, ncol = 8) +
  theme(legend.position = 'none',
        axis.text = element_text(size=5),
        strip.text = element_text(size=5),
        panel.grid = element_blank())
ggplotly(p_choice)



