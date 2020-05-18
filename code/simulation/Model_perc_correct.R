
Model_perc_correct = function(results){
  
  # Arguments
  #   results: data frame of model output (result from 'Apply_model' function)
  #   last_perc_of_trials: Last percentage of trials that should be taken into account
  #           - e.g.: 20, only last 20 percent of trials will be used to form the mean over the
  #                   estimated value of a stimulus
  #   average_subjects: TRUE/FALSE, Determines if results are averaged over all subjects or given for each
  #                     subject individually.
  #   mean_value_calculation: Determines calculation of mean value for each stimulus. Available options:
  #           - 'all_trials': Calculating the average value for a stimulus uses all trials, even the value
  #                           estimates in wich this stimulus was not chosen and is not updated ('value 
  #                           plateaus')
  #           - 'chosen_trials':  Average value for stimulus is only calculated over outcomes of choices
  #                               so 'value plateaus' do not influence the mean value
  
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
  
  # Prepare data table for plotting of sampling
  data_sampling = melt(results,
                       id.vars=c('sub_id',
                                 'trial',
                                 'stim_1',
                                 'stim_2',
                                 'reward_stim_1',
                                 'reward_stim_2',
                                 'choice',
                                 'choice_prob',
                                 'forced_choice',
                                 'comp_number',
                                 'task_version'))
  
  # Get sampling of rewards per task version from random subject
  data_sampling = subset(results, sub_id == sample(results$sub_id, 1))
  data_sampling = data.frame(
    rbind(cbind(data_sampling$task_version,
                data_sampling$stim_1,
                data_sampling$reward_stim_1),
          cbind(data_sampling$task_version,
                data_sampling$stim_2,
                data_sampling$reward_stim_2)))
  colnames(data_sampling) = c('task_version', 'variable', 'samples')
  data_sampling$task_version = as.factor(data_sampling$task_version)
  data_sampling$variable = as.factor(data_sampling$variable)
  levels(data_sampling$variable) = paste("v_stim_", levels(data_sampling$variable), sep='')
  data_sampling$samples = as.numeric(data_sampling$samples)
  
  # Plot sampling histogram
  plot_sampling = ggplot(data = data_sampling,
                         aes(x = samples,
                             fill = variable,
                             group = variable)) +
    geom_histogram(binwidth = 1,
                   alpha = 0.7,
                   position = "identity") +
    facet_wrap(~task_version) +
    labs(x = 'Samples',
         y = 'Count',
         title = 'Reward sampling of random subject') +
    theme_bw() +
    theme(plot.title = element_text(face = 'bold', hjust = 0.5),
          axis.title.x = element_blank())
  
  
  # Assess percentage correct of choices for each comparison
  # Add correct choice to model data
  stimuli = cbind(results$stim_1, results$stim_2)
  results$correct_choice = apply(stimuli, 1, function(x) max(c(x[1], x[2])))
  results$correct = results$choice == results$correct_choice
  # Add kind ov comparison to data (1v2, 2v3, 1v3)
  comparisons = cbind(results$stim_1, results$stim_2)
  results$comp = apply(comparisons,
                          1,
                          function(x) paste(sort(c(x[1], x[2]))[1],
                                            'v',
                                            sort(c(x[1], x[2]))[2],
                                            sep=''))
  # Get percentage correct
  # Delete last trials in task version (since NA)
  data_choice = results[complete.cases(results$choice)]
  data_choice = ddply(data_choice,
                      .(sub_id, task_version, comp),
                      summarize,
                      perc_correct = sum(as.numeric(correct))/length(correct))
  
  # Plot choice percentages
  data_plot = data_choice
  plot_choice = ggplot(data_plot,
                       aes(x = comp,
                           y = perc_correct,
                           fill = comp)) +
    geom_violin(draw_quantiles = c(0.25,0.5,0.75),
                alpha = 0.7) +
    geom_jitter(height = 0,
                width = 0.2,
                alpha = 0.7,
                size = 0.5) +
    stat_summary(fun = 'mean',
                 geom = 'point',
                 shape = 18,
                 size = 4,
                 color = 'white') +
    stat_summary(fun = 'mean',
                 geom = 'point',
                 shape = 5,
                 size = 3,
                 color = 'black',
                 stroke = 1) +
    scale_y_continuous(limits = c(0,1)) +
    labs(title = 'Percentage of correct choices') +
    theme_bw() +
    theme(plot.title = element_text(face = 'bold', hjust = 0.5)) +
    facet_wrap(~task_version)
  
  # Combine sampling and choice plot
  plot = plot_grid(plot_sampling,
                   plot_choice,
                   rel_heights = c(0.5,1),
                   ncol = 1,
                   align = 'v')
  
  # Return bias and plot
  output = list('perc_correct' = data_choice,
                'plot' = plot)
  return(output)
  
}