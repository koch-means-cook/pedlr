
library(reshape2)
library(ggplot2)
library(plotly)
library(plyr)
library(Rfast)
library(data.table)
library(knitr)
#library(rstudioapi)
#library(here)
library(viridis)
library(cowplot)
library(here)
library(optparse)



Model_results = function(results,
                         last_perc_of_trials,
                         average_subjects,
                         mean_value_calculation){
  
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
  
  
  # Get sampling mean (considering all rewards sampled)
  # Mean of all rewards shown on left (left stimulus)
  sampling_mean_left = ddply(results,
                             .(sub_id, task_version, stim_1),
                             summarize,
                             sampling_mean = mean(reward_stim_1))
  # Mean of rewards shown on right (right stimulus)
  sampling_mean_right = ddply(results,
                              .(sub_id, task_version, stim_2),
                              summarize,
                              sampling_mean = mean(reward_stim_2))
  # Mean of left and right mean to get average over all samples
  sampling_mean = (sampling_mean_left$sampling_mean + sampling_mean_right$sampling_mean) / 2
  # Form into data frame for plotting
  data_mean_sampling = sampling_mean_left
  data_mean_sampling$sampling_mean = sampling_mean
  colnames(data_mean_sampling) = c('sub_id', 'task_version', 'variable', 'sampling_mean')
  # Use factor levels according to results data
  data_mean_sampling$variable = as.factor(data_mean_sampling$variable)
  levels(data_mean_sampling$variable) = paste('v_stim_', levels(data_mean_sampling$variable), sep='')
  
  # Cut down data to last percentage of trials which should be used
  trial_cutoff = round(max(results$trial) / 100 * (100 - last_perc_of_trials))
  data_mean_value = subset(results, trial >= trial_cutoff)
  # Count number of trials for each stimulus in specified upper percentage of data
  counts = data.table(ddply(data_mean_value,
                            .(task_version, sub_id),
                            function(data_mean_value) table(data_mean_value$variable)))
  counts = melt(counts, id.vars = c('sub_id', 'task_version'))
  # Raise error in case there were less than 5 trials of at least one option
  # in the upper percentage of trials
  if(any(counts$value < 5)){
    stop(paste("There are less than 5 trials in the upper ",
               last_perc_of_trials,
               "% of trials (not sufficient to construct a mean).",
               sep=''))
  }
  # Get mean of estimated value (all trials or chosen)
  if(mean_value_calculation == 'all_trials'){
    data_mean_value = ddply(data_mean_value,
                            .(sub_id, task_version, variable),
                            summarize,
                            mean_value = mean(value))
  } else if(mean_value_calculation == 'chosen_trials'){
    # Get only trials in which specific stimulus was chosen and updated
    data_mean_value = subset(data_mean_value, choice == as.numeric(substr(variable, 8,8)))
    # Count number of chosen trials for each stimulus in specified upper percentage of data
    counts = data.table(ddply(data_mean_value,
                              .(task_version, sub_id),
                              function(data_mean_value) table(data_mean_value$variable)))
    counts = melt(counts, id.vars = c('sub_id', 'task_version'))
    # Raise error in case one option was not chosen in the upper percentage of trials
    if(any(counts$value < 3)){
      stop(paste("At least one stimulus in the upper ",
                 last_perc_of_trials,
                 "% of trials was chosen chosen less than three times in at least one subject ",
                 "and at least one task version (insufficient to construct mean).\n",
                 sep=''))
    }
    data_mean_value = ddply(data_mean_value,
                            .(sub_id, task_version, variable),
                            summarize,
                            mean_value = mean(value))
  }
  
  # Plot for averaged subjects
  if(average_subjects){
    # Average data over subjects
    data_plot = ddply(results,
                      .(trial, task_version, variable),
                      summarize,
                      mean_value = mean(value),
                      sd_value = sd(value))
    data_mean_sampling = ddply(data_mean_sampling,
                               .(task_version, variable),
                               summarize,
                               sampling_mean = mean(sampling_mean))
    data_mean_value = ddply(data_mean_value,
                            .(task_version, variable),
                            summarize,
                            mean_value = mean(mean_value))
    counts = ddply(counts,
                   .(task_version, variable),
                   summarize,
                   mean_count = mean(value))
  
    # Plot average value for each trial
    plot_value = ggplot(data_plot,
                        aes(x = trial,
                            y = mean_value,
                            group = variable,
                            color = variable,
                            fill = variable)) +
      # Add line with mean over all rewards
      geom_hline(data = data_mean_sampling,
                 aes(yintercept = sampling_mean,
                     color = variable),
                 alpha = 0.5,
                 linetype = 'dashed') +
      # Add line and ribbon of mean value per trial over participants
      geom_ribbon(aes(ymax = mean_value + sd_value,
                      ymin = mean_value - sd_value),
                  alpha = 0.3,
                  size = 0) +
      geom_line() +
      # Add line with mean over value estimates
      geom_hline(data = data_mean_value,
                 aes(yintercept = mean_value,
                     color = variable),
                 linetype = 'solid') +
      facet_wrap(~task_version) +
      scale_y_continuous(limits = c(0,100)) +
      labs(x = 'Trial',
           y = 'Value') +
      theme_bw() +
      theme(plot.title = element_text(face = 'bold', hjust = 0.5))
    
    if(mean_value_calculation == 'all_trials'){
      plot_value = plot_value +
        labs(title = paste("Value estimate, real mean (dashed),",
                           " estimated mean (last ",
                           last_perc_of_trials,
                           "% of trials, solid)\n",
                           "Mean over all trials",
                           sep = ''))
    } else if(mean_value_calculation == 'chosen_trials'){
      plot_value = plot_value +
        labs(title = paste("Value estimate, real mean (dashed),",
                           " estimated mean (last ",
                           last_perc_of_trials,
                           "% of trials, solid)\n",
                           "Mean over chosen trials",
                           sep = ''))
    }
    
    # Combine plots
    plot = plot_grid(plot_sampling,
                     plot_value,
                     ncol = 1,
                     rel_heights = c(1,2))
    
    # Combine results
    data_results = data_mean_sampling
    data_results$mean_value = data_mean_value$mean_value
    data_results$n_values_used = counts$mean_count
    
    # In case of plot for each subject
  } else if(average_subjects == FALSE){
    
    # Plot average value for each trial for each subject
    plot_value = ggplot(results,
                        aes(x = trial,
                            y = value,
                            group = variable,
                            color = variable)) +
      # Add line with mean over all rewards
      geom_hline(data = data_mean_sampling,
                 aes(yintercept = sampling_mean,
                     color = variable),
                 alpha = 0.5,
                 linetype = 'dashed') +
      # Add line of mean value per trial over participants
      geom_line() +
      # Add line with mean over value estimates
      geom_hline(data = data_mean_value,
                 aes(yintercept = mean_value,
                     color = variable),
                 linetype = 'solid') +
      facet_grid(cols = vars(task_version), rows = vars(sub_id)) +
      labs(x = 'Trial',
           y = 'Value') +
      theme_bw() +
      theme(plot.title = element_text(face = 'bold', hjust = 0.5))
    
    if(mean_value_calculation == 'all_trials'){
      plot_value = plot_value +
        labs(title = paste("Value estimate, real mean (dashed),",
                           " estimated mean (last ",
                           last_perc_of_trials,
                           "% of trials, solid)\n",
                           "Mean over all trials",
                           sep = ''))
    } else if(mean_value_calculation == 'chosen_trials'){
      plot_value = plot_value +
        labs(title = paste("Value estimate, real mean (dashed),",
                           " estimated mean (last ",
                           last_perc_of_trials,
                           "% of trials, solid)\n",
                           "Mean over chosen trials",
                           sep = ''))
    }
    
    # Combine plots
    plot = plot_grid(plot_sampling,
                     plot_value,
                     ncol = 1,
                     rel_heights = c(1,length(unique(data_mean_sampling$sub_id))))
    
    # Combine results
    data_results = data_mean_sampling
    data_results$mean_value = data_mean_value$mean_value
    # Sort counts so structure is the same as in other results
    setorder(counts, sub_id, task_version)
    data_results$n_values_used = counts$value
  }
  
  # Add bias column between sampling mean and estimated mean value
  data_results$bias = data_results$mean_value - data_results$sampling_mean
  
  # Form list to return plot and result
  output = list('bias' = data_results, 'plot' = plot)
  
  return(output)
  
}

