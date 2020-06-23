
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


# Source functions required for this script
base_path = source_path = file.path(here::here(), fsep = .Platform$file.sep)
derivatives_path = file.path(base_path, 'derivatives', 'simulation',
                             fsep = .Platform$file.sep)
source_path = file.path(base_path, 'code', fsep = .Platform$file.sep)
source(file.path(source_path, 'simulation', 'Sample_subjects.R',
                 fsep = .Platform$file.sep))
source(file.path(source_path, 'models', 'Pedlr.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'models', 'Pedlr_interdep.R',
                 fsep = .Platform$file.sep))
source(file.path(source_path, 'simulation', 'Apply_model.R',
                 fsep = .Platform$file.sep))
source(file.path(source_path, 'simulation', 'Model_results.R',
                 fsep = .Platform$file.sep))

Model_bias_value = function(n_subjects,
                            set_seed,
                            n_blocks,
                            perc_forced,
                            blocks_per_task,
                            dist_list,
                            model,
                            parameters,
                            init_values,
                            last_perc_of_trials,
                            average_subjects,
                            mean_value_calculation,
                            save_data,
                            save_file,
                            load_data,
                            load_file){
  
  # Arguments
  #   n_subjects: int, Number of participants to simulate designs for
  #   set_seed:   bool, set seed at design creation
  #   n_blocks:   int, Total number of blocks for design creation
  #   perc_forced:  int (10 or 20), Percentage of forced choice trials in design
  #   blocks_per_task:  int, how many blocks per task version (three distributions)
  #   dist_list:  list, list of distribution vectors including distribution type and variables to use,
  #                     three for each task version
  #   model: string (Pedlr, Pedlr_interdep), specifies model to use
  #   parameters: data.frame, All combinations of parameters to loop over, 1 column per parameters
  #   init_values:  list, list of three-entry vectors giving start values for all three stimuli in one 
  #                 task version  
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
  #   save_data:  TRUE/FALSE, Determines if output of data should be saved or not (as .tsv)
  #   save_file:  path, Path to save output to (must be .tsv)
  #   load_data:  TRUE/FALSE, Determines if data should be loaded (only plots will run in case)
  #   load_file:  path, Path to load data from
  
  # Adjust parameters for models to avoid (unneccessary) looping over irrelevant parameters
  if(model == 'Pedlr'){
    parameters = select(parameters,
                        alpha0,
                        alpha1,
                        temperature,
                        choice_policy,
                        reward_space_ub)
  }
  if(model == 'Pedlr_interdep'){
    parameters = select(parameters,
                        alpha0,
                        alpha1,
                        temperature,
                        choice_policy,
                        reward_space_ub,
                        interdep)
  }
  
  # Simulate designs for number of subjects
  data_sim = Sample_subjects(n_subjects = n_subjects,
                             set_seed = set_seed,
                             n_blocks = n_blocks,
                             perc_forced = perc_forced,
                             blocks_per_task = blocks_per_task,
                             dist_list = dist_list)
  
  # Plot: Sampling histogram
  # Create template to check sampling of each distribution
  data_plot = data.frame(matrix(NA, 0, 3))
  colnames(data_plot) = c('task_version', 'stimulus', 'samples')
  
  # Pick subject to plot sampled distributions
  design = subset(data_sim, sub_id == 1)
  
  # For each task version
  for(version_count in unique(design$task_version)){
    data = subset(design, task_version == version_count)
    # For each of the three distributions per task version
    for(stim_count in sort(unique(data$option_left))){
      # Get data frame of rewards sampled for each stimulus (regardless if presented
      # left or right) and task version
      samples = data.frame(c(data$reward_stim_1[data$option_left == stim_count],
                             data$reward_stim_2[data$option_right == stim_count]))
      samples$task_version = version_count
      samples$stimulus = stim_count
      colnames(samples) = c('samples', 'task_version', 'stimulus')
      # Add samples to plotting data
      data_plot = rbind(data_plot, samples)
    }
  }
  
  # Prepare for plotting
  data_plot$stimulus = as.factor(data_plot$stimulus)
  # Plot sampling for each task version and stimulus
  p_sampling = ggplot(data = data_plot,
                      aes(x = samples,
                          fill = stimulus,
                          group = stimulus)) +
    geom_histogram(binwidth = 1,
                   position = 'identity',
                   alpha = 0.7) +
    # facet_grid(rows = vars(task_version),
    #            cols = vars(stimulus))
    facet_wrap(~ task_version) +
    theme_bw()
  
  
  # Assess bias
  # Create template
  data_model = ddply(data_sim,
                     "sub_id",
                     Apply_model,
                     model = model,
                     parameters = parameters[1,],
                     init_values = init_values)
  data_bias = Model_results(results = data_model,
                            last_perc_of_trials = last_perc_of_trials,
                            average_subjects = average_subjects,
                            mean_value_calculation = mean_value_calculation)$bias
  data_bias = cbind(data_bias[0,], parameters[0,])
  
  # Load data in case specified, skip creating data in this case
  if(load_data){
    load_file = load_file
    data_bias = read.table(load_file, header = TRUE, sep='\t')
    # If no data to load, create data
  } else if(!load_data){
    # Loop over all parameter combinations
    for(run_count in seq(nrow(parameters))){
      # Apply model with different parameters to subjects
      data_model = ddply(data_sim,
                         "sub_id",
                         Apply_model,
                         model = model,
                         parameters = parameters[run_count,],
                         init_values = init_values)
      # Get bias for specific parameters
      results = Model_results(results = data_model,
                              last_perc_of_trials = last_perc_of_trials,
                              average_subjects = average_subjects,
                              mean_value_calculation = mean_value_calculation)
      data = cbind(results$bias,
                   do.call("rbind",
                           replicate(nrow(data),
                                     parameters[run_count,],
                                     simplify = FALSE)))
      # Append results for parameters
      data_bias = rbind(data_bias, data)
    }
  }
  
  # Add model variables to data
  data_bias$last_perc_of_trials = last_perc_of_trials
  data_bias$average_subjects = as.numeric(average_subjects)
  data_bias$mean_value_calculation = mean_value_calculation
  
  # If specified, save bias simulation
  if(save_data){
    write.table(data_bias, save_file, sep='\t', row.names = FALSE)
  }
  
  # Plots: Bias
  p_bias_softmax = NA
  p_bias_greedy = NA
  # Adjust data frame for plotting
  data_plot = data_bias
  # Kick out learning rates of 0 due to too large biases
  data_plot = subset(data_plot, alpha0 > 0 & alpha1 > 0)
  data_plot$alpha0 = as.factor(data_plot$alpha0)
  data_plot$alpha1 = as.factor(data_plot$alpha1)
  data_plot$temperature = as.factor(data_plot$temperature)
  
  # Softmax choice
  if('softmax' %in% unique(data_plot$choice_policy)){
    p_bias_softmax = ggplot(data = subset(data_plot, choice_policy == 'softmax'),
                            aes(x = as.factor(alpha0),
                                y = as.factor(alpha1),
                                fill = bias)) +
      geom_raster() + 
      scale_fill_viridis(option='D') +
      facet_grid(variable ~ task_version + temperature) +
      labs(title = paste('Pedlr - Softmax choice: Influence on bias, ',
                         unique(data_plot$mean_value_calculation),
                         sep=''),
           x = 'alpha0',
           y = 'alpha1') +
      theme(plot.title = element_text(face = 'bold', hjust = 0.5))
    
    # Add sampling plot to bias
    p_bias_softmax = plot_grid(p_sampling, p_bias_softmax,
                               rel_heights = c(0.5,1),
                               ncol = 1)
  }
  
  # Greedy choice
  if('greedy' %in% unique(data_plot$choice_policy)){
    p_bias_greedy = ggplot(data = subset(data_plot, choice_policy == 'greedy'),
                           aes(x = as.factor(alpha0),
                               y = as.factor(alpha1),
                               fill = bias)) +
      geom_raster() + 
      scale_fill_viridis(option='D') +
      facet_grid(variable ~ task_version) +
      labs(title = paste('Pedlr - Greedy choice: Influence on bias, ',
                         unique(data_plot$mean_value_calculation),
                         sep=''),
           x = 'alpha0',
           y = 'alpha1') +
      theme(plot.title = element_text(face = 'bold', hjust = 0.5))
    
    # Add sampling plot to bias
    p_bias_greedy = plot_grid(p_sampling, p_bias_greedy,
                              rel_heights = c(0.5,1),
                              ncol = 1)
  }

  # Output
  output = list(bias = data_bias,
                plot_softmax = p_bias_softmax,
                plot_greedy = p_bias_greedy)
  return(output)
  
  
}