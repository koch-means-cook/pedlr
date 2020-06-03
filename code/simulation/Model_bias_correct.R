
# Source functions required for this script
base_path = file.path('/Volumes', 'MPRG-Neurocode', 'Users', 'christoph',
                      'pedlr')
derivatives_path = file.path(base_path, 'derivatives', 'simulation')
source_path = file.path(base_path, 'code', 'simulation')
source(file.path(source_path, 'Sample_subjects.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'Pedlr.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'Pedlr_interdep.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'Apply_model.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'Model_results.R', fsep = .Platform$file.sep))

Model_bias_correct = function(n_subjects,
                              set_seed,
                              n_blocks,
                              perc_forced,
                              blocks_per_task,
                              dist_list,
                              model,
                              parameters,
                              init_values,
                              shrink_distance_vec,
                              save_data,
                              save_file,
                              load_data,
                              load_file){
  
  
  # Adjust parameters for models to avoid (unneccessary) looping over irrelevant
  # parameters
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
  
  # Create template to append results
  data_sim = Sample_subjects(n_subjects = 3,
                             set_seed = set_seed,
                             n_blocks = n_blocks,
                             perc_forced = perc_forced,
                             blocks_per_task = blocks_per_task,
                             dist_list = dist_list)
  
  # Load previously created data if specified
  if(load_data){
    data_choice = read.table(file, header = TRUE, sep = '\t')
  }
  
  # If not specified to load data, assess bias based on parameters
  if(!load_data){
    
    # Apply model to data
    data_model = ddply(data_sim,
                       "sub_id",
                       Apply_model,
                       model = model,
                       parameters = parameters[1,],
                       init_values = init_values)
    data_choice = Model_perc_correct(data_model)
    data_choice = data_choice$perc_correct
    data_choice$distance_shrinkage = NA
    data_choice = data_choice[0,]
    data_choice = cbind(data_choice, parameters[0,])
    
    # For all provided parameters
    for(param_count in seq(nrow(parameters))){
      
      # Stepwise minimize distance between means
      for(shrinkage in shrink_distance_vec){
        
        # Simulate data
        data_sim = Sample_subjects(n_subjects = n_subjects,
                                   set_seed = set_seed,
                                   n_blocks = n_blocks,
                                   perc_forced = perc_forced,
                                   blocks_per_task = blocks_per_task,
                                   dist_list = dist_list)
        
        # Move samples of edge distributions closer to center by amount of 
        # shrinkage
        data_sim$reward_stim_1[data_sim$option_left == 1] = (
          data_sim$reward_stim_1[data_sim$option_left == 1] + shrinkage)
        data_sim$reward_stim_1[data_sim$option_left == 3] = (
          data_sim$reward_stim_1[data_sim$option_left == 3] - shrinkage)
        data_sim$reward_stim_2[data_sim$option_right == 1] = (
          data_sim$reward_stim_2[data_sim$option_right == 1] + shrinkage)
        data_sim$reward_stim_2[data_sim$option_right == 3] = (
          data_sim$reward_stim_2[data_sim$option_right == 3] - shrinkage)
        
        # Apply model to data
        data_model = ddply(data_sim,
                           "sub_id",
                           Apply_model,
                           model = model,
                           parameters = parameters[param_count],
                           init_values = init_values)
        
        # Get percentage correct
        perc_correct = Model_perc_correct(data_model)
        data = perc_correct$perc_correct
        
        # Add distance to data
        data = cbind(data,
                     do.call("rbind",
                             replicate(nrow(data),
                                       parameters[param_count,],
                                       simplify = FALSE)))
        data$distance_shrinkage = shrinkage
        
        # Append data for each shrinkage
        data_choice = rbind(data_choice, data)
      }
    }
  }
  
  if(save_data){
    write.table(data_choice, save_file, sep='\t', row.names = FALSE)
  }
  
  # Plot sampling
  # Get sampling of rewards per task version from random subject
  data_sampling = subset(data_sim, sub_id == sample(data_sim$sub_id, 1))
  data_sampling = data.frame(
    rbind(cbind(data_sampling$task_version,
                data_sampling$option_left,
                data_sampling$reward_stim_1),
          cbind(data_sampling$task_version,
                data_sampling$option_right,
                data_sampling$reward_stim_2)))
  colnames(data_sampling) = c('task_version', 'variable', 'samples')
  data_sampling$task_version = as.factor(data_sampling$task_version)
  data_sampling$variable = as.factor(data_sampling$variable)
  levels(data_sampling$variable) = paste("v_stim_",
                                         levels(data_sampling$variable),
                                         sep='')
  data_sampling$samples = as.numeric(data_sampling$samples)
  
  # Plot sampling histogram
  p_sampling = ggplot(data = data_sampling,
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
  
  # Plot bias
  data_plot = data_choice
  data_plot = ddply(data_plot,
                    .(task_version, comp, distance_shrinkage, alpha0, alpha1,
                      temperature, choice_policy),
                    summarize,
                    perc_correct = mean(perc_correct))
  
  # Set limits according to lowest value (default c(0.45,1))
  limits = c(0.45,1)
  if(min(data_plot$perc_correct) < 0.45){
    limits = c(min(data_plot$perc_correct), 1)
  }
  
  p_bias = ggplot(data_plot, aes(x = comp,
                                 y = perc_correct,
                                 group = distance_shrinkage,
                                 color = distance_shrinkage)) +
    geom_hline(yintercept = 0.5, linetype='dashed', alpha=0.5) +
    geom_line() + 
    scale_color_viridis(option='D') +
    scale_y_continuous(limits=limits) +
    facet_grid(temperature + alpha1 ~ task_version + alpha0) +
    labs(title = 'Changing perc_correct for different parameters\nand approaching distributions') +
    theme(legend.position = 'right',
          plot.title = element_text(face = 'bold', hjust = 0.5))
  
  # Combine plots
  plot = plot_grid(p_sampling,
                   p_bias,
                   rel_heights = c(0.15,1),
                   ncol = 1,
                   axis = 'lr',
                   align = 'v')
  
  # Return data and plot
  outcome = list('data_bias_correct' = data_choice,
                 'plot' = plot)
  return(outcome)
  
}