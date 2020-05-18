
# Source functions required for this script
base_path = file.path('/Volumes', 'MPRG-Neurocode', 'Users', 'christoph', 'pedlr')
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
  
  # Create template to append results
  data_sim = Sample_subjects(n_subjects = 3,
                             set_seed = TRUE,
                             n_blocks = n_blocks,
                             perc_forced = perc_forced,
                             blocks_per_task = blocks_per_task,
                             dist_list = dist_list)
  # Apply model to data
  data_model = ddply(data_sim,
                     "sub_id",
                     Apply_model,
                     model = 'Pedlr',
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
                                 set_seed = TRUE,
                                 n_blocks = n_blocks,
                                 perc_forced = perc_forced,
                                 blocks_per_task = blocks_per_task,
                                 dist_list = dist_list)
      
      # Move samples of edge distributions closer to center by amount of shrinkage
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
                         model = 'Pedlr',
                         parameters = parameters[param_count],
                         init_values = init_values)
      
      # Get percentage correct
      perc_correct = Model_perc_correct(data_model)
      data = perc_correct$perc_correct
      
      # Add distance to data
      data$distance_shrinkage = shrinkage
      data$alpha0 = parameters$alpha0[param_count]
      data$alpha1 = parameters$alpha1[param_count]
      data$temperature = parameters$temperature[param_count]
      data$choice_policy = parameters$choice_policy[param_count]
      
      # Append data for each shrinkage
      data_choice = rbind(data_choice, data)
    }
  }
  
  if(save_data){
    write.table(data_choice, save_file, sep='\t', row.names = FALSE)
  }
  
  return(data_choice)
  
}