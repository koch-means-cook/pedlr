
# For knitting:
source_path = file.path(here::here(), 'code', fsep = .Platform$file.sep)

# Source functions required for this script
source(file.path(source_path, 'models', 'Pedlr.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'models', 'Pedlr_interdep.R',
                 fsep = .Platform$file.sep))
source(file.path(source_path, 'models', 'Rw.R',
                 fsep = .Platform$file.sep))

# Function to apply model to design
Apply_model = function(design,
                       model,
                       parameters,
                       init_values){
  
  # Arguments
  # design: Complete design of PEDLR experiment
  # model: String, name of model that should be applied
    # Options: pedlr_choice, pedlr_choice_alt, pedlr_choice_interdep
  # parameters: Data frame with one row, columns named after input arguments of model
  # init_values: List the length of number of task versions containing vector of three initialization values
    # for each task version (one vector entry for three distributions in each task version)
  
  # Argument checks
  # Raise error if too many/not enough initialization values for number of task versions
  n_task_version = max(design$task_version)
  if(length(init_values) != n_task_version){
    stop(paste("Number of provided initialization values (",
               length(init_values),
               " ) does not match number of task versions (",
               n_task_version,
               ") in design.",
               sep=''))
  }
  # Check if all init_values have three entries (one for each stimulus)
  for(list_count in seq(length(init_values))){
    if(length(init_values[[list_count]]) != 3){
      stop(paste("Number of initialization values (",
                 length(init_values[[list_count]]),
                 ") for task version ",
                 list_count,
                 " does not match number of distributions (3) in this task version.",
                 sep=''))
    }
  }
  
  
  # Create matrix to store model data
  data_model = matrix(NA,0,19)
  # Convert to data frame
  data_model = data.frame(data_model)
  colnames(data_model) = c('trial',
                           'stim_1',
                           'stim_2',
                           'reward_stim_1',
                           'reward_stim_2',
                           'comp_number',
                           'choice',
                           'choice_prob',
                           'forced_choice',
                           'v_stim_1',
                           'v_stim_2',
                           'v_stim_3',
                           'pe_stim_1',
                           'pe_stim_2',
                           'pe_stim_3',
                           'fpe_stim_1',
                           'fpe_stim_2',
                           'fpe_stim_3',
                           'task_version')
  
  # Apply model to each task version
  for(version_count in unique(design$task_version)){
    
    # Restrict data to specific task version
    design_version = subset(design, task_version == version_count)
    
    data = data.frame(matrix(NA,nrow(design_version),19))
    colnames(data) = colnames(data_model)
    
    # Add data important for model behavior
    data$trial = c(1:nrow(design_version))
    data$stim_1 = design_version$option_left
    data$stim_2 = design_version$option_right
    data$reward_stim_1 = design_version$reward_stim_1
    data$reward_stim_2 = design_version$reward_stim_2
    data$comp_number = design_version$comp_number
    
    # Apply specified model to data
    if(model == 'Pedlr'){
      sim = Pedlr(design = design_version,
                  params.alpha0 = parameters$alpha0,
                  params.alpha1 = parameters$alpha1,
                  params.temperature = parameters$temperature,
                  params.reward_space_ub = parameters$reward_space_ub,
                  choice_policy = parameters$choice_policy,
                  init_values = init_values[[version_count]])
    } else if(model == 'Pedlr_interdep'){
      sim = Pedlr_interdep(design = design_version,
                           params.alpha0 = parameters$alpha0,
                           params.alpha1 = parameters$alpha1,
                           params.interdep = parameters$interdep,
                           params.temperature = parameters$temperature,
                           params.reward_space_ub = parameters$reward_space_ub,
                           choice_policy = parameters$choice_policy,
                           init_values = init_values[[version_count]])
    } else if(model == 'Rw'){
      sim = Rw(design = design_version,
               params.alpha = parameters$alpha,
               params.temperature = parameters$temperature,
               params.reward_space_ub = parameters$reward_space_ub,
               choice_policy = parameters$choice_policy,
               init_values = init_values[[version_count]])
    }
    
    # Save model values, PE and fPE for ech trial and subject
    data$choice = sim$choices$choice
    data$choice_prob = sim$choices$choice_prob
    data$forced_choice = sim$choices$forced_choice
    data$v_stim_1 = sim$values$stim_1
    data$v_stim_2 = sim$values$stim_2
    data$v_stim_3 = sim$values$stim_3
    data$pe_stim_1 = sim$PE$stim_1
    data$pe_stim_2 = sim$PE$stim_2
    data$pe_stim_3 = sim$PE$stim_3
    data$fpe_stim_1 = sim$fPE$stim_1
    data$fpe_stim_2 = sim$fPE$stim_2
    data$fpe_stim_3 = sim$fPE$stim_3
    data$task_version = version_count
    
    # Append model data for each task_version
    data_model = rbind(data_model, data)
  }

  return(data_model)
}
