library(optimr)
library(here)
library(data.table)
library(magrittr)

# Load models
source_path = file.path(here::here(), 'code', 'models',
                        fsep = .Platform$file.sep)
source_files = list.files(source_path, pattern = "[.][rR]$",
                          full.names = TRUE, recursive = TRUE)
invisible(lapply(source_files, function(x) source(x)))

# Load pre-written functions
source_path = file.path(here::here(), 'code', 'utils',
                        fsep = .Platform$file.sep)
source_files = list.files(source_path, pattern = "[.][rR]$",
                          full.names = TRUE, recursive = TRUE)
invisible(lapply(source_files, function(x) source(x)))

# Load model fitting
source_path = file.path(here::here(), 'code', 'model_fitting',
                        'Fit_model.R',
                        fsep = .Platform$file.sep)
source(source_path)

# model = 'Rw'
# design_path = file.path(here::here(), 'pedlr-task', 'client', 'public',
#                         'designs', 'design-01_run-1.tsv',
#                         fsep = .Platform$file.sep)
# true_parameters = c(0.3, 7)
# fit_start_values = c(0.6,2)
# fit_lb = c(0,1)
# fit_ub = c(1,10)

# real_data = Load_data() %>% 
#   Prepare_data_for_fit(.)

Parameter_recovery = function(model,
                              design_path,
                              true_parameters,
                              fit_start_values,
                              fit_lb,
                              fit_ub){
  
  # Set up model list
  model_list = data.table('model_name' = c('Rw', 'Pedlr_simple', 'Pedlr', 'Pedlr_fixdep', 'Pedlr_interdep'),
                          'n_parameters' = c(2,2,3,3,4))
  
  # See if provided model is in model list
  if(!model %in% model_list$model_name){
    stop('Chosen model is not specified. Check for typos')
  }
  
  # See if number of parameters provided fits model
  n_parameters = model_list[model_name == model]$n_parameters
  if(length(fit_start_values) != n_parameters |
     length(fit_lb) != n_parameters | 
     length(fit_ub) != n_parameters |
     length(true_parameters) != n_parameters){
    stop('Mismatch between provided n parameters and n parameters of model')
  }
  
  # Give message to user
  message('      ######### True params #########')
  message('      Ground truth: ', paste(true_parameters, collapse = '  '))
  
  # Load design
  design = data.table::fread(design_path, sep = '\t', na.strings = 'n/a')
  
  # Apply model to get simulated data
  if(model == 'Rw'){
    model_data = Rw(design = design,
                    params.alpha = true_parameters[1],
                    params.temperature = true_parameters[2],
                    params.reward_space_ub = 100,
                    choice_policy = 'softmax',
                    init_values = c(50,50,50))
  } else if(model == 'Pedlr_simple'){
    model_data = Pedlr_simple(design = design,
                              params.alpha1 = true_parameters[1],
                              params.temperature = true_parameters[2],
                              params.reward_space_ub = 100,
                              choice_policy = 'softmax',
                              init_values = c(50,50,50))
  } else if(model == 'Pedlr'){
    model_data = Pedlr(design = design,
                       params.alpha0 = true_parameters[1],
                       params.alpha1 = true_parameters[2],
                       params.temperature = true_parameters[3],
                       params.reward_space_ub = 100,
                       choice_policy = 'softmax',
                       init_values = c(50,50,50))
  } else if(model == 'Pedlr_fixdep'){
    model_data = Pedlr_fixdep(design = design,
                              params.alpha0 = true_parameters[1],
                              params.alpha1 = true_parameters[2],
                              params.temperature = true_parameters[3],
                              params.reward_space_ub = 100,
                              choice_policy = 'softmax',
                              init_values = c(50,50,50))
  } else if(model == 'Pedlr_interdep'){
    model_data = Pedlr_interdep(design = design,
                              params.alpha0 = true_parameters[1],
                              params.alpha1 = true_parameters[2],
                              params.interdep = true_parameters[3],
                              params.temperature = true_parameters[4],
                              params.reward_space_ub = 100,
                              choice_policy = 'softmax',
                              init_values = c(50,50,50))
  }
  
  
  # Add simulated columns (created by model) to data
  data = design %>%
    .[, choice := model_data$choices$choice] %>%
    # Calculate outcome of choices
    .[, trial := seq(.N)] %>%
    .[, chosen_side := which(c(option_left, option_right) == choice),
      by = 'trial'] %>%
    .[,outcome := c(reward_stim_1, reward_stim_2)[chosen_side],
      by = 'trial'] %>%
    # Mark errors (wrong choice on forced choices, very very rare due to simulation)
    .[, error := FALSE] %>%
    .[forced_left == 1 & chosen_side == 2, error := TRUE] %>%
    .[forced_right == 1 & chosen_side == 1, error := TRUE]
  
  # Fit model to simulated data
  results = Fit_model(data = data,
                      model = model,
                      start_values = fit_start_values,
                      lb = fit_lb,
                      ub = fit_ub)
  
  return(results)
  
}
