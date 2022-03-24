library(data.table)
library(here)
library(optparse)
library(magrittr)

source(file.path(here::here(), 'code', 'parameter_recovery', 'Parameter_recovery.R',
                 fsep = .Platform$file.sep))

# model = 'Rw'
# design_path = file.path(here::here(), 'pedlr-task', 'client', 'public',
#                         'designs', 'design-01_run-1.tsv',
#                         fsep = .Platform$file.sep)
# true_parameters = c(runif(1,0,1),runif(1,1,10))
# fit_start_values = c(runif(1,0,1),runif(1,1,10))
# fit_lb = c(0,1)
# fit_ub = c(1,10)
# random_true_parameters = TRUE
# random_fit_start_values = TRUE
# n_iter = 2


Parameter_recovery_wrapper = function(output_path,
                                      model,
                                      design_path_run1,
                                      design_path_run2,
                                      true_parameters,
                                      fit_start_values,
                                      fit_lb,
                                      fit_ub,
                                      random_true_parameters,
                                      random_fit_start_values,
                                      random_design_run_order,
                                      n_iter){
  
  # Set parameter names for model
  if(model == 'Rw'){
    p_names = c('alpha', 'temperature')
  } else if(model == 'Pedlr_simple'){
    p_names = c('alpha1', 'temperature')
  } else if(model == 'Pedlr_simple_const'){
    p_names = c('alpha1', 'temperature')
  } else if(model == 'Pedlr'){
    p_names = c('alpha0', 'alpha1', 'temperature')
  } else if(model == 'Pedlr_step'){
    p_names = c('alpha0', 'alpha1', 'temperature')
  } else if(model == 'Pedlr_fixdep'){
    p_names = c('alpha0', 'alpha1', 'temperature')
  }else if(model == 'Pedlr_interdep'){
    p_names = c('alpha0', 'alpha1', 'interdep', 'temperature')
  }
  
  # Initialize output as data table
  output = data.table()
  
  message('\n')
  message('--------------------------')
  
  # For iterations of fitting
  for(i_iter in seq(n_iter)){
    
    # Give message to user
    message(paste('Iteration: ', i_iter, sep = ''))
    
    # If specified, take new random values for each iteration
    # True parameters
    if(random_true_parameters){
      true_parameters = c()
      for(i in seq(length(fit_lb))){
        # Get new random value between lower and upper boundary of each parameter
        val = round(runif(1, fit_lb[i], fit_ub[i]), 3)
        true_parameters = c(true_parameters, val)
      }
    }
    # Starting values for fit
    if(random_fit_start_values){
      fit_start_values = c()
      for(i in seq(length(fit_lb))){
        # Get new random value between lower and upper boundary of each parameter
        val = round(runif(1, fit_lb[i], fit_ub[i]), 3)
        fit_start_values = c(fit_start_values, val)
      }
    }
    # Design run order
    if(random_design_run_order){
      randomize = c(design_path_run1,
                    design_path_run2)
      randomize = sample(randomize)
      design_path_run1 = randomize[1]
      design_path_run2 = randomize[2]
    }
    
    # Give message to user
    message('   Fitting Designs: ',
            basename(design_path_run1),
            ' and ',
            basename(design_path_run2),
            '...')
    
    # Run parameter recovery
    result = Parameter_recovery(model,
                                design_path_run1,
                                design_path_run2,
                                true_parameters,
                                fit_start_values,
                                fit_lb,
                                fit_ub)
    
    # Form model fit into returnable data frame
    result = as.data.table(result) %>%
      .[, ':='(para = p_names,
               iter = i_iter,
               design_run1 = basename(design_path_run1),
               design_run2 = basename(design_path_run2),
               true = true_parameters)] %>%
      .[, data.table::setcolorder(., c("iter",
                                       "design_run1",
                                       "design_run2",
                                       "para",
                                       "true"))]
    
    # Concatenate result of each iteration
    output = rbind(output, result)
    
  }
  
  # Give message to user
  message('Writing output...')
  
  # Write output
  data.table::fwrite(x = output,
                     file = output_path,
                     sep = '\t',
                     na = 'n/a',
                     row.names = FALSE,
                     col.names = TRUE)
  
  # Give message to user
  message('\n...Le Fin')
  
}

# Function to split list inputs (used as callback function during argument parsing)
split_list = function(object,
                      flag,
                      value,
                      parser){
  x = as.numeric(unlist(strsplit(value, ',')))
  return(x)
}






# Create options to pass to script
option_list = list(
  make_option(c('-o', '--output_path'),
              type='character',
              default = NULL,
              help = 'Path to data file fitting results should be saved to (includes name of target file)',
              metavar = 'OUTPUT_PATH'),
  make_option(c('-m', '--model'),
              type='character',
              default = NULL,
              help = 'Name of model fitting should be based on',
              metavar = 'MODEL'),
  make_option(c('-j', '--design_path_run1'),
              type='character',
              default = NULL,
              help = 'Path to design file used to simulate first run of data',
              metavar = 'DESIGN_PATH_RUN1'),
  make_option(c('-k', '--design_path_run2'),
              type='character',
              default = NULL,
              help = 'Path to design file used to simulate second run of data',
              metavar = 'DESIGN_PATH_RUN2'),
  make_option(c('-t', '--true_parameters'),
              type='character',
              default = NULL,
              callback = split_list,
              help = 'List of true parameter values matching the number of parameters of the model. Used to simulate ground truth data. Example: 0.1,0.4,7',
              metavar = 'TRUE_PARAMETERS'),
  make_option(c('-s', '--fit_start_values'),
              type='character',
              default = NULL,
              callback = split_list,
              help = 'List of starting values matching the number of parameters of the model. Example: 0.1,0.4,7',
              metavar = 'START_VALUES'),
  make_option(c('-l', '--fit_lb'),
              type='character',
              default = NULL,
              callback = split_list,
              help = 'List of values matching the number of parameters of the model giving the lower bound for the fitting process. Example: 0,0,0',
              metavar = 'LOWER_BOUND'),
  make_option(c('-u', '--fit_ub'),
              type='character',
              default = NULL,
              callback = split_list,
              help = 'List of values matching the number of parameters of the model giving the upper bound for the fitting process. Example: 1,1,10',
              metavar = 'UPPER_BOUND'),
  make_option(c('-r', '--random_true_parameters'),
              type='character',
              default = NULL,
              help = 'TRUE or FALSE if random true parameters should be used',
              metavar = 'RANDOM_TRUE_PARAMETERS'),
  make_option(c('-f', '--random_fit_start_values'),
              type='character',
              default = NULL,
              help = 'TRUE or FALSE if random start values should be used',
              metavar = 'RANDOM_FIT_START_VALUES'),
  make_option(c('-z', '--random_design_run_order'),
              type='character',
              default = NULL,
              help = 'TRUE or FALSE if first and second run of design should be randomized in order',
              metavar = 'RANDOM_DESIGN_RUN_ORDER'),
  make_option(c('-n', '--n_iter'),
              type='integer',
              default = NULL,
              help = 'Int giving number of iterations per fitting process. If random_start_values == TRUE new random starting values will be set for each iteration',
              metavar = 'N_ITER'))

# provide options in list to be callable by script
opt_parser = OptionParser(option_list = option_list)
opt = parse_args(opt_parser)

# Cal wrapper with command line inputs
Parameter_recovery_wrapper(output_path = opt$output_path,
                           model = opt$model,
                           design_path_run1 = opt$design_path_run1,
                           design_path_run2 = opt$design_path_run2,
                           true_parameters = opt$true_parameters,
                           fit_start_values = opt$fit_start_values,
                           fit_lb = opt$fit_lb,
                           fit_ub = opt$fit_ub,
                           random_true_parameters = opt$random_true_parameters,
                           random_fit_start_values = opt$random_fit_start_values,
                           random_design_run_order = opt$random_design_run_order,
                           n_iter = opt$n_iter)

# Rscript Parameter_recovery_wrapper.R --output_path "/Users/koch/Docs/pedlr/derivatives/parameter_recovery/_bla.tsv" --model Rw --design_path_run1 "/Users/koch/Docs/pedlr/pedlr-task/client/public/designs/design-01_run-1.tsv" --design_path_run2 "/Users/koch/Docs/pedlr/pedlr-task/client/public/designs/design-01_run-2.tsv" --true_parameters 0.3,7 --fit_start_values 0.5,5 --fit_lb 0,1 --fit_ub 0.1,10 --random_true_parameters TRUE --random_fit_start_values TRUE --random_design_run_order TRUE --n_iter 1
