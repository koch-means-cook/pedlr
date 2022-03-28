library(data.table)
library(here)
library(optparse)
library(magrittr)

source(file.path(here::here(), 'code', 'Model_recovery', 'Model_recovery.R',
                 fsep = .Platform$file.sep))

# gen_model = 'Rw'
# design_path_run1 = file.path(here::here(), 'pedlr-task', 'client', 'public',
#                         'designs', 'design-01_run-1.tsv',
#                         fsep = .Platform$file.sep)
# design_path_run2 = file.path(here::here(), 'pedlr-task', 'client', 'public',
#                         'designs', 'design-01_run-2.tsv',
#                         fsep = .Platform$file.sep)
# gen_parameters = c(runif(1,0,1),runif(1,1,10))
# gen_lb = c(0,1)
# gen_ub = c(1,10)
# random_gen_parameters = TRUE
# random_design_run_order = TRUE
# n_iter = 2


Model_recovery_wrapper = function(output_path,
                                  gen_model,
                                  design_path_run1,
                                  design_path_run2,
                                  gen_parameters,
                                  gen_lb,
                                  gen_ub,
                                  random_gen_parameters,
                                  random_design_run_order,
                                  n_iter){
  
  # Set parameter names for model
  if(gen_model == 'Rw'){
    p_names = c('alpha', 'temperature')
  } else if(gen_model == 'Pedlr_simple'){
    p_names = c('alpha1', 'temperature')
  } else if(gen_model == 'Pedlr_simple_const'){
    p_names = c('alpha1', 'temperature')
  } else if(gen_model == 'Pedlr'){
    p_names = c('alpha0', 'alpha1', 'temperature')
  } else if(gen_model == 'Pedlr_step'){
    p_names = c('alpha0', 'alpha1', 'temperature')
  } else if(gen_model == 'Pedlr_fixdep'){
    p_names = c('alpha0', 'alpha1', 'temperature')
  }else if(gen_model == 'Pedlr_interdep'){
    p_names = c('alpha0', 'alpha1', 'interdep', 'temperature')
  }
  
  # Get list of all models and parameter names
  model_list = list('models' = c('Rw',
                                 'Pedlr_simple',
                                 'Pedlr_simple_const',
                                 'Pedlr',
                                 'Pedlr_step',
                                 'Pedlr_fixdep',
                                 'Pedlr_interdep'),
                    'p_names' = list('Rw' = c('alpha', 'temperature'),
                                     'Pedlr_simple' = c('alpha1', 'temperature'),
                                     'Pedlr_simple_const' = c('alpha1', 'temperature'),
                                     'Pedlr' = c('alpha0', 'alpha1', 'temperature'),
                                     'Pedlr_step' = c('alpha0', 'alpha1', 'temperature'),
                                     'Pedlr_fixdep' = c('alpha0', 'alpha1', 'temperature'),
                                     'Pedlr_interdep' = c('alpha0', 'alpha1', 'interdep', 'temperature')))
  
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
    if(random_gen_parameters){
      gen_parameters = c()
      for(i in seq(length(gen_lb))){
        # Get new random value between lower and upper boundary of each parameter
        val = round(runif(1, gen_lb[i], gen_ub[i]), 3)
        gen_parameters = c(gen_parameters, val)
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
    results = Model_recovery(gen_model,
                             design_path_run1,
                             design_path_run2,
                             gen_parameters)
    
    
    recov_result = data.table()
    # Summarize recovery for every model fit to the simulated data
    for(model_count in seq(length(model_list$models))){
      
      # Get current model and parameter names
      recov_model = model_list$models[model_count]
      p_names = unname(unlist(model_list$p_names[recov_model]))
      
      # Pose data to data frame
      temp = as.data.table(results[recov_model][[1]]) %>%
        .[, ':='(para = p_names,
                 iter = i_iter,
                 design_run1 = basename(design_path_run1),
                 design_run2 = basename(design_path_run2))] %>%
        .[, data.table::setcolorder(., c("iter",
                                         "design_run1",
                                         "design_run2",
                                         "para"))]
      # Append to full model_recovery results
      recov_result = rbind(recov_result, temp)
    }
    
    # Concatenate result of each iteration
    output = rbind(output, recov_result)
    
  }
  
  # Set true model that generated the data
  output$gen_model = gen_model
  
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
  make_option(c('-m', '--gen_model'),
              type='character',
              default = NULL,
              help = 'Name of model recovery should be based on',
              metavar = 'GEN_MODEL'),
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
  make_option(c('-t', '--gen_parameters'),
              type='character',
              default = NULL,
              callback = split_list,
              help = 'List of parameter values used to generate data from the generative model Example: 0.1,0.4,7',
              metavar = 'GEN_PARAMETERS'),
  make_option(c('-l', '--gen_lb'),
              type='character',
              default = NULL,
              callback = split_list,
              help = 'List of values matching the number of parameters of the model giving the lower bound for the fitting process. Example: 0,0,0',
              metavar = 'GEN_LOWER_BOUND'),
  make_option(c('-u', '--gen_ub'),
              type='character',
              default = NULL,
              callback = split_list,
              help = 'List of values matching the number of parameters of the model giving the upper bound for the fitting process. Example: 1,1,10',
              metavar = 'GEN_UPPER_BOUND'),
  make_option(c('-r', '--random_gen_parameters'),
              type='character',
              default = NULL,
              help = 'TRUE or FALSE if random gen parameters should be used',
              metavar = 'RANDOM_GEN_PARAMETERS'),
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
Model_recovery_wrapper(output_path = opt$output_path,
                           gen_model = opt$gen_model,
                           design_path_run1 = opt$design_path_run1,
                           design_path_run2 = opt$design_path_run2,
                           gen_parameters = opt$gen_parameters,
                           gen_lb = opt$gen_lb,
                           gen_ub = opt$gen_ub,
                           random_gen_parameters = opt$random_gen_parameters,
                           random_design_run_order = opt$random_design_run_order,
                           n_iter = opt$n_iter)

# Rscript Model_recovery_wrapper.R --output_path "/Users/koch/Docs/pedlr/derivatives/model_recovery/_bla.tsv" --gen_model Pedlr_step --design_path_run1 "/Users/koch/Docs/pedlr/pedlr-task/client/public/designs/design-01_run-1.tsv" --design_path_run2 "/Users/koch/Docs/pedlr/pedlr-task/client/public/designs/design-01_run-2.tsv" --gen_parameters 0.3,0.7,7 --gen_lb 0,0,1 --gen_ub 1,1,20 --random_gen_parameters TRUE --random_design_run_order TRUE --n_iter 1
