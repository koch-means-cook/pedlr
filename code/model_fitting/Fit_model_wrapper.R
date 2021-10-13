library(data.table)
library(here)
library(optparse)
library(magrittr)

source(file.path(here::here(), 'code', 'model_fitting', 'Fit_model.R',
                 fsep = .Platform$file.sep))
source(file.path(here::here(), 'code', 'utils', 'Prepare_data_for_fit.R',
                 fsep = .Platform$file.sep))

# input_path = '/Users/koch/Docs/pedlr/data/G5RTD96_exp_data.tsv'
# model = 'Rw'
# start_values = c(runif(1,0,1),runif(1,1,10))
# lb = c(0,1)
# ub = c(1,10)
# random_start_values = TRUE
# n_iter = 2

Fit_model_wrapper = function(input_path,
                             output_path,
                             model,
                             start_values,
                             lb,
                             ub,
                             random_start_values,
                             n_iter){
  
  # Set parameter names for model
  if(model == 'Rw'){
    p_names = c('alpha', 'temperature')
  } else if(model == 'Pedlr'){
    p_names = c('alpha0', 'alpha1', 'temperature')
  } else if(model == 'Pedlr_interdep'){
    p_names = c('alpha0', 'alpha1', 'interdep', 'temperature')
  }
  
  # Load data
  data = data.table::fread(input_path,
                           sep = '\t',
                           header = TRUE,
                           na.strings = 'n/a') %>%
    Prepare_data_for_fit(.)
  
  output = data.table()
  
  message('\n')
  message('--------------------------')
  
  # Iterations of fit
  for(i_iter in seq(n_iter)){
    
    message(paste('Iteration: ', i_iter, sep = ''))
    
    # If random staring values, take new random values for each iteration
    if(random_start_values){
      start_values = c()
      for(i in seq(length(lb))){
        # Get new random value between lower and upper boundary of each parameter
        val = round(runif(1, lb[i], ub[i]), 3)
        start_values = c(start_values, val)
      }
    }
    
    message('   Starting values: ', paste(start_values, collapse = ' '))
    
    # Separate fits for runs (each iteration)
    for(i_version in unique(data$task_version)){
      
      message('   Fitting Run: ', i_version, '...')
      
      # Subset single run of data
      fit_data = data[task_version == i_version]
      # Fit selected model
      result = Fit_model(data = fit_data,
                         model = model,
                         start_values = start_values,
                         lb = lb,
                         ub = ub)
      # Form model fit into returnable data frame
      result = as.data.table(result) %>%
        .[, ':='(para = p_names,
                 iter = i_iter,
                 participant_id = unique(data$participant_id),
                 task_version = i_version,
                 name_design_r1 = unique(data$name_design_r1),
                 name_design_r2 = unique(data$name_design_r2))] %>%
        .[, data.table::setcolorder(., c("participant_id",
                                         "task_version",
                                         "name_design_r1",
                                         "name_design_r2",
                                         "para"))]
      
      message('      Result:\t', paste(round(result$second_solution, 3), collapse = ' '))
      message('      LL:\t', round(unique(result$second_ll), 3))
      
      output = rbind(output, result)
    }
  }
  
  # Write output
  data.table::fwrite(x = output,
                     file = output_path,
                     sep = '\t',
                     na = 'n/a',
                     row.names = FALSE,
                     col.names = TRUE)
  
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
  make_option(c('-i', '--input_path'),
              type='character',
              default = NULL,
              help = 'Path to data file model should be fit to',
              metavar = 'INPUT_PATH'),
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
  make_option(c('-s', '--start_values'),
              type='character',
              default = NULL,
              callback = split_list,
              help = 'List of starting values matching the number of parameters of the model. Example: 0.1,0.4,7',
              metavar = 'START_VALUES'),
  make_option(c('-l', '--lb'),
              type='character',
              default = NULL,
              callback = split_list,
              help = 'List of values matching the number of parameters of the model giving the lower bound for the fitting process. Example: 0,0,0',
              metavar = 'LOWER_BOUND'),
  make_option(c('-u', '--ub'),
              type='character',
              default = NULL,
              callback = split_list,
              help = 'List of values matching the number of parameters of the model giving the upper bound for the fitting process. Example: 1,1,10',
              metavar = 'UPPER_BOUND'),
  make_option(c('-r', '--random_start_values'),
              type='character',
              default = NULL,
              help = 'TRUE or FALSE if random start values should be used',
              metavar = 'RANDOM_START_VALUES'),
  make_option(c('-n', '--n_iter'),
              type='integer',
              default = NULL,
              help = 'Int giving number of iterations per fitting process. If random_start_values == TRUE new random starting values will be set for each iteration',
              metavar = 'N_ITER'))

# provide options in list to be callable by script
opt_parser = OptionParser(option_list = option_list)
opt = parse_args(opt_parser)

# Cal wrapper with command line inputs
Fit_model_wrapper(input_path = opt$input_path,
                  output_path = opt$output_path,
                  model = opt$model,
                  start_values = opt$start_values,
                  lb = opt$lb,
                  ub = opt$ub,
                  random_start_values = opt$random_start_values,
                  n_iter = opt$n_iter)

# Rscript Fit_model_wrapper.R --input_path "/Users/koch/Docs/pedlr/data/G5RTD96_exp_data.tsv" --output_path "/Users/koch/Docs/pedlr/derivatives/model_fitting/bla.tsv" --model Rw --start_values 0.5,5 --lb 0,1 --ub 1,10 --random_start_values TRUE --n_iter 2
# Rscript Fit_model_wrapper.R --input_path "/home/mpib/koch/pedlr/data/G5RTD96_exp_data.tsv" --output_path "/home/mpib/koch/pedlr/derivatives/model_fitting/bla.tsv" --model Rw --start_values 0.5,5 --lb 0,1 --ub 1,10 --random_start_values TRUE --n_iter 2

# Rscript Fit_model_wrapper.R --input_path "/Users/koch/Docs/pedlr/data/G5RTD96_exp_data.tsv" --output_path "/Users/koch/Docs/pedlr/derivatives/model_fitting/bla.tsv" --model Pedlr --start_values 0.5,0.5,5 --lb 0,0,1 --ub 1,1,10 --random_start_values TRUE --n_iter 2

