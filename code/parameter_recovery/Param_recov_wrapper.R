library(optparse)
library(data.table)
library(here)

Param_recov_wrapper = function(participant_id,
                               model,
                               random_input_params,
                               random_starting_values,
                               lb,
                               ub,
                               algorithm,
                               xtol_rel,
                               maxeval,
                               iterations,
                               beta_weights,
                               tau,
                               ips,
                               svs){
  
  # participant_id = '09RI1ZH'
  # model = 'seplr'
  # random_input_params = FALSE
  # random_starting_values = TRUE
  # lb = c(0.01, 0.01, NA, NA)
  # ub = c(1, 1, NA, NA)
  # algorithm = 'NLOPT_GN_DIRECT_L'
  # xtol_rel = 1.0e-5
  # maxeval = 1000
  # iterations = 3
  # beta_weights = c(1, -0.5, 0.5, NA, NA)
  # tau = 0.2
  # ips = c(0.2, 0.1, NA, NA)
  # svs = c(0.5, 0.5, NA, NA)
  
  # Give message to user
  message(paste('Starting ID ', participant_id, '...\n', sep = ''), appendLF = FALSE)
  # Print input
  message(paste('   model:\t\t\t', model, '\n', sep = ''), appendLF = FALSE)
  message(paste('   random_input_params:\t\t', random_input_params, '\n', sep = ''), appendLF = FALSE)
  message(paste('   random_starting_values:\t', random_starting_values, '\n', sep = ''), appendLF = FALSE)
  message(paste('   lb:\t\t\t\t', sep = ''), appendLF = FALSE)
  message(paste(lb, collapse = ' | '), appendLF = FALSE)
  message('\n', appendLF = FALSE)
  message(paste('   ub:\t\t\t\t', sep = ''), appendLF = FALSE)
  message(paste(ub, collapse = ' | '), appendLF = FALSE)
  message('\n', appendLF = FALSE)
  message(paste('   algorithm:\t\t\t', algorithm, '\n', sep = ''), appendLF = FALSE)
  message(paste('   xtol_rel:\t\t\t', xtol_rel, '\n', sep = ''), appendLF = FALSE)
  message(paste('   maxeval:\t\t\t', maxeval, '\n', sep = ''), appendLF = FALSE)
  message(paste('   iterations:\t\t\t', iterations, '\n', sep = ''), appendLF = FALSE)
  message(paste('   beta_weights:\t\t\t', beta_weights, '\n', sep = ''), appendLF = FALSE)
  message(paste('   tau:\t\t\t\t', tau, '\n', sep = ''), appendLF = FALSE)
  if(random_input_params == FALSE){
    message(paste('   ips:\t\t\t', sep = ''), appendLF = FALSE)
    message(paste(ips, collapse = ' | '), appendLF = FALSE)
    message('\n', appendLF = FALSE)
  }
  if(random_starting_values == FALSE){
    message(paste('   svs:\t\t\t', sep = ''), appendLF = FALSE)
    message(paste(svs, collapse = ' | '), appendLF = FALSE)
    message('\n', appendLF = FALSE)
  }

  # Load own functions
  source(file.path(here::here(), 'code', 'parameter_recovery', 'Param_recov.R',
                   fsep = .Platform$file.sep))
  
  # Function to check number of parameters for models
  Check_n_params = function(model,
                            corr_n_params,
                            lb,
                            ub,
                            ips,
                            svs){
    
    # Check if number of parameters is correct for model
    if(any(length(lb[!is.na(lb)]) != corr_n_params,
           length(ub[!is.na(ub)]) != corr_n_params,
           length(ips[!is.na(ips)]) != corr_n_params,
           length(svs[!is.na(svs)]) != corr_n_params)){
      stop(paste('Specified model (',
                 model,
                 ') does not fit number of specified parameters in lb/ub/ips/svs (',
                 corr_n_params,
                 ').',
                 sep = ''))
    }
  }
  
  # Load base data for simulation based on participant
  data = data.table::fread(file.path(here::here(),
                                     'data',
                                     paste(participant_id,
                                           '_exp_data.tsv',
                                           sep = '')),
                           sep = '\t',
                           na.strings = 'n/a')
  
  out = data.table()
  
  # Loop over simulation iterations
  for(n_iter in seq(iterations)){
    
    # Give message to user
    message(paste('\n----------Iteration: ', n_iter, '\n', sep = ''), appendLF = FALSE)
    
    # Get random input parameters and optim starting values for each model if specified
    # (otherwise take specified ips and svs)
    
    # RW
    if(model == 'rw'){
      
      # Create random parameters (if specified)
      if(random_input_params == TRUE){
        ips = c(runif(n = 1, min = lb[1], max = ub[1]),
                NA,
                NA,
                NA)
      }
      if(random_starting_values == TRUE){
        svs = c(runif(n = 1, min = lb[1], max = ub[1]),
                NA,
                NA,
                NA)
      }
      
      # Check if number of parameters is correct for model
      Check_n_params(model = model,
                     corr_n_params = 1,
                     lb = lb,
                     ub = ub,
                     ips = ips,
                     svs = svs)
      
    # Uncertainty
    } else if(model == 'uncertainty'){
      if(random_input_params == TRUE){
        ips = c(runif(n = 1, min = lb[1], max = ub[1]),
                runif(n = 1, min = lb[2], max = ub[2]),
                NA,
                NA)
      }
      if(random_starting_values == TRUE){
        svs = c(runif(n = 1, min = lb[1], max = ub[1]),
                runif(n = 1, min = lb[2], max = ub[2]),
                NA,
                NA)
      }
      
      # Check if number of parameters is correct for model
      Check_n_params(model = model,
                     corr_n_params = 2,
                     lb = lb,
                     ub = ub,
                     ips = ips,
                     svs = svs)
      

    # Seplr
  } else if(model == 'seplr'){
    if(random_input_params == TRUE){
      ips = c(runif(n = 1, min = lb[1], max = ub[1]),
              runif(n = 1, min = lb[2], max = ub[2]),
              NA,
              NA)
    }
    if(random_starting_values == TRUE){
      svs = c(runif(n = 1, min = lb[1], max = ub[1]),
              runif(n = 1, min = lb[2], max = ub[2]),
              NA,
              NA)
    }
    
    # Check if number of parameters is correct for model
    Check_n_params(model = model,
                   corr_n_params = 2,
                   lb = lb,
                   ub = ub,
                   ips = ips,
                   svs = svs)
    
    # Uncertainty+Seplr
  } else if(model == 'uncertainty_seplr'){
    if(random_input_params == TRUE){
      ips = c(runif(n = 1, min = lb[1], max = ub[1]),
              runif(n = 1, min = lb[2], max = ub[2]),
              runif(n = 1, min = lb[3], max = ub[3]),
              NA)
    }
    if(random_starting_values == TRUE){
      svs = c(runif(n = 1, min = lb[1], max = ub[1]),
              runif(n = 1, min = lb[2], max = ub[2]),
              runif(n = 1, min = lb[3], max = ub[3]),
              NA)
    }
    
    # Check if number of parameters is correct for model
    Check_n_params(model = model,
                   corr_n_params = 3,
                   lb = lb,
                   ub = ub,
                   ips = ips,
                   svs = svs)
    
    # Surprise
  } else if(model == 'surprise'){
      if(random_input_params == TRUE){
        ips = c(runif(n = 1, min = lb[1], max = ub[1]),
                runif(n = 1, min = lb[2], max = ub[2]),
                runif(n = 1, min = lb[3], max = ub[3]),
                NA)
      }
      if(random_starting_values == TRUE){
        svs = c(runif(n = 1, min = lb[1], max = ub[1]),
                runif(n = 1, min = lb[2], max = ub[2]),
                runif(n = 1, min = lb[3], max = ub[3]),
                NA)
      }
      
      # Check if number of parameters is correct for model
      Check_n_params(model = model,
                     corr_n_params = 3,
                     lb = lb,
                     ub = ub,
                     ips = ips,
                     svs = svs)
      
    # Uncertainty+Surprise
    } else if(model == 'uncertainty_surprise'){
      if(random_input_params == TRUE){
        ips = c(runif(n = 1, min = lb[1], max = ub[1]),
                runif(n = 1, min = lb[2], max = ub[2]),
                runif(n = 1, min = lb[3], max = ub[3]),
                runif(n = 1, min = lb[4], max = ub[4]))
      }
      if(random_starting_values == TRUE){
        svs = c(runif(n = 1, min = lb[1], max = ub[1]),
                runif(n = 1, min = lb[2], max = ub[2]),
                runif(n = 1, min = lb[3], max = ub[3]),
                runif(n = 1, min = lb[4], max = ub[4]))
      }
      
      # Check if number of parameters is correct for model
      Check_n_params(model = model,
                     corr_n_params = 4,
                     lb = lb,
                     ub = ub,
                     ips = ips,
                     svs = svs)
    }
    
    # Round inputs
    ips = round(ips, 2)
    svs = round(svs, 2)
      
    # Give message to user
    message(paste('Simulating and fitting model...\n', sep = ''), appendLF = FALSE)
    message(paste('   Input parameters:\t\t\t', sep = ''), appendLF = FALSE)
    message(paste(ips, collapse = ' | '), appendLF = FALSE)
    message('\n', appendLF = FALSE)
    message(paste('   Fitting starting values:\t\t', sep = ''), appendLF = FALSE)
    message(paste(svs, collapse = ' | '), appendLF = FALSE)
    message('\n', appendLF = FALSE)
    
    # Start parameter recovery
    res = Param_recov(data = data,
                      input_params = ips,
                      algorithm = algorithm,
                      xtol_rel = xtol_rel,
                      maxeval = maxeval,
                      x0 = svs,
                      lb = lb,
                      ub = ub,
                      temperature = temperature,
                      tau = tau,
                      model = model)
    
    # Fuse output over iterations
    res$iter = n_iter
    out = rbind(out, res)
    
  }
  
  # Add identifiers to output
  out$model = model
  out$algorithm = algorithm
  out$xtol_rel = xtol_rel
  out$maxeval = maxeval
  out$temperature = temperature
  out$tau = tau
  
  # Savefile name giving base of simulation for recovery
  file_name = paste('paramrecov_base-',
                    participant_id,
                    '_model-',
                    model,
                    '_randips-',
                    random_input_params,
                    '_randsvs-',
                    random_starting_values,
                    '.tsv',
                    sep = '')
  
  # Create save directory in case it does not exist yet
  save_dir = file.path(here::here(),
                       'derivatives',
                       'parameter_recovery',
                       fsep = .Platform$file.sep)
  if(!dir.exists(save_dir)){
    dir.create(save_dir)
  }
  
  # Save output
  file = file.path(save_dir,
                   file_name,
                   fsep = .Platform$file.sep)
  data.table::fwrite(x = out,
                     file = file,
                     na = 'n/a',
                     sep = '\t')
  
  message(paste('\nSaving output to: ', file, '...\n', sep = ''), appendLF = FALSE)
  message(paste('...Well done Superperson!', sep = ''), appendLF = FALSE)
  message('\n')
  
  # Write analysis script for param recov

  }

# Function to split list inputs (used as callback function during argument parsing)
split_list = function(object,
                      flag,
                      value,
                      parser){
  x = unlist(strsplit(value, ','))
  x[x == 'NA'] = NA
  x = as.numeric(x)
  return(x)
}

# Create options to pass to script
option_list = list(
  make_option(c('-p', '--participant_id'),
              type='character',
              default = NULL,
              help = 'ID of participant',
              metavar = 'PARTICIPANT_ID'),
  make_option(c('-M', '--model'),
              type='character',
              default = NULL,
              help = 'Name of model. Options are `rw`, `uncertainty`, `seplr`, `uncertainty_seplr`, `surprise`, & `uncertainty_surprise`',
              metavar = 'MODEL'),
  make_option(c('-R', '--random_input_params'),
              type='character',
              default = NULL,
              help = 'if `TRUE`, parameters for simulation will be random within specified lower and upper bound',
              metavar = 'RANDOM_INPUT_PARAMS'),
  make_option(c('-r', '--random_starting_values'),
              type='character',
              default = NULL,
              help = 'if `TRUE`, starting values for fitting will be random within specified lower and upper bound',
              metavar = 'RANDOM_STARTING_VALUES'),
  make_option(c('-l', '--lb'),
              action = 'callback',
              callback = split_list,
              type='character',
              default = NULL,
              help = 'series of values giving lower bound of parameters. E.g. `0.01,0.01,-20,NA`',
              metavar = 'LB'),
  make_option(c('-u', '--ub'),
              action = 'callback',
              callback = split_list,
              type='character',
              default = NULL,
              help = 'series of values giving upper bound of parameters. E.g. `1,1,20,NA`',
              metavar = 'UB'),
  make_option(c('-a', '--algorithm'),
              type='character',
              default = NULL,
              help = 'nloptr algorithm. Example: NLOPT_GN_DIRECT_L',
              metavar = 'ALGORITHM'),
  make_option(c('-x', '--xtol_rel'),
              type='numeric',
              default = NULL,
              help = 'Int giving tolerance for fitting. Example: 0.00001',
              metavar = 'XTOL_REL'),
  make_option(c('-m', '--maxeval'),
              type='numeric',
              default = NULL,
              help = 'Maximum number of evaluations before fitting terminates. Example: 1000',
              metavar = 'MAXEVAL'),
  make_option(c('-i', '--iterations'),
              type='numeric',
              default = NULL,
              help = 'Int giving number of iterations of simulating data and fitting model. Random starting values will be different each iteration.',
              metavar = 'ITERATIONS'),
  make_option(c('-T', '--temperature'),
              type='numeric',
              default = NULL,
              help = 'Value for temperature parameter of softmax choice used during simulation of data. E.g. `7`',
              metavar = 'TEMPERATURE'),
  make_option(c('-t', '--tau'),
              type='numeric',
              default = NULL,
              help = 'Value for fixed tau parameter used in Suprise model. E.g. `0.2`',
              metavar = 'TAU'),
  make_option(c('-P', '--ips'),
              action = 'callback',
              callback = split_list,
              type='character',
              default = NULL,
              help = 'series of values giving input parameters for simulation. Number of parameters unique to model: 1 = `rw`, 2 = `uncertainty`, 3 = `surprise`, 4 = `uncertainty_surprise`. Needs to be padded with NA to reach length = 4. E.g. `0.1,0.6,5,NA`. Ignored if `random_input_params == TRUE`',
              metavar = 'IPS'),
  make_option(c('-s', '--svs'),
              action = 'callback',
              callback = split_list,
              type='character',
              default = NULL,
              help = 'series of values giving starting values for model fitting. Number of parameters unique to model: 1 = `rw`, 2 = `uncertainty`, 3 = `surprise`, 4 = `uncertainty_surprise`. Needs to be padded with NA to reach length = 4. E.g. `0.5,0.5,7,NA`. Ignored if `random_starting_values == TRUE`',
              metavar = 'SVS'))

# provide options in list to be callable by script
opt_parser = OptionParser(option_list = option_list)
opt = parse_args(opt_parser)

# Cal wrapper with command line inputs
Param_recov_wrapper(participant_id = opt$participant_id,
                    model = opt$model,
                    random_input_params = opt$random_input_params,
                    random_starting_values = opt$random_starting_values,
                    lb = opt$lb,
                    ub = opt$ub,
                    algorithm = opt$algorithm,
                    xtol_rel = opt$xtol_rel,
                    maxeval = opt$maxeval,
                    iterations = opt$iterations,
                    temperature = opt$temperature,
                    tau = opt$tau,
                    ips = opt$ips,
                    svs = opt$svs)

# Rscript Param_recov_wrapper.R --participant_id '09RI1ZH' --model 'surprise' --random_input_params 'TRUE' --random_starting_values 'TRUE' --lb 0.01,0.01,-20,NA --ub 1,1,20,NA --algorithm 'NLOPT_GN_DIRECT_L' --xtol_rel 0.0001 --maxeval 10 --iterations 3 --temperature 7 --tau 0.2 --ips 0.1,0.7,2,NA --svs 0.5,0.5,0,NA
