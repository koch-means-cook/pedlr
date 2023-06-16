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
                               temperature,
                               tau,
                               ips,
                               svs){
  
  # participant_id = '09RI1ZH'
  # model = 'surprise'
  # random_input_params = TRUE
  # random_starting_values = TRUE
  # lb = c(0.01, 0.01, -20, NA)
  # ub = c(1, 1, 20, NA)
  # algorithm = 'NLOPT_GN_DIRECT_L'
  # xtol_rel = 1.0e-5
  # maxeval = 1000
  # iterations = 3
  # temperature = 7
  # tau = 0.2
  # ips = c(0.1, 0.7, 2, NA)
  # svs = c(0.5, 0.5, 5, NA)

  # Load own functions
  source(file.path(here::here(), 'code', 'parameter_recovery', 'Param_recov.R', fsep = .Platform$file.sep))
  
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
  
  # Loop over iterations
  for(n_iter in seq(iterations)){
    
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
      
    # Suprise
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
                      tau = tau)
    
    # Fuse output over iterations
    res$iter = n_iter
    out = rbind(out, res)
    
  }
  
  
  
  # Set up optparse
  # Set up hpc submit script
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
  make_option(c('-m', '--model'),
              type='character',
              default = NULL,
              help = 'Name of model. Options are `rw`, `uncertainty`, `surprise`, and `uncertainty_surprise`',
              metavar = 'MODEL'),
  # random_input_params
  # random_starting_values
  make_option(c('-s', '--starting_values'),
              type='character',
              default = NULL,
              help = '`random` for uniform random between boundaries, `fixed` for fixed starting values',
              metavar = 'STARTING_VALUES'),
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
              metavar = 'LB'),
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
              help = 'Int giving number of iterations fitting is repeated. Random starting values will be different each iteration',
              metavar = 'ITERATIONS'),
  # temperature
  # tau
  # ips
  # svs
  )

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