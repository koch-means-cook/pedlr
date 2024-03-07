library(optparse)
library(data.table)
library(here)

r2c02_sim_wrapper = function(model,
                             random_input_params,
                             param_lb,
                             param_ub,
                             iterations,
                             tau,
                             ips,
                             beta_weights,
                             principle_mode,
                             condition){
  
  # model = 'seplr_surprise'
  # random_input_params = TRUE
  # param_lb = c(0.1, 0.7, 2, 0.7, 0.1, 2)
  # param_ub = c(0.2, 0.8, 4, 0.8, 0.2, 4)
  # iterations = 1
  # tau = 0.2
  # ips = c(0.93, 0.42, 5, 0.1, 0.7, 3)
  # beta_weights = c(0, -0.2, 0.2, NA, NA)
  # principle_mode = TRUE
  # condition = 'PosIncNegDec'
  
  # Load own functions
  source(file.path(here::here(), 'code', 'plos_review_01', 'r2c02_sim.R',
                   fsep = .Platform$file.sep))
  source(file.path(here::here(), 'code', 'utils', 'Load_data.R',
                   fsep = .Platform$file.sep))
  
  # If principle_mode is on, set betas to predefined values
  if(principle_mode == TRUE){
    if(model %in% c('rw', 'seplr', 'surprise', 'seplr_surprise')){
      beta_weights = c(0, -1, 1, NA, NA)
    } else{
      beta_weights = c(0, -1, 1, -1, 1)
    }
  }
  
  
  # Function to check number of parameters for models
  Check_n_params = function(model,
                            corr_n_params,
                            corr_n_betas,
                            param_lb,
                            param_ub,
                            ips,
                            beta_weights){
    
    # Check if number of parameters is correct for model
    if(any(length(param_lb[!is.na(param_lb)]) != corr_n_params,
           length(param_ub[!is.na(param_ub)]) != corr_n_params,
           length(ips[!is.na(ips)]) != corr_n_params)){
      stop(paste('Specified model (',
                 model,
                 ') does not fit number of specified parameters in params_lb/params_ub/ips (',
                 corr_n_params,
                 ').',
                 sep = ''))
      # Check if number of beta weights fits model
    } else if(length(beta_weights[!is.na(beta_weights)]) != corr_n_betas){
      stop(paste('Specified model (',
                 model,
                 ') does not fit number of specified beta weights in betas_lb/betas_ub/beta_weights (',
                 corr_n_betas,
                 ').',
                 sep = ''))
    }
  }
  
  # Load participant data
  schedules = Load_data() %>%
    # Combine reward schedule runs to one variable
    .[, schedule := paste(name_design_r1, name_design_r2, sep = '')] %>%
    # Get one participant for each combination of reward schedules
    .[, .(example_participant = participant_id[1]),
      by = 'schedule'] %>%
    # Sort by schedules
    .[order(rank(schedule)),]
  schedule_participants = schedules$example_participant
  
  # Allocate output table
  out = data.table()
  
  for(participant_id in schedule_participants){
    
    # Load base data for simulation based on participant
    data = data.table::fread(file.path(here::here(),
                                       'data',
                                       paste(participant_id,
                                             '_exp_data.tsv',
                                             sep = '')),
                             sep = '\t',
                             na.strings = 'n/a')
    
    # Loop over simulation iterations
    for(n_iter in seq(iterations)){
      
      # Give message to user
      message(paste('\n----------Iteration: ', n_iter, '\n', sep = ''), appendLF = FALSE)
      
      # Get random input parameters values for each model if specified
      # (otherwise take specified ips)
      
      # RW
      if(model == 'rw'){
        
        # Create random parameters (if specified)
        if(random_input_params == TRUE){
          ips = c(runif(n = 1, min = param_lb[1], max = param_ub[1]),
                  NA,
                  NA,
                  NA,
                  NA,
                  NA)
        }
        
        # Check if number of parameters is correct for model
        Check_n_params(model = model,
                       corr_n_params = 1,
                       corr_n_betas = 3,
                       param_lb = param_lb,
                       param_ub = param_ub,
                       ips = ips,
                       beta_weights = beta_weights)
        
        # Uncertainty
      } else if(model == 'uncertainty'){
        if(random_input_params == TRUE){
          ips = c(runif(n = 1, min = param_lb[1], max = param_ub[1]),
                  runif(n = 1, min = param_lb[2], max = param_ub[2]),
                  NA,
                  NA,
                  NA,
                  NA)
        }
        
        # Check if number of parameters is correct for model
        Check_n_params(model = model,
                       corr_n_params = 2,
                       corr_n_betas = 5,
                       param_lb = param_lb,
                       param_ub = param_ub,
                       ips = ips,
                       beta_weights = beta_weights)
        
        
        # Seplr
      } else if(model == 'seplr'){
        if(random_input_params == TRUE){
          ips = c(runif(n = 1, min = param_lb[1], max = param_ub[1]),
                  runif(n = 1, min = param_lb[2], max = param_ub[2]),
                  NA,
                  NA,
                  NA,
                  NA)
        }
        
        # Check if number of parameters is correct for model
        Check_n_params(model = model,
                       corr_n_params = 2,
                       corr_n_betas = 3,
                       param_lb = param_lb,
                       param_ub = param_ub,
                       ips = ips,
                       beta_weights = beta_weights)
        
        # Uncertainty+Seplr
      } else if(model == 'uncertainty_seplr'){
        if(random_input_params == TRUE){
          ips = c(runif(n = 1, min = param_lb[1], max = param_ub[1]),
                  runif(n = 1, min = param_lb[2], max = param_ub[2]),
                  runif(n = 1, min = param_lb[3], max = param_ub[3]),
                  NA,
                  NA,
                  NA)
        }
        
        # Check if number of parameters is correct for model
        Check_n_params(model = model,
                       corr_n_params = 3,
                       corr_n_betas = 5,
                       param_lb = param_lb,
                       param_ub = param_ub,
                       ips = ips,
                       beta_weights = beta_weights)
        
        # Surprise
      } else if(model == 'surprise'){
        if(random_input_params == TRUE){
          ips = c(runif(n = 1, min = param_lb[1], max = param_ub[1]),
                  runif(n = 1, min = param_lb[2], max = param_ub[2]),
                  runif(n = 1, min = param_lb[3], max = param_ub[3]),
                  NA,
                  NA,
                  NA)
        }
        
        # Check if number of parameters is correct for model
        Check_n_params(model = model,
                       corr_n_params = 3,
                       corr_n_betas = 3,
                       param_lb = param_lb,
                       param_ub = param_ub,
                       ips = ips,
                       beta_weights = beta_weights)
        
        # Uncertainty+Surprise
      } else if(model == 'uncertainty_surprise'){
        if(random_input_params == TRUE){
          ips = c(runif(n = 1, min = param_lb[1], max = param_ub[1]),
                  runif(n = 1, min = param_lb[2], max = param_ub[2]),
                  runif(n = 1, min = param_lb[3], max = param_ub[3]),
                  runif(n = 1, min = param_lb[4], max = param_ub[4]),
                  NA,
                  NA)
        }
        
        # Check if number of parameters is correct for model
        Check_n_params(model = model,
                       corr_n_params = 4,
                       corr_n_betas = 5,
                       param_lb = param_lb,
                       param_ub = param_ub,
                       ips = ips,
                       beta_weights = beta_weights)
        
        # seplr_surprise
      } else if(model == 'seplr_surprise'){
        if(random_input_params == TRUE){
          ips = c(runif(n = 1, min = param_lb[1], max = param_ub[1]),
                  runif(n = 1, min = param_lb[2], max = param_ub[2]),
                  runif(n = 1, min = param_lb[3], max = param_ub[3]),
                  runif(n = 1, min = param_lb[4], max = param_ub[4]),
                  runif(n = 1, min = param_lb[5], max = param_ub[5]),
                  runif(n = 1, min = param_lb[6], max = param_ub[6]))
        }
        
        # Check if number of parameters is correct for model
        Check_n_params(model = model,
                       corr_n_params = 6,
                       corr_n_betas = 3,
                       param_lb = param_lb,
                       param_ub = param_ub,
                       ips = ips,
                       beta_weights = beta_weights)
      }
      
      # Round inputs
      ips = round(ips, 2)
      beta_weights = round(beta_weights, 2)
      
      # Give message to user
      message(paste('Simulating...\n', sep = ''), appendLF = FALSE)
      message(paste('   Input parameters:\t\t\t', sep = ''), appendLF = FALSE)
      message(paste(ips, collapse = ' | '), appendLF = FALSE)
      message('\n', appendLF = FALSE)
      message(paste('   Input beta weights:\t\t\t', sep = ''), appendLF = FALSE)
      message(paste(beta_weights, collapse = ' | '), appendLF = FALSE)
      message('\n', appendLF = FALSE)
      
      # Run simulation
      sim = r2c02_sim(data = data,
                      input_params = ips,
                      tau = tau,
                      model = model,
                      beta_weights = beta_weights)
      
      # Fuse output over iterations
      # Recovery
      sim$iter = n_iter
      out = rbind(out, sim)
      
    }
  }
  
  # Add condition name to simulation
  out$condition = condition
  
  # Savefile name giving base of simulation for recovery
  file_name_out = paste('r2c02_sim_model-',
                        model,
                        '_principle-',
                        principle_mode,
                        '_cond-',
                        condition,
                        '.tsv',
                        sep = '')
  
  # Create save directory in case it does not exist yet
  save_dir = file.path(here::here(),
                       'derivatives',
                       'plos_review_01',
                       fsep = .Platform$file.sep)
  if(!dir.exists(save_dir)){
    dir.create(save_dir)
  }
  
  # Save output
  # Simulation
  file = file.path(save_dir,
                   file_name_out,
                   fsep = .Platform$file.sep)
  data.table::fwrite(x = out,
                     file = file,
                     na = 'n/a',
                     sep = '\t')
  
  message(paste('\nSaving output to: ', file, '...\n', sep = ''), appendLF = FALSE)
  message('\n')
  
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
  make_option(c('-l', '--param_lb'),
              action = 'callback',
              callback = split_list,
              type='character',
              default = NULL,
              help = 'series of values giving lower bound of parameters. E.g. `0.01,0.01,-20,NA`',
              metavar = 'PARAM_LB'),
  make_option(c('-u', '--param_ub'),
              action = 'callback',
              callback = split_list,
              type='character',
              default = NULL,
              help = 'series of values giving upper bound of parameters. E.g. `1,1,20,NA`',
              metavar = 'PARAM_UB'),
  make_option(c('-i', '--iterations'),
              type='numeric',
              default = NULL,
              help = 'Int giving number of iterations of simulating data and fitting model. Random starting values will be different each iteration.',
              metavar = 'ITERATIONS'),
  make_option(c('-B', '--beta_weights'),
              action = 'callback',
              callback = split_list,
              type='character',
              default = NULL,
              help = 'Values for beta weights used in regression to create trial-by-trial choices in simulation. E.g. `0,-0.2,0.2,NA,NA`',
              metavar = 'BETA_WEIGHTS'),
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
  make_option(c('-X', '--principle_mode'),
              type='character',
              default = NULL,
              help = 'if `TRUE`, beta weights for simulation will be set to values to demonstrate the priciple of models',
              metavar = 'PRINCIPLE_MODE'),
  make_option(c('-c', '--condition'),
              type='character',
              default = NULL,
              help = 'Name of the condition that the input parameters reflect. E.g. "dec" for surprise model parameters that decrease learning with higher PEs.',
              metavar = 'CONDITION'))

# provide options in list to be callable by script
opt_parser = OptionParser(option_list = option_list)
opt = parse_args(opt_parser)

# Cal wrapper with command line inputs
r2c02_sim_wrapper(model = opt$model,
                  random_input_params = opt$random_input_params,
                  param_lb = opt$param_lb,
                  param_ub = opt$param_ub,
                  iterations = opt$iterations,
                  beta_weights = opt$beta_weights,
                  tau = opt$tau,
                  ips = opt$ips,
                  principle_mode = opt$principle_mode,
                  condition = opt$condition)

# Example command line prompt
# Rscript r2c02_sim_wrapper.R --model 'uncertainty' --random_input_params 'TRUE' --param_lb 0.01,0.01,NA,NA --param_ub 1,1,NA,NA --iterations 3 --beta_weights 0,0,0,0,0 --tau 0.2 --ips 0.1,0.7,NA,NA --principle_mode 'TRUE' --condition 'HighPi'
