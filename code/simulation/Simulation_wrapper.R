library(optparse)
library(data.table)
library(here)

Simulation_wrapper = function(participant_id,
                              model,
                              x1_low,
                              x1_high,
                              x1_n,
                              x2_low,
                              x2_high,
                              x2_n,
                              x3_low,
                              x3_high,
                              x3_n,
                              x4_low,
                              x4_high,
                              x4_n,
                              tau,
                              beta_weights){
  
  # participant_id = '09RI1ZH'
  # model = 'seplr'
  # x1_low = 0.1
  # x1_high = 0.7
  # x1_n = 2
  # x2_low = 0.1
  # x2_high = 0.7
  # x2_n = 2
  # x3_low = NA
  # x3_high = NA
  # x3_n = NA
  # x4_low = NA
  # x4_high = NA
  # x4_n = NA
  # tau = 0.2
  # beta_weights = c(1, -0.2, 0.2, NA, NA)
  
  # Load own functions
  source(file.path(here::here(), 'code', 'simulation', 'Simulate.R', fsep = .Platform$file.sep))
  source(file.path(here::here(), 'code', 'simulation', 'Compute_value_per_trial.R', fsep = .Platform$file.sep))
  source(file.path(here::here(), 'code', 'model_fitting', 'LRfunction.R', fsep = .Platform$file.sep))
  
  
  para_combs = data.table::data.table()
  # Get relevant parameter combinations
  if(model == 'rw'){
    # RW only has one parameter (fixed learning rate)
    para_combs = data.table::data.table('x1' = seq(x1_low, x1_high, length.out = x1_n))
    para_combs$x2 = NA
    para_combs$x3 = NA
    para_combs$x4 = NA
    # Specify parameter names
    para_names = 'alpha'
    
  } else if(model == 'uncertainty'){
    # Fixed learning rate with pi parameter (trailing surprise)
    x1 = seq(x1_low, x1_high, length.out = x1_n)
    x2 = seq(x2_low, x2_high, length.out = x2_n)
    # Get all combinations of specified parameters
    para_combs = data.table::data.table(expand.grid(x1, x2))
    colnames(para_combs) = c('x1', 'x2')
    para_combs$x3 = NA
    para_combs$x4 = NA
    # Specify parameter names
    para_names = c('alpha', 'pi')
    
  } else if(model == 'seplr'){
    # Separate learning rate for pos and neg PEs
    x1 = seq(x1_low, x1_high, length.out = x1_n)
    x2 = seq(x2_low, x2_high, length.out = x2_n)
    # Get all combinations of specified parameters
    para_combs = data.table::data.table(expand.grid(x1, x2))
    colnames(para_combs) = c('x1', 'x2')
    para_combs$x3 = NA
    para_combs$x4 = NA
    # Specify parameter names
    para_names = c('alpha_pos', 'alpha_neg')
    
  } else if(model == 'uncertainty_seplr'){
    # Separate learning rate for pos and neg PEs with pi parameter (trailing surprise)
    x1 = seq(x1_low, x1_high, length.out = x1_n)
    x2 = seq(x2_low, x2_high, length.out = x2_n)
    x3 = seq(x3_low, x3_high, length.out = x3_n)
    # Get all combinations of specified parameters
    para_combs = data.table::data.table(expand.grid(x1, x2, x3))
    colnames(para_combs) = c('x1', 'x2', 'x3')
    para_combs$x4 = NA
    # Specify parameter names
    para_names = c('alpha_pos', 'alpha_neg', 'pi')
    
  } else if(model == 'surprise'){
    # Three parameters identifying LRfunction (l,u,s)
    x1 = seq(x1_low, x1_high, length.out = x1_n)
    x2 = seq(x2_low, x2_high, length.out = x2_n)
    x3 = seq(x3_low, x3_high, length.out = x3_n)
    # Get all combinations of specified parameters
    para_combs = data.table::data.table(expand.grid(x1, x2, x3))
    colnames(para_combs) = c('x1', 'x2', 'x3')
    para_combs$x4 = NA
    # Specify parameter names
    para_names = c('l', 'u', 's')
    
  } else if(model == 'uncertainty_surprise'){
    # 3 LRfunction parameters + pi (trailing surprise)
    x1 = seq(x1_low, x1_high, length.out = x1_n)
    x2 = seq(x2_low, x2_high, length.out = x2_n)
    x3 = seq(x3_low, x3_high, length.out = x3_n)
    x4 = seq(x4_low, x4_high, length.out = x4_n)
    para_combs = data.table::data.table(expand.grid(x1,x2, x3, x4))
    colnames(para_combs) = c('x1', 'x2', 'x3', 'x4')
    # Specify parameter names
    para_names = c('l', 'u', 's', 'pi')
    
  } else{
    # Throw error if specified model is not in list
    stop('Specified model not found')
  }
  
  # Give message to user about supplied betas used for regression
  message('---- Fixed betas for regression-based choice ----')
  message(paste(c('b0: ', 'b1: ', 'b2: ', 'b3: ', 'b4: '), beta_weights, '\n', sep = ''))
  message('--------------------------')
  
  # Load specified participant (we are not using participant choices, only the
  # specific design a participant was assigned to)
  file = file.path(here::here(), 'data', paste(participant_id, '_exp_data.tsv', sep = ''),
                   fsep = .Platform$file.sep)
  data = data.table::fread(file, sep = '\t', na.strings = 'n/a')
  
  # simulate across all combinations given
  # Allocate output table
  out = data.table::data.table()
  for(i in seq(nrow(para_combs))){
    # Get parameter combination of current iteration
    paras = as.matrix(para_combs[i,])[1,]
    
    message('---- Parameter values ----')
    message(paste(c('x1: ', 'x2: ', 'x3: ', 'x4: '), paras, '\n', sep = ''))
    message('--------------------------')
    
    # Simulate data based on parameter combination
    temp = Simulate(data = data,
                    x = paras,
                    tau = tau,
                    model = model,
                    beta_weights = beta_weights)
    # fuse data  
    out = rbind(out, temp)
  }
  
  # Add base generating of simulation to output
  out$model = model
  
  # Save data in simulation out
  # Create save location if it does not exist yet
  save_dir = file.path(here::here(), 'derivatives', 'simulation',
                       fesp=.Platform$file.sep)
  if(!file.exists(save_dir)){
    dir.create(save_dir)
  }
  # Save output
  file = file.path(save_dir, paste('modelsim_base-', participant_id,
                                   '_model-', model, '.tsv', sep = ''),
                   fsep = .Platform$file.sep)
  data.table::fwrite(x = out, file = file, sep = '\t', na = 'n/a')
  
  
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
  optparse::make_option(c('-p', '--participant_id'),
                        type='character',
                        default = NULL,
                        help = 'ID of participant',
                        metavar = 'PARTICIPANT_ID'),
  optparse::make_option(c('-m', '--model'),
                        type='character',
                        default = NULL,
                        help = 'Specify model to use for simulation. "rw": x = c(alpha, NA, NA, NA); "uncertainty": x = c(alpha, pi, NA, NA); "seplr": x = c(alpha_pos, alpha_neg, NA, NA); "uncertainty_seplr": x = c(alpha_pos, alpha_neg, pi, NA); "surprise": x = c(l, u, s, NA); "uncertainty_surprise": x = c(l, u, s, NA)',
                        metavar = 'MODEL'),
  optparse::make_option(c('--x1_low'),
                        type='numeric',
                        default = NULL,
                        help = 'Lower bound of grid for search over parameter 1',
                        metavar = 'X1_LOW'),
  optparse::make_option(c('--x1_high'),
                        type='numeric',
                        default = NULL,
                        help = 'Upper bound of grid for search over parameter 1',
                        metavar = 'X1_HIGH'),
  optparse::make_option(c('--x1_n'),
                        type='numeric',
                        default = NULL,
                        help = 'Number of equally spaced steps to form grid between lower and upper bound of parameter 1',
                        metavar = 'X1_N'),
  optparse::make_option(c('--x2_low'),
                        type='numeric',
                        default = NULL,
                        help = 'Lower bound of grid for search over parameter 2',
                        metavar = 'X2_LOW'),
  optparse::make_option(c('--x2_high'),
                        type='numeric',
                        default = NULL,
                        help = 'Upper bound of grid for search over parameter 2',
                        metavar = 'X2_HIGH'),
  optparse::make_option(c('--x2_n'),
                        type='numeric',
                        default = NULL,
                        help = 'Number of equally spaced steps to form grid between lower and upper bound of parameter 2',
                        metavar = 'X2_N'),
  optparse::make_option(c('--x3_low'),
                        type='numeric',
                        default = NULL,
                        help = 'Lower bound of grid for search over parameter 3',
                        metavar = 'X3_LOW'),
  optparse::make_option(c('--x3_high'),
                        type='numeric',
                        default = NULL,
                        help = 'Upper bound of grid for search over parameter 3',
                        metavar = 'X3_HIGH'),
  optparse::make_option(c('--x3_n'),
                        type='numeric',
                        default = NULL,
                        help = 'Number of equally spaced steps to form grid between lower and upper bound of parameter 3',
                        metavar = 'X3_N'),
  optparse::make_option(c('--x4_low'),
                        type='numeric',
                        default = NULL,
                        help = 'Lower bound of grid for search over parameter 4',
                        metavar = 'X4_LOW'),
  optparse::make_option(c('--x4_high'),
                        type='numeric',
                        default = NULL,
                        help = 'Upper bound of grid for search over parameter 4',
                        metavar = 'X4_HIGH'),
  optparse::make_option(c('--x4_n'),
                        type='numeric',
                        default = NULL,
                        help = 'Number of equally spaced steps to form grid between lower and upper bound of parameter 4',
                        metavar = 'X4_N'),
  optparse::make_option(c('--beta_weights'),
                        action = 'callback',
                        callback = split_list,
                        type='character',
                        default = NULL,
                        help = 'List of beta weights supplied to regression model to simulate choices in the form of c(b0,b1,b2,b3,b4); e.g. `1,-0.4,0.4,-0.1,0.1',
                        metavar = 'BETA_WEIGHTS'),
  optparse::make_option(c('--tau'),
                        type='numeric',
                        default = NULL,
                        help = 'Parameter involved in LRfunction shape',
                        metavar = 'TAU'))

# provide options in list to be callable by script
opt_parser = optparse::OptionParser(option_list = option_list)
opt = optparse::parse_args(opt_parser)

# Cal wrapper with command line inputs
Simulation_wrapper(participant_id = opt$participant_id,
                   model = opt$model,
                   x1_low = opt$x1_low,
                   x1_high = opt$x1_high,
                   x1_n = opt$x1_n,
                   x2_low = opt$x2_low,
                   x2_high = opt$x2_high,
                   x2_n = opt$x2_n,
                   x3_low = opt$x3_low,
                   x3_high = opt$x3_high,
                   x3_n = opt$x3_n,
                   x4_low = opt$x4_low,
                   x4_high = opt$x4_high,
                   x4_n = opt$x4_n,
                   tau = opt$tau,
                   beta_weights = opt$beta_weights)

# Rscript Simulation_wrapper.R --participant_id '1SLA8RA' --model 'rw' --x1_low 0.1 --x1_high 0.7 --x1_n 5 --x2_low 0 --x2_high 0 --x2_n 0 --x3_low 0 --x3_high 0 --x3_n 0 --x4_low 0 --x4_high 0 --x4_n 0 --tau 0.2 --beta_weights 1,-0.5,0.5,NA,NA
# Rscript Simulation_wrapper.R --participant_id '09RI1ZH' --model 'uncertainty' --x1_low 0.1 --x1_high 0.7 --x1_n 5 --x2_low 0.1 --x2_high 0.7 --x2_n 5 --x3_low 0 --x3_high 0 --x3_n 0 --x4_low 0 --x4_high 0 --x4_n 0 --tau 0.2 --beta_weights 1,-0.5,0.5,-0.1,0.1
# Rscript Simulation_wrapper.R --participant_id '09RI1ZH' --model 'surprise' --x1_low 0.1 --x1_high 0.7 --x1_n 5 --x2_low 0.1 --x2_high 0.7 --x2_n 5 --x3_low -10 --x3_high 10 --x3_n 5 --x4_low 0 --x4_high 0 --x4_n 0 --tau 0.2 --beta_weights 1,-0.5,0.5,NA,NA
# Rscript Simulation_wrapper.R --participant_id '09RI1ZH' --model 'uncertainty_surprise' --x1_low 0.1 --x1_high 0.7 --x1_n 5 --x2_low 0.1 --x2_high 0.7 --x2_n 5 --x3_low -10 --x3_high 10 --x3_n 5 --x4_low 0.1 --x4_high 0.7 --x4_n 5 --tau 0.2 --beta_weights 1,-0.5,0.5,-0.1,0.1
