library(here)
library(optparse)
library(nloptr)

Fit_models_new_wrapper = function(participant_id,
                                  starting_values,
                                  algorithm,
                                  xtol_rel,
                                  maxeval,
                                  iterations){
  
  # participant_id = '09RI1ZH'
  # starting_values = 'random'
  # algorithm = 'NLOPT_GN_DIRECT_L'
  # xtol_rel = 1.0e-5
  # maxeval = 10
  # iterations = 1

  # Set base_path
  base_path = here::here()
  
  # Source own functions
  source(file.path(base_path, 'code', 'utils', 'Load_data.R'))
  source(file.path(base_path, 'code', 'utils', 'Apply_exclusion_criteria.R'))
  source(file.path(base_path, 'code', 'utils', 'Add_comp.R'))
  source(file.path(base_path, 'code', 'model_fitting', 'Fit_models_new.R'))
  
  # Load participant data
  data = Load_data()
  # Apply exclusion criteria to dataset (exclude participants based on choice performance, ignoring estimation performance)
  data = Apply_exclusion_criteria(data, choice_based_exclusion = TRUE)
  
  # Get participant
  pid = participant_id
  # Stop model fitting in case participant was excluded based on choice performance
  # and is not present in data set anymore
  if(!pid %in% unique(data$participant_id)){
    stop('Participant was excluded based on choice performance. No model fitting possible.')
  } else{
    # In case participant was not excluded, select only data of specified participant
    data = data[participant_id == pid]
  }
  
  # Get correct choices
  # Find correct choice
  data$correct = c('left', 'right')[(data$option_right > data$option_left)*1+1]
  # See if choice was correct
  data$correct_choice = data$correct == data$choice
  # Get correct choices for forced choices
  data$correct_choice[data$forced_left == 1] = data$choice[data$forced_left == 1] == 'left'
  data$correct_choice[data$forced_right == 1] = data$choice[data$forced_right == 1] == 'right'
  # Duplicate chosen bandit
  data$chosen_bandit = data$option_choice
  # Get bandit comparison for each trial
  data = Add_comp(data)
  # Delete 'v' in middle of comparison
  data = data %>%
    .[, bandit := paste(substr(comp, 1, 1), substr(comp, 3,3), sep = '')]
  
  
  # Allocate data frame for storage of all iterations
  # Model Fit
  out_fit = data.table::data.table()
  # PE
  out_model_data = data.table::data.table()
  
  # Give message to user
  message(paste('Starting ID ', participant_id, '...\n', sep = ''), appendLF = FALSE)
  
  
  # Loop over iterations
  for(n_iter in seq(iterations)){
    
    # Set starting values
    # rw (alpha)
    lb = list(0.01,
              # uncertainty (alpha, pi)
              c(0.01, 0.01),
              # seplr (alpha_pos, alpha_neg)
              c(0.01, 0.01),
              # uncertainty+seplr (alpha_pos, alpha_neg, pi)
              c(0.01, 0.01, 0.01),
              # surprise (l,u,s)
              c(exp(-5), exp(-5), 1),
              # uncertainty+surprise (l,u,s,pi)
              c(exp(-5), exp(-5), 1, 0.01))
    ub = list(1,
              c(1, 1),
              c(1, 1),
              c(1, 1, 1),
              c(1, 1, 7),
              c(1, 1, 7, 1))
    # Set starting values either fixed or random, depending on function input
    if(starting_values == 'fixed'){
      x0 = list(0.2,
                c(0.2, 0.2),
                c(0.2, 0.2),
                c(0.2, 0.2, 0.2),
                c(0.2, 0.5, 1),
                c(0.2, 0.5, 1, 0.2))
    } else if(starting_values == 'random'){
      # Use same random starting value for similar models
      rand_alpha = runif(1, min = lb[[1]], max = ub[[1]])
      rand_pi = runif(1, min = lb[[2]][2], max = ub[[2]][2])
      rand_l = runif(1, min = lb[[5]][1], max = ub[[5]][1])
      rand_u = runif(1, min = lb[[5]][2], max = ub[[5]][2])
      rand_s = runif(1, min = lb[[5]][3], max = ub[[5]][3])
                # rw (alpha)
      x0 = list(rand_alpha,
                # uncertainty (alpha, pi)
                c(rand_alpha, rand_pi),
                # seplr (alpha_pos, alpha_neg)
                c(rand_alpha, rand_alpha),
                # uncertainty+seplr (alpha_pos, alpha_neg, pi)
                c(rand_alpha, rand_alpha, rand_pi),
                # surprise (l,u,s)
                c(rand_l, rand_u, rand_s),
                # uncertainty+surprise (l,u,s,pi)
                c(rand_l, rand_u, rand_s, rand_pi))
    }
    
    # Send message to user
    message(paste('   Iteration:   ', n_iter, '...', sep = ''), appendLF = FALSE)
    
    # Fit model
    res = Fit_models_new(data = data,
                         algorithm = algorithm,
                         xtol_rel = xtol_rel,
                         maxeval = maxeval,
                         x0 = x0,
                         lb = lb,
                         ub = ub)
    
    # Get model fit output
    fit = res$fitting_out
    # Get PEs
    model_data = res$model_data
    # Add iteration
    fit$iter = n_iter
    model_data$iter = n_iter
    
    # Concatenate data
    out_fit = rbind(out_fit, fit)
    out_model_data = rbind(out_model_data, model_data)
   
    # Send message to user
    message(paste('done!\n', sep = ''), appendLF = FALSE)
     
  }
  
  # Add fitting variables to output
  # Fit
  out_fit$starting_values = starting_values
  out_fit$algorithm = algorithm
  out_fit$xtol_rel = xtol_rel
  out_fit$maxeval = maxeval
  out_fit$n_iterations = iterations
  # model_data
  out_model_data$starting_values = starting_values
  out_model_data$algorithm = algorithm
  out_model_data$xtol_rel = xtol_rel
  out_model_data$maxeval = maxeval
  out_model_data$n_iterations = iterations
  
  # Add demographic variable to output
  # Fit
  out_fit$age = unique(data$age)
  out_fit$sex = unique(data$sex)
  out_fit$group = unique(data$group)
  # model_data
  out_model_data$age = unique(data$age)
  out_model_data$sex = unique(data$sex)
  out_model_data$group = unique(data$group)
  
  # Change column order
  # Fit
  out_fit = setcolorder(out_fit, neworder = c('participant_id', 'group', 'age', 'sex',
                                              'starting_values', 'algorithm', 'xtol_rel',
                                              'maxeval', 'n_iterations', 'iter'))
  # model_data
  out_model_data = setcolorder(out_model_data, neworder = c('participant_id', 'group', 'age', 'sex',
                                              'starting_values', 'algorithm', 'xtol_rel',
                                              'maxeval', 'n_iterations', 'iter'))

  # Save outputs
  # Fit
  file_name = paste('fit', '-', participant_id, '_', 'sv', '-', starting_values, '.tsv', sep = '')
  save_dir = file.path(base_path, 'derivatives', 'model_fitting', file_name,
                       fsep = .Platform$file.sep)
  data.table::fwrite(out_fit, file = save_dir, sep = '\t', na = 'n/a',
                     col.names = TRUE, row.names = FALSE)
  # model_data
  file_name = paste('modeldata', '-', participant_id, '_', 'sv', '-', starting_values, '.tsv', sep = '')
  save_dir = file.path(base_path, 'derivatives', 'model_fitting', file_name,
                       fsep = .Platform$file.sep)
  data.table::fwrite(out_model_data, file = save_dir, sep = '\t', na = 'n/a',
                     col.names = TRUE, row.names = FALSE)
  
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
  make_option(c('-p', '--participant_id'),
              type='character',
              default = NULL,
              help = 'ID of participant',
              metavar = 'PARTICIPANT_ID'),
  make_option(c('-s', '--starting_values'),
              type='character',
              default = NULL,
              help = '`random` for uniform random between boundaries, `fixed` for fixed starting values',
              metavar = 'STARTING_VALUES'),
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
              metavar = 'ITERATIONS'))

# provide options in list to be callable by script
opt_parser = OptionParser(option_list = option_list)
opt = parse_args(opt_parser)

# Cal wrapper with command line inputs
Fit_models_new_wrapper(participant_id = opt$participant_id,
                       starting_values = opt$starting_values,
                       algorithm = opt$algorithm,
                       xtol_rel = opt$xtol_rel,
                       maxeval = opt$maxeval,
                       iterations = opt$iterations)

# Rscript Fit_models_new_wrapper.R --participant_id '1NU6KP5' --starting_values 'random' --algorithm 'NLOPT_GN_DIRECT_L' --xtol_rel 0.00001 --maxeval 100 --iterations 1
