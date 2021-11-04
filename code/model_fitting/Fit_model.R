library(optimr)
library(here)
library(data.table)

# Load LL function
source_path = file.path(here::here(), 'code', 'utils',
                        'Log_Likelihood.R',
                        fsep = .Platform$file.sep)
source(source_path)

# data = Load_data()
# data = Prepare_data_for_fit(data)
# data = data[participant_id == 'G5RTD96' & task_version == 2]
# model = 'Rw'

Fit_model = function(data,
                     model,
                     start_values,
                     lb,
                     ub){
  
  # Set minimizer options (nloptr)
  # Algorithm:
  #   First letter:
  #     G = global minimization
  #     L = local minimization
  #   Second letter:
  #     N = not derivative-based
  #     D = derivative-based
  
  # Options for first (global) optimization
  # opts1 = list('algorithm'='NLOPT_GN_CRS2_LM',
  #              'xtol_rel'=1.0e-4,
  #              'maxeval'=500)
  opts1 = list('algorithm'='NLOPT_GN_DIRECT_L',
               'xtol_rel'=1.0e-4,
               'maxeval'= 5000)
  # Options for second (local) optimization
  opts2 = list('algorithm'='NLOPT_LN_COBYLA',
               'xtol_rel'=1.0e-4,
               'maxeval'=100)
  
  # Set up model list
  model_list = data.table('model_name' = c('Rw', 'Pedlr', 'Pedlr_interdep'),
                          'n_parameters' = c(2,3,4))
  
  # See if provided model is in model list
  if(!model %in% model_list$model_name){
    stop('Chosen model is not specified. Check for typos')
  }
  
  # See if number of parameter concerning values provided fits model
  n_parameters = model_list[model_name == model]$n_parameters
  if(length(start_values) != n_parameters |
     length(lb) != n_parameters | 
     length(ub) != n_parameters){
    stop('Mismatch between provided n of parameter concerning values and n parameters of model')
  }
  
  # Give message to user
  message('      ####### Options #######')
  message('      LB: ', paste(lb, collapse = '  '))
  message('      UB: ', paste(ub, collapse = '  '))
  
  # Give message to user
  message('      ##### First Optim #####')
  message('      x0: ', paste(start_values, collapse = '  '))
  message('      ', paste(names(opts1), opts1, collapse = '\n      ', sep = ': '))
  
  
  # Solve function for minimum
  first = nloptr::nloptr(x0=start_values,
                         # Minimize neg LL
                         eval_f=Log_Likelihood,
                         # Lower bound of parameters (e.g. c(0,0,1))
                         lb=lb,
                         # Upper bound of parameters (e.g. c(1,1,10))
                         ub=ub,
                         # Minimizer options
                         opts=opts1,
                         # Inputs to LL function
                         data=data,
                         design=data,
                         model = model)
  
  # Give message to user
  message('      LL: ', round(first$objective, 3))
  message('      Solution: ', paste(round(first$solution, 3), collapse = '  '))
  
  # Give message to user
  message('      ###### Sec Optim ######')
  message('      x0: ', paste(round(first$solution, 3), collapse = '  '))
  message('      ', paste(names(opts2), opts2, collapse = '\n      ', sep = ': '))
  
  # Use results of global minimization as inputs for local minimization algorithm
  # (to find lowest point in the rough estimate provided by first, global minimization)
  second = nloptr::nloptr(x0=first$solution,
                          # Minimize neg LL
                          eval_f=Log_Likelihood,
                          # Lower bound of parameters
                          lb=lb,
                          # Upper bound of parameters
                          ub=ub,
                          # Minimizer options
                          opts=opts2,
                          # Inputs to LL function
                          data=data,
                          design=data,
                          model = model)
  
  message('      LL: ', round(second$objective, 3))
  message('  ->  Solution: ', paste(round(second$solution, 3), collapse = '  '))
  
  return(list('first_x0' = first$x0,
              'first_solution' = first$solution,
              'first_ll' = first$objective,
              'first_status' = first$status,
              'second_x0' = first$solution,
              'second_solution' = second$solution,
              'second_ll' = second$objective,
              'second_status' = second$status))
  
}
