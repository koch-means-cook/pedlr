library(optimr)
library(here)

# Load models
source_path = file.path(here::here(), 'code', 'models',
                        fsep = .Platform$file.sep)
source_files = list.files(source_path, pattern = "[.][rR]$",
                          full.names = TRUE, recursive = TRUE)
invisible(lapply(source_files, function(x) source(x)))

# Load other functions function
source_path = file.path(here::here(), 'code', 'utils',
                        'Log_Likelihood.R',
                        fsep = .Platform$file.sep)
source(source_path)
source_path = file.path(here::here(), 'code', 'utils',
                        'Load_data.R',
                        fsep = .Platform$file.sep)
source(source_path)
source_path = file.path(here::here(), 'code', 'utils',
                        'Prepare_data_for_fit.R',
                        fsep = .Platform$file.sep)
source(source_path)

# 
# data = Load_data()
# data = Prepare_data_for_fit(data)
# data = data[participant_id == 'G5RTD96' & task_version == 2]
# model = 'Rw'

Fit_model = function(data,
                     model){

  # Set minimizer options (nloptr)
  # Algorithm:
  #   First letter:
  #     G = global minimization
  #     L = local minimization
  #   Second letter:
  #     N = not derivative-based
  #     D = derivative-based
  # opts = list('algorithm'='NLOPT_LN_COBYLA',
  #             'xtol_rel'=1.0e-8)
  opts = list('algorithm'='NLOPT_GN_CRS2_LM',
              'xtol_rel'=1.0e-8,
              'maxeval'=1.0e5)
  
  # Pedlr_interdep
  if(model == 'Pedlr_interdep'){
    # Randomly sampled initial values
    x = c(runif(1,0,1),    # alpha0
          runif(1,0,1),    # alpha1
          runif(1,0,1),    # interdependence
          runif(1,0.5,10)) # temperature (softmax)
    # Solve function for minimum
    opt = nloptr::nloptr(x0=x,
                         # Minimize neg LL
                         eval_f=Log_Likelihood,
                         # Lower bound of parameters
                         lb=c(0,    # alpha0
                              0,    # alpha1
                              0,    # interdependence
                              0.5), # temperature
                         # Upper bound of parameters
                         ub=c(1,    # alpha0
                              1,    # alpha1
                              1,    # interdependence
                              10),  # temperature
                         # Minimizer options
                         opts=opts,
                         # Inputs to LL function
                         data=data,
                         design=data,
                         model = 'Pedlr_interdep')
    opt$solution
    
    # Use results of global minimization as inputs for local minimization algorithm
    x = opt$solution
    opt = nloptr::nloptr(x0=x,
                         # Minimize neg LL
                         eval_f=Log_Likelihood,
                         # Lower bound of parameters
                         lb=c(0,    # alpha0
                              0,    # alpha1
                              0,    # interdependence
                              0.5), # temperature
                         # Upper bound of parameters
                         ub=c(1,    # alpha0
                              1,    # alpha1
                              1,    # interdependence
                              10),  # temperature
                         # Minimizer options
                         opts=list('algorithm'='NLOPT_LN_COBYLA',
                                   'xtol_rel'=1.0e-8),
                         # Inputs to LL function
                         data=data,
                         design=data,
                         model = 'Pedlr_interdep')
    opt$solution
    
  # Pedlr
  } else if(model == 'Pedlr'){
    x = c(runif(1,0,1),    # alpha0
          runif(1,0,1),    # alpha1
          runif(1,0.5,10)) # temperature (softmax)
    opt = nloptr::nloptr(x0=x,
                         eval_f=Log_Likelihood,
                         lb=c(0,    # alpha0
                              0,    # alpha1
                              0.5), # temperature
                         ub=c(1,    # alpha0
                              1,    # alpha1
                              10),  # temperature
                         opts=opts,
                         data=data,
                         design=data,
                         model = 'Pedlr')
    opt$solution
    
    # Use results of global minimization as inputs for local minimization algorithm
    x = opt$solution
    opt = nloptr::nloptr(x0=x,
                         eval_f=Log_Likelihood,
                         lb=c(0,    # alpha0
                              0,    # alpha1
                              0.5), # temperature
                         ub=c(1,    # alpha0
                              1,    # alpha1
                              10),  # temperature
                         opts=list('algorithm'='NLOPT_LN_COBYLA',
                                   'xtol_rel'=1.0e-8),
                         data=data,
                         design=data,
                         model = 'Pedlr')
    opt$solution
    
    
  # Rw
  } else if(model == 'Rw'){
    # x = c(runif(1,0,1),    # alpha
    #       runif(1,0.5,10)) # temperature (softmax)
    # opt = nloptr::nloptr(x0=x,
    #                      eval_f=Log_Likelihood,
    #                      lb=c(0,    # alpha
    #                           0.5), # temperature
    #                      ub=c(1,    # alpha
    #                           10),  # temperature
    #                      opts=opts,
    #                      data=data,
    #                      design=data,
    #                      model = 'Rw')
    # print(opt$solution)
    
    # Use results of global minimization as inputs for local minimization algorithm
    #x = opt$solution
    
    fit = data.table()
    for(i in seq(100)){
      x = c(runif(1,0,1),    # alpha
            runif(1,0.5,10)) # temperature (softmax)
      opt = nloptr::nloptr(x0=x,
                           eval_f=Log_Likelihood,
                           lb=c(0,    # alpha
                                0.5), # temperature
                           ub=c(1,    # alpha
                                10),  # temperature
                           opts=list('algorithm'='NLOPT_LN_COBYLA',
                                     'xtol_rel'=1.0e-8),
                           data=data,
                           design=data,
                           model = 'Rw')
      #print(opt$solution)
      fit = rbind(fit, data.table('alpha' = opt$solution[1], 'temp' = opt$solution[2]))
      print(i)
    }
    
    hist(fit$alpha)
    hist(fit$temp)
    
  }

}