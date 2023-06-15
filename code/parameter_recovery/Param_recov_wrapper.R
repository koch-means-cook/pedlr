library(optparse)
library(data.table)
library(here)

Param_recov_wrapper = function(participant_id,
                               model,
                               input_params,
                               starting_values,
                               algorithm,
                               xtol_rel,
                               maxeval,
                               iterations,
                               temperature,
                               tau){
  
  # participant_id = '09RI1ZH'
  # input_params = c(0.1, 0.7, 2, NA)
  # starting_values = c(0.5, 0.5, 5, NA)
  # algorithm = 'NLOPT_GN_DIRECT_L'
  # xtol_rel = 1.0e-5
  # maxeval = 1000
  # iterations = 1
  # tau = 0.2
  # temperature = 7
  
  # Load own functions
  source(file.path(here::here(), 'code', 'parameter_recovery', 'Param_recov.R', fsep = .Platform$file.sep))
  
  # Load base data for simulation based on participant
  data = data.table::fread(file.path(here::here(),
                                     'data',
                                     paste(participant_id,
                                           '_exp_data.tsv',
                                           sep = '')),
                           sep = '\t',
                           na.strings = 'n/a')
  
  # Have variable for random vs. user-supplied input_params
    # Chose number of parameters based on model variable
    # have variables at the end of function in case input params are user supplied
      # ip1, ip2, ip3, ip4 (get ignored if random are used)
    # Do the same for starting values
  # Have iterations run in a way that there are different random input params every time (or the same fixed ones)
  # Set up data aggregation and data saving
  # Set up optparse
  # Set up hpc submit script
  # Write analysis script for param recov
  
  
}

