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
                              temperature,
                              tau){

    # participant_id = '09RI1ZH'
    # model = 'surprise'
    # x1_low = 0.1
    # x1_high = 0.7
    # x1_n = 5
    # x2_low = 0.1
    # x2_high = 0.7
    # x2_n = 5
    # x3_low = -10
    # x3_high = 10
    # x3_n = 5
    # x4_low = NA
    # x4_high = NA
    # x4_n = NA
    # tau = 0.2
    # temperature = 7
  
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
    para_combs = data.table::data.table(expand.grid(x1,x2))
    colnames(para_combs) = c('x1', 'x2')
    para_combs$x3 = NA
    para_combs$x4 = NA
    # Specify parameter names
    para_names = c('alpha', 'pi')
  } else if(model == 'surprise'){
    # Three parameters identifying LRfunction (l,u,s)
    x1 = seq(x1_low, x1_high, length.out = x1_n)
    x2 = seq(x2_low, x2_high, length.out = x2_n)
    x3 = seq(x3_low, x3_high, length.out = x3_n)
    # Get all combinations of specified parameters
    para_combs = data.table::data.table(expand.grid(x1,x2, x3))
    colnames(para_combs) = c('x1', 'x2', 'x3')
    para_combs$x4 = NA
    # Specify parameter names
    para_names = c('l', 'u', 's')
  } else if(model == 'uncertainty+surprise'){
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
  
  # Load specified participant (we are not using participant choices, only the
  # specific design a participant was assigned to)
  file = file.path(here::here(), 'data', paste('09RI1ZH', '_exp_data.tsv', sep = ''),
                   fsep = .Platform$file.sep)
  data = data.table::fread(file, sep = '\t', na.strings = 'n/a')
  
  # simulate across all combinations given
  # Allocate output table
  out = data.table::data.table()
  for(i in seq(nrow(para_combs))){
    # Get parameter combination of current iteration
    paras = as.matrix(para_combs[i,])[1,]
    
    # Simulate data based on parameter combination
    temp = Simulate(data = data,
                    x = paras,
                    temperature = temperature,
                    tau = tau)
    # fuse data  
    out = rbind(out, temp)
  }
  
  # Save data in simulation out
  file_pattern = file.path()
  
  # After: build HPC script

  
}