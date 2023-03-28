Simulation_wrapper = function(){
  
  # Load own functions
  source(file.path(here::here(), 'code', 'simulation', 'Simulate.R', fsep = .Platform$file.sep))
  source(file.path(here::here(), 'code', 'simulation', 'Compute_value_per_trial.R', fsep = .Platform$file.sep))
  source(file.path(here::here(), 'code', 'model_fitting', 'LRfunction.R', fsep = .Platform$file.sep))
  
}