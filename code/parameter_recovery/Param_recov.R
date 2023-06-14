

Param_recov = function(data,
                       x,
                       temperature,
                       tau){
  
    # data = data.table::fread(file.path(here::here(), 'data', '09RI1ZH_exp_data.tsv'),
    #                          sep = '\t', na.strings = 'n/a')
    # x = c(0.1, 0.5, 0.7, NA)
    # tau = 0.2
    # temperature = 7
  
  # Source self-written functions
  source(file.path(here::here(), 'code', 'simulation', 'Simulate.R'))
  
  # Simulate behavior given model parameters
  # NUMBER OF PARAMETERS sets which model to use
  #   x = c(0.1, NA, NA, NA) ==> RW model ==> alpha = 0.1
  #   x = c(0.1, 0.5, NA, NA) ==> Uncertainty model ==> alpha = 0.1, pi = 0.5
  #   x = c(0.1, 0.5, 0.7, NA) ==> Surprise model ==> l = 0.1, u = 0.5, s = 0.7 
  #   x = c(0.1, 0.5, 0.7, 0.5) ==> Suprise + uncertainty model ==> l = 0.1, u = 0.5, s = 0.7, pi = 0.5
  bla = Simulate(data = data,
                 x = x,
                 temperature = temperature,
                 tau = tau)
  
}