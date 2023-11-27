library(data.table)
library(here)

Get_fit = function(participant_id,
                   starting_values){
  
  # participant_id = '2HOJBMD'
  # starting_values = 'random'
  
  # Check input
  if(!starting_values %in% c('random', 'fixed')){
    stop(paste0(starting_values, ' is not a valid option for the starting_values argument (random or fixed)'))
  }
  
  # Load file containing parameter fits of participant
  file = file.path(here::here(),
                   'derivatives',
                   'model_fitting',
                   paste0('fit-', participant_id, '_sv-', starting_values, '.tsv'),
                   fsep = .Platform$file.sep)
  param_data = data.table::fread(file,
                                 sep = '\t',
                                 na.strings = 'n/a') %>%
    .[variable != 'LRs',]
  
  # Load file containing experiment data
  file = file.path(here::here(),
                   'derivatives',
                   'model_fitting',
                   paste0('modeldata-', participant_id, '_sv-', starting_values, '.tsv'),
                   fsep = .Platform$file.sep)
  modeldata = data.table::fread(file,
                                 sep = '\t',
                                 na.strings = 'n/a')
  
  # Fuse output
  out = list(fit = param_data,
             modeldata = modeldata)
  
  return(out)
}
