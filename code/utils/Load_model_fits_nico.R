library(data.table)
library(here)
library(magrittr)

# Function to load data
Load_model_fits_nico = function(){
  
  # Repository directory
  base_path = here::here()
  
  # Get groups of participants
  source_file = file.path(base_path, 'code', 'utils', 'Load_data.R',
                          fsep = .Platform$file.sep)
  source(source_file)
  info_groups = Load_data() %>%
    .[, c('participant_id', 'group')] %>%
    .[, .(participant_id = unique(participant_id)),
      by = 'group']
    
  # Data dir
  data_dir = file.path(base_path,
                       'derivatives',
                       'model_fitting',
                       'nico',
                       fsep = .Platform$file.sep)
  # List of all files
  data_list = Sys.glob(file.path(data_dir,
                                 '*_fit-*.tsv',
                                 fsep = .Platform$file.sep))
  
  # Load data
  data = data.table()
  for(file in data_list){
    # get fit iteration
    iter = as.numeric(substr(unlist(strsplit(basename(file), '-'))[2], 0,3))
    
    # load individual file in data directory
    temp = data.table::fread(file = file,
                             sep = '\t',
                             na.strings = 'n/a',
                             header = TRUE)
    cols = colnames(temp)[!colnames(temp) %in% c('participant_id', 'run', 'random_x0')]
    temp = temp %>%
      .[, (cols) := lapply(.SD, as.numeric), .SDcols = cols] %>%
      data.table::melt(id.vars = c('participant_id', 'run', 'random_x0', 'Choice1v2', 'Choice2v3', 'Rating2', 'RatingSD2')) %>%
      # Get model
      .[, ':='(model = unlist(strsplit(as.character(variable), '_'))[1],
               var = unlist(strsplit(as.character(variable), '_'))[2]),
        by = c('participant_id', 'run', 'random_x0', 'variable')] %>%
      # 
      # .[substr(var, 0,2) != 'x0', type := 'solution'] %>%
      data.table::dcast(participant_id + run + random_x0 + Choice1v2 + Choice2v3 + Rating2 + RatingSD2 + model ~ var,
                        value.var = 'value') %>%
      data.table::melt(measure.vars = c('alpha', 'alpha0', 'alpha1', 'temp',
                                        'x0alpha', 'x0alpha0', 'x0alpha1', 'x0temp')) %>%
      .[substr(variable, 0,2) == 'x0', type := 'x0'] %>%
      .[substr(variable, 0,2) != 'x0', type := 'solution'] %>%
      .[variable == 'x0alpha', variable := 'alpha'] %>%
      .[variable == 'x0alpha0', variable := 'alpha0'] %>%
      .[variable == 'x0alpha1', variable := 'alpha1'] %>%
      .[variable == 'x0temp', variable := 'temp'] %>%
      data.table::dcast(participant_id + run + random_x0 + Choice1v2 + 
                          Choice2v3 + Rating2 + RatingSD2 + model + LL + init + 
                          value1 + value2 + value3 + variable ~ type, value.var = 'value') %>%
      # Rename cols
      data.table::setnames(old = 'variable', new = 'para') %>%
      data.table::setnames(old = 'LL', new = 'll') %>%
      # exclude parameters from other models
      .[!is.na(solution),]
      
    # Rename models
    temp$model = factor(temp$model)
    levels(temp$model) = c('Pedlr', 'Pedlr_fixdep', 'Pedlr_simple', 'Rw')
    
    # Add iteration
    temp$iter = iter
    
    # Bind all participants into one file
    data = rbind(data, temp)
  }
  
  data = data %>%
    # Add group variable
    data.table::merge.data.table(x = .,
                                 y = info_groups,
                                 all =TRUE) %>%
    # Order columns
    data.table::setcolorder(., neworder = c('participant_id',
                                            'group',
                                            'run',
                                            'model',
                                            'll',
                                            'para',
                                            'x0',
                                            'solution'))
  
  # Return loaded data
  return(data)
  
}