library(data.table)

Prepare_data_for_plot = function(data){
  
  data = as.data.frame(data)
  
  for(col in colnames(data)){
    
    # Rename age-groups
    if('older' %in% data[,col] | 'younger' %in% data[,col]){
      data[,col] = as.character(data[,col])
      data[data[,col] == 'older', col] = 'Older\nadults'
      data[data[,col] == 'younger', col] = 'Younger\nadults'
      data[,col] = factor(data[,col],
                          levels = c('Younger\nadults', 'Older\nadults'))
    }
    
    # Sort model names
    if('Rw' %in% data[,col] |
       'Pedlr' %in% data[,col] |
       'Pedlr_interdep' %in% data[,col] |
       'Pedlr_simple' %in% data[,col] |
       'Pedlr_fixdep' %in% data[,col] |
       'Pedlr_simple_const' %in% data[,col]){
      
      # Set different models
      models = c('Rw',
                 'Pedlr_simple',
                 'Pedlr_simple_const',
                 'Pedlr',
                 'Pedlr_step',
                 'Pedlr_fixdep',
                 'Pedlr_interdep')
      # Raise error if you find a new model
      for(i_model in unique(data[,col])){
        if(!i_model %in% models){
          stop(paste('New model in data: ',
                     '"', i_model, '"',
                     ' cannot be found in model pool (',
                     paste(models, collapse = ', '),
                     ')',
                     sep = ''))
        }
      }
      # Set factor order
      data[,col] = factor(data[,col],
                          levels = models)
    }
    
    # Sort parameters
    if('alpha' %in% data[,col] |
       'alpha0' %in% data[,col] |
       'alpha1' %in% data[,col] |
       'interdep' %in% data[,col] |
       'temperature' %in% data[,col]){
      
      # Set all parameters
      paras = c('alpha',
                'alpha0',
                'alpha1',
                'interdep',
                'temperature')
      # Raise error if you find a new parameter
      for(i_para in unique(data[,col])){
        if(!i_para %in% paras){
          stop(paste('New parameter in data: ',
                     '"', i_para, '"',
                     ' cannot be found in parameter pool (',
                     paste(paras, collapse = ', '),
                     ')',
                     sep = ''))
        }
      }
      # Set factor order
      data[,col] = factor(data[,col],
                          levels = paras)
      
    }
    
    # Make run a factor variable
    if(col == 'run'){
      data[,col] = factor(data[,col])
    }
    
    
    # Rename trial types
    if('choice' %in% data[,col] | 'forced' %in% data[,col]){
      data[,col] = as.character(data[,col])
      data[data[,col] == 'choice', col] = 'Free\nchoices'
      data[data[,col] == 'forced', col] = 'Guided\nchoices'
      data[,col] = factor(data[,col],
                          levels = c('Free\nchoices', 'Guided\nchoices'))
    }
    
    # Rename bandits
    if(length(unique(data[,col])) == 3 & grepl('est', col)){
      data[,col] = as.character(data[,col])
      data[data[,col] == '1', col] = 'Low'
      data[data[,col] == '2', col] = 'Mid'
      data[data[,col] == '3', col] = 'High'
      data[,col] = factor(data[,col],
                          levels = c('Low', 'Mid', 'High'))
    }
  }
  
  data = data.table::as.data.table(data)
  return(data)
}