library(data.table)

Prepare_data_for_plot = function(data){
  
  data = as.data.frame(data)
  
  for(col in colnames(data)){
    
    # Rename age-groups
    if('older' %in% data[,col] | 'younger' %in% data[,col]){
      data[,col] = as.character(data[,col])
      data[data[,col] == 'older', col] = 'Older\nAdults'
      data[data[,col] == 'younger', col] = 'Younger\nAdults'
      data[,col] = factor(data[,col],
                          levels = c('Younger\nAdults', 'Older\nAdults'))
    }
    
    # Sort model names
    if('Rw' %in% data[,col] |
       'Pedlr' %in% data[,col] |
       'Pedlr_interdep' %in% data[,col] |
       'Pedlr_simple' %in% data[,col] |
       'Pedlr_fixdep' %in% data[,col]){
      
      # Set different models
      models = c('Rw',
                 'Pedlr_simple',
                 'Pedlr',
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
  }
  
  data = data.table::as.data.table(data)
  return(data)
}