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
    
    # Sort and rename model names
    if('rw' %in% data[,col] |
       'uncertainty' %in% data[,col] |
       'seplr' %in% data[,col] |
       'uncertainty_seplr' %in% data[,col] |
       'surprise' %in% data[,col] |
       'uncertainty_surprise' %in% data[,col]){
      
      #rw uncertainty seplr uncertainty_seplr surprise uncertainty_surprise
      
      # Set different models
      models = c('rw',
                 'uncertainty',
                 'seplr',
                 'uncertainty_seplr',
                 'surprise',
                 'uncertainty_surprise')
      model_names_new = c('RW',
                          'Uncertainty',
                          'Valence',
                          'Unc+Valence',
                          'Surprise',
                          'Unc+Surprise')
      
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
      
      # Rename models
      data[,col] = as.character(data[,col])
      data[data[,col] == 'rw', col] = 'RW'
      data[data[,col] == 'uncertainty', col] = 'Uncertainty'
      data[data[,col] == 'seplr', col] = 'Valence'
      data[data[,col] == 'uncertainty_seplr', col] = 'Unc+Valence'
      data[data[,col] == 'surprise', col] = 'Surprise'
      data[data[,col] == 'uncertainty_surprise', col] = 'Unc+Surprise'
      
      # Set factor order
      data[,col] = factor(data[,col],
                          levels = model_names_new)
    }
    
    # Sort and rename parameters
    if('z_(Intercept)' %in% data[,col] |
       'z_V1' %in% data[,col] |
       'z_V2' %in% data[,col] |
       '(Intercept)' %in% data[,col] |
       'V1' %in% data[,col] |
       'V2' %in% data[,col] |
       'alpha' %in% data[,col] |
       'z_V1u' %in% data[,col] |
       'z_V2u' %in% data[,col] |
       'V1u' %in% data[,col] |
       'V2u' %in% data[,col] |
       'pi' %in% data[,col] |
       'alpha_pos' %in% data[,col] |
       'alpha_neg' %in% data[,col] |
       'l' %in% data[,col] |
       'u' %in% data[,col] |
       's' %in% data[,col]){
      
      # Set all parameters
      paras = c('z_(Intercept)',
                'z_V1',
                'z_V2',
                '(Intercept)',
                'V1',
                'V2',
                'alpha',
                'z_V1u',
                'z_V2u',
                'V1u',
                'V2u',
                'pi',
                'alpha_pos',
                'alpha_neg',
                'l',
                'u',
                's')
      paras_names_new = c('\\beta_0 (z)',
                          '\\beta_1 (z)',
                          '\\beta_2 (z)',
                          '\\beta_0',
                          '\\beta_1',
                          '\\beta_2',
                          '\\alpha',
                          '\\beta_3 (z)',
                          '\\beta_4 (z)',
                          '\\beta_3',
                          '\\beta_4',
                          '\\pi',
                          '\\alpha_+',
                          '\\alpha_-',
                          'l',
                          'u',
                          's')
      # Raise error if you find a new parameter
      # for(i_para in unique(data[,col])){
      #   if(!i_para %in% paras){
      #     stop(paste('New parameter in data: ',
      #                '"', i_para, '"',
      #                ' cannot be found in parameter pool (',
      #                paste(paras, collapse = ', '),
      #                ')',
      #                sep = ''))
      #   }
      # }
      
      # Rename parameters
      data[,col] = as.character(data[,col])
      for(name_count in seq(length(paras))){
        data[data[,col] == paras[name_count], col] = paras_names_new[name_count]
      }
      
      # # Set factor order
      # data[,col] = factor(data[,col],
      #                     levels = paras_names_new)
      
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