library(data.table)

Prepare_data_for_plot = function(data){
  
  data = as.data.frame(data)
  
  for(col in colnames(data)){
    
    if('older' %in% data[,col] | 'younger' %in% data[,col]){
      data[,col] = as.character(data[,col])
      data[data[,col] == 'older', col] = 'Older\nAdults'
      data[data[,col] == 'younger', col] = 'Younger\nAdults'
      data[,col] = factor(data[,col])
    }
    
    
  }
  
  data = data.table::as.data.table(data)
  return(data)
}