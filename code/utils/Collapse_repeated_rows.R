library(data.table)

Collapse_repeated_rows = function(x,
                                  columns,
                                  replace = ''){
  # Convert to matrix for easier replace
  x = as.matrix(x)
  
  # For each specified column
  for(j in columns){
    # Check if columns is character or factor
    if(class(x[1:nrow(x),j]) %in% c('character', 'factor')){
      # Get index of repeated rows
      idx = x[1:nrow(x), j] == data.table::shift(x[1:nrow(x), j], 1)
      # Replace natural NAs introduced by shift function
      idx[is.na(idx)] = FALSE
      x[idx,j] = replace
    } else{
      stop(paste0('Column j=', j, ' is neither character or factor.'))
    }
  }
  
  # Return as data.table object
  return(data.table::data.table(x))
}