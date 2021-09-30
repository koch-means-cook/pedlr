library(data.table)

# Function to add comparison to pedlr data
Add_comp = function(data){
  
  comp = apply(cbind(data$option_left, data$option_right),
               1,
               function(x) paste(sort(x), collapse = 'v'))
  
  data$comp = factor(comp, levels = c('1v2', '2v3', '1v3'))
  
  return(data)
}