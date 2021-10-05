
Prepare_data_for_fit = function(data){
  
  # Set choice to be stimulus chosen
  data$choice = data$option_choice
  data[choice == 0]$choice = NA
  
  return(data)
  
}