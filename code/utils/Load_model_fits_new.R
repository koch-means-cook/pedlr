library(data.table)
library(here)

# Function to load data
Load_model_fits_new = function(){
  
  # Repository directory
  base_path = here::here()
  
  # Data dir
  data_dir = file.path(base_path,
                       'derivatives',
                       'model_fitting',
                       fsep = .Platform$file.sep)
  # List of all files
  data_list = Sys.glob(file.path(data_dir,
                                 'fit-*_sv-*.tsv',
                                 fsep = .Platform$file.sep))
  
  # Load data
  data = data.table()
  for(file in data_list){
    # load individual file in data directory
    temp = data.table::fread(file = file,
                             sep = '\t',
                             na.strings = 'n/a',
                             header = TRUE)
    # Bind all participants into one file
    data = rbind(data, temp)
  }
  
  # Return loaded data
  return(data)
  
}