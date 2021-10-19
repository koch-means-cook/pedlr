library(data.table)
library(here)

# Function to load data
Load_model_fits = function(){
  
  # Repository directory
  base_path = here::here()
  
  # Data dir
  data_dir = file.path(base_path,
                       'derivatives',
                       'model_fitting',
                       fsep = .Platform$file.sep)
  # List of all files
  data_list = Sys.glob(file.path(data_dir,
                                 '*.tsv',
                                 fsep = .Platform$file.sep))
  # Select only summary files (summarizing multiple iterations of fits)
  fuse_index = unlist(lapply(data_list,
                             function(x) length(unlist(strsplit(x, '-')))))
  fuse_index = which(fuse_index == min(fuse_index))
  data_list = data_list[fuse_index]
  
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