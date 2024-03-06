library(data.table)
library(here)
library(parallel)

# Function to load data
Load_model_data = function(sv = 'random'){
  
  load_path = file.path(here::here(),
                        'derivatives',
                        'model_fitting',
                        fsep = .Platform$file.sep)
  # Load data with fixed betas during simulation
  files = Sys.glob(file.path(load_path,
                             paste('modeldata-*_sv-', sv, '.tsv', sep = ''),
                             fsep = .Platform$file.sep))
  
  # Function to load text files (.tsv)
  Load_tsv = function(file_path){
    tsv = data.table::fread(file_path,
                            sep = '\t',
                            na.strings = 'n/a')
    return(tsv)
  }
  
  # Get list of all text files using parallel processing
  data_list = parallel::mclapply(X = files,
                                 FUN = Load_tsv,
                                 mc.cores = 4)
  # Bind individual list entries (loaded text files) to single data frame
  # (using optimized bind function by data.table package)
  data = data.table::rbindlist(data_list)
  
  # Return loaded data
  return(data)
  
}