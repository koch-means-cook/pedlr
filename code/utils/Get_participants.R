library(here)


Get_participants = function(){
  
  # Get repo path
  base_path = here::here()
  # Get data path
  data_path = file.path(base_path, 'data', '*.tsv', fsep = .Platform$file.sep)
  
  # Get participant names
  data = basename(Sys.glob(data_path))
  data = strsplit(data, split = '_')
  data = sapply(data, "[[", 1)
  
  return(data)
}

Get_participants()