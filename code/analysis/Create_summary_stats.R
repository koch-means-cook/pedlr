library(data.table)
library(here)
library(magrittr)

Create_summary_stats = function(){
  
  # Get directory of repository
  base_path = here::here()
  
  # Load pre-written functions
  source_path = file.path(base_path, 'code', 'utils',
                          fsep = .Platform$file.sep)
  source_files = list.files(source_path, pattern = "[.][rR]$",
                            full.names = TRUE, recursive = TRUE)
  invisible(lapply(source_files, function(x) source(x)))
  
  # Load data
  data = Load_data()
  
  # Summary: Mistakes on forced choices
  check_fce = data %>%
    .[]
  
  
}