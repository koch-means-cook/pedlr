library(here)
library(data.table)

source(file.path(here::here(),
                 'code',
                 'utils',
                 'Get_exclusions.R',
                 fsep = .Platform$file.sep))

Apply_exclusion_criteria = function(data){
  
  # Get participants to exclude
  excl = unique(Get_exclusions()$participant_id)
  
  # Exclude from parsed data
  data = as.data.table(data)
  res = data[!participant_id %in% excl, ]
  
  return(res)
  
}