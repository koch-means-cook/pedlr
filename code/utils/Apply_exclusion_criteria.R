library(here)
library(data.table)
library(magrittr)

source(file.path(here::here(),
                 'code',
                 'utils',
                 'Get_exclusions.R',
                 fsep = .Platform$file.sep))

Apply_exclusion_criteria = function(data,
                                    choice_based_exclusion){
  
  # Excluded participants
  excl_table = Get_exclusions()
  
  # Assure data.table format
  data = as.data.table(data)
  
  # If required only exclude participants based on choice performance
  if(choice_based_exclusion){
    
    # Get participants to exclude
    excl = unique(excl_table[mod == 'choice_performance']$participant_id)
    
    # Exclude from parsed data (across runs)
    res = data[!participant_id %in% excl, ]
    
    
    
  } else if(!choice_based_exclusion){
    
    # Exclude by criteria that span runs (choice based exclusions are also important for ratings, but not vice versa)
    excl = unique(excl_table[mod == 'choice_performance']$participant_id)
    res = data[!participant_id %in% excl, ]
    
    # Exclude single runs based on rating performance
    rating_excl_table = excl_table[mod == 'rating_performance']
    res = res %>%
      # Mark runs to exclude
      .[run == '1', excl := participant_id %in% rating_excl_table[run == '1']$participant_id,
        by = c('participant_id')] %>%
      .[run == '2', excl := participant_id %in% rating_excl_table[run == '2']$participant_id,
        by = c('participant_id')] %>%
      # Exclude marked runs
      .[excl == FALSE, ] %>%
      # Delete marking column to return data
      .[, excl := NULL]
    
  }
  
  # Return excluded data
  return(res)
  
}
