library(here)
library(data.table)
library(magrittr)

# source(file.path(here::here(),
#                  'code',
#                  'utils',
#                  'Load_data.R',
#                  fsep = .Platform$file.sep))

Get_sampling = function(){
  
  # Get directory of repository
  base_path = here::here()
  
  # Load designs
  file_pattern = file.path(base_path, 'pedlr-task', 'client', 'public', 'designs', '*run-1.tsv',
                           fsep = .Platform$file.sep)
  files_r1 = Sys.glob(file_pattern)
  file_pattern = file.path(base_path, 'pedlr-task', 'client', 'public', 'designs', '*run-2.tsv',
                           fsep = .Platform$file.sep)
  files_r2 = Sys.glob(file_pattern)
  data = data.table()
  for(i in seq(length(files_r1))){
    file_r1 = files_r1[i]
    file_r2 = files_r2[i]
    temp_r1 = data.table::fread(file_r1, sep = '\t', na.strings = 'n/a')
    temp_r1$design_name = substr(basename(file_r1), 0,15)
    temp_r2 = data.table::fread(file_r2, sep = '\t', na.strings = 'n/a')
    temp_r2$design_name = substr(basename(file_r2), 0,15)
    temp = rbind(temp_r1, temp_r2)
    temp$n_design = i
    data = rbind(data, temp)
  }
  n_designs = max(unique(data$n_design))
  
  data_diff = data %>%
    # Concatenate all samples
    .[, .(stim = c(option_left, option_right),
          outcome = c(reward_stim_1, reward_stim_2)),
      by = c('n_design', 'task_version', 'design_name')] %>%
    # get mean for each bandit
    .[, stim := as.factor(stim)] %>%
    .[, .(mean_outcome = mean(outcome)),
      by = c('n_design', 'task_version', 'design_name', 'stim')] %>%
    # Cast to wide format to compare stimuli
    data.table::dcast(., n_design + task_version + design_name ~ paste0('mean_', stim), value.var = 'mean_outcome') %>%
    # Get differences between bandits
    .[, ':='(diff2m1 = mean_2 - mean_1,
             diff3m2 = mean_3 - mean_2)] %>%
    .[, ':='(loc2m1 = mean_1 + (diff2m1/2),
             loc3m2 = mean_2 + (diff3m2/2))]
  
  # Return sampling summary
  return(data_diff)
  
}
