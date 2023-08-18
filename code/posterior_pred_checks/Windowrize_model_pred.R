library(here)
library(data.table)
library(magrittr)
library(optparse)

Windowrize_model_pred = function(participant_id){
  
  # participant_id = '1NU6KP5'
  
  # Get repo root
  base_path = here::here()
  
  # Load own functions
  source_path = file.path(base_path, 'code', 'utils',
                          fsep = .Platform$file.sep)
  source(file.path(source_path, 'Add_comp.R', fsep = .Platform$file.sep))
  source(file.path(source_path, 'Get_running_avg.R', fsep = .Platform$file.sep))
  
  # Load data of models playing task with participant's parameters
  file_pattern = file.path(base_path, 'derivatives', 'posterior_pred_checks',
                           paste0('postpred-', participant_id, '_model-*.tsv'),
                           fsep = .Platform$file.sep)
  files = Sys.glob(file_pattern)
  
  # Allocate table holding data for all models
  data = data.table()
  
  # Load and fuse data of each model
  for(file in files){
    temp = data.table::fread(file, sep = '\t', na.strings = 'n/a')
    data = rbind(data, temp)
  }
  
  # Prepare data for windowrization
  data_ii = data %>%
    .[, choice := 'left'] %>%
    .[model_choice_side == 2, choice := 'right'] %>%
    .[, ':='(option_choice = model_choice_option,
             outcome = model_outcome,
             value_low = val_b_1,
             value_mid = val_b_2,
             value_high = val_b_3)] %>%
    # Get bandit comparison of trial
    Add_comp(.) %>%
    # Add 1 and 0 for correct and incorrect choices, respectively
    .[, correct := if(option_left > option_right) 'left' else 'right',
      by = c('participant_id', 'model', 'run', 'trial')] %>%
    .[, correct_choice := correct == choice] %>%
    # Set correctness of choice to NA for irrelevant bandits and forced choices
    # to ignore during calculation
    .[comp != '1v2' | trial_type != 'choice', correct_choice := NA,
      by = c('participant_id', 'model', 'run')] %>%
    # Get running averages
    .[, ':='(avg_1_running = Get_running_avg(choice_option = option_choice,
                                             choice_outcome = outcome,
                                             stim = 1),
             avg_2_running = Get_running_avg(choice_option = option_choice,
                                             choice_outcome = outcome,
                                             stim = 2),
             avg_3_running = Get_running_avg(choice_option = option_choice,
                                             choice_outcome = outcome,
                                             stim = 3)),
      by = c('participant_id', 'model', 'run')]
  
  # Allocate data holding +-5 trials from rare outcome of bandit 2
  window_data = data.table()
  
  # Function to get data slice +-5 trials from rare outcome of bandit 2
  Windowrize = function(data,
                        index_rare,
                        window_size){
    result = data[seq(max(index_rare - window_size + 1, 1),
                      index_rare + window_size), ]
    result$window_center = data$trial[index_rare]
    result$window_relative = seq(max(index_rare - window_size + 1, 1),
                                 index_rare + window_size) - index_rare
    result[window_relative == 0]$correct_choice = NA
    
    result$window_center = as.factor(result$window_center)
    result$window_relative = as.factor(result$window_relative)
    return(result)
  }
  
  # For each Model, & run
  for(i_model in unique(data_ii$model)){
    for(i_run in unique(data_ii$run)){
      
      # Subset data
      temp_data = data_ii[model == i_model & run == i_run, ]
      
      # Get all trials where rare outcomes were obtained
      idx_chosen_rare_outcome = which(temp_data$is_rare == 1 &
                                        # Also allow rare outcomes that came from forced choices
                                        temp_data$trial_type %in% c('choice', 'forced') &
                                        temp_data$option_choice == 2)
      # For each of the rare-outcome trials
      for(rare_count in seq(1,length(idx_chosen_rare_outcome))){
        # Get data slice
        temp = Windowrize(data = temp_data,
                          index_rare = idx_chosen_rare_outcome[rare_count],
                          window_size = 2) %>%
          .[, window_relative := factor(window_relative, levels = unique(sort(window_relative)))]
        # Add count of the rare outcome
        temp$i_rare = rare_count
        # Fuse data for each participant & run
        window_data = rbind(window_data, temp)
        
      }
    }
  }
  
  # Summarize data
  window_data_run = window_data %>%
    # Eliminate windows which extended across trial boundaries of run
    .[, relative_trial := as.numeric(as.character(window_center)) + as.numeric(as.character(window_relative))] %>%
    .[!(run == 1 &  (relative_trial < 1 | relative_trial > 240)),] %>%
    .[ !(run == 2 & (relative_trial < 241 | relative_trial > 480)),] %>%
    # Sort by relative window
    .[order(rank(participant_id), rank(model), rank(run), rank(window_relative)),] %>%
    # Get mean accuracy across all relative window positions (-1 to +2)
    .[, .(mean_accuracy = mean(correct_choice, na.rm = TRUE),
          n_data = sum(!is.na(correct_choice), na.rm = TRUE)),
      by = c('participant_id', 'model', 'run', 'window_relative')]
  
  # Write output
  out = file.path(base_path, 'derivatives', 'posterior_pred_checks',
                  paste0('windowrizepred-', participant_id, '.tsv'),
                  fsep = .Platform$file.sep)
  data.table::fwrite(x = window_data_run, file = out, sep = '\t', na = 'n/a')
  
}

# Create options to pass to script
option_list = list(
  make_option(c('-p', '--participant_id'),
              type='character',
              default = NULL,
              help = 'ID of participant',
              metavar = 'PARTICIPANT_ID'))

# provide options in list to be callable by script
opt_parser = OptionParser(option_list = option_list)
opt = parse_args(opt_parser)

# Run function with command line arguments
Windowrize_model_pred(participant_id = opt$participant_id)
