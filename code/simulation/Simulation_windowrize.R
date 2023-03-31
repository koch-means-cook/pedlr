library(here)
library(parallel)
library(data.table)
library(magrittr)

Simulation_windowrize = function(design_base){

  # design_base = '0IUKYRW'
  
  # Get repo path
  base_path = here::here()
  
  # Source own functions
  source(file.path(base_path, 'code', 'utils', 'Add_comp.R',
                   fsep = .Platform$file.sep))
  source(file.path(base_path, 'code', 'utils', 'Get_running_avg.R',
                   fsep = .Platform$file.sep))
  
  message(paste0('Loading data...'))
  
  # Load data
  # Get paths of files to load
  pattern = file.path(base_path,
                      'derivatives',
                      'simulation',
                      paste0('modelsim_base-', design_base, '_model-*.tsv'))
  files = Sys.glob(pattern)
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
  
  # Organize data table
  data = data %>%
    # Rename "participants" column to avoid confusion (not real participant data, 
    # but used design is tied to participant)
    setnames(old = 'participant_id',
             new = 'design_base',
             skip_absent = TRUE) %>%
    # Set PE for not updated bandits to NA
    .[model_choice_option == 1, ':='(pe_b_2 = NA,
                                     pe_b_3 = NA)] %>%
    .[model_choice_option == 2, ':='(pe_b_1 = NA,
                                     pe_b_3 = NA)] %>%
    .[model_choice_option == 3, ':='(pe_b_1 = NA,
                                     pe_b_2 = NA)] %>%
    # Add ID for unique parameter combinations
    .[, para_id := paste0(x1,x2,x3,x4)] %>%
    Add_comp(.) %>%
    # Get side of model choice
    .[model_choice_side == 1, model_choice_side_rl := 'left'] %>%
    .[model_choice_side == 2, model_choice_side_rl := 'right'] %>%
    # Set correct choice by selecting (on average) higher outcome bandit
    .[, correct_choice := if(option_left > option_right) 'left' else 'right',
      by = c('design_base', 'model', 'para_id', 'run', 'trial')] %>%
    .[, correct := correct_choice == model_choice_side_rl]
  
  
  # Use analysis of rare-outcome-impact
  # Make sure we only include choices in 1v2 comps when calculating prob of choosing 2
  data_ii = data %>%
    .[comp != '1v2' | trial_type != 'choice', correct_choice := NA,
      by = c('design_base', 'model', 'para_id', 'run')] %>%
    # Get running averages
    .[, ':='(avg_1_running = Get_running_avg(choice_option = model_choice_option,
                                             choice_outcome = model_outcome,
                                             stim = 1),
             avg_2_running = Get_running_avg(choice_option = model_choice_option,
                                             choice_outcome = model_outcome,
                                             stim = 2),
             avg_3_running = Get_running_avg(choice_option = model_choice_option,
                                             choice_outcome = model_outcome,
                                             stim = 3)),
      by = c('design_base', 'model', 'para_id', 'run')]
  
  message(paste0('Windorization...'))
  
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
  
  # Allocate data holding +-5 trials from rare outcome of bandit 2
  window_data = data.table()
  
  # For each model, parameters combination & run
  for(i_model in unique(data_ii$model)){
    message(paste0('   ', i_model, '...'))
    for(i_paras in unique(data_ii[model == i_model]$para_id)){
      for(i_run in unique(data_ii$run)){
        
        # Select data
        temp_data = data_ii[model == i_model &
                              para_id == i_paras &
                              run == i_run]
        
        # Get parameter values of current simulation
        paras = c(unique(temp_data$x1),
                  unique(temp_data$x2),
                  unique(temp_data$x3),
                  unique(temp_data$x4))
        
        # Get all trials where rare outcomes were obtained
        idx_chosen_rare_outcome = which(temp_data$is_rare == 1 &
                                          # CHANGE: I also allow rare outcomes that came from forced choices
                                          temp_data$trial_type %in% c('choice', 'forced') &
                                          temp_data$model_choice_option == 2)
        # For each of the rare-outcome trials
        for(rare_count in seq(1,length(idx_chosen_rare_outcome))){
          # Get data slice
          temp = Windowrize(data = temp_data,
                            index_rare = idx_chosen_rare_outcome[rare_count],
                            window_size = 3) %>%
            .[, window_relative := factor(window_relative, levels = unique(sort(window_relative)))]
          # Add count of the rare outcome
          temp$i_rare = rare_count
          
          temp
          
          # Fuse data for each participant & run
          window_data = rbind(window_data, temp)
        }
      }
    }
  }
  
  # Summarize data
  window_data_run = window_data %>%
    # Eliminate windows which extended across trial boundaries (<1 or >240)
    .[, relative_trial := as.numeric(as.character(window_center)) + as.numeric(as.character(window_relative))] %>%
    .[!(relative_trial < 1 | relative_trial > 240),] %>%
    # Sort by relative window
    .[order(rank(design_base), rank(run), rank(window_relative)),] %>%
    # Get mean accuracy across all relative window positions (-2 to +3)
    .[, .(mean_accuracy = mean(correct, na.rm = TRUE),
          n_data = sum(!is.na(correct)),
          x1 = unique(x1),
          x2 = unique(x2),
          x3 = unique(x3),
          x4 = unique(x4)),
      by = c('para_id', 'design_base', 'model', 'run', 'window_relative')]
  
  message(paste0('Saving table...'))
  
  # Export data
  file = file.path(here::here(), 'derivatives', 'simulation',
                   paste0('modelsim_analysis-windowrize_base-', design_base, '.tsv'),
                   fsep = .Platform$file.sep)
  data.table::fwrite(window_data_run, file = file, sep = '\t', na = 'n/a')
  
  
  message(paste0('...done!'))
}

# Function to split list inputs (used as callback function during argument parsing)
split_list = function(object,
                      flag,
                      value,
                      parser){
  x = as.numeric(unlist(strsplit(value, ',')))
  return(x)
}

# Create options to pass to script
option_list = list(
  optparse::make_option(c('-d', '--design_base'),
                        type='character',
                        default = NULL,
                        help = 'Participant ID that will determine which design will be base of the simulation',
                        metavar = 'DESIGN_BASE'))

# provide options in list to be callable by script
opt_parser = optparse::OptionParser(option_list = option_list)
opt = optparse::parse_args(opt_parser)

# Cal wrapper with command line inputs
Simulation_windowrize(design_base = opt$design_base)

# Rscript Simulation_windowrize.R --design_base '0IUKYRW'