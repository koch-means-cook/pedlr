library(optparse)
library(here)
library(jsonlite)
library(data.table)
library(magrittr)

# Function to convert raw data to experiment data
Raw_to_data = function(data,
                       demo_data,
                       add_demo = FALSE,
                       delete_prolific = TRUE){
  
  # Check if participant has hiragana skills, add as variable
  if(grepl(pattern = 'No', x = data[type == 'hiragana']$responses,fixed = TRUE)){
    data$hiragana = FALSE
  } else {
    data$hiragana = TRUE
  }
  
  # Get duration of intro (consent + questions)
  data$duration_intro = data$time_elapsed[which(data$type == 'hiragana') + 1]
  
  # See if participant repeated training, get duration of training, delete training
  if('train_screen_repeat_end' %in% data$type){
    data$train_repeat = TRUE
    # Index of end of training
    end_train_idx = which(data$type == 'train_screen_repeat_end') + 1
    # Get duration of training
    data$duration_training = data$time_elapsed[end_train_idx] - unique(data$duration_intro)
    # Delete training entries
    data = data[end_train_idx:nrow(data),]
  } else {
    data$train_repeat = FALSE
    # Index of end of training
    end_train_idx = which(data$type == 'train_screen_last') + 1
    # Get duration of training
    data$duration_training = data$time_elapsed[end_train_idx] - unique(data$duration_intro)
    # Delete training entries
    data = data[end_train_idx:nrow(data),]
  }
  
  # Get duration of breaks
  duration_breaks = data$rt[which(data$type == 'break')]
  data$duration_break_first = duration_breaks[1]
  data$duration_break_second = duration_breaks[length(duration_breaks)]
  
  # Get duration of break between task versions
  data$duration_halftime = data[type == 'halftime']$rt
  
  # # Get task version variable
  data$task_version[1:which(data$type == 'halftime')] = 1
  data$task_version[which(data$type == 'halftime'):nrow(data)] = 2
  
  # Remove irrelevant columns for data analysis
  data = data %>%
    setDT() %>%
    .[, c('url',
          'trial_type',
          'trial_index',
          'internal_node_id',
          'success',
          'stimulus',
          'button_pressed',
          'responses',
          'question_order',
          'correct',
          'estimation',
          'response_reward',
          'response_range'):=NULL]
  
  # Delete all entries not requiring response (watch out to not delete missed responses/timeouts)
  data = data[!is.na(data$rt) | data$type %in% c('free', 'forced'),]
  
  # Delete break entries and goodbye
  data = data[!which(data$type %in% c('break', 'halftime', 'goodbye'))]
  
  # Add estimation trials to trial before
  # Find pre-estimation lines
  pre_est_index = which(data$type == 'estimation')[seq(1,length(which(data$type == 'estimation')), by = 3)] - 1
  # Add indicator
  data$with_rating = FALSE
  data$with_rating[pre_est_index] = TRUE
  
  # Get low/mid/high stimulus
  data$option_left = 0
  data$option_right = 0
  data$option_estimation = 0
  for(version_count in unique(data$task_version)){
    # Index each version by itself
    version_index = data$task_version == version_count
    # Get used stimuli
    stimuli = unique(data$stimulus_left[version_index])
    stimuli = stimuli[grepl(x = stimuli, pattern = '.png')]
    stimuli = stimuli[!grepl(x = stimuli, pattern = '_forced')]
    # Make text patterns applicable to forced and free stimuli
    stimuli = substr(stimuli, 1, 10)
    stimuli = as.data.table(stimuli)
    stimuli$mean = 0
    # Get mean reward of stimuli
    for(stim in stimuli$stimuli){
      stimuli$mean[grepl(x = stimuli$stimuli, pattern = stim)] = mean(as.numeric(data[grepl(x = stimulus_left, pattern = stim)]$outcome_left))
    }
    # Get low/mid/high dist
    stimuli$option = rank(stimuli$mean)
    # Enter options
    for(stim_count in seq(nrow(stimuli))){
      # Left stimulus
      stim_index_left = grepl(x = data$stimulus_left, pattern = stimuli$stimuli[stim_count]) & version_index
      data$option_left[stim_index_left] = stimuli$option[stim_count]
      # Right sitmlus
      stim_index_right = grepl(x = data$stimulus_right, pattern = stimuli$stimuli[stim_count]) & version_index
      data$option_right[stim_index_right] = stimuli$option[stim_count]
      # estimation stimulus
      stim_index_est = grepl(x = data$stimulus_estimation, pattern = stimuli$stimuli[stim_count]) & version_index
      data$option_estimation[stim_index_est] = stimuli$option[stim_count]
    }
  }
  
  # Add estimation columns
  data$est_1_reward = 0
  data$est_1_range = 0
  data$est_1_rt = 0
  data$est_2_reward = 0
  data$est_2_range = 0
  data$est_2_rt = 0
  data$est_3_reward = 0
  data$est_3_range = 0
  data$est_3_rt = 0
  estimation_index = data$with_rating
  for(option_count in seq(3)){
    # Get index for stimulus specific estimation columns
    est_columns = which(grepl(x = colnames(data),
                              pattern = paste('_',
                                              as.character(option_count),
                                              '_',
                                              sep = '')) )
    # Get index of estimation trials involving specific option
    option_est_index = which(data$option_estimation == option_count)
    # Add estimation to option-specific estimation columns
    data[estimation_index, est_columns[1]] = data[option_est_index]$estimation_reward
    data[estimation_index, est_columns[2]] = data[option_est_index]$estimation_range
    data[estimation_index, est_columns[3]] = data[option_est_index]$rt
  }
  
  # Delete rows giving only estimation
  data = data[is.na(data$estimation_reward),]
  # Delete columns now useless after estimation has been transferred to columns
  data = data %>%
    setDT() %>%
    # Remove irrelevant columns for data analysis
    .[, c('stimulus_estimation',
          'estimation_reward',
          'estimation_range',
          'option_estimation',
          'key_press'):=NULL]
  
  # Data coding clean-up (turn different NA codings into proper NAs)
  data[outcome == 'n/a']$outcome = NA
  data$outcome = as.numeric(data$outcome)
  data[forced == 'NA']$forced = NA
  data[choice == 'n/a']$choice = NA
  estimation_cols = colnames(data)[grepl(x = colnames(data), pattern = 'est_')]
  for(est_col in estimation_cols){
    data[with_rating == FALSE, est_col] = NA
  }
  # Specify data type of each column
  data$rt = as.numeric(data$rt)
  data$time_elapsed = as.numeric(data$time_elapsed)
  data$participant_id = as.character(data$participant_id)
  data$name_design_r1 = as.character(data$name_design_r1)
  data$name_design_r2 = as.character(data$name_design_r2)
  data$browser_name = as.character(data$browser_name)
  data$window_resolution = as.character(data$window_resolution)
  data$screen_resolution = as.character(data$screen_resolution)
  data$type = as.factor(data$type)
  data$ts_finish = as.numeric(data$ts_finish)
  data$browser_version = as.character(data$browser_version)
  data$os_name = as.character(data$os_name)
  data$os_version = as.character(data$os_version)
  data$hiragana = as.logical(data$hiragana)
  data$stimulus_left = as.character(data$stimulus_left)
  data$stimulus_right = as.character(data$stimulus_right)
  data$outcome_left = as.numeric(data$outcome_left)
  data$outcome_right = as.numeric(data$outcome_right)
  data$forced = as.character(data$forced)
  data$choice = as.character(data$choice)
  data$outcome = as.numeric(data$outcome)
  data$duration_intro = as.numeric(data$duration_intro)
  data$train_repeat = as.logical(data$train_repeat)
  data$duration_training = as.numeric(data$duration_training)
  data$duration_break_first = as.numeric(data$duration_break_first)
  data$duration_break_second = as.numeric(data$duration_break_second)
  data$duration_halftime = as.numeric(data$duration_halftime)
  data$task_version = as.numeric(data$task_version)
  data$with_rating = as.logical(data$with_rating)
  data$option_left = as.numeric(data$option_left)
  data$option_right = as.numeric(data$option_right)
  data$est_1_reward = as.numeric(data$est_1_reward)
  data$est_1_range = as.numeric(data$est_1_range)
  data$est_1_rt = as.numeric(data$est_1_rt)
  data$est_2_reward = as.numeric(data$est_2_reward)
  data$est_2_range = as.numeric(data$est_2_range)
  data$est_2_rt = as.numeric(data$est_2_rt)
  data$est_3_reward = as.numeric(data$est_3_reward)
  data$est_3_range = as.numeric(data$est_3_range)
  data$est_3_rt = as.numeric(data$est_3_rt)
  
  # Get chosen option
  data$option_choice = 0
  data[choice == 'left']$option_choice = data[choice == 'left']$option_left
  data[choice == 'right']$option_choice = data[choice == 'right']$option_right
  
  # Get time-outs (only for free and forced)
  # Mark all missing outcomes as time-outs
  data$timeout = FALSE
  data[is.na(outcome)]$timeout = TRUE
  # If missing outcome was because of false choice in forced, dont mark as timeout but as error
  data$error = FALSE
  data[type == 'forced' & forced != choice]$timeout = FALSE
  data[type == 'forced' & forced != choice]$error = TRUE
  
  # Adjust to design file
  data[type == 'free']$type = 'choice'
  data$type = as.factor(data$type)
  data$free_choice = 0
  data[type == 'choice']$free_choice = 1
  data$forced_left = 0
  data[forced == 'left']$forced_left = 1
  data$forced_right = 0
  data[forced == 'right']$forced_right = 1
  
  data = data %>%
    data.table::setnames(.,
                         old = c('stimulus_left',
                                 'stimulus_right',
                                 'outcome_left',
                                 'outcome_right',
                                 'type'),
                         new = c('pic_left',
                                 'pic_right',
                                 'reward_stim_1',
                                 'reward_stim_2',
                                 'trial_type'))
  
  
  # Put design columns to start
  data.table::setcolorder(data,
                          c('option_left', 'option_right', 'pic_left', 'pic_right',
                            'reward_stim_1', 'reward_stim_2', 'comp_number', 'trial_type',
                            'free_choice', 'forced_left', 'forced_right',
                            'with_rating', 'task_version', 'block_n', 'is_rare', 'bad_forced'))
  
  # Add demographic data
  if(add_demo){
    # find demo data of participant
    prolific_id = unique(data$prolific_id)
    # Add demo data
    demo_data = demo_data[participant_id == prolific_id, c('time_taken',
                                                           'age',
                                                           'num_approvals',
                                                           'num_rejections',
                                                           'prolific_score',
                                                           'Country.of.Birth',
                                                           'Current.Country.of.Residence',
                                                           'Employment.Status',
                                                           'Nationality',
                                                           'Sex')]
    demo_data$prolific_id = prolific_id
    data = data.table::merge.data.table(data, demo_data, by = 'prolific_id')
    setnames(data, 'Country.of.Birth', 'country_of_birth')
    setnames(data, 'Current.Country.of.Residence', 'curent_country_of_residence')
    setnames(data, 'Employment.Status', 'employment_status')
    setnames(data, 'Nationality', 'nationality')
    setnames(data, 'Sex', 'sex')
  }
  
  # Delete prolific ID from data for anonymization
  if(delete_prolific){
    data[, prolific_id := NULL]
  }

  
  
  return(data)
}


