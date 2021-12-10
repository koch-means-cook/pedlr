library(optparse)
library(here)
library(jsonlite)
library(data.table)
library(magrittr)

# input = '/Users/koch/Desktop/results/2021-09-25 20_19_39.json'
# input = '/Users/koch/Desktop/results/2021-09-21 15_28_40.json'
#input = '/Users/koch/Desktop/20210915_inlab_pilot'
#input = '/Users/koch/Desktop/pedlr-pilot-01'
# input = '/Volumes/MPRG-Neurocode/Data/pedlr/20210928_prolific_pedlr-pilot-01/raw'
#input = '/Volumes/MPRG-Neurocode/Data/pedlr/20210929_prolific_pedlr-pilot-03/raw/2021-09-29 13_19_24.json'
# input = '/Volumes/MPRG-Neurocode/Data/pedlr_2021_koch/20211130_prolific_pedlr-main-younger-01/raw'

Calculate_bonus = function(input){
  
  # Get folder contents
  file_list = dir(input, pattern = '*.json', full.names = TRUE)
  
  # Give message header to user
  message('')
  message('#########')
  message('')
  message(input)
  message('-----------------')
  message('')
  
  # Go through files
  for(file in file_list){
    
    # Load json file of participant
    data = data.table(jsonlite::fromJSON(file))
    
    # Delete all entries not requiring response (watch out to not delete missed responses/timeouts)
    data = data[!is.na(data$rt) | data$type %in% c('free', 'forced'),]
    
    # Set prolific id for all entries
    data$prolific_id = data$prolific_id[1]
    
    # Get forced choices and clean up outcomes
    data_forced = data[type == 'forced']
    data_forced[outcome == 'n/a']$outcome = NA
    data_forced$outcome = as.numeric(data_forced$outcome)
    
    # Set data to only free choices (only free choices off performance measure)
    data = data[type == 'free']
    data$outcome_left = as.numeric(data$outcome_left)
    data$outcome_right = as.numeric(data$outcome_right)
    # Exclude training
    data = data[!is.na(comp_number)]
    # Get possible max points (optimal choice each time)
    data[, max_points := max(outcome_left, outcome_right), by = trial_index]
    max_points = sum(data$max_points)
    
    # Get 75% accuracy points (multiple times to take mean after)
    p_75_cols = paste('p_75_points_', seq(20), sep = '')
    for(col in p_75_cols){
      data[, (col) := sample(c(max(outcome_left, outcome_right),
                                min(outcome_left, outcome_right)),
                              1,
                              prob = c(0.75, 0.25)),
           by = trial_index]
    }
    
    # Get mean of 70% accuracy points
    mean_p_75 = mean(data[, sapply(.SD, sum), .SDcols = p_75_cols])
    
    # Set bonus hallmarks (max at absolute maximum of points, 50% of bonus at 75% performance)
    bonus_100 = max_points
    bonus_50 = mean_p_75
    points_per_percent = (bonus_100 - bonus_50) / 50
      
    # Get participants outcomes
    data[outcome == 'n/a']$outcome = NA
    data$outcome = as.numeric(data$outcome)
    points = sum(data$outcome, na.rm = TRUE)
    
    # Bonus pool (possible max bonus pay, in pounds)
    bonus_pool = 3
    
    # Get bonus based on scale
    surplus_50 = (points - bonus_50) / points_per_percent
    perc_bonus = (50 + surplus_50) / 100
    bonus = round(bonus_pool * perc_bonus, 2)

    # If bonus would be negative, bonus is 0
    if(bonus < 0){
      bonus = 0
    }
    
    # Print bonus result to command line (in format for Prolific batch payment)
    msg = paste(unique(data$prolific_id), bonus, sep = ',')
    message(msg)
    
    # # Print bonus result to command line
    # message('\n')
    # message('#########')
    # message(paste("Prolific_id:\t\t\t", unique(data$prolific_id), sep = ''))
    # message(paste("Max points above chance:\t", max_points, sep = ''))
    # message(paste("Points above chance:\t\t", points, sep = ''))
    # message(paste("Max bonus possible:\t\t", bonus_pool, sep = ''))
    # message('------------------------------------------')
    # message(paste("Bonus payment:\t\t\t", bonus, sep = ''))
    # message('------------------------------------------')
    # message('#########')
    # message('\n')
    # 
    
  }
  
  # Give message tail to user
  message('')
  message('#########')
  message('')
  
}

# Create options to pass to script
option_list = list(
  make_option(c('-i', '--input'),
              type='character',
              default = NULL,
              help = 'Path to folder containing batch of experiment results',
              metavar = 'INPUT'))

# provide options in list to be callable by script
opt_parser = OptionParser(option_list = option_list)
opt = parse_args(opt_parser)

# Call main function
Calculate_bonus(input = opt$input)


