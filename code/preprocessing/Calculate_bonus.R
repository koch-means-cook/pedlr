library(optparse)
library(here)
library(jsonlite)
library(data.table)
library(magrittr)

# input = '/Users/koch/Desktop/results/2021-09-25 20_19_39.json'
# input = '/Users/koch/Desktop/results/2021-09-21 15_28_40.json'
#input = '/Users/koch/Desktop/20210915_inlab_pilot'
#input = '/Users/koch/Desktop/pedlr-pilot-01'

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
    
    # Set data to only free choices (only free choices off performance measure)
    data = data[type == 'free']
    data$outcome_left = as.numeric(data$outcome_left)
    data$outcome_right = as.numeric(data$outcome_right)
    # Exclude training
    data = data[!is.na(comp_number)]
    # Get possible max points (optimal choice each time)
    data[, max_points := max(outcome_left, outcome_right), by = trial_index]
    
    # Get chance points (multiple times to take mean after)
    data[, ':='(chance_points_1 = sample(c(outcome_left, outcome_right), 1),
                chance_points_2 = sample(c(outcome_left, outcome_right), 1),
                chance_points_3 = sample(c(outcome_left, outcome_right), 1),
                chance_points_4 = sample(c(outcome_left, outcome_right), 1),
                chance_points_5 = sample(c(outcome_left, outcome_right), 1),
                chance_points_6 = sample(c(outcome_left, outcome_right), 1),
                chance_points_7 = sample(c(outcome_left, outcome_right), 1),
                chance_points_8 = sample(c(outcome_left, outcome_right), 1),
                chance_points_9 = sample(c(outcome_left, outcome_right), 1),
                chance_points_10 = sample(c(outcome_left, outcome_right), 1)),
         by = trial_index]
    
    # Get participants outcomes
    data[outcome == 'n/a']$outcome = NA
    data$outcome = as.numeric(data$outcome)
    
    # Compare max, chance, and actual points
    max_points = sum(data$max_points)
    chance_points = mean(c(sum(data$chance_points_1),
                           sum(data$chance_points_2),
                           sum(data$chance_points_3),
                           sum(data$chance_points_4),
                           sum(data$chance_points_5),
                           sum(data$chance_points_6),
                           sum(data$chance_points_7),
                           sum(data$chance_points_8),
                           sum(data$chance_points_9),
                           sum(data$chance_points_10)))
    points = sum(data$outcome, na.rm = TRUE)
    
    # Set up scale between chance and max
    max_points = round(max_points - chance_points, 2)
    points = round(points - chance_points, 2)
    
    # Get percentage between chance and max points
    bonus_perc = points / max_points
    
    # Bonus pool (possible max bonus pay, in pounds)
    bonus_pool = 3
    
    # Get percentage of bonus pool
    bonus = round(bonus_pool * bonus_perc, 2)
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


