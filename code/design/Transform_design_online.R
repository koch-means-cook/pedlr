
require(jsonlite)
require(stringr)

# For knitting:
source_path = file.path(here::here(), 'code', fsep = .Platform$file.sep)

# Function transforming the design into an online format
Transform_design_online <- function(design){
  
  # Add columns needed for online experiment
  design$type = design$trial_type
  design$type[design$type == 'choice'] = 'free'
  design$stimulus_left = design$pic_left
  design$stimulus_right = design$pic_right
  design$outcome_left = design$reward_stim_1
  design$outcome_right = design$reward_stim_2
  design$forced = NA
  design$forced[design$forced_left == 1] = 'left'
  design$forced[design$forced_right == 1] = 'right'
  design$stimulus_estimation = NA

  # Rename stimuli pictures in case its forced choice
  design$stimulus_left[design$forced_left == 1] = paste(
    substr(design$stimulus_left[design$forced_left == 1], 1, 10),
    '_forced',
    substr(design$stimulus_left[design$forced_left == 1], 11, 14),
    sep = ''
    )
  design$stimulus_right[design$forced_right == 1] = paste(
    substr(design$stimulus_right[design$forced_right == 1], 1, 10),
    '_forced',
    substr(design$stimulus_right[design$forced_right == 1], 11, 14),
    sep = ''
  )
  
  # Add break lines
  break_lines = which(design$with_block_break == 1)
  insert_break = design[1,]
  insert_break[,] = NA
  insert_break$type = 'break'
  added_lines = 0
  for(bl in break_lines){
    design = rbind(design[1:(bl+added_lines-1),], insert_break, design[(bl+added_lines):nrow(design),])
    added_lines = added_lines + 1
  }
  
  # Add estimation lines
  estimation_lines = which(design$with_rating == 1)
  insert_est = design[1:3,]
  insert_est[,] = NA
  insert_est$type = 'estimation'
  added_lines = 0
  for(el in estimation_lines){
    # Get all stimuli used in the current block of the estimation
    curr_block = design$block_n[el + added_lines]
    stimuli = unique(c(design$pic_left[design$block_n == curr_block],
                       design$pic_left[design$block_n == curr_block]))
    stimuli = stimuli[!is.na(stimuli)]
    # Fill the estimation trials with the stimuli of the block in random order
    insert_est$stimulus_estimation = sample(stimuli)
    # In case the last trial is an estimation, just append the estimation trials
    if((el + added_lines) == nrow(design)){
      design = rbind(design, insert_est)
    } else{
      design = rbind(design[1:(el+added_lines),], insert_est, design[(el+added_lines+1):nrow(design),])
    }
    added_lines = added_lines + nrow(insert_est)
  }
  
  # Reset rownumbers
  rownames(design) = seq(nrow(design))
  
  # Fill important variables of NAs with previous values (possible since NAs are never the first line of new block/version)
  fills = which(is.na(design$task_version))
  for(l in fills){
    design$task_version[l] = design$task_version[l-1]
    design$block_n[l] = design$block_n[l-1]
  }
  
  # Drop columns irrelevant for online testing to save file size
  drop = c('trial_type',
           'pic_left',
           'pic_right',
           'reward_stim_1',
           'reward_stim_2',
           'free_choice',
           'forced_left',
           'forced_right',
           'with_rating',
           'rt_fix_durat',
           'with_block_break',
           'with_new_instr')
  design = design[,!colnames(design) %in% drop]
  
  # Split design by task version to get different runs
  run_list = split(design, f = design$task_version)
  
  # Let function return complete design
  return(run_list)
  
}


