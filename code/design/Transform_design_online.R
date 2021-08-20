
require(jsonlite)
require(stringr)

# For knitting:
source_path = file.path(here::here(), 'code', fsep = .Platform$file.sep)

# Source functions required for this script
source(file.path(source_path, 'design', 'Beta_pseudo_sim.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'design', 'Gaussian_pseudo_sim.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'design', 'Bimodal_pseudo_sim.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'design', 'Uniform_pseudo_sim.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'design', 'Create_plan.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'design', 'Create_block.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'design', 'Create_design.R', fsep = .Platform$file.sep))

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

# Create Design
n_blocks = 4
perc_forced = 20
blocks_per_task = 2
dist_list = list(c('bimodal', 30, 30, 80, 10, 0.2),
                 c('beta', 3, 2),
                 c('gaussian', 2*100/3, 30),
                 c('beta', 3, 2),
                 c('bimodal', 30, 0.2, 40, 10, 10),
                 c('beta', 2, 3))

for(i in seq(20)){
  
  # Set seed for each iteration of design creation
  set.seed(i)
  
  # Get task plan
  plan = Create_plan(n_blocks,
                     perc_forced,
                     blocks_per_task,
                     dist_list)
  
  # Get task design
  design = Create_design(n_blocks,
                         perc_forced,
                         blocks_per_task,
                         dist_list)
  
  # Port design for online task
  design_online = Transform_design_online(design)
  
  # Put different runs into json format
  json_list = lapply(design_online, FUN = jsonlite::toJSON,
                     dataframe = 'rows',
                     pretty = TRUE,
                     na = 'string')
  
  # Save json files for each run
  file = paste('/Users/koch/Docs/pedlr-task/code/task/designs/design-',
               str_pad(as.character(i), width = 2, pad = '0'),
               sep = '')
  for(json_count in seq(length(json_list))){
    name = paste(file,
                 paste('_run-', as.character(json_count), sep = ''),
                 '.json',
                 sep = '')
    write(json_list[[json_count]], file = name)
  }
}


