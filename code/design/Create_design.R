
# Load libraries
library(optparse)

# For knitting:
source_path = file.path(here::here(), 'code', fsep = .Platform$file.sep)

# Source functions required for this script
source(file.path(source_path, 'design', 'Beta_pseudo_sim.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'design', 'Gaussian_pseudo_sim.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'design', 'Bimodal_pseudo_sim.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'design', 'Uniform_pseudo_sim.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'design', 'Create_plan.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'design', 'Create_block.R', fsep = .Platform$file.sep))


Create_design <- function(n_blocks,
                          perc_forced,
                          blocks_per_task,
                          dist_list,
                          prop_rare = 0.2,
                          min_forced_with_rare_per_block = 2,
                          min_rate_after_rare_forced_per_block = 2){
  
  # n_blocks = 4
  # perc_forced = 20
  # blocks_per_task = 2
  # dist_list = list(c('gaussian', 100 * 2/6, (100 * 1/6) / 3),
  #                  c('bimodal', 100 * 3/6, 0.2, -35, (100 * 1/6) / 3, (100 * 1/6) / 3),
  #                  c('gaussian', 100 * 4/6, (100 * 1/6) / 3),
  #                  c('gaussian', 100 * 3/6, (100 * 1/6) / 3),
  #                  c('bimodal', 100 * 4/6, 0.2, -35, (100 * 1/6) / 3, (100 * 1/6) / 3),
  #                  c('gaussian', 100 * 5/6, (100 * 1/6) / 3))
  # prop_rare = 0.2
  # min_forced_with_rare_per_block = 2
  # min_rate_after_rare_forced_per_block = 2
  
  # Build plan mapping blocks, versions, distributions, and parameters
  plan = Create_plan(n_blocks,
                     perc_forced,
                     blocks_per_task,
                     dist_list)
  
  # Set upper and lower bound of reward space
  reward_space_lb = 1
  reward_space_ub = 100
  
  # Create design template (parameters for block creation do not make sense on purpose)
  design = Create_block(0,
                        perc_forced,
                        task_version = 1,
                        stim_1 = 1,
                        stim_2 = 1,
                        stim_3 = 1)
  # Take only column structure for design template (to append task_versions to design)
  design = design[0,]
  
  # For each task version
  for(version_count in unique(plan$version)){
    
    #print(paste('version_count: ', as.character(version_count)))
    
    # Get plan for specific task version
    plan_version = plan[plan$version == version_count,]
    
    # Take only column structure for version template (to append blocks to task version)
    version = design[0,]
    
    # Get block numbers in this version
    version_blocks = unique(plan$block[plan$version == version_count])
    
    # For each block in this task version
    for(block_count in version_blocks){
      
      #print(paste('block_count: ', as.character(block_count)))
      
      # Get plan for specific block
      plan_block = plan_version[plan_version$block == block_count,]
      # Create block structure
      block = Create_block(blocknumber = block_count,
                           perc_forced = perc_forced,
                           task_version = version_count,
                           stim_1 = plan_block$stimuli[1],
                           stim_2 = plan_block$stimuli[2],
                           stim_3 = plan_block$stimuli[3])
      # Append blocks of this version
      version = rbind(version, block)
      
    }
    
    # Get number of rewards to sample over all blocks in task version
    n_sample = nrow(subset(version, option_left == 1 | option_right == 1))
    
    # Add column indicating rare event
    version$is_rare = 0
    
    # Sample and insert rewards for each distribution in task version (do this for each task version rather
    # than block to maximize number of samples drawn for each distribution because a low number of samples
    # cannot represent the whole distribution in our Pseudo_sampling)
    for(dist_count in seq(3)){
      
      #print(paste('dist_count: ', as.character(dist_count)))
      
      # Set number of distribution according to overall provided distributions
      dist_nr = plan_version$dist_nr[dist_count]
      
      # Get plan for current distribution (we can take the block here since the distributions of the same 
      # version have to be the same by definition)
      plan_dist = plan_block[plan_block$dist_nr == dist_nr,]
      # Get current distribution
      dist = plan_dist$dist_name
      
      # See if current distribution relies on rare events
      needs_rare = FALSE
      if(dist %in% c('beta', 'bimodal')){
        needs_rare = TRUE
      }
      
      # Sample rewards according to distributions
      # Beta distribution
      if(dist == 'beta'){
        reward = Beta_pseudo_sim(n_sim = n_sample,
                                 a = plan_dist$arg_1,
                                 b = plan_dist$arg_2,
                                 dist_name = dist,
                                 reward_space_lb = reward_space_lb,
                                 reward_space_ub = reward_space_ub)
      }
      # Bimodal distribution
      if(dist == 'bimodal'){
        reward = Bimodal_pseudo_sim(n_sim = n_sample,
                                    mean = plan_dist$arg_1,
                                    rel_proportion = plan_dist$arg_2,
                                    distance = plan_dist$arg_3,
                                    main.sd = plan_dist$arg_4,
                                    second.sd = plan_dist$arg_5,
                                    dist_name = dist,
                                    reward_space_lb = reward_space_lb,
                                    reward_space_ub = reward_space_ub)
      }
      # Gaussian distribution
      if(dist == 'gaussian'){
        reward = Gaussian_pseudo_sim(n_sim = n_sample,
                                     mean = plan_dist$arg_1,
                                     sd = plan_dist$arg_2,
                                     dist_name = dist,
                                     reward_space_lb = reward_space_lb,
                                     reward_space_ub = reward_space_ub)
      }
      # Uniform distribution
      if(dist == 'uniform'){
        reward = Uniform_pseudo_sim(n_sim = n_sample,
                                    min = plan_dist$arg_1,
                                    max = plan_dist$arg_2,
                                    dist_name = dist,
                                    reward_space_lb = reward_space_lb,
                                    reward_space_ub = reward_space_ub)
      }
      
      # If desired relocate rare events
      if(needs_rare){
        # Mode
        counts = table(reward$outcome)
        sample_mode = as.numeric(names(counts)[which(counts == max(counts))])[1]
        # Mean
        sample_mean = mean(reward$outcome)
        # Get number of rare events (according to proportion rate)
        prop = round(length(reward$outcome) * prop_rare)
        # If skewed to left
        if(sample_mean < sample_mode){
          # Find lowest x entries (rare entries) 
          rare_pool = sort(reward$outcome)[seq(prop)]
        } else { # If skewed to right
          # Find highest x entries (rare entries) 
          rare_pool = sort(reward$outcome)[seq(length(reward$outcome) - (prop - 1), length(reward$outcome))]
        }
        # Find location of rare events
        is_rare = as.numeric(reward$outcome %in% rare_pool)
      }
      
      # Insert rewards into task_version
      # Left rewards
      index = version$option_left == dist_count
      version$reward_stim_1[index] = reward$outcome[1:length(version$reward_stim_1[index])]
      if(needs_rare){
        version$is_rare[index] = version$is_rare[index] + is_rare[1:length(version$reward_stim_1[index])] 
      }
      # Right rewards
      index = version$option_right == dist_count
      version$reward_stim_2[index] = reward$outcome[(length(version$reward_stim_2[index])+1):nrow(reward)]
      if(needs_rare){
        version$is_rare[index] = version$is_rare[index] + is_rare[(length(version$reward_stim_2[index])+1):nrow(reward)] 
      }
      
      # Find rare events which are in the wrong forced choices (wouldnt get selected)
      if(needs_rare){
        bad_forced_left = as.numeric(version$forced_left & version$option_right == dist_count)
        bad_forced_right = as.numeric(version$forced_right & version$option_left == dist_count)
        bad_forced = bad_forced_left + bad_forced_right
        version$bad_forced = bad_forced
      }
      
      if(needs_rare){
        # Swap bad forced choices with random reward from same option
        swap_index = which(version$is_rare > 0 & version$bad_forced > 0)
        n_swaps = length(swap_index)
        # Check if any swaps need to be made
        if(n_swaps > 0){
          for(i in seq(n_swaps)){
            # Find if left or right stimulus (if forced stimulus is on right, swap stimulus is on left, because we only look at bad forced trials)
            left = version$forced_right[swap_index[i]]
            if(left == 1){
              # Get pool of possible swap locations (swap only with same side rewards)
              pool_index = which(version$option_left == dist_count & version$is_rare == 0 & version$bad_forced == 0)
              # Get single location to swap with
              swap_with_index = sample(pool_index, 1)
              # Swap
              version$reward_stim_1 = replace(version$reward_stim_1,
                                              c(swap_index[i], swap_with_index),
                                              version$reward_stim_1[c(swap_with_index, swap_index[i])])
            } else{
              pool_index = which(version$option_right == dist_count & version$is_rare == 0 & version$bad_forced == 0)
              swap_with_index = sample(pool_index, 1)
              version$reward_stim_2 = replace(version$reward_stim_2,
                                              c(swap_index[i], swap_with_index),
                                              version$reward_stim_2[c(swap_with_index, swap_index[i])])
            }
            # Swap rare indicator (unaffected by stimulus side)
            version$is_rare = replace(version$is_rare,
                                      c(swap_index[i], swap_with_index),
                                      version$is_rare[c(swap_with_index, swap_index[i])])
          }
        }
        
        # Add more of the rare events to the (good) forced choice trials
        # Go through each block individually
        for(block_count in unique(version$block_n)){
          
          #print(paste('block_count: ', as.character(block_count)))
          
          curr_block = version[version$block_n == block_count,]
          # Get number of forced choices which force a rare outcome
          n_forced_with_rare_per_block = sum(curr_block$is_rare == 1 & curr_block$free_choice == 0)
          # Less than expected?
          if(n_forced_with_rare_per_block < min_forced_with_rare_per_block){
            # Find potential forced choices (forcing choice of skewed dist)
            index = curr_block$free_choice == 0 &
              curr_block$bad_forced == 0 &
              curr_block$is_rare == 0 &
              (curr_block$option_left == dist_count | curr_block$option_right == dist_count)
            # Select forced choices to put rare event in
            index = sample(which(index), min_forced_with_rare_per_block - n_forced_with_rare_per_block)
            # For each selected forced choice
            for(i in index){
              # Find side of stimulus
              left = curr_block$forced_left[i]
              # If stimulus on left side: replace with rare event of free choice on left side
              if(left == 1){
                replace_pool = which(curr_block$option_left == dist_count & curr_block$is_rare == 1 & curr_block$free_choice == 1)
                if(length(replace_pool) == 0){
                  stop('Error during min forced choices with rare outcome: No possible replacements')
                }
                # Sample replacement location from pool
                replacement = sample(replace_pool, 1)
                # Swap forced outcome with rare free outcome
                curr_block$reward_stim_1 = replace(curr_block$reward_stim_1,
                                                   c(i, replacement),
                                                   curr_block$reward_stim_1[c(replacement, i)])
              } else {
                replace_pool = which(curr_block$option_right == dist_count & curr_block$is_rare == 1 & curr_block$free_choice == 1)
                if(length(replace_pool) == 0){
                  stop('Error during min forced choices with rare outcome: No possible replacements')
                }
                # Sample replacement location from pool
                replacement = sample(replace_pool, 1)
                # Swap forced outcome with rare free outcome
                curr_block$reward_stim_2 = replace(curr_block$reward_stim_2,
                                                   c(i, replacement),
                                                   curr_block$reward_stim_2[c(replacement, i)])
              }
              # Swap rare indicator (unaffected by stimulus side)
              curr_block$is_rare = replace(curr_block$is_rare,
                                           c(i, replacement),
                                           curr_block$is_rare[c(replacement, i)])
              
              
            }
          }
          
          # - Put some estimates to after forced choice rare event
          # Find number of ratings after forced rare events
          n_crit_rate = nrow(curr_block[curr_block$is_rare == 1 &
                                          curr_block$free_choice == 0 &
                                          curr_block$bad_forced == 0 &
                                          curr_block$with_rating == 1,])
          # Less than expected?
          if(n_crit_rate < min_rate_after_rare_forced_per_block){
            # Location of forced choices with rare outcome
            crit_index = which(curr_block$is_rare == 1 & curr_block$free_choice == 0 & curr_block$bad_forced == 0)
            # Randomly go through desired number of these trials to add rating
            crit_index = sample(crit_index, min_rate_after_rare_forced_per_block)
            for(i in crit_index){
              # Find closes rating trial which is not after a rare event
              rating_index = which(curr_block$with_rating == 1 & curr_block$is_rare == 0)
              rating_index = rating_index[which(abs(rating_index - i) == min(abs(rating_index - i)))]
              # Swap with closest rating
              curr_block$with_rating[i] = 1
              curr_block$with_rating[rating_index] = 0
            }
          }
          
          # Overwrite block with new one containing required forced choices
          version[version$block_n == block_count,] = curr_block
          
        }  
      }
    }
    
    # Concatenate task_versions to create design
    design = rbind(design, version)
    
  }
  
  # Eliminate break at end of each task version since there are breaks in the task
  for(version_count in unique(design$task_version)){
    design$with_block_break[design$task_version == version_count][1] = 0
  }
  
  
  # Let function return complete design
  return(design)
  
}

# # Some checks
# library(data.table)
# bla = data.table(design)
# if(nrow(bla[bad_forced == 1 & is_rare == 1]) != 0){
#   stop('Still contains bad forced choices')
# }
# # Number of forced choices with rare outcomes
# for(i in unique(bla$block_n)){
#   if(nrow(bla[block_n == i & is_rare == 1 & free_choice == 0]) < 2){
#     stop(paste('Still too few forced choices with rare outcome per block in block ', as.character(i), sep = ''))
#   }
# }
# # Print number of forced choices with rare outcomes with rating after
# for(i in unique(bla$block_n)){
#   if(nrow(bla[block_n == i & is_rare == 1 & free_choice == 0 & with_rating == 1]) < 2){
#     stop(paste('Still too few ratings after forced choices with rare outcome per block in block ', as.character(i), sep = ''))
#   }
# }

