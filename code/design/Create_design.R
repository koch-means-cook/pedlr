
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
                          prop_rare = 0.2){
  
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
    
    # Get plan for specific task version
    plan_version = plan[plan$version == version_count,]
    
    # Take only column structure for version template (to append blocks to task version)
    version = design[0,]
    
    # Get block numbers in this version
    version_blocks = unique(plan$block[plan$version == version_count])
    
    # For each block in this task version
    for(block_count in version_blocks){
      
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
        reward = Bimodal_pseudo_sim(n_sample,
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
      bad_forced_left = as.numeric(version$forced_left & version$option_right == dist_count)
      bad_forced_right = as.numeric(version$forced_right & version$option_left == dist_count)
      bad_forced = involved_forced_left + involved_forced_right
      
      # - Swap bad forced choices with random reward from same option
      # - Put some estimates to after forced choice rare event
      # - Add more of the rare events to the (good) forced choice trials
      
      version$bad_forced = bad_forced
      
      
      bla = version[version$is_rare == 1 & bad_forced == 1, ]
      
      
    }
    
    
    # Concatenate task_versions to create design
    design = rbind(design, version)
    
  }
  
  # - Present reward estimates after critical forced choice trial
  # - Ideally rating should also be done before (too suspicious?)
  
  # Let function return complete design
  return(design)
  
}


# # Provide standard values
# n_blocks = 6
# perc_forced = 20
# blocks_per_task = 2
# dist_list = list(c('bimodal', 30, 30, 80, 10, 0.2),
#                  c('beta', 3, 2),
#                  c('gaussian', 2*100/3, 30),
#                  c('beta', 3, 2),
#                  c('bimodal', 30, 0.2, 40, 10, 10),
#                  c('beta', 2, 3),
#                  c('uniform', 1, 100),
#                  c('uniform', 1, 100),
#                  c('gaussian', 66, 10))
# 
# # Testrun plan
# plan = Create_plan(n_blocks,
#                    perc_forced,
#                    blocks_per_task,
#                    dist_list)
# 
# # Testrun rewards
# design = Create_design(n_blocks,
#                        perc_forced,
#                        blocks_per_task,
#                        dist_list)

# Provide standard values
n_blocks = 4
perc_forced = 20
blocks_per_task = 2
dist_list = list(c('bimodal', 30, 30, 80, 10, 0.2),
                 c('beta', 3, 2),
                 c('gaussian', 2*100/3, 30),
                 c('beta', 3, 2),
                 c('bimodal', 30, 0.2, 40, 10, 10),
                 c('beta', 2, 3))

# Testrun plan
plan = Create_plan(n_blocks,
                   perc_forced,
                   blocks_per_task,
                   dist_list)

# Testrun rewards
design = Create_design(n_blocks,
                       perc_forced,
                       blocks_per_task,
                       dist_list)
