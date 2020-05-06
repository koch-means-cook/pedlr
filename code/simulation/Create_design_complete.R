
# For knitting:
source_path = '/Volumes/MPRG-Neurocode/Users/christoph/pedlr/code/simulation/'

# Source functions required for this script
source(file.path(source_path, 'Create_miniblock.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'Create_miniblock_var_forced.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'Beta_pseudo_sim.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'Gaussian_pseudo_sim.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'Bimodal_pseudo_sim.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'Uniform_pseudo_sim.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'Create_plan.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'Create_block.R', fsep = .Platform$file.sep))

Create_design <- function(n_blocks,
                          perc_forced,
                          blocks_per_task,
                          dist_list){
  
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
  # Take only column structure for design template (to append blocks to design)
  design = design[0,]
  
  # For each block
  for(block_count in unique(plan$block)){
    
    # Get plan for specific block
    plan_sub = plan[plan$block == block_count,]
    # Create block structure
    block = Create_block(blocknumber = block_count,
                         perc_forced = perc_forced,
                         task_version = unique(plan_sub$version),
                         stim_1 = plan$stimuli[1],
                         stim_2 = plan$stimuli[2],
                         stim_3 = plan$stimuli[3])
    # Get number of rewards to sample from block structure
    n_sample = nrow(subset(block, block$option_left == 1 | block$option_right == 1))
    
    # For each distributions
    for(dist_count in seq(3)){
      
      # Set number of distribution according to overall provided distributions
      dist_nr = plan_sub$dist_nr[dist_count]
      
      # Get plan for current distribution
      plan_dist = plan_sub[plan_sub$dist_nr == dist_nr,]
      # Get current distribution
      dist = plan_dist$dist_name
      
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
                                    main.mean = plan_dist$arg_1,
                                    main.sd = plan_dist$arg_2,
                                    second.mean = plan_dist$arg_3,
                                    second.sd = plan_dist$arg_4,
                                    relative_proportion = 0.2,
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
      
      # Insert rewards into block
      # Left rewards
      index = block$option_left == dist_count
      block$reward_stim_1[index] = reward$outcome[1:length(block$reward_stim_1[index])]
      # Right rewards
      index = block$option_right == dist_count
      block$reward_stim_2[index] = reward$outcome[(length(block$reward_stim_2[index])+1):nrow(reward)]
    }
    
    # In case this is not the first block (because there cannot be 'new' instructions on the first block) AND
    # in case the task version changed, set new instructions to TRUE on first trial of block with new version
    if(nrow(design) != 0){
      if(design$task_version[nrow(design)] != block$task_version[1]){
        block$with_new_instr[1] = 1
      }
    }
    
    # Concatenate blocks to create design
    design = rbind(design, block)
    
  }
  
  return(design)
  
}

# Provide standard values
n_blocks = 6
perc_forced = 20
blocks_per_task = 2
dist_list = list(c('gaussian', 100/3, 30),
                 c('beta', 3, 2),
                 c('gaussian', 2*100/3, 30),
                 c('beta', 3, 2),
                 c('uniform', 1, 100),
                 c('beta', 2, 3),
                 c('uniform', 1, 100),
                 c('uniform', 1, 100),
                 c('gaussian', 66, 10))
# dist_list = list(c('gaussian', 100/3, 50),
#                  c('gaussian', 100/3, 50),
#                  c('gaussian', 100/3, 50),
#                  c('gaussian', 100/3, 50),
#                  c('gaussian', 100/3, 50),
#                  c('gaussian', 100/3, 50),
#                  c('gaussian', 100/3, 50),
#                  c('gaussian', 100/3, 50),
#                  c('gaussian', 100/3, 50))

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
