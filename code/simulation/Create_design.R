
# Function to create building blocks of whole design
# By building the whole design out of mini blocks we make sure that in each mini block we have the same amount
# of forced and choice trials as well as rating trials as soon as one miniblock is finished

# Load required libraries
library(rstudioapi)

# Get path current function is in
#source_path = dirname(rstudioapi::getSourceEditorContext()$path)
# For knitting:
source_path = '/Volumes/MPRG-Neurocode/Users/christoph/pedlr/code/simulation/'

# Source functions required for this script
source(file.path(source_path, 'Create_miniblock.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'Beta_pseudo_sim.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'Gaussian_pseudo_sim.R', fsep = .Platform$file.sep))

Create_design = function(n_miniblock,
                         dist1_val1,
                         dist1_val2,
                         dist2_val1,
                         dist2_val2,
                         dist3_val1,
                         dist3_val2,
                         two_betas,
                         reward_space_lb,
                         reward_space_ub){
  
  # Create raw data frame to combine mini_blocks
  design = data.frame(matrix(NA, 0, 18))
  colnames(design) =  c('option_left', 'option_right',
                        'pic_left', 'pic_right',
                        'rating_pic1', 'rating_pic2', 'rating_pic3',
                        'reward_stim_1', 'reward_stim_2',
                        'comp_number',
                        'trial_type', 'free_choice', 'forced_left', 'forced_right',
                        'with_rating', 'rating_durat', 'rt_fix_durat',
                        'n_miniblock')
  
  # Combine mini_blocks to complete design
  for(block_count in c(1:n_miniblock)){
    mini_block = Create_miniblock(block_count)
    design = rbind(design, mini_block)
  }
  
  # Sample rewards for design
  # Number of samples per mini_block (2 forced choice, 16 free choice)
  n_samples = 18*n_miniblock
  
  # Sample from different distributions according to logical given in function
  if(as.logical(two_betas)){
  
    # Lower beta
    rewards_1 = Beta_pseudo_sim(n_samples,
                                dist1_val1, dist1_val2, 'beta_le',
                                reward_space_lb, reward_space_ub)$outcome
    # Gaussian anchor
    rewards_2 = Gaussian_pseudo_sim(n_samples,
                                    dist2_val1, dist2_val2, 'gaussian',
                                    reward_space_lb, reward_space_ub)$outcome
    # Upper beta
    rewards_3 = Beta_pseudo_sim(n_samples,
                                dist3_val1, dist3_val2, 'beta_ue',
                                reward_space_lb, reward_space_ub)$outcome
      
  } else{
    
    # Lower Gaussian
    rewards_1 = Gaussian_pseudo_sim(n_samples,
                                    dist1_val1, dist1_val2, 'gaussian',
                                    reward_space_lb, reward_space_ub)$outcome
    # Middle beta
    rewards_2 = Beta_pseudo_sim(n_samples,
                                dist2_val1, dist2_val2, 'beta_ue',
                                reward_space_lb, reward_space_ub)$outcome
    # Upper Gaussian
    rewards_3 = Gaussian_pseudo_sim(n_samples,
                                    dist3_val1, dist3_val2, 'gaussian',
                                    reward_space_lb, reward_space_ub)$outcome
    
  }
  
  # Fill design with rewards
  # Stimulus 1
  # Left presentation
  left_index = which(as.numeric(design$trial_type == 'choice') + as.numeric(design$option_left == 1) == 2)
  design$reward_stim_1[left_index] = rewards_1[1:length(left_index)]
  # Right presentation
  right_index = which(as.numeric(design$trial_type == 'choice') + as.numeric(design$option_right == 1) == 2)
  design$reward_stim_2[right_index] = rewards_1[(length(left_index)+1):(length(left_index) + 
                                                                          length(right_index))]
  # Forced choices
  forced_index = which(as.numeric(design$trial_type == 'forced') + as.numeric(design$option_left == 1) == 2)
  design$reward_stim_1[forced_index] = rewards_1[(length(left_index) +
                                                    length(right_index) + 1):(length(rewards_1))]
  # Copy forced choice rewards to second stimulus
  design$reward_stim_2[forced_index] = rewards_1[(length(left_index) + 
                                                    length(right_index) + 1):(length(rewards_1))]
  
  # Stimulus 2
  left_index = which(as.numeric(design$trial_type == 'choice') + as.numeric(design$option_left == 2) == 2)
  design$reward_stim_1[left_index] = rewards_2[1:length(left_index)]
  right_index = which(as.numeric(design$trial_type == 'choice') + as.numeric(design$option_right == 2) == 2)
  design$reward_stim_2[right_index] = rewards_2[(length(left_index)+1):(length(left_index) + 
                                                                          length(right_index))]
  forced_index = which(as.numeric(design$trial_type == 'forced') + as.numeric(design$option_left == 2) == 2)
  design$reward_stim_1[forced_index] = rewards_2[(length(left_index) +
                                                    length(right_index) + 1):(length(rewards_2))]
  design$reward_stim_2[forced_index] = rewards_2[(length(left_index) + 
                                                    length(right_index) + 1):(length(rewards_2))]
  
  # Stimulus 3
  left_index = which(as.numeric(design$trial_type == 'choice') + as.numeric(design$option_left == 3) == 2)
  design$reward_stim_1[left_index] = rewards_3[1:length(left_index)]
  right_index = which(as.numeric(design$trial_type == 'choice') + as.numeric(design$option_right == 3) == 2)
  design$reward_stim_2[right_index] = rewards_3[(length(left_index)+1):(length(left_index) + 
                                                                          length(right_index))]
  forced_index = which(as.numeric(design$trial_type == 'forced') + as.numeric(design$option_left == 3) == 2)
  design$reward_stim_1[forced_index] = rewards_3[(length(left_index) +
                                                    length(right_index) + 1):(length(rewards_3))]
  design$reward_stim_2[forced_index] = rewards_3[(length(left_index) + 
                                                    length(right_index) + 1):(length(rewards_3))]
  
  # Set number of comparisons
  # 1v2
  comp_index = sort(append(
    which((as.numeric(design$option_left == 1) + as.numeric(design$option_right == 2)) == 2),
    which((as.numeric(design$option_left == 2) + as.numeric(design$option_right == 1)) == 2)))
  design$comp_number[comp_index] = c(1:length(comp_index))
  # 1v3
  comp_index = sort(append(
    which((as.numeric(design$option_left == 1) + as.numeric(design$option_right == 3)) == 2),
    which((as.numeric(design$option_left == 3) + as.numeric(design$option_right == 1)) == 2)))
  design$comp_number[comp_index] = c(1:length(comp_index))
  # 2v3
  comp_index = sort(append(
    which((as.numeric(design$option_left == 2) + as.numeric(design$option_right == 3)) == 2),
    which((as.numeric(design$option_left == 3) + as.numeric(design$option_right == 2)) == 2)))
  design$comp_number[comp_index] = c(1:length(comp_index))
  # Forced choices
  comp_index = which((as.numeric(design$option_left == 1) + as.numeric(design$option_right == 1)) == 2)
  design$comp_number[comp_index] = c(1:length(comp_index))
  comp_index = which((as.numeric(design$option_left == 2) + as.numeric(design$option_right == 2)) == 2)
  design$comp_number[comp_index] = c(1:length(comp_index))
  comp_index = which((as.numeric(design$option_left == 3) + as.numeric(design$option_right == 3)) == 2)
  design$comp_number[comp_index] = c(1:length(comp_index))
  
  # Reset index of design
  rownames(design) = c(1:nrow(design))
  
  # # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # # Introduce specific comparison after unusual event
  # # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 
  # # Define limits for rare outcomes for each distribution
  # rare_lim_lb = quantile(rewards_beta_le, 1-rare_lim)
  # rare_lim_lb = unname(rare_lim_lb)
  # rare_lim_ub = quantile(rewards_beta_ue, rare_lim)
  # rare_lim_ub = unname(rare_lim_ub)
  # 
  # # Go though each mini_block
  # for(block_count in c(1:n_miniblock)){
  #   # Get subset of data for one miniblock
  #   block = subset(design, n_miniblock == block_count)
  #   
  #   # Find rare events
  #   # See if rare event is followed by wanted comparison
  #   # if there are any rare events
  #     # If rare events not followed by wanted comparison
  #       # Swap one wanted comparison to the position of the next comprison the stimulus is involved in
  #   
  # }
  # 
  # # - - - - - - - - - - - - - - - - - - - - - -Lower beta (lb)- - - - - - - - - - - - - - - - - - - - - - - -
  # # Get all trials of lower beta (regardless of forced or free choice) for stim 1
  # lb_index_stim1 = which(design$option_left == 1)
  # # Find all outcomes above rare threshold
  # rare_index_stim1 = which(design$reward_stim_1 >= rare_lim_lb)
  # # Join index vectors to get rare events for lower beta
  # rare_index_stim1 = intersect(lb_index_stim1, rare_index_stim1)
  # 
  # # Same for stimulus 2
  # lb_index_stim2 = which(design$option_right == 1)
  # rare_index_stim2 = which(design$reward_stim_2 >= rare_lim_lb)
  # rare_index_stim2 = intersect(lb_index_stim2, rare_index_stim2)
  # 
  # # Join together over stimuli
  # rare_index_lb = unique(sort(append(rare_index_stim1, rare_index_stim2)))
  # 
  # # Eliminate comparisons with anchor
  # anchor_comp_index = unique(sort(append(which(design$option_left == 2), which(design$option_right == 2))))
  # rare_index_lb = setdiff(rare_index_lb, anchor_comp_index)
  # 
  # # Get target comparisons of lower beta and anchor
  # lb_index = unique(sort(append(lb_index_stim1, lb_index_stim2)))
  # target_comp_index = intersect(anchor_comp_index, lb_index)
  # 
  # # Swap position of target comparison with first presentation of beta after rare event
  # replace_counter = 1
  # order = c(1:nrow(design))
  # # Exclude rare outcome in case it is the last entry in the design matrix
  # if(rare_index_lb[length(rare_index_lb)] == nrow(design)){
  #   rare_index_lb = rare_index_lb[-length(rare_index_lb)]
  # }
  # # Go through each rare event
  # for(rare_line in rare_index_lb){
  #   # Check if next comparison is already correct
  #   next_comp = lb_index[which(lb_index == rare_line) + 1]
  #   # In case the last occurence of the beta is a rare event and there is no next comparison
  #   if((next_comp >= lb_index[length(lb_index)]) | (is.na(next_comp))){
  #     # Take the next trial to swap
  #     next_comp = rare_line + 1
  #   }
  #   # Check if target comparison got eliminated from the pool of swappable trials since it is already in the 
  #   # correct position (see below) 
  #   
  #   # WHAT HAPPENS WHEN REPLACE COUNTER IS LARGER THAN TARGETS? STOP PROCESS?
  #   while_counter = 0
  #   while((is.na(target_comp_index[replace_counter]) && (replace_counter <= length(target_comp_index)))) {
  #     replace_counter = replace_counter + 1
  #     
  #     while_counter = while_counter +1
  #     if (while_counter > 10){
  #       print('While loop stuck')
  #     }
  #   }
  #   # If the there is no event left to be swapped with break loop
  #   if(replace_counter > length(target_comp_index)){
  #     print('Loop broke')
  #     break
  #   }
  #   # In case the next comparison is not a target comparison or a rare event
  #   if(!(next_comp %in% target_comp_index) && !(next_comp %in% rare_index_lb)){
  #     # Get a comparison between 2 and 3
  #     target_line = target_comp_index[replace_counter]
  #     # Swap the target comparison with the comparison after a rare event
  #     order[c(next_comp, target_line)] = c(target_line, next_comp)
  #     # Increase counter to take the next target comparison
  #     replace_counter = replace_counter + 1
  #     # In case a target comparison is already in the correct position, eliminate it from the pool of 
  #     # swappable items
  #   } else if(next_comp %in% target_comp_index){
  #     target_comp_index[which(target_comp_index == next_comp)] = NA
  #   }
  # }
  # 
  # # Order design according to swaped comparisons
  # design = design[order,]
  # 
  # # - - - - - - - - - - - - - - - - - - - - - -Upper beta (ub)- - - - - - - - - - - - - - - - - - - - - - - -
  # # Get rare events and comparisons with anchor
  # ub_index_stim1 = which(design$stim_1 == 4)
  # rare_index_stim1 = which(design$reward_stim_1 <= rare_lim_ub)
  # rare_index_stim1 = intersect(ub_index_stim1, rare_index_stim1)
  # ub_index_stim2 = which(design$stim_2 == 4)
  # rare_index_stim2 = which(design$reward_stim_2 <= rare_lim_ub)
  # rare_index_stim2 = intersect(ub_index_stim2, rare_index_stim2)
  # rare_index_ub = unique(sort(append(rare_index_stim1, rare_index_stim2)))
  # anchor_comp_index = unique(sort(append(which(design$stim_1 == 3), which(design$stim_2 == 3))))
  # rare_index_ub = setdiff(rare_index_ub, anchor_comp_index)
  # ub_index = unique(sort(append(ub_index_stim1, ub_index_stim2)))
  # target_comp_index = intersect(anchor_comp_index, ub_index)
  # 
  # # Swap position of target comparison with first presentation of beta after rare event
  # replace_counter = 1
  # order = c(1:nrow(design))
  # # Exclude rare outcome in case it is the last trial of the design
  # if(rare_index_ub[length(rare_index_ub)] == nrow(design)){
  #   rare_index_ub = rare_index_ub[-length(rare_index_ub)]
  # }
  # # Go through rare events and swap with target comparisons
  # for(rare_line in rare_index_ub){
  #   next_comp = ub_index[which(ub_index == rare_line) + 1]
  #   if((next_comp >= ub_index[length(ub_index)]) | (is.na(next_comp))){
  #     next_comp = rare_line + 1
  #   }
  #   # Check if target comparison got eliminated from the pool of swappable trials since it is already in the 
  #   # correct position (see below) HAS TO BE CHECKED BEFORE EACH ITERATION
  #   while_counter = 0
  #   while((is.na(target_comp_index[replace_counter]) && (replace_counter <= length(target_comp_index)))) {
  #     replace_counter = replace_counter + 1
  #     
  #     while_counter = while_counter +1
  #     if (while_counter > 10){
  #       print('While loop stuck')
  #     }
  #   }
  #   # If the there is no event left to be swapped with break loop
  #   if(replace_counter > length(target_comp_index)){
  #     print('Loop broke')
  #     break
  #   }
  #   # In case the next comparison is not a target comparison or a rare event and the line it should be swapped with swap it with a target comparison
  #   if(!(next_comp %in% target_comp_index) && !(next_comp %in% rare_index_ub)){
  #     target_line = target_comp_index[replace_counter]
  #     order[c(next_comp, target_line)] = c(target_line, next_comp)
  #     replace_counter = replace_counter + 1
  #     # In case a target comparison is already in the correct position, eliminate it from the pool of 
  #     # swappable items
  #   } else if(next_comp %in% target_comp_index){
  #     target_comp_index[which(target_comp_index == next_comp)] = NA
  #   }
  # }
  # # Order design according to swaped comparisons
  # design = design[order,]
  # 
  # 
  # # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # # Name columns, add trial number, and introduce comparison numbers
  # # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 
  # # Rename rows accoding to new order
  # rownames(design) = c(1:nrow(design))
  # # Add trial number
  # design$trial_nr = c(1:nrow(design))
  # 
  # # Get count numbers
  # design$comp_number = NA
  # # Get all possible comparisons sorted
  # comps = t(apply(cbind(design$stim_1, design$stim_2), 1, sort))
  # # Count each iteration of a certain comparison
  # for(i in c(1:nrow(combs))){
  #   design$comp_number[which(apply(comps, 1, function(x) all(x == combs[i,])))] = c(1:n_reps)
  # }
  # 
  # # Change column order so trial_nr is first column
  # design = design[, c('trial_nr',
  #                     'stim_1',
  #                     'stim_2',
  #                     'reward_stim_1',
  #                     'reward_stim_2',
  #                     'comp_number',
  #                     'trial_type')]
  
  return(design)
  
}

