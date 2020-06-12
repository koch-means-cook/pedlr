
# Function to create building blocks of whole design
# By building the whole design out of mini blocks we make sure that in each mini block we have the same amount
# of forced and choice trials as well as rating trials as soon as one miniblock is finished

Create_block = function(blocknumber,
                        perc_forced,
                        task_version,
                        stim_1,
                        stim_2,
                        stim_3){
  
  if(perc_forced %% 10 != 0){
    stop(paste("Percentage of forced choices must be multiple of 10.",
               sep=''))
  }
  
  block = data.frame(matrix(NA, 0, 17))
  # colnames(block) =  c('option_left',
  #                           'option_right',
  #                           'pic_left',
  #                           'pic_right',
  #                           'rating_pic1',
  #                           'rating_pic2',
  #                           'rating_pic3',
  #                           'reward_stim_1',
  #                           'reward_stim_2',
  #                           'comp_number',
  #                           'trial_type',
  #                           'free_choice',
  #                           'forced_left',
  #                           'forced_right',
  #                           'with_rating',
  #                           'rating_durat',
  #                           'rt_fix_durat',
  #                           'n_miniblock')
  colnames(block) =  c('option_left',
                       'option_right',
                       'pic_left',
                       'pic_right',
                       'reward_stim_1',
                       'reward_stim_2',
                       'comp_number',
                       'trial_type',
                       'free_choice',
                       'forced_left',
                       'forced_right',
                       'with_rating',
                       'rt_fix_durat',
                       'with_block_break',
                       'with_new_instr',
                       'task_version',
                       'block_n')
  
  # Create free choice data frame
  # Get all combinations for three stimuli
  combs = t(combn(3,2))
  # Get mirrors to have equal amounts of right and left presentations
  combs = rbind(combs, combs[,c(2,1)])
  # Create data frame to hold free choices
  free = data.frame(matrix(NA, nrow(combs), 17))
  colnames(free) = colnames(block)
  # Enter free choice combinations into free choice df
  free$option_left = combs[,1]
  free$option_right = combs[,2]
  
  # Stimlus pictures
  stimuli = c(stim_1, stim_2, stim_3)
  free$pic_left = apply(combs,
                        1,
                        function(x) paste('stimuli/s', as.character(stimuli[x[1]]), '.png', sep=''))
  free$pic_right = apply(combs,
                         1,
                         function(x) paste('stimuli/s', as.character(stimuli[x[2]]), '.png', sep=''))
  
  # Set trial type
  free$trial_type = 'choice'
  free$free_choice = 1
  free$forced_left = free$forced_right = 0

  # Create forced choice data frame
  # Combs same for forced choices (need all combinations plus left and right since we display normal stimuli)
  forced_combs = combs
  # Repeat forced choices so cueing can be done equally for left and right
  forced_combs = rbind(forced_combs,forced_combs)
  # Create data frame to hold forced choices
  forced = data.frame(matrix(NA, nrow(forced_combs), 17))
  colnames(forced) = colnames(block)
  # Enter forced choice combinations into free choice df
  forced$option_left = forced_combs[,1]
  forced$option_right = forced_combs[,2]
  
  # Stimlus pictures
  forced$pic_left = apply(forced_combs,
                          1,
                          function(x) paste('stimuli/s', as.character(stimuli[x[1]]), '.png', sep=''))
  forced$pic_right = apply(forced_combs,
                           1,
                           function(x) paste('stimuli/s', as.character(stimuli[x[2]]), '.png', sep=''))
  
  # Set trial type
  forced$trial_type = 'forced'
  forced$free_choice = 0
  forced$forced_left = c(rep(1, nrow(forced)/2), rep(0, nrow(forced)/2))
  forced$forced_right = c(rep(0, nrow(forced)/2), rep(1, nrow(forced)/2))
  
  
  # Combine free and forced choice dfs according to proportion of forced choices
  # Set number of trials to smallest common denominator of 20% / 10% (120)
  n_trials = 120
  # Number of forced trials is 24 for 20 percent or 12 for 10 percent
  times_forced = perc_forced/10
  forced = do.call('rbind', replicate(times_forced, forced, simplify = FALSE))
  # Get number of left trials gived the percentage of forced trials
  n_free = n_trials - nrow(forced)
  times_free = n_free/nrow(free)
  free = do.call('rbind', replicate(times_free, free, simplify = FALSE))
  block = rbind(free, forced)
  
  # Randomize trial order
  block = block[sample(dim(block)[1]),]
  
  # Reset rownames (row numbers)
  rownames(block) = c(1:nrow(block))
  
  # Set number of comparison
  # 1v2
  block$comp_number[
    (block$option_left == 1 | block$option_left == 2) &
      (block$option_right == 2 | block$option_right == 1)] = (
        seq(length(block$comp_number[
          (block$option_left == 1 | block$option_left == 2) &
            (block$option_right == 2 | block$option_right == 1)]
        ))
      )
  # 1v3
  block$comp_number[
    (block$option_left == 1 | block$option_left == 3) &
      (block$option_right == 3 | block$option_right == 1)] = (
        seq(length(block$comp_number[
          (block$option_left == 1 | block$option_left == 3) &
            (block$option_right == 3 | block$option_right == 1)]
        ))
      )
  # 2v3
  block$comp_number[
    (block$option_left == 2 | block$option_left == 3) &
      (block$option_right == 3 | block$option_right == 2)] = (
        seq(length(block$comp_number[
          (block$option_left == 2 | block$option_left == 3) &
            (block$option_right == 3 | block$option_right == 2)]
        ))
      )
  
  # Add rating blocks (at least 10 trials apart) with amount of 20 % of total trial count
  # 120 trials, 120*0.2 = 24, 3 trials a rating block ==> 24/3 = 8 rating blocks
  block$with_rating = 0
  # Get one rating trial each 15 trials (mean) and randomly shift them back by 0-5 steps (assures at least 10
  # steps between rating trials but randomizes them to some degree)
  rating_index = 15*c(1:8) - sample(0:5, 8, replace=TRUE)
  # Add rating logical to rating trials
  block$with_rating[rating_index] = 1
  # Define presented stimulus during rating in randomized order and replace it with stimulus path
  #rating_stims = data.frame(do.call('rbind', lapply(1:8, function(x) sample(1:3,3))))
  #rating_stims[] = lapply(rating_stims, function(x) paste('stimuli/s', as.character(x), sep=''))
  #colnames(rating_stims) = c('rating_pic1', 'rating_pic2','rating_pic3')
  # Fill different rating orders with stimuli
  #block$rating_pic1[rating_index] = rating_stims$rating_pic1
  #block$rating_pic2[rating_index] = rating_stims$rating_pic2
  #block$rating_pic3[rating_index] = rating_stims$rating_pic3

  # Set ITIs in rating trials
  block$rt_fix_durat = 0
  block$rt_fix_durat[rating_index] = 0.5
  
  # Set block_count
  block$block_n = blocknumber
  
  # with block break
  block$with_block_break = 0
  # Set block break to TRUE on first entry of block if it is not the first block
  if(blocknumber!=1){
    block$with_block_break[1] = 1
  }
  
  # Task version (for different distributions)
  block$task_version = task_version
  
  # With new instructions (gets changed during design creation)
  block$with_new_instr = 0
  
  
  # Return complete block
  return(block)
  
}