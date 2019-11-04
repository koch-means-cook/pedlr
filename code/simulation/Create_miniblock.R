
# Function to create building blocks of whole design
# By building the whole design out of mini blocks we make sure that in each mini block we have the same amount
# of forced and choice trials as well as rating trials as soon as one miniblock is finished

Create_miniblock = function(blocknumber){
  
  mini_block = data.frame(matrix(NA, 30, 18))
  colnames(mini_block) =  c('option_left', 'option_right',
                            'pic_left', 'pic_right',
                            'rating_pic1', 'rating_pic2', 'rating_pic3',
                            'reward_stim_1', 'reward_stim_2',
                            'comp_number',
                            'trial_type', 'free_choice', 'forced_left', 'forced_right',
                            'with_rating', 'rating_durat', 'rt_fix_durat',
                            'n_miniblock')
  
  # Get all combinations for three stimuli
  combs = t(combn(3,2))
  # Get mirrors to have equal amounts of right and left presentations
  combs = rbind(combs, combs[,c(2,1)])
  # Repeat combinations to reach 24 free choices in mini block
  combs = cbind(rep(combs[,1], 4), rep(combs[,2], 4))
  # Get forced choices (twice, once with mirrored presentation)
  forced = matrix(rep(c(1:3),4), 6, 2, byrow = FALSE)
  # Including all identical combinations (for forced choice)
  combs = rbind(combs, forced)
  
  # Fill mini block
  # Presented options
  mini_block[,c(1,2)] = combs
  # Stimlus pictures
  mini_block$pic_left = apply(combs, 1, function(x) paste('stimuli/s', as.character(x[1]), '.png', sep=''))
  mini_block$pic_right = apply(combs, 1, function(x) paste('stimuli/s', as.character(x[2]), '.png', sep=''))
  # trial_type
  mini_block$trial_type[1:24] = 'choice'
  mini_block$trial_type[25:30] = 'forced'
  # Logical indicating free choice
  mini_block$free_choice = as.numeric(mini_block$trial_type == 'choice')
  # Side of forced choice presentation
  mini_block$forced_left = mini_block$forced_right = 0
  mini_block$forced_left[25:27] = 1
  mini_block$forced_right[28:30] = 1
  
  # Randomize trial order
  mini_block = mini_block[sample(dim(mini_block)[1]),]
  
  # Add two rating blocks (at least 10 trials apart)
  # To make sure they are also at least 10 trials apart from another mini_block there can be no rating block
  # in the first and last 5 trials of a mini_block, the others are sampled from the 1st and 2nd half
  mini_block$with_rating = 0
  rating_1 = sample(6:15, 1)
  rating_2 = sample(c((rating_1+10):25), 1)
  # In case first sample leaves only one option to keep minimum distance
  if(rating_1 == 15){
    rating_2 = 25
  }
  mini_block$with_rating[c(rating_1, rating_2)] = 1
  # Define presented stimulus during rating in randomized order and replace it with stimulus path
  rating_stim_1 = sample(1:3, 3, replace=FALSE)
  rating_stim_2 = sample(1:3, 3, replace=FALSE)
  for(i in c(1:3)){
    rating_stim_1[i] = paste('stimuli/s', as.character(rating_stim_1[i]), '.png', sep='')
    rating_stim_2[i] = paste('stimuli/s', as.character(rating_stim_2[i]), '.png', sep='')
  }
  mini_block[c(rating_1), c(5,6,7)] = rating_stim_1
  mini_block[c(rating_2), c(5,6,7)] = rating_stim_2
  # Set timings and ITIs in rating trials
  mini_block$rating_durat = 0
  mini_block$rating_durat[c(rating_1, rating_2)] = 20
  mini_block$rt_fix_durat = 0
  mini_block$rt_fix_durat[c(rating_1, rating_2)] = 0.5
  
  # Set block_count
  mini_block$n_miniblock = blocknumber
  
  # Return complete mini_block
  return(mini_block)
}

