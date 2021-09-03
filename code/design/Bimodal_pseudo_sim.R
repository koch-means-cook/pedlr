# Function to sample from Bimodal distribution while equally sampling whole distribution
# This way you don't have to rely on chance to get all possible events

Bimodal_pseudo_sim = function(n_sim,
                              mean,
                              rel_proportion,
                              distance,
                              main.sd,
                              second.sd,
                              dist_name,
                              reward_space_lb,
                              reward_space_ub){

  # default values
  # n_sim = 240
  # mean = 100 * 2/6
  # rel_proportion = 0.2
  # distance = 40
  # main.sd = (100 * 1/6) / 3
  # second.sd = (100 * 1/6) / 3
  # dist_name = 'bimodal'
  # reward_space_lb = 1
  # reward_space_ub = 100
  
  # Calculate mean of distributions to combine if we want the specified mean 
  # (needs distance and relative proportion of samples)
  main.mean = mean - ((rel_proportion * distance) / (rel_proportion + 1))
  second.mean = main.mean + distance
  
  # Calculate standard deviation of distirbutions to use if we want the specified SD
  # main.sd = sqrt(sqrt(
  #   ((sd^2 * (rel_proportion + 1)) - ((main.mean - mean)^2 - (second.mean - mean)^2)) /
  #     ((rel_proportion * rel_proportion_sd^2) + 1)))
  # second.sd = main.sd * rel_proportion_sd
  
  #main.sd = sqrt(((sd^2 * (n_sim/(a+1)) * (a+1)) - 1) / (((n_sim/(a+1)) * (1+a*b^2)) - b^2 - 1))
  
  # Set sd for both modes since calculation does not work yet
  main.sd = main.sd
  second.sd = second.sd
  
  # Set up data frame holding value and distribution value was sampled from
  data_sim = data.frame(matrix(0,nrow=n_sim,ncol=2))
  colnames(data_sim) = c('outcome', 'dist')
  
  # Get how often each number would be sampled given the distribution and n_sim trials
  # Get probability to sample each value over reward space given main distribution
  main.outcome = dnorm(seq(from=reward_space_lb,to=reward_space_ub), mean = main.mean, sd=main.sd)
  # Get probability to sample each value over reward space given second mode
  second.outcome = dnorm(seq(from=reward_space_lb,to=reward_space_ub), mean=second.mean, sd=second.sd)
  # Fuse sample probabilities given relative ratio of second distribution
  d_outcome = main.outcome + rel_proportion * second.outcome
  # Normalize density
  outcome = d_outcome/sum(d_outcome)
  # Get how often a specific outcome will appear based on its probability
  outcome = round(outcome*n_sim)
  
  # If samples are missing, add newly sampled values
  missing_samples = n_sim - sum(outcome)
  # If there are too few samples add the missing ones to the outcome
  if(missing_samples > 0){
    # Sample from both distributions and put them in data frame
    main.samples = round(rnorm(missing_samples, main.mean, main.sd))
    second.samples = round(rnorm(missing_samples, second.mean, second.sd))
    add_samples = data.frame(cbind(main.samples, second.samples))
    colnames(add_samples) = c('main.samples', 'second.samples')
    # Chose randomly from which distribution to add the sample to the outcome based on relative proportion
    samples = apply(add_samples,
                    1,
                    function(x) sample(c(x[1], x[2]),
                                       1,
                                       prob=c((1-rel_proportion), rel_proportion)))
    # Resample in case the sampling does not fulfill boundary requirements
    while(any(samples < reward_space_lb | samples > reward_space_ub)){
      main.samples = round(rnorm(missing_samples, main.mean, main.sd))
      second.samples = round(rnorm(missing_samples, second.mean, second.sd))
      add_samples = data.frame(cbind(main.samples, second.samples))
      colnames(add_samples) = c('main.samples', 'second.samples')
      samples = apply(add_samples,
                      1,
                      function(x) sample(c(x[1], x[2]),
                                         1,
                                         prob=c((1-rel_proportion), rel_proportion)))
    }
    # Go thourgh all amples individually in case one digit was sampled multiple times (which prohibits
    # one-liners)
    for(i in samples){
      # In case sampling produces a 0 (because beta densities range from 0-1 this can happen)
      # Embedded in both functions for symmetry
      if(i == 0){
        i = i+1
      }
      outcome[i] = outcome[i] +1
    }
  }
  # Create outcomes based on how often each reward was sampled
  outcome = unlist(mapply(function(x,y) rep(x,y), seq(from=reward_space_lb,to=reward_space_ub), outcome))
  
  # Check for relative proportion in sample
  # If there is any detectable gap between modes
  if(max(diff(sort(outcome))) >= 3){
    boundary = sort(outcome)[diff(sort(outcome)) == max(diff(sort(outcome)))]
    
    #print(as.character(distance))
    #print(as.character(boundary))
    
    # If smaller mode is to the left
    if(distance < 0){
      # Find proportion of "rare" outcomes in sample
      n_rare = sum(outcome <= boundary)
      n_normal = sum(outcome > boundary)
      # Find what proportion should be
      target_prop = (n_rare + n_normal) * rel_proportion
      # If proportion of rare too low
      if(n_rare < target_prop){
        # Get how many rare outcomes need to be added
        n_new_samples = target_prop - n_rare
        # Sample from "rare" mode
        new_samples = round(rnorm(n_new_samples, second.mean, second.sd))
        
        # Get random samples from 'normal' mode to replace
        index_resample = sample(seq(which(outcome == boundary)[1],
                                    length(outcome)),
                                n_new_samples)
        # Replace old "normal" samples with new samples
        outcome[index_resample] = new_samples
      }
    } else if(distance > 0){ # If smaller mode is to the right
      # Find proportion of "rare" outcomes in sample
      n_rare = sum(outcome >= boundary)
      n_normal = sum(outcome < boundary)
      # Find what proportion should be
      target_prop = (n_rare + n_normal) * rel_proportion
      # If proportion of rare too low
      if(n_rare < target_prop){
        # Get how many rare outcomes need to be added
        n_new_samples = target_prop - n_rare
        # Sample from "rare" mode
        new_samples = round(rnorm(n_new_samples, second.mean, second.sd))
        # Get random samples from 'normal' mode to replace
        index_resample = sample(seq(which(outcome == boundary)[1]),
                                n_new_samples)
        # Replace old "normal" samples with new "rare" samples
        outcome[index_resample] = new_samples
      }
    }
    
    # Adjust for potential mean changes by adding whole numbers to all samples
    sample_mean = mean(outcome)
    mean_diff = mean - sample_mean
    outcome = outcome + round(mean_diff)
    
  }
  
  # If sample adjustment created numbers outside of reward space, adjust them
  if(min(outcome) < reward_space_lb | max(outcome) > reward_space_ub){
    outcome[outcome < reward_space_lb] = reward_space_lb
    outcome[outcome > reward_space_ub] = reward_space_ub
  }
  
  # Randomize sample
  outcome = sample(outcome)
  
  # If there are too many samples, delete the same amount of samples randomly from distribution while
  # orienting the probability of a sample being deleted similar to distribution
  if(missing_samples < 0){
    outcome = outcome[-sample(c(1:length(outcome)),abs(missing_samples), replace=FALSE)]
  }
  
  # Fill output with results
  data_sim$outcome = outcome
  data_sim$dist = dist_name
  return(data_sim)
}
