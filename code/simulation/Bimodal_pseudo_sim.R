# Function to sample from Bimodal distribution while equally sampling whole distribution
# This way you don't have to rely on chance to get all possible events

Bimodal_pseudo_sim = function(n_sim,
                              main.mean,
                              main.sd,
                              second.mean,
                              second.sd,
                              relative_proportion,
                              dist_name,
                              reward_space_lb,
                              reward_space_ub){
  
  # Set up data frame holding value and distribution value was sampled from
  data_sim = data.frame(matrix(0,nrow=n_sim,ncol=2))
  colnames(data_sim) = c('outcome', 'dist')
  
  # Get how often each number would be sampled given the distribution and n_sim trials
  # Get probability to sample each value over reward space given main distribution
  main.outcome = dnorm(seq(from=reward_space_lb,to=reward_space_ub), mean = main.mean, sd=main.sd)
  # Get probability to sample each value over reward space given second mode
  second.outcome = dnorm(seq(from=reward_space_lb,to=reward_space_ub), mean=second.mean, sd=second.sd)
  # Fuse sample probabilities given relative ratio of second distribution
  d_outcome = main.outcome + relative_proportion * second.outcome
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
    samples = apply(add_samples, 1, function(x) sample(c(x[1], x[2]), 1, prob=c((1-relative_proportion), relative_proportion)))
    # Resample in case the sampling does not fulfill boundary requirements
    while(any(samples < reward_space_lb | samples > reward_space_ub)){
      main.samples = round(rnorm(missing_samples, main.mean, main.sd))
      second.samples = round(rnorm(missing_samples, second.mean, second.sd))
      add_samples = data.frame(cbind(main.samples, second.samples))
      colnames(add_samples) = c('main.samples', 'second.samples')
      samples = apply(add_samples, 1, function(x) sample(c(x[1], x[2]), 1, prob=c((1-relative_proportion), relative_proportion)))
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
  # Randomize sample
  outcome = sample(outcome)
  
  # Fill output with results
  data_sim$outcome = outcome
  data_sim$dist = dist_name
  return(data_sim)
}