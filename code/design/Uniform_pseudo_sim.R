
# Function to sample from Gaussian distribution while equally sampling whole distirbution
# This way you don't have to rely on chance to get all possible events

Uniform_pseudo_sim = function(n_sim, min, max, dist_name, reward_space_lb, reward_space_ub){
  
  # Set up data frame holding value and distribution value was sampled from
  data_sim = data.frame(matrix(0,nrow=n_sim,ncol=2))
  colnames(data_sim) = c('outcome', 'dist')
  # Get how often each number would be sampled given the distribution and n_sim trials
  # Get probability to sample each value over reward space given the distribution
  outcome = dunif(seq(from=min,to=max), min, max)
  outcome = outcome/sum(outcome)
  outcome = round(outcome*n_sim)
  # If samples are missing, add newly sampled values
  missing_samples = n_sim - sum(outcome)
  # If there are too few samples add the missing ones to the outcome
  if(missing_samples > 0){
    samples = round(runif(missing_samples, min, max))
    # Resample in case the sampling does not fulfill boundary requirements
    while(any(samples < reward_space_lb | samples > reward_space_ub)){
      samples = round(runif(missing_samples, min, max))
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