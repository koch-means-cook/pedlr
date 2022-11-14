Compute_value = function(V,
                         x,
                         bandit,
                         tau) {
  
  # Get index for all rewards relevant for current bandit
  idx = which(!is.na(V))
  
  # Initialize PE vector
  PE = rep(0, length(V))
  # Initialize LR vector
  LR = rep(NA, length(V))
  # alpha_hat = x[1]
  # w = x[2]
  
  # Safe parameters for surprise models
  # l
  low = x[1]
  # u
  up = x[2]
  # s
  slope = x[3]
  
  # Loop over each trial
  for (i in 2:length(idx)){
    
    # Get reward in current trial
    # note: before updating V[idx[i]] reflects the reward, not the value
    R = V[idx[i]]
    # Calculate PE from current outcome
    PE[idx[i]] = R-V[idx[i-1]]
    
    # In case of surprise model (models that include l,u,s parameters)
    if (!is.na(x[2])) {
      #res = LRfunction(alpha_hat, w, PE[idx[i]], tau)
      
      # Calculate LR (based on experienced PE and relationship between PE and LR, given by parameters l,u,s)
      res = LRfunction(x[1], x[2], x[3], PE[idx[i]], tau)
      alpha_star = res[[1]]
      
    # In case of simple LR model
    } else {
      #alpha_star = alpha_hat
      
      # Set constant LR
      alpha_star = low
    }
    
    # Calculate and enter current value 
    # (based on previous value, LR, and current outcome; regular RW updating, V_1 = alpha * (V_0-R))
    V[idx[i]] = V[idx[i-1]]*(1-alpha_star) +  R*alpha_star
    # Safe current LR
    LR[idx[i]] = alpha_star
    # Fill in values and PE for empty trials in which bandit was not chosen
    V[idx[i-1]:(idx[i]-1)] = V[idx[i-1]]
    PE[idx[i-1]:(idx[i]-1)] = PE[idx[i-1]]
  }
  
  # Set current value and PE to all following values (only next one is important)
  V[idx[i]:length(V)] = V[idx[i]]
  PE[idx[i]:length(V)] = PE[idx[i]]
  
  # Return Value, PE, and LR as data.frame
  VPE = data.frame(V, PE, LR)
  return(VPE)
}


#comp_updates = function(x) {
#  idx = which(abs(x) > 0)
#  for (i in 2:length(idx)){
#    if ((idx[i] - idx[i-1]) > 0) {
#      x[idx[i-1]:(idx[i]-1)] = x[idx[i-1]]
#    }
#  }
#  x[idx[i]:length(x)] = x[idx[i]]
#  return(x)
#}
