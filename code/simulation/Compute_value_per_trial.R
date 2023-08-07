Compute_value_per_trial = function(V,
                                   R,
                                   S,
                                   x,
                                   tau) {

  # Example 
  # source(file.path(here::here(), 'code', 'model_fitting', 'LRfunction.R'))
  # V = 12
  # R = 45
  # S = 0
  # x = c(0.1,0.5,0.9,NA)
  # tau = 0.2
  
  # Allocate parameters
  # rw
  if(is.na(x[2])){
    alpha = x[1]
    pi = NA
    
    # Uncertainty model
  } else if(is.na(x[3])){
    alpha = x[1]
    pi = x[2]
    
    # Surprise model
  } else if(is.na(x[4])){
    # l
    low = x[1]
    # u
    up = x[2]
    # s
    slope = x[3]
    pi = NA
    
    # Surprise+Uncertainty model
  } else{
    # l
    low = x[1]
    # u
    up = x[2]
    # s
    slope = x[3]
    pi = x[4]
  }
  
  # allocate trailing surprise
  #S = V*0
    
    # Value before reward
    V = V
    # Reward in current trial
    R = R
    # Calculate PE from value and outcome
    PE = R-V
    
    # In case of surprise model (models that include l,u,s parameters) (rw = 1 para, uncertainty = 2 para)
    if (!is.na(x[3])) {
      # Calculate LR (based on experienced PE and relationship between PE and LR, given by parameters l,u,s)
      res = LRfunction(low = low,
                       up = up,
                       slope = slope,
                       PE = PE,
                       tau = tau)
      alpha_star = res[[1]]
      
    # In case of simple LR model
    } else {
      # Set constant LR
      alpha_star = alpha
    }
    
    # Calculate and enter current value 
    # (based on previous value, LR, and current outcome; regular RW updating, V_1 = alpha * (V_0-R))
    V_updated = V*(1-alpha_star) +  R*alpha_star
    # Safe current LR
    LR = alpha_star
    
    # Calculate and enter trailing surprise
    if(!is.na(pi)){
      # The higher pi, the less reliance on history
      S_updated = S*(1-pi) + abs(PE)*pi
    } else{
      S_updated = NA
    }
  
  # Return Value, PE, LR, and surprise
  out = list(v_updated = V_updated,
             pe = PE,
             lr = alpha_star,
             s_updated = S_updated)
  return(out)
}

