Compute_value_per_trial = function(V,
                                   R,
                                   S,
                                   x,
                                   tau,
                                   model) {

  # Example 
  # source(file.path(here::here(), 'code', 'model_fitting', 'LRfunction.R'))
  # V = 12
  # R = 45
  # S = 0
  # x = c(0.1,0.5,0.9,NA)
  # tau = 0.2
  
  # Allocate parameters
  # rw
  if(model == 'rw'){
    alpha = x[1]
    pi = NA
    
    # Uncertainty model
  } else if(model == 'uncertainty'){
    alpha = x[1]
    pi = x[2]
    
    # Separate LR model
  } else if(model == 'seplr'){
    alpha_pos = x[1]
    alpha_neg = x[2]
    pi = NA
    
    # Separate LR + Uncertainty model
  } else if(model == 'uncertainty_seplr'){
    alpha_pos = x[1]
    alpha_neg = x[2]
    pi = x[3]
    
    # Surprise model
  } else if(model == 'surprise'){
    # l
    low = x[1]
    # u
    up = x[2]
    # s
    slope = x[3]
    pi = NA
    
    # Surprise+Uncertainty model
  } else if(model == 'uncertainty_surprise'){
    # l
    low = x[1]
    # u
    up = x[2]
    # s
    slope = x[3]
    pi = x[4]
    
    # Throw error if model unknown
  } else{
    stop(paste0("Specified model (\'",
                model,
                "\') not found."))
  }
  
  # allocate trailing surprise
  #S = V*0
  
  # Value before reward
  V = V
  # Reward in current trial
  R = R
  # Calculate PE from value and outcome
  PE = R-V
  
  # In case of surprise models (dynamic LRs based in parameters l,u,s)
  if (model %in% c('surprise', 'uncertainty_surprise')) {
    
    # Calculate LR (based on experienced PE and relationship between PE and
    # LR, given by parameters l,u,s)
    res = LRfunction(low = low,
                     up = up,
                     slope = slope,
                     PE = PE,
                     tau = tau)
    alpha_star = res[[1]]
    
    # In case of seplr model, use different LR depending on pos or neg PE
  } else if(model %in% c('seplr', 'uncertainty_seplr')){
    
    # Positive PEs
    if(PE >= 0){
      alpha_star = alpha_pos
      
      # Negative PEs
    } else{
      alpha_star = alpha_neg
    }
    
    # In case of RW and uncertainty model: use single, static LR (but updating
    # process works identical)
  } else if(model %in% c('rw', 'uncertainty')){
    
    # Set constant LR
    alpha_star = alpha
    
    # Throw error in case no conditions apply
  } else{
    stop(paste0("Specified model (\'",
                model,
                "\') not found."))
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

