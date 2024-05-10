Compute_value = function(V,
                         x,
                         bandit,
                         tau,
                         model) {
  
  # Get index for all rewards relevant for current bandit
  idx = which(!is.na(V))
  
  # Initialize PE vector
  PE = rep(0, length(V))
  # Initialize LR vector
  LR = rep(NA, length(V))
  
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
    
  } else if(model == 'seplr_surprise'){
    # l_pos
    low_pos = x[1]
    # u_pos
    up_pos = x[2]
    # s_pos
    slope_pos = x[3]
    # l_neg
    low_neg = x[4]
    # u_neg
    up_neg = x[5]
    # s_neg
    slope_neg = x[6]
    pi = NA
    
    # Throw error if model unknown
  } else{
    stop(paste0("Specified model (\'",
                model,
                "\') not found."))
  }
  
  # allocate trailing surprise
  S = V*0
  
  # Loop over each trial
  for (i in 2:length(idx)){
    
    # Get reward in current trial
    # note: before updating, V[idx[i]] reflects the reward, not the value
    R = V[idx[i]]
    # Calculate PE from current outcome
    PE[idx[i]] = R-V[idx[i-1]]
    
    # Calculation of V
    # Get LR to use for updating
    # In case of surprise models (dynamic LRs based in parameters l,u,s)
    if (model %in% c('surprise', 'uncertainty_surprise')) {
      
      # Calculate LR (based on experienced PE and relationship between PE and
      # LR, given by parameters l,u,s)
      res = LRfunction(low = low,
                       up = up,
                       slope = slope,
                       PE = PE[idx[i]],
                       tau = tau)
      alpha_star = res[[1]]
      
      # In case of seplr model, use different LR depending on pos or neg PE
    } else if(model %in% c('seplr', 'uncertainty_seplr')){
      
      # Positive PEs
      if(PE[idx[i]] >= 0){
        alpha_star = alpha_pos
        
      # Negative PEs
      } else{
        alpha_star = alpha_neg
      }
      
      # In case combination of seplr and surprise: use different LR function
      # for pos and neg PEs
    } else if(model == 'seplr_surprise'){
      
      # Positive PEs
      if(PE[idx[i]] >= 0){
        res = LRfunction(low = low_pos,
                         up = up_pos,
                         slope = slope_pos,
                         PE = PE[idx[i]],
                         tau = tau)
        alpha_star = res[[1]]
        
        # Negative PEs
      } else{
        res = LRfunction(low = low_neg,
                         up = up_neg,
                         slope = slope_neg,
                         PE = PE[idx[i]],
                         tau = tau)
        alpha_star = res[[1]]
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
    
    # Calculate and enter current value using single LR updating rule
    # (based on previous value, LR, and current outcome; regular RW updating,
    # V_1 = alpha * (V_0-R))
    V[idx[i]] = V[idx[i-1]]*(1-alpha_star) +  R*alpha_star
    # Safe current LR
    LR[idx[i]] = alpha_star
    
    # Check if there is an uncertainty module and (in case) calculate and
    # enter trailing surprise
    # Uncertainty module present: use trailing surprise
    if(!is.na(pi)){
      
      # The higher pi, the less reliance on history
      S[idx[i]] = S[idx[i-1]]*(1-pi) + abs(PE[idx[i]])*pi
      
      # Uncertainty module absent (pi == NA): fill surprise with NA
    } else{
      S[idx[i]] = NA
    }
    
    # Fill in values and PE for empty trials in which bandit was not chosen
    V[idx[i-1]:(idx[i]-1)] = V[idx[i-1]]
    PE[idx[i-1]:(idx[i]-1)] = PE[idx[i-1]]
    S[idx[i-1]:(idx[i]-1)] = S[idx[i-1]]
  }
  
  # Set current value and PE to all following values (only next one is important)
  V[idx[i]:length(V)] = V[idx[i]]
  PE[idx[i]:length(V)] = PE[idx[i]]
  S[idx[i]:length(V)] = S[idx[i]]
  
  # Return Value, PE, and LR as data.frame
  VPE = data.frame(V, PE, LR, S)
  return(VPE)
}
