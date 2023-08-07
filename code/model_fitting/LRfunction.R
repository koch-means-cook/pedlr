LRfunction = function(low,
                      up,
                      slope,
                      PE,
                      tau = 0.2) {
  
  
  PEscaled = 2/(1 + exp(-tau*abs(PE))) -1
  # Scale by logistic function to avoid going beyond 1 as scaling by PE
  # MaxPE across all participants is 51; possibly largest is left edge of mid bandit to right edge of mid bandit
  PEmax = 2/(1 + exp(-tau*60)) -1
  
  # Normalize by maximum prediction error
  PEscaled = PEscaled/PEmax
  
  
  # Get LR based on parameters
  alpha_star = low  + ((up-low) / (1 + PEscaled^(-exp(slope))) *2)
  
  
  # Return LR and normalized scaled PE
  return(list(alpha_star,
              PEscaled))
}

