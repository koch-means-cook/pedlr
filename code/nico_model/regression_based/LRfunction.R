LRfunction = function(low, up, slope, PE, tau = 0.1) {
  PEscaled = 2/(1 + exp(-tau*abs(PE))) -1
  PEmax = 2/(1 + exp(-tau*60)) -1
  PEscaled = PEscaled/PEmax
  alpha_star = low  + ((up-low) / (1 + PEscaled^(-exp(slope))) *2)
  return(list(alpha_star, PEscaled))
}
