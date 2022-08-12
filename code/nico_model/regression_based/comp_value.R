comp_value = function(V, x, bandit, tau) {
  idx = which(!is.na(V))
  PE = rep(0, length(V))
  LR = rep(NA, length(V))
  # alpha_hat = x[1]
  # w = x[2]
  low = x[1]
  up = x[2]
  slope = x[3]
  for (i in 2:length(idx)){
    # note: before udating V[idx[i]] reflects the reward, not the value
    R = V[idx[i]]
    PE[idx[i]] = R-V[idx[i-1]]
    if (!is.na(x[2])) {
      #res = LRfunction(alpha_hat, w, PE[idx[i]], tau)
      res = LRfunction(x[1], x[2], x[3], PE[idx[i]], tau)
      alpha_star = res[[1]]
    } else {
      #alpha_star = alpha_hat
      alpha_star = low
    }
    V[idx[i]] = V[idx[i-1]]*(1-alpha_star) +  R*alpha_star
    LR[idx[i]] = alpha_star
    V[idx[i-1]:(idx[i]-1)] = V[idx[i-1]]
    PE[idx[i-1]:(idx[i]-1)] = PE[idx[i-1]]
  }

  V[idx[i]:length(V)] = V[idx[i]]
  PE[idx[i]:length(V)] = PE[idx[i]]
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
