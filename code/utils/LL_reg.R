# Log likelihood function based on regression
LL_reg = function(x,
                  data,
                  glmods,
                  model) {
  
  # Run regression
  cres = Regression_model(x = x,
                          cdf = data,
                          glmods = glmods,
                          model = model)
  
  # Judge LL only based on choices in critical comparisons (bandit 1 vs. bandit 2)
  preds = predict(cres[[1]], type="response")
  probs = dbinom(cres[[2]]$choice=='right', prob=preds, size=1, log=TRUE)
  cidx = which((cres[[2]]$bandit == '12' | cres[[2]]$bandit == '21'))
  b2_logLik = -sum(probs[cidx])
  
  # Return negative LL of bandit comparison 1 vs 2
  return(b2_logLik)
}