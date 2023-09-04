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
  # (use regression model based on z-scored predictors, but -LL is identical 
  # for regression model based on normed and non-normed predictors)
  preds = predict(cres$norm[[1]], type="response")
  probs = dbinom(cres$norm[[2]]$choice=='right', prob=preds, size=1, log=TRUE)
  cidx = which((cres$norm[[2]]$bandit == '12' | cres$norm[[2]]$bandit == '21'))
  
  # Ignore the first 3 updates of each bandit in the LL calculation (here large
  # PEs arise which are not reflecting any surprise/rare outcomes because the
  # paticipant has not built an expectation yet)
  # GET INDEX FIRST 3 UPDATES OF CHOICE='1' IN [COMP='12' | COMP='21']
  # SAME FOR CHOICE=='2'
  # DELETE THOSE INDEXES FROM CIDX
  
  
  b2_logLik = -sum(probs[cidx])
  
  # Return negative LL of bandit comparison 1 vs 2
  return(b2_logLik)
}