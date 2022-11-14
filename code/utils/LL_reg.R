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
  
  # Return negative LL
  return(-stats::logLik(cres[[1]])[1])
}