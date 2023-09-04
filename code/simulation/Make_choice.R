Make_choice = function(model,
                       beta_weights,
                       predictors){
  
  # Example
  # model = 'uncertainty_seplr'
  # beta_weights = c(0, -0.2, 0.2, -0, 0)
  # predictors = c(1, 5, 20, 4, 6)
  
  # Safety checks
  if(model %in% c('rw', 'seplr', 'surprise')){
    # Exclude uncertainty in models that don't have an uncertainty module
    beta_weights = beta_weights[1:3]
    predictors = predictors[1:3]
  } else if(model %in% c('uncertainty', 'uncertainty_seplr', 'uncertainty_surprise')){
    # Check if 5 values were supplied
    beta_weights = beta_weights[1:5]
    predictors = predictors[1:5]
  } else{
    stop(paste0("Supplied unknown model to choice module: \'", model, "\'"))
  }
  
  # Problem:  when fitting the model for each participant, the predictor values
  #           are z-scored. When we want to use these betas to predict choices
  #           on a single trial, the predictor values (e.g. value estimates of
  #           certain bandit) are not z-scored yet. We also don't know how the
  #           z-scored values would look like, because we only z-score once we
  #           have the predictor value for the entire experiment (which we don't
  #           have on a single-trial basis.
  # Solution: Make output of fitting process include betas based on z-scored
  #           predictors AND based on non-z-scored predictors. The way we could
  #           check the recovery of beta values/posterior predictive checks
  #           while still providing normalized beta values capable of across-
  #           predictor comparison.
  
  # Get prediction according to model formula
  linear_prediction = sum(beta_weights * predictors)
  
  # Use link function of regression model (logit) = probability to choose
  # RIGHTMOST bandit
  link_prediction = 1/(1 + exp(-linear_prediction))
  
  # Get probabilities to choose:
  # RIGHTMOST option
  choice_prob_2 = link_prediction
  # leftmost option
  choice_prob_1 = 1- link_prediction
  # Form vector of both choices
  prob = c(choice_prob_1, choice_prob_2)
  
  # Sample choice from probabilities (2 = choosing rightmost bandit,
  # 1 = left bandit)
  choice = sample(c(1,2),
                  1,
                  replace = TRUE,
                  prob = prob)
  # Get probability of choice
  choice_prob = prob[choice]
  
  # Returns index and probability of choice if options are ordered in a two-entry vector
  ans = list('choice' = choice,
             'choice_prob' = choice_prob)
  return(ans)
  
  
}