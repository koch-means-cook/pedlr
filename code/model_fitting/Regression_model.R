Regression_model = function(x,
                            cdf,
                            glmods,
                            model,
                            tau = 0.2,
                            plot = FALSE) {
  
  # Allocate matrices to store important variables
  values = updates = fupdates = alphas = surprise = matrix(NA, 3, 240*2)
  
  # Pad x (parameter vector) with NA (to always be length = 6)
  x = c(x, rep(NA, 6-length(x)))
  
  # Loop over runs
  for (run_count in 1:2) {
    
    # Get data of run
    data_run = cdf[run == run_count][1:239]
    # Set index for each run (1:240);(241:480)
    ccidx = ((run_count-1)*240 + 1):(run_count*240)
    
    # Dont use bandit_choice == 0 (time-outs)
    data_run$chosen_bandit[data_run$chosen_bandit == 0] = NA
    # Get outcomes separately for each bandit (1 vs. 2 vs. 3)
    experienced_outcomes = tapply(data_run$outcome,
                                  data_run$chosen_bandit,
                                  function(x) x)
    # Get trials for experienced outcomes
    outcome_trials  = sapply(1:3, function(x) which(data_run$chosen_bandit == x))
    
    # Loop over all three bandits
    for (cbandit in 1:3) {
      
      # Initialize values with 50
      cvals = c(50, rep(NA, 239))
      
      # Get experienced outcomes for current bandit
      ccvals = experienced_outcomes[[cbandit]]
      #ccvals_round = round((ccvals/4))*4
      #ctab = table(ccvals_round)/sum(table(ccvals_round))
      #cprobs = sapply(ccvals_round, function(x) ctab[which(x == names(ctab))])
      
      # Enter experienced values into vector holding each trial's outcome across
      # bandits, but at position (trial) they were experienced at
      cvals[outcome_trials[[cbandit]]+1] = ccvals
      
      # Compute value & prediction error for current bandit
      cVPE = Compute_value(V = cvals,
                           x = x,
                           bandit = cbandit,
                           tau = tau,
                           model = model)
      
      # Enter values, LR, and PE into allocated matrices
      values[cbandit,ccidx] = cVPE$V
      alphas[cbandit,ccidx] = cVPE$LR
      updates[cbandit,ccidx] = cVPE$PE
      surprise[cbandit,ccidx] = cVPE$S
      
      # # - - - - - - - - POTENTIAL BUG 
      # # (convolution with PEs from future (2 sided convolution); instead of recency weighted PE with lag of 10)
      # # Get "recency-weighted" PEs to measure uncertainty (how strong was PE over last 10 trials incl leakage?) (atm the filter is applied to both sides instead of only past values)
      # fupdates[cbandit,ccidx] = stats::filter(cVPE$PE, 
      #                                         filter = rev(0.40^(1:10)/sum(0.40^(1:10))),
      #                                         method = 'convolution',
      #                                         sides = 2) # should be `sides = 1`
      # # - - - - - - - - POTENTIAL BUG
      
      # # - - - - - - - - Fixed BUG 
      # # Get "recency-weighted" PEs to measure uncertainty (how strong was PE over last 10 trials incl leakage?)
      # fupdates[cbandit,ccidx] = stats::filter(cVPE$PE, 
      #                                         filter = rev(0.40^(1:10)/sum(0.40^(1:10))),
      #                                         method = 'convolution',
      #                                         sides = 1)
      # # - - - - - - - - Fixed BUG
      
      # - - - - - - - - Alternative
      # Use trailing surprise
      fupdates[cbandit,ccidx] = cVPE$S
      # - - - - - - - - Alternative
      
    }
  }
  
  # Enter trailing surprise for each bandit (measure of uncertainty)
  #supdates = matrix(scale(c(abs(fupdates)))[,1], 3, 480)
  supdates = matrix(c(fupdates), 3, 480)
  
  # Enter values for left option and right option
  cdf$V1 = values[cbind(cdf$option_left,1:480)]
  cdf$V2 = values[cbind(cdf$option_right,1:480)]
  
  # Enter choice
  cdf$choice = as.factor(cdf$choice)
  
  # Enter uncertainty for left and right option (set uncertainty to 0 for NAs)
  cdf$V1u = supdates[cbind(cdf$option_left,1:480)]
  cdf$V2u = supdates[cbind(cdf$option_right,1:480)]
  cdf$V1u[is.na(cdf$V1u)] = cdf$V2u[is.na(cdf$V2u)] = 0
  
  # Exclude forced choices and missing responses (e.g. time-outs)
  cdf = subset(cdf, is.na(cdf$forced) & !is.na(cdf$choice))
  # Run binomial regression with logit-link (due to probability for left choice vs. right choice)
  cglm = glm(glmods[[which(names(glmods) == model)]],
             family = binomial(link = 'logit'),
             data = cdf)
  
  # z-score input to regression model to aid interpretability of betas
  cols = c('V1', 'V2', 'V1u', 'V2u')
  z_cdf = cdf[, (cols) := lapply(.SD, scale), .SDcols = cols]
  # Run binomial regression with logit-link (due to probability for left choice vs. right choice)
  z_cglm = glm(glmods[[which(names(glmods) == model)]],
               family = binomial(link = 'logit'),
               data = z_cdf)
  
  # Form output including regression based on normalized predictors and
  # non-normalized predictors
  out = list(norm = list(z_cglm,
                         z_cdf,
                         updates,
                         alphas,
                         values,
                         supdates),
             no_norm = list(cglm,
                            cdf,
                            updates,
                            alphas,
                            values,
                            supdates))
  
  # Return regression results
  return(out)
}
