reg_model = function(x, cdf, model, tau = 0.2, plot = FALSE) {
  values = updates = fupdates = alphas = matrix(NA, 3, 240*2)
  x = c(x, rep(NA, 3-length(x)))
  for (crun in 1:2) {
    ccdf = subset(cdf, cdf$run == crun)[1:239,]
    ccidx = ((crun-1)*240 + 1):(crun*240)
    experienced_outcomes = tapply(ccdf$outcome, ccdf$chosen_bandit, function(x) x)
    outcome_trials  = sapply(1:3, function(x) which(ccdf$chosen_bandit == x))
    for (cbandit in 1:3) {
      cvals = c(50, rep(NA, 239))
      ccvals = experienced_outcomes[[cbandit]]
      #ccvals_round = round((ccvals/4))*4
      #ctab = table(ccvals_round)/sum(table(ccvals_round))
      #cprobs = sapply(ccvals_round, function(x) ctab[which(x == names(ctab))])
      cvals[outcome_trials[[cbandit]]+1] = ccvals
      cVPE = comp_value(cvals, x, cbandit, tau)
      values[cbandit,ccidx] = cVPE$V
      updates[cbandit,ccidx] = cVPE$PE
      fupdates[cbandit,ccidx] = filter(cVPE$PE, rev(0.40^(1:10)/sum(0.40^(1:10))))
      alphas[cbandit,ccidx] = cVPE$LR
    }
  }
  supdates = matrix(scale(c(abs(fupdates)))[,1], 3, 480)
  cdf$V1 = values[cbind(cdf$option_left,1:480)]
  cdf$V2 = values[cbind(cdf$option_right,1:480)]
  cdf$choice = as.factor(cdf$choice)
  cdf$V1u = supdates[cbind(cdf$option_left,1:480)]
  cdf$V2u = supdates[cbind(cdf$option_right,1:480)]
  cdf$V1u[is.na(cdf$V1u)] = cdf$V2u[is.na(cdf$V2u)] = 0
  cdf = subset(cdf, is.na(cdf$forced) & !is.na(cdf$choice))
  cglm = glm(glmods[[model]], family=binomial(link='logit'), data = cdf)
  return(list(cglm, cdf, updates, alphas))
}
