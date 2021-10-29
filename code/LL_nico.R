LL_nico = function(x, data, model) {
  if (model == 'PEDLR') {
    params = NULL
    params$alpha0 = x[1]
    params$alpha1 = x[2]
    params$temp = x[3]
    params$init_values = 50
    cdf = PEDLR_nico(data, params)
  } else if (model == 'RW') {
    params = NULL
    params$alpha = x[1]
    params$temp = x[2]
    params$init_values = 50
    cdf = RW_nico(data, params)
  }
  LL = -sum(log(cdf$choice_prob[cdf$forced_choice == 0 & !is.na(cdf$PE)]))
  return(LL)
}
