Get_svs = function(starting_values){
  
  # Set starting values
  # rw (alpha)
  lb = list(0.01,
            # uncertainty (alpha, pi)
            c(0.01, 0.01),
            # seplr (alpha_pos, alpha_neg)
            c(0.01, 0.01),
            # uncertainty+seplr (alpha_pos, alpha_neg, pi)
            c(0.01, 0.01, 0.01),
            # surprise (l,u,s)
            c(exp(-5), exp(-5), 1),
            # uncertainty+surprise (l,u,s,pi)
            c(exp(-5), exp(-5), 1, 0.01))
  ub = list(1,
            c(1, 1),
            c(1, 1),
            c(1, 1, 1),
            c(1, 1, 7),
            c(1, 1, 7, 1))
  # Set starting values either fixed or random, depending on function input
  if(starting_values == 'fixed'){
    x0 = list(0.2,
              c(0.2, 0.2),
              c(0.2, 0.2),
              c(0.2, 0.2, 0.2),
              c(0.2, 0.5, 1),
              c(0.2, 0.5, 1, 0.2))
  } else if(starting_values == 'random'){
    # Use same random starting value for similar models
    rand_alpha_1 = runif(1, min = lb[[1]], max = ub[[1]])
    rand_alpha_2 = runif(1, min = lb[[1]], max = ub[[1]])
    rand_pi = runif(1, min = lb[[2]][2], max = ub[[2]][2])
    rand_l = runif(1, min = lb[[5]][1], max = ub[[5]][1])
    rand_u = runif(1, min = lb[[5]][2], max = ub[[5]][2])
    rand_s = runif(1, min = lb[[5]][3], max = ub[[5]][3])
    # rw (alpha)
    x0 = list(rand_alpha_1,
              # uncertainty (alpha, pi)
              c(rand_alpha_1, rand_pi),
              # seplr (alpha_pos, alpha_neg)
              c(rand_alpha_1, rand_alpha_2),
              # uncertainty+seplr (alpha_pos, alpha_neg, pi)
              c(rand_alpha_1, rand_alpha_2, rand_pi),
              # surprise (l,u,s)
              c(rand_l, rand_u, rand_s),
              # uncertainty+surprise (l,u,s,pi)
              c(rand_l, rand_u, rand_s, rand_pi))
  }
  out = list(lb = lb,
             ub = ub,
             x0 = x0)
  return(out)
  
}