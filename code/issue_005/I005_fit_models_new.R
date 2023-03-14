library(here)
library(data.table)
library(nloptr)
library(magrittr)

I005_fit_models_new = function(data,
                          algorithm,
                          xtol_rel,
                          maxeval,
                          x0,
                          lb,
                          ub){

  # data = Load_data() %>%
  #   Apply_exclusion_criteria(., choice_based_exclusion = TRUE) %>%
  #   .[participant_id == '1NU6KP5']
  # algorithm = 'NLOPT_GN_DIRECT_L'
  # xtol_rel = 1.0e-5
  # maxeval = 10
  # x0 = list(0.2,
  #           c(0.2, 0.2),
  #           c(0.2, 0.5, 1),
  #           c(0.2, 0.5, 1, 0.2))
  # lb = list(0.01,
  #           c(0.01, 0.01),
  #           c(exp(-5), exp(-5), -20),
  #           c(exp(-5), exp(-5), -20, 0.01))
  # ub = list(1,
  #           c(1, 1),
  #           c(1, 1, 20),
  #           c(1, 1, 20, 1))
  
  # Set base_path
  base_path = here::here()
  
  # Source own functions
  source(file.path(base_path, 'code', 'utils', 'LL_reg.R'))
  source(file.path(base_path, 'code', 'model_fitting', 'Regression_model.R'))
  source(file.path(base_path, 'code', 'model_fitting', 'Compute_value.R'))
  source(file.path(base_path, 'code', 'model_fitting', 'LRfunction.R'))
  
  participant_id = unique(data$participant_id)
  
  
  # Define fitting parameters
  copts = list('algorithm' = algorithm,
               'xtol_rel' = xtol_rel,
               'maxeval'= maxeval)
  # copts = list('algorithm'='NLOPT_GN_DIRECT_L',
  #              'xtol_rel'=1.0e-5,
  #              'maxeval'= 100)
  
  # Get formulas for regression
  glmods = list(
    # RW model
    rw = as.formula('choice ~ V1 + V2'),
    # Uncertainty Model
    uncertainty = as.formula('choice ~ V1 + V2 + V1u + V2u'),
    # Surprise Model
    surprise = as.formula('choice ~ V1 + V2'),
    # Uncertainty+Surprise Model
    uncertainty_surprise = as.formula('choice ~ V1 + V2 + V1u + V2u'))
  # Get number of models
  n_models = length(glmods)
  
  # Set empty data.table
  out = data.table::data.table()
  
  # Loop over models that include lower bound
  for (model_count in c(3,4)) {
    
    # Get model name
    model_name = names(glmods)[model_count]
    
    # Set names of parameters
    if(model_name == 'rw'){
      para_names = 'alpha'
    } else if(model_name == 'uncertainty'){
      para_names = c('alpha', 'pi')
    } else if(model_name == 'surprise'){
      para_names = c('l', 'u', 's')
    }else if(model_name == 'uncertainty_surprise'){
      para_names = c('l', 'u', 's', 'pi')
    }
    
    # Fit model
    cmod = nloptr::nloptr(x0 = x0[[model_count]],
                          eval_f = LL_reg,
                          lb = lb[[model_count]],
                          ub = ub[[model_count]],
                          opts = copts,
                          data = data,
                          glmods = glmods,
                          model = model_count)
    
    # Keep solution of fit BUT vary lower bound parameter to see impact on LL
    # Create array of lower bounds to vary over
    lower_grid = seq(from = 0, to = 1, length = 101)
    # Get LL for each new value of lower bound
    for(new_lower_count in seq(length(lower_grid))){
      
      # Set new lower bound from array
      new_lower = lower_grid[new_lower_count]
      
      # Replace lower bound parameter while keeping others constant
      cmod$solution[1] = new_lower
      # Get LL
      cres = Regression_model(x = cmod$solution,
                              cdf = data,
                              glmods = glmods,
                              model = model_count)
      
      # Get regression report
      cglm = cres[[1]]
      #cres[[2]]
      # Learning rate on each trial
      lr = cres[[4]][2,] #colSums(cres[[4]], na.rm = TRUE)
      lr = lr[-which(is.na(lr))]
      # PE on each trial
      pe = cres[[3]][2,which(!is.na(cres[[4]][2,]))]
      
      
      # Enter variables into output array
      # Learning rates for each possible PE (0-100)
      LRs = tapply(c(lr, rep(NA, 100)), c(round(abs(pe)), seq(0, 99)), mean, na.rm = TRUE)
      LRs = cbind(seq(length(LRs)), LRs)
      LRs = as.data.table(LRs)
      colnames(LRs) = c('x', 'value')
      LRs$variable = 'LRs'
      # P values for V1 and V2
      ps = summary(cglm)$coefficients[2:3,4]
      # Betas
      # Regression betas
      coefs = c(names(coef(cglm)), para_names)
      coefs = as.data.table(cbind(coefs,
                                  unname(c(coef(cglm), cmod$solution))))
      colnames(coefs) = c('x', 'value')
      coefs$variable = 'coefs'
      # Add model prediction
      cres[[2]]$model_p = predict(cglm, type = 'response')
      model_p_my = cres[[2]]$model_p
      # AICs
      probs = dbinom(cres[[2]]$choice=='right', prob=cres[[2]]$model_p, size=1, log=TRUE)
      probs_my = probs
      # Potential bug: also includes chosen bandit = 2; cidx = which(cres[[2]]$bandit == '12' | cres[[2]]$bandit == '21' & cres[[2]]$chosen_bandit == 1)
      cidx = which((cres[[2]]$bandit == '12' | cres[[2]]$bandit == '21'))
      cidx_my = cidx
      b2_logLik = sum(probs[cidx])
      b2_logLik_my = b2_logLik
      #AICs[cid, model_count] = 2*(length(coef(cglm)) + length(x0[[model_count]])) + 2*b2_logLik
      # number of parameters
      k = (length(coef(cglm)) + length(x0[[model_count]]))
      # Number of samples
      n = length(cidx)
      AICs = 2*k - 2*b2_logLik
      AICc = AICs + ((2*(k^2) + 2*k) / (n - k - 1))
      # x0
      x0_vals = as.data.table(cbind(para_names, x0[[model_count]]))
      colnames(x0_vals) = c('x', 'value')
      x0_vals$variable = 'x0'
      
      # Fuse measures into one data table
      temp = data.table(rbind(LRs, coefs, x0_vals))
      temp$b2_ll = b2_logLik
      temp$AIC = AICs
      temp$AICc = AICc
      temp$p_V1 = ps['V1']
      temp$p_V2 = ps['V2']
      # Add model
      temp$model = model_name
      # Add participant_id
      temp$participant_id = participant_id
      temp$l_bound_iteration = new_lower_count
      # sort columns
      temp = setcolorder(temp, c('participant_id', 'model', 'b2_ll', 'AIC', 'AICc', 'p_V1', 'p_V2',
                                 'variable', 'x', 'value'))
      
      out = rbind(out, temp)
    }
    
  }
  
  # Return fitting for all models
  return(out)
}

