library(here)
library(data.table)

Fit_models_new = function(data,
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
  #           0.2,
  #           c(0.2, 0.5, 1),
  #           c(0.2, 0.5, 1))
  # lb = list(0.01,
  #           0.01,
  #           c(exp(-5), exp(-5), -20),
  #           c(exp(-5), exp(-5), -20))
  # ub = list(1,
  #           1,
  #           c(1, 1, 20),
  #           c(1, 1, 20))
  
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
  
  # Send message to user
  message(paste('Starting ID', participant_id, '...\n'), appendLF = FALSE)
  
  # Set empty data.table
  out = data.table::data.table()
  
  # Loop over models
  for (model_count in 1:n_models) {
    
    # Get model name
    model_name = names(glmods)[model_count]
    
    # Set names of parameters
    if(model_name %in% c('rw', 'uncertainty')){
      para_names = 'alpha'
    } else if(model_name %in% c('surprise', 'uncertainty_surprise')){
      para_names = c('l', 'u', 's')
    }
    
    # Fit model
    cmod = nloptr(x0 = x0[[model_count]],
                  eval_f = LL_reg,
                  lb = lb[[model_count]],
                  ub = ub[[model_count]],
                  opts = copts,
                  data = data,
                  glmods = glmods,
                  model = model_count)
    
    # Run regression with best fitting parameters
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
    # LR
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
    # AICs
    AICs = AIC(cglm) + 2*length(x0[[model_count]])
    # x0
    x0_vals = as.data.table(cbind(para_names, x0[[model_count]]))
    colnames(x0_vals) = c('x', 'value')
    x0_vals$variable = 'x0'
    
    # Fuse measures into one data table
    temp = data.table(rbind(LRs, coefs, x0_vals))
    temp$AIC = AICs
    temp$p_V1 = ps['V1']
    temp$p_V2 = ps['V2']
    # Add model
    temp$model = model_name
    # Add participant_id
    temp$participant_id = participant_id
    # sort colums
    temp = setcolorder(temp, c('participant_id', 'model', 'AIC', 'p_V1', 'p_V2',
                               'variable', 'x', 'value'))
    
    out = rbind(out, temp)

  }
  
  # Send message to user
  message(paste('done!'), appendLF = FALSE)
  
  # Return fitting for all models
  return(out)
}

