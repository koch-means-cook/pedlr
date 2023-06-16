library(here)
library(data.table)
library(nloptr)
library(magrittr)

Fit_models_new = function(data,
                          algorithm,
                          xtol_rel,
                          maxeval,
                          x0,
                          lb,
                          ub,
                          param_recov = FALSE){

  # data = Load_data() %>%
  #   Apply_exclusion_criteria(., choice_based_exclusion = TRUE) %>%
  #   .[participant_id == '1NU6KP5']
  # algorithm = 'NLOPT_GN_DIRECT_L'
  # xtol_rel = 1.0e-5
  # maxeval = 10
  # x0 = list(0.2,
  #           c(0.2, 0.2),
  #           c(0.2, 0.5, 1),
  #           c(0.2, 0.5, 1,0.2))
  # lb = list(0.01,
  #           c(0.01, 0.01),
  #           c(0.01, 0.01, -20),
  #           c(0.01, 0.01, -20, 0.01))
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
  pes = data.table::data.table()
  
  # Normal fitting:
  # Fit all 4 models
  if(param_recov == FALSE){
    model_pool = 1:n_models
  } else if(param_recov == TRUE){
    # For parameter recovery:
    # Fit only model that was used for simulation of data (based on number of parameters)
    if(is.na(unique(data$x2))){
      # RW model
      model_pool = 1
    } else if(is.na(unique(data$x3))){
      # Uncertainty model
      model_pool = 2
    } else if(is.na(unique(data$x4))){
      # Surprise model
      model_pool = 3
    } else{
      # Surprise+Uncertainty model
      model_pool = 4
    }
  }
  
  
  # Loop over models
  for (model_count in model_pool) {
    
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
    
    if(param_recov == FALSE){
      x0_model = x0[[model_count]]
      lb_model = lb[[model_count]]
      ub_model = ub[[model_count]]
      
    # Only allow specified parameters for single model for parameter recovery
    } else if(param_recov == TRUE){
      x0_model = x0
      lb_model = lb
      ub_model = ub
    }
    
    # Fit model
    cmod = nloptr::nloptr(x0 = x0_model,
                          eval_f = LL_reg,
                          lb = lb_model,
                          ub = ub_model,
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
    #AICs[cid, model_count] = 2*(length(coef(cglm)) + length(x0_model)) + 2*b2_logLik
    # number of parameters
    k = (length(coef(cglm)) + length(x0_model))
    # Number of samples
    n = length(cidx)
    AICs = 2*k - 2*b2_logLik
    AICc = AICs + ((2*(k^2) + 2*k) / (n - k - 1))
    # x0
    x0_vals = as.data.table(cbind(para_names, x0_model))
    colnames(x0_vals) = c('x', 'value')
    x0_vals$variable = 'x0'
    
    # Fuse measures into one data table
    temp = data.table(rbind(LRs, coefs, x0_vals))
    temp$AIC = AICs
    temp$AICc = AICc
    temp$p_V1 = ps['V1']
    temp$p_V2 = ps['V2']
    # Add model
    temp$model = model_name
    # Add participant_id
    temp$participant_id = participant_id
    # sort colums
    temp = setcolorder(temp, c('participant_id', 'model', 'AIC', 'p_V1', 'p_V2',
                               'variable', 'x', 'value'))
    # Fuse fitting output
    out = rbind(out, temp)
    
    # Output of updates/PEs at each trial
    temp_pes = data.table::data.table(t(cres[[3]]))
    colnames(temp_pes) = c('low', 'mid', 'high')
    temp_pes = temp_pes %>%
      .[, ':='(update_low = as.logical(c(0, diff(low)) != 0),
               update_mid = as.logical(c(0, diff(mid)) != 0),
               update_high = as.logical(c(0, diff(high) != 0)),
               trial = seq(.N))] %>%
      # Mark first trials of runs with NA (since no PE)
      .[trial %in% c(1, 241), ':='(update_low = NA,
                                   update_mid = NA,
                                   update_high = NA)] %>%
      # Put bandit that was updated in a trial into column
      .[, updated_bandit := which(c(update_low, update_mid, update_high) == TRUE),
        by = 'trial'] %>%
      # Get PE for bandit that was updated
      .[, pe := c(low,mid,high)[updated_bandit],
        by = 'trial'] %>%
      # Select only neccessary columns
      .[, c('trial', 'updated_bandit', 'pe')]
    # Add relevant information for output
    temp_pes$participant_id = participant_id
    temp_pes$model = model_name
    temp_pes$b2_ll = b2_logLik
    temp_pes$AIC = AICs
    temp_pes$AICc = AICc
    temp_pes = setcolorder(temp_pes, c('participant_id', 'model', 'b2_ll', 'AIC', 'AICc'))
    # Fuse PE output
    pes = rbind(pes, temp_pes)

  }
  
  # Return fitting and PE output for all models
  output = list(fitting_out = out,
                pes_out = pes)
  return(output)
}

