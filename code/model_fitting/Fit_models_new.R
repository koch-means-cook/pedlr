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
                          param_recov = FALSE,
                          recov_model = NA){
    
  
  
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
    # Seplr Model
    seplr = as.formula('choice ~ V1 + V2'),
    # Uncertainty+Seplr Model
    uncertainty_seplr = as.formula('choice ~ V1 + V2 + V1u + V2u'),
    # Surprise Model
    surprise = as.formula('choice ~ V1 + V2'),
    # Uncertainty+Surprise Model
    uncertainty_surprise = as.formula('choice ~ V1 + V2 + V1u + V2u'),
    # Valence+Surprise Model
    seplr_surprise = as.formula('choice ~ V1 + V2'))
  # Get number of models
  n_models = length(glmods)
  
  # Set empty data.table
  out = data.table::data.table()
  model_data = data.table::data.table()
  
  # Normal fitting:
  # Fit all models
  if(param_recov == FALSE){
    model_pool = 1:n_models
  } else if(param_recov == TRUE){
    # For parameter recovery:
    # Fit only model that was used for simulation of data
    # RW model
    if(recov_model == 'rw'){
      model_pool = 1
      
      # Uncertainty model
    } else if(recov_model == 'uncertainty'){
      model_pool = 2
      
      # Seplr model
    } else if(recov_model == 'seplr'){
      model_pool = 3
      
      # Uncertainty+Seplr model
    } else if(recov_model == 'uncertainty_seplr'){
      model_pool = 4
      
      # Surprise model
    } else if(recov_model == 'surprise'){
      model_pool = 5
      
      # Uncertainty+Surprise model
    } else if(recov_model == 'uncertainty_surprise'){
      model_pool = 6
      
      # Uncertainty+Surprise model
    } else if(recov_model == 'seplr_surprise'){
      model_pool = 7
      
      # Throw error in case model is not found
    } else{
      stop(paste0("Model specified for recovery (\'",
                  model,
                  "\') not found."))
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
    } else if(model_name == 'seplr'){
      para_names = c('alpha_pos', 'alpha_neg')
    } else if(model_name == 'uncertainty_seplr'){
      para_names = c('alpha_pos', 'alpha_neg', 'pi')
    } else if(model_name == 'surprise'){
      para_names = c('l', 'u', 's')
    } else if(model_name == 'uncertainty_surprise'){
      para_names = c('l', 'u', 's', 'pi')
    } else if(model_name == 'seplr_surprise'){
      para_names = c('l_pos', 'u_pos', 's_pos', 'l_neg', 'u_neg', 's_neg')
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
                          model = model_name)
    
    
    # Run regression with best fitting parameters
    cres = Regression_model(x = cmod$solution,
                            cdf = data,
                            glmods = glmods,
                            model = model_name)
    
    # Get regression report (regression model using z-scored predictors)
    z_cglm = cres$norm[[1]]
    #cres[[2]]
    # Learning rate IN SECOND BANDIT on each trial
    lr = cres$norm[[4]][2,] #colSums(cres[[4]], na.rm = TRUE)
    lr = lr[-which(is.na(lr))]
    # PE IN SECOND BANDIT on each trial
    pe = cres$norm[[3]][2,which(!is.na(cres$norm[[4]][2,]))]
    # Values for each bandit on each trial
    vals = data.table::data.table(t(cres$norm[[5]]))
    colnames(vals) = c('value_low', 'value_mid', 'value_high')
    # Get behavioral data
    cdf = cres$norm[[2]]
    
    # Enter variables into output array
    # Average LRs IN SECOND BANDIT for each possible PE (0:99 or (-99):99)
    # Models using absolute PE (or don't distinguish between neg and pos PE)
    if(model_name %in% c('rw',
                         'uncertainty',
                         'surprise',
                         'uncertainty_surprise')){
      LRs = tapply(
        # Take all seen LRs, extend with buffer of NA for every possible PE (in
        # case a specific PE did not happen for participant)
        c(lr, rep(NA, 100)),
        # Take absolute and rounded value of all seen PEs, extend with buffer of
        # all possible PEs (0-99) (in case a specific PE did not happen)
        c(round(abs(pe)), seq(0, 99)),
        # use 'mean' function to get LR in case a PE happened, and NA in case it
        # did not
        mean,
        na.rm = TRUE)
      
      # Models using separate LRs for positive and negative LR
    } else if(model_name %in% c('seplr',
                                'uncertainty_seplr',
                                'seplr_surprise')){
      LRs = tapply(
        # Take all seen LRs, extend with buffer of NA for every possible PE (in
        # case a specific PE did not happen for participant)
        c(lr, rep(NA, 199)),
        # Take rounded value of all seen PEs, extend with buffer of
        # all possible PEs (-99:99) (in case a specific PE did not happen)
        c(round(pe), seq(-99, 99)),
        # use 'mean' function to get LR in case a PE happened, and NA in case it
        # did not
        mean,
        na.rm = TRUE)
      # Set LR for PE == 0 to NA (because rounding of e.g.PE=-0.3 &
      # PE=0.2 causes mixing of separate LRS for pos and neg PEs)
      LRs[names(LRs) == "0"] = NaN
    }
    
    # Bring LRs for each PE IN SECOND BANDIT into output format
    LRs = cbind(as.numeric(names(LRs)),
                LRs)
    LRs = as.data.table(LRs)
    colnames(LRs) = c('x', 'value')
    LRs$variable = 'LRs'
    # Replace NaN with NA
    LRs$value[is.nan(LRs$value)] = NA
    
    # P values for V1 and V2
    ps = summary(z_cglm)$coefficients[2:3,4]
    
    # Regression report of model NOT using z-scored predictors
    cglm = cres$no_norm[[1]]
    
    # Betas
    # Regression betas
    coefs = c(paste0('z_', names(coef(z_cglm))),
              names(coef(cglm)),
              para_names)
    coefs = as.data.table(cbind(coefs,
                                unname(c(coef(z_cglm),
                                         coef(cglm),
                                         cmod$solution))))
    colnames(coefs) = c('x', 'value')
    coefs$variable = 'coefs'
    # Add model prediction
    cres$norm[[2]]$model_p = predict(z_cglm, type = 'response')
    model_p_my = cres$norm[[2]]$model_p
    # AICs
    probs = dbinom(cres$norm[[2]]$choice=='right', prob=cres$norm[[2]]$model_p, size=1, log=TRUE)
    probs_my = probs
    # Only take low/mid bandit comparisons
    cidx = which((cres$norm[[2]]$bandit == '12' | cres$norm[[2]]$bandit == '21'))
    cidx_my = cidx
    # Log likelihood of relevant trials
    b2_logLik = sum(probs[cidx])
    b2_logLik_my = b2_logLik
    #AICs[cid, model_count] = 2*(length(coef(cglm)) + length(x0_model)) + 2*b2_logLik
    # number of parameters
    k = (length(coef(z_cglm)) + length(x0_model))
    # Number of samples
    n = length(cidx)
    AICs = 2*k - 2*b2_logLik
    AICc = AICs + ((2*(k^2) + 2*k) / (n - k - 1))
    # BIC
    BIC = k*log(n) - 2*b2_logLik
    # chance-level log likelihood for pseudo-r2 (50/50 choice probability in
    # each relevant trial)
    b2_logLik_chance = n * log(.5)
    # Pseudo-r2 (Cramerer & Ho 1999; Daw 2011)
    # 1 - L/R = 1 - logLik under model/loglik under chance
    pseudorsq = 1 - (b2_logLik/b2_logLik_chance)
    # x0
    x0_vals = as.data.table(cbind(para_names, x0_model))
    colnames(x0_vals) = c('x', 'value')
    x0_vals$variable = 'x0'
    
    # Fuse measures into one data table
    temp = data.table(rbind(LRs, coefs, x0_vals))
    temp$AIC = AICs
    temp$AICc = AICc
    temp$BIC = BIC
    temp$b2_loglik = b2_logLik
    temp$b2_loglik_chance = b2_logLik_chance
    temp$pseudorsq = pseudorsq
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
    temp_model_data = data.table::data.table(t(cres$norm[[3]]))
    colnames(temp_model_data) = c('low', 'mid', 'high')
    temp_model_data = temp_model_data %>%
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
      # Add current value of each bandit for every trial
      cbind(., vals) %>%
      # Add uncertainty values
      .[, ':='(u_low = cres$norm[[6]][1,],
               u_mid = cres$norm[[6]][2,],
               u_high = cres$norm[[6]][3,])] %>%
      # Select only neccessary columns
      .[, c('trial', 'updated_bandit', 'pe', 'value_low', 'value_mid', 'value_high', 'u_low', 'u_mid', 'u_high')]
    # Add relevant information for output
    temp_model_data$model = model_name
    temp_model_data$b2_loglik = b2_logLik
    temp_model_data$b2_loglik_chance = b2_logLik_chance
    temp_model_data$pseudorsq = pseudorsq
    temp_model_data$AIC = AICs
    temp_model_data$AICc = AICc
    temp_model_data$BIC = BIC
    temp_model_data = setcolorder(temp_model_data, c('model', 'b2_loglik', 'b2_loglik_chance', 'pseudorsq', 'AIC', 'AICc', 'BIC'))
    
    # Add behavioral data to modeling output
    temp_model_data = cbind(data, temp_model_data)
    
    # Fuse PE output
    model_data = rbind(model_data, temp_model_data)

  }
  
  # Return fitting and PE output for all models
  output = list(fitting_out = out,
                model_data = model_data)
  return(output)
}

