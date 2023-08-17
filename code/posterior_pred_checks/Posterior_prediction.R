library(here)
library(optparse)

Posterior_prediction = function(participant_id,
                                tau = 0.2){
  
  # participant_id = '4BKAHC6'
  # tau = 0.2
  
  # Source own functions
  source_path = file.path(here::here(), 'code', fsep = .Platform$file.sep)
  source(file.path(source_path, 'posterior_pred_checks', 'Get_fit.R', fsep = .Platform$file.sep))
  source(file.path(source_path, 'simulation', 'Simulate.R', fsep = .Platform$file.sep))
  
  # Load participants behavioral data
  file = file.path(here::here(), 'data', paste0(participant_id, '_exp_data.tsv'))
  data = data.table::fread(file, sep = '\t', na.strings = 'n/a')
  
  # Load fit parameters of participant
  fit = Get_fit(participant_id = participant_id,
                starting_values = 'random')
  fit_params = fit$fit
  fit_data = fit$modeldata
  
  # Iterate over models
  for(model_name in unique(fit_params$model)){
    
    # Get fit parameters for model
    model_fit = fit_params[model == model_name, ]
    param_names = model_fit[variable == 'x0', x]
    param_values = model_fit[variable == 'coefs' & x %in% param_names, value]
    params = rep(NA, 4)
    params[1:length(param_values)] = param_values
    
    # Get beta weights (based on NON z-scored predictors)
    beta_names = c('(Intercept)', 'V1', 'V2', 'V1u', 'V2u')
    beta_values = model_fit[variable == 'coefs' & x %in% beta_names, value]
    beta_weights = rep(NA, 5)
    beta_weights[1:length(beta_values)] = beta_values
    
    # Simulate data with participant's fitted parameters
    sim = Simulate(data = data,
                   x = params,
                   tau = tau,
                   model = model_name,
                   beta_weights = beta_weights)
    
    # Save data simulated with participants parameters
    save_dir = file.path(here::here(),
                         'derivatives',
                         'posterior_pred_checks',
                         fsep = .Platform$file.sep)
    # Create save directory if it does not exist yet
    if(!dir.exists(save_dir)){
      dir.create(save_dir)
    }
    # Save model-specific posterior predictive check
    message(paste0('   Writing output for model:\t', model_name, '...'))
    file = file.path(save_dir,
                     paste0('postpred-',
                            participant_id,
                            '_model-',
                            model_name,
                            '.tsv'))
    data.table::fwrite(x = sim,
                       file = file,
                       sep = '\t',
                       na = 'n/a')
  }
  
  # Give message to user
  message('...Done!')
  
}

# Create options to pass to script
option_list = list(
  make_option(c('-p', '--participant_id'),
              type='character',
              default = NULL,
              help = 'ID of participant',
              metavar = 'PARTICIPANT_ID'),
  make_option(c('-t', '--tau'),
              type='numeric',
              default = NULL,
              help = 'Value for fixed tau parameter used in Suprise model. E.g. `0.2`',
              metavar = 'TAU'))

# Run function with command line arguments
Posterior_prediction(participant_id = opt$participant_id,
                     tau = opt$tau)

# Rscript Posterior_prediction.R --participant_id='2HOJBMD' --tau=0.2