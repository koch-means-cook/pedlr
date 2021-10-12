library(data.table)
library(here)

source(file.path(here::here(), 'code', 'model_fitting', 'Fit_model.R',
                 fsep = .Platform$file.sep))
source(file.path(here::here(), 'code', 'utils', 'Prepare_data_for_fit.R',
                 fsep = .Platform$file.sep))

# input_path = '/Users/koch/Docs/pedlr/data/158HUXE_exp_data.tsv'
# model = 'Rw'
# start_values = c(0.5,7)
# lb = c(0,1)
# ub = c(1,10)

Fit_model_wrapper = function(input_path,
                             output_path,
                             model,
                             start_values,
                             lb,
                             ub,
                             random_start_values){
  
  # Set parameter names for model
  if(model == 'Rw'){
    p_names = c('alpha', 'temperature')
  } else if(model == 'Pedlr'){
    p_names = c('alpha0', 'alpha1', 'temperature')
  } else if(model == 'Pedlr_interdep'){
    p_names = c('alpha0', 'alpha1', 'interdep', 'temperature')
  }
  
  # Load data
  data = data.table::fread(input_path,
                           sep = '\t',
                           header = TRUE,
                           na.strings = 'n/a') %>%
    Prepare_data_for_fit(.)
  
  output = data.table()
  
  # Separate fits for runs
  for(i_version in unique(data$task_version)){
    fit_data = data[task_version == i_version]
    result = Fit_model(data = fit_data,
                       model = model,
                       start_values = start_values,
                       lb = lb,
                       ub = ub)
    result = as.data.table(result) %>%
      .[, ':='(para = p_names,
               participant_id = unique(data$participant_id),
               task_version = i_version,
               name_design_r1 = unique(data$name_design_r1),
               name_design_r2 = unique(data$name_design_r2))] %>%
      .[, data.table::setcolorder(., c("participant_id",
                                       "task_version",
                                       "name_design_r1",
                                       "name_design_r2",
                                       "para"))]
    output = rbind(output, result)
    
  }
  
  
}