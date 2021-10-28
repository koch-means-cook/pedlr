library(data.table)

Get_best_fit = function(data){
  result = data.table::setDT(data) %>%
    # Enumerate different fitting attempts
    .[, fit := seq(.N),
      by = c('participant_id', 'task_version', 'model','para')] %>%
    # Add number of parameters
    .[, n_para := length(para),
      by = c('participant_id', 'task_version', 'model', 'fit')] %>%
    # Cast into long format (one line per fit attempt)
    data.table::dcast(., fit + participant_id + task_version + name_design_r1 + name_design_r2 + model + n_para + second_ll ~ para,
                      value.var = c('first_x0', 'second_solution')) %>%
    # Calculate AIC = 2k + 2(-LL)
    .[, ':='(AIC = 2*n_para + 2*second_ll)]
  
  # Get best fit for each model by minimal AIC
  result = result[, .SD[which.min(AIC)],
                  by = c('participant_id', 'task_version', 'model')]
  
  return(result)
}