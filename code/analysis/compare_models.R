library(data.table)
library(here)
library(magrittr)

base_path = here::here()

# Load pre-written functions
source_path = file.path(base_path, 'code', 'utils',
                        fsep = .Platform$file.sep)
source_files = list.files(source_path, pattern = "[.][rR]$",
                          full.names = TRUE, recursive = TRUE)
invisible(lapply(source_files, function(x) source(x)))

source_files = list(file.path(base_path, 'code', 'RW_nico.R', fsep = .Platform$file.sep),
                    file.path(base_path, 'code', 'PEDLR_nico.R', fsep = .Platform$file.sep),
                    file.path(base_path, 'code', 'LL_nico.R', fsep = .Platform$file.sep),
                    file.path(base_path, 'code', 'model_fitting', 'Fit_Pedlr.R', fsep = .Platform$file.sep))
invisible(lapply(source_files, function(x) source(x)))

data = Load_data() %>%
  .[participant_id == 'IRDNG4E' & run == 1] %>%
  Prepare_data_for_fit(.)

nico_fit = PEDLR_nico(data = data,
                      params = list(alpha0 = 0.2,
                                    alpha1 = 0.7,
                                    temp = 5,
                                    init_values = 50),
                      init_values = 50)
nico_fit$fit = 'nico'
nico_fit$trial = seq(nrow(nico_fit))

chris = Fit_Pedlr(data = data,
                  params.alpha0 = 0.2,
                  params.alpha1 = 0.7,
                  params.temperature = 5,
                  params.reward_space_ub = 100,
                  choice_policy = 'softmax',
                  init_values = c(50,50,50))
chris_fit = data.table(choice = chris$choices$choice,
                       choice_prob = chris$choices$choice_prob,
                       forced_choice = chris$choices$forced_choice,
                       value1 = chris$values$stim_1,
                       value2 = chris$values$stim_2,
                       value3 = chris$values$stim_3,
                       PE = rowMeans(chris$PE, na.rm = TRUE),
                       LR = rowMeans(chris$fPE, na.rm = TRUE))
chris_fit$fit = 'chris'
chris_fit$trial = seq(nrow(chris_fit))

fit = rbind(as.data.table(nico_fit), chris_fit) %>%
  data.table::dcast(., trial ~ fit, value.var = c('choice', 'choice_prob', 'forced_choice', 'value1', 'value2', 'value3', 'PE', 'LR'))

# LL
nico_LL = -sum(log(nico_fit$choice_prob[nico_fit$forced_choice == 0 & !is.na(nico_fit$PE)]))

chris_LL = chris_fit$choice_prob
index_fc = which((data$trial_type == 'forced'))
index_to = which(is.na(data$outcome))
index_excl = unique(sort(c(index_fc, index_to)))
chris_LL = chris_LL[-index_excl]
if(any(is.na(chris_LL))){
  stop('NA in likelihood vector')
}
# Log likelihood
chris_LL = log(chris_LL)
# Sum up log likelihood over all choices
chris_LL = sum(chris_LL)
# Negative log likelihood
chris_LL = -chris_LL