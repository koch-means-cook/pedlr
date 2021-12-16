library(data.table)
library(here)

Get_exclusions = function(){
  
  # Source pre-written functions
  source(file.path(here::here(),
                   'code',
                   'utils',
                   'Load_data.R',
                   fsep = .Platform$file.sep))
  source(file.path(here::here(),
                   'code',
                   'utils',
                   'Get_running_avg.R',
                   fsep = .Platform$file.sep))
  source(file.path(here::here(),
                   'code',
                   'utils',
                   'Add_comp.R',
                   fsep = .Platform$file.sep))
  
  # Load experiment data
  data = Load_data() %>%
    Add_comp(.)
  
  # Allocate data frame holding excluded participants
  excludes = data.table()
  
  # ---
  # Exclusion criterion 1: Errors in forced choices
  # ---
  # Set max allowed percentage of forced choice errors
  max_fce = 25
  # Get correctness in forced choices
  fce = data %>%
    .[trial_type == 'forced',] %>%
    .[, forced_error := error] %>%
    .[, .(n_forced = length(forced),
          n_error = sum(as.numeric(forced_error)),
          group = unique(group)), by = 'participant_id'] %>%
    # Percentage false
    .[, perc_forced_error := round((n_error/n_forced) * 100, 2)] %>%
    .[, count := seq(.N), by = c('n_error', 'group')] %>%
    .[perc_forced_error > max_fce, ]
  
  # Add excludes
  temp = fce[, c('participant_id', 'group')]
  temp$run = NA
  temp$reason = 'Less than 75% accuracy in forced choices'
  excludes = rbind(excludes, temp)
  
  # ---
  # Exclusion criterion 2: Consistently similar estimates between bandit 1 and 3
  # ---
  # Get data of all estimations
  est = data %>%
    .[, ':='(avg_1_running = Get_running_avg(choice_option = option_choice,
                                             choice_outcome = outcome,
                                             stim = 1),
             avg_2_running = Get_running_avg(choice_option = option_choice,
                                             choice_outcome = outcome,
                                             stim = 2),
             avg_3_running = Get_running_avg(choice_option = option_choice,
                                             choice_outcome = outcome,
                                             stim = 3)),
      by = c('participant_id', 'run')] %>%
    .[, forced_rare := as.numeric(as.logical(is_rare) & trial_type == 'forced' & (comp == '1v2' | comp == '2v3'))] %>%
    .[!is.na(est_1_reward),] %>%
    .[, est_trial := seq(.N), by = c('participant_id', 'run')] %>%
    data.table::melt(.,
                     id.vars = c('participant_id',
                                 'group',
                                 'run',
                                 'est_trial',
                                 'forced_rare'),
                     measure.vars = c('est_1_reward',
                                      'est_1_range',
                                      'avg_1_running',
                                      'est_2_reward',
                                      'est_2_range',
                                      'avg_2_running',
                                      'est_3_reward',
                                      'est_3_range',
                                      'avg_3_running')) %>%
    .[, est_stim := substr(variable, 5, 5)] %>%
    .[, type := substr(variable, 7, 9)] %>%
    .[type == 'rew', type := 'reward'] %>%
    .[type == 'ran', type := 'range'] %>%
    .[type == 'run', type := 'r_avg'] %>%
    data.table::dcast(., participant_id + group + run + est_trial + forced_rare + est_stim ~ type,
                      value.var = 'value')
  
  # Compare estimates of bandit 1 and 3 with paired t-test within subject
  est_1v3 = est %>%
    .[est_stim != 2,] %>%
    data.table::dcast(participant_id + group + run + est_trial ~ paste0('est_', est_stim),
                      value.var = 'reward') %>%
    .[, .(mean_1 = mean(est_1),
          mean_3 = mean(est_3),
          # Test for sig difference between estimate of bandit 1 and 3 (across 
          # task versions is okay because of paired)
          statistic = t.test(x = est_1,
                             y = est_3,
                             alternative = 'less',
                             paired = TRUE)$statistic,
          parameter = t.test(x = est_1,
                             y = est_3,
                             alternative = 'less',
                             paired = TRUE)$parameter,
          p.value = round(t.test(x = est_1,
                                 y = est_3,
                                 alternative = 'less',
                                 paired = TRUE)$p.value, 3)),
      by = c('participant_id', 'group')]
  
  # Exclude participants who's estimates of bandit 1 and 3 are not significantly
  # different
  temp = est_1v3[p.value > 0.05, c('participant_id', 'group')]
  temp$run = NA
  temp$reason = 'Consistently similar estimate for bandit 1 and 3'
  excludes = rbind(excludes, temp)
  
  
  # ---
  # Exclusion criterion 3: Chance-level performance for bandit 1 vs. bandit 3
  # ---
  # Add accuracy of 1v3 choices across runs
  perf_1v3 = data %>%
    .[comp == '1v3' & trial_type == 'choice',] %>%
    .[, trial := seq(.N),
      by = c('participant_id', 'group')] %>%
    .[, corr_outcome := max(c(reward_stim_1, reward_stim_2)),
      by = c('participant_id', 'group', 'trial')] %>%
    .[, corr := corr_outcome == outcome,
      by = c('participant_id', 'group', 'trial')] %>%
    # Get accuracy for valid 1v3 trials (timeouts and forced choices excluded)
    .[!is.na(corr), .(corr_1v3 = sum(corr) / length(corr)),
      by = c('participant_id', 'group')]
  
  # Exclude participants with less than 55% overall accuracy (probably guessing)
  temp = perf_1v3[corr_1v3 < 0.55, c('participant_id', 'group')]
  temp$run = NA
  temp$reason = 'Less than 55% overall accuracy chosing between bandit 1 and 3'
  excludes = rbind(excludes, temp)

  # Prepare and return
  excludes = excludes %>%
    .[, n_criteria := .N,
      by = participant_id] %>%
    .[order(rank(-n_criteria), group, participant_id), ]
  return(excludes)
  
  
}