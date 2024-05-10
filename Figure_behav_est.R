library(data.table)
library(here)
library(magrittr)
library(ggplot2)
library(viridis)
library(binhf)
library(pwr)
library(knitr)
library(kableExtra)
library(cowplot)
library(pdftools)
library(emojifont)
library(magick)
library(patchwork)
library(sdamr)

Figure_est_diff_deviation = function(){
  
  # Get directory of repository
  base_path = here::here()
  
  # Load pre-written functions
  source_path = file.path(base_path, 'code', 'utils',
                          fsep = .Platform$file.sep)
  source_files = list.files(source_path, pattern = "[.][rR]$",
                            full.names = TRUE, recursive = TRUE)
  invisible(lapply(source_files, function(x) source(x)))
  
  # Get color schemes
  custom_guides = Get_plot_guides()
  
  # Load data
  data = Load_data() %>%
    Apply_exclusion_criteria(.) %>%
    Add_comp(.) %>%
    .[, run := as.factor(run)]
  
  # get number of participants (to adjust figure height)
  n_participants = length(unique(data$participant_id))
  
  # State melt columns (to align data types to avoid warnings)
  measure_cols = c('est_1_reward',
                   'est_1_range',
                   'avg_1_running',
                   'est_2_reward',
                   'est_2_range',
                   'avg_2_running',
                   'est_3_reward',
                   'est_3_range',
                   'avg_3_running')
  
  
  check_est_dist = data %>%
    # Get running average of chosen rewards
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
    # Unify data types of measure columns
    .[, (measure_cols) := lapply(.SD, as.double), .SDcols = measure_cols] %>%
    # Melt to change variable names of estimation variables
    data.table::melt(id.vars = c('participant_id',
                                 'group',
                                 'run',
                                 'est_trial',
                                 'forced_rare'),
                     measure.vars = measure_cols) %>%
    .[, est_stim := substr(variable, 5, 5)] %>%
    .[, type := substr(variable, 7, 9)] %>%
    .[type == 'rew', type := 'reward'] %>%
    .[type == 'ran', type := 'range'] %>%
    # Put back into long format to calculate distance between estimates
    data.table::dcast(., participant_id + group + run + est_trial + forced_rare ~ paste0(type, '_', est_stim),
                      value.var = 'value') %>%
    # Get distance between critical estimates
    .[, ':='(dist_2m1 = reward_2 - reward_1,
             dist_3m2 = reward_3 - reward_2)] %>%
    # Add first and second half of run variable
    .[, half := rep(x = c(1,2), each = (max(est_trial)/2)),
      by = c('participant_id', 'group', 'run')] %>%
    .[, half := as.factor(half)] %>%
    # Get mean of distance between estimates (separately for each half)
    .[, .(mean_dist_2m1 = mean(dist_2m1, na.rm = TRUE),
          mean_dist_3m2 = mean(dist_3m2, na.rm = TRUE)),
      by = c('participant_id', 'group', 'run', 'half')] %>%
    # Melt for data analysis
    data.table::melt(id.vars = c('participant_id', 'group', 'run', 'half'),
                     measure.vars = c('mean_dist_2m1', 'mean_dist_3m2'),
                     variable.name = 'comp')
  
  # take only means of second half of experiment (to avoid uninformed estimates at start)
  check_est_dist_sh = check_est_dist[half == '2']
  
  data_plot = Prepare_data_for_plot(check_est_dist_sh)
  # Transform levels for better plot labels
  data_plot$comp = factor(data_plot$comp)
  levels(data_plot$comp) = c('LM', 'MH')
  
  data_plot_mean = data_plot %>%
    .[!is.na(value), .(value = mean(value),
                       sd = sd(value),
                       n = .N,
                       sem = sd(value) / sqrt(.N)),
      by = c('group', 'half', 'comp')]
  # Transform levels for better plot labels
  data_plot_mean$comp = factor(data_plot_mean$comp)
  levels(data_plot_mean$comp) = c('LM', 'MH')
  
  dodge_width = 0.3
  
  # Value to shift y axis
  y_axis_shift = 1/6*100
  
  # 4x3
  p_est_diff_deviation_sh = ggplot(data = data_plot,
                         aes(x = comp,
                             y = value - y_axis_shift,
                             color = group,
                             fill = group)) +
    geom_hline(yintercept = 1/6 * 100 - y_axis_shift,
               linetype = 'dashed') +
    geom_point(size = 0.5,
               position = position_jitterdodge(dodge.width = dodge_width,
                                               jitter.width = 0.1,
                                               jitter.height = 0,
                                               seed = 666),
               show.legend = FALSE) +
    geom_col(data = data_plot_mean,
             color = 'black',
             size = 0.5,
             width = dodge_width,
             inherit.aes = TRUE,
             position = position_dodge(width = dodge_width)) +
    geom_errorbar(data = data_plot_mean,
                  aes(ymin = value - y_axis_shift - sem,
                      ymax = value - y_axis_shift + sem),
                  width = 0.1,
                  color = 'black',
                  position = position_dodge(width = dodge_width)) +
    # Geom to demonstrate shift did not change data
    # geom_text(data = data_plot_mean,
    #            aes(label = round(value, 2)),
    #           size = 2,
    #           color = 'black',
    #           position = position_dodge(width = dodge_width)) +
    scale_color_manual(values = custom_guides) +
    scale_fill_manual(values = custom_guides) +
    scale_y_continuous(breaks = round(c(-15 - y_axis_shift, 0 - y_axis_shift, 0, 30 - y_axis_shift, 45 - y_axis_shift), 2),
                       labels = round(c(-15, 0, y_axis_shift, 30, 45), 2)) +
    labs(x = 'Comparison',
         y = 'Average distance between estimates')
  p_est_diff_deviation_sh = Neurocodify_plot(p_est_diff_deviation_sh) +
    theme(panel.grid = element_blank(),
          legend.position = c(0.5,0.1),
          legend.background = element_rect(fill = 'transparent'),
          legend.title = element_blank(),
          legend.key.size = unit(10, 'pt'),
          legend.key = element_rect(size = 0.1),
          legend.text = element_text(size = 8, margin = margin(2,0,2,0, 'pt')),
          legend.justification = c(0.5,0.5),
          legend.direction = 'vertical',
          legend.margin = margin(0,0,0,0,'pt'))
  
  return(p_est_diff_deviation_sh)
  
}

Figure_behav_est = function(){
  
  p_behav_est = cowplot::plot_grid()
  
  return(p_behav_est)
  
}

