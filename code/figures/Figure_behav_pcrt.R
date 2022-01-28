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

Figure_pc_overall = function(){
  
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
  
  # Percentage of correct choices
  check_noc = data %>%
    .[, trial := seq(.N),
      by = c('participant_id', 'run')] %>%
    .[trial_type == 'choice',] %>%
    .[, correct_choice := if(option_left > option_right) 'left' else 'right',
      by = c('participant_id', 'run', 'trial')] %>%
    .[, correct := correct_choice == choice] %>%
    # Get percentage of correct choices (exclude timeouts from overall trials)
    .[, .(perc_correct = sum(as.numeric(correct), na.rm = TRUE) / length(which(!is.na(as.numeric(correct))))),
      by = c('participant_id', 'group', 'run', 'comp')]
  
  # Prepare plotting data
  data_plot = Prepare_data_for_plot(check_noc)
  
  # Convert to percentages
  data_plot$perc_correct = 100*data_plot$perc_correct
  
  # Rename plotted factor levels
  levels(data_plot$comp) = c('LM',
                             'MH',
                             'LH')
  levels(data_plot$run) = c('Run 1',
                            'Run 2')
  
  
  # Plot (size 3x3 inches)
  p_pc_overall = ggplot(data = data_plot,
                        aes(x = comp,
                            y = perc_correct,
                            color = group,
                            fill = group)) +
    geom_point(size = 0.4,
               position = position_jitterdodge(dodge.width = 0.6,
                                               jitter.width = 0.2,
                                               jitter.height = 0,
                                               seed = 666)) +
    geom_boxplot(width = 0.4,
                 color = 'black',
                 outlier.shape = NA,
                 position = position_dodge(width = 0.6)) +
    stat_summary(fun = 'mean',
                 geom = 'point',
                 na.rm = TRUE,
                 shape = 23,
                 fill = 'white',
                 size = 1.5,
                 stroke = 0.5,
                 position = position_dodge(width = 0.6)) +
    scale_color_manual(values = custom_guides) +
    scale_fill_manual(values = custom_guides) +
    facet_grid( ~ run) +
    scale_x_discrete(labels = c('LM\n', 'MH\n', 'LH\n')) +
    labs(x = 'Presented combination of arms',
         y = 'Optimal choices (in %)') +
    theme(strip.text.y = element_text(angle = 0))
  
  # Additional design
  p_pc_overall = Neurocodify_plot(p_pc_overall) +
    theme(panel.grid = element_blank(),
          legend.position = c(0.05,0.1),
          legend.background = element_rect(fill = '#f5f5f5'),
          legend.title = element_blank(),
          legend.key.size = unit(5, 'pt'),
          legend.text = element_text(size = 8),
          legend.justification = c(0,0),
          legend.direction = 'horizontal',
          legend.margin = margin(0,0,0,0,'pt'))
  
  return(p_pc_overall)
  
}

Figure_behav_rt_diff = function(){
  
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
  
  # Get RT (restricted to critical comparisons 1v2 & 2v3)
  check_rt_crit = data %>%
    .[, trial := seq(.N),
      by = c('participant_id', 'run')] %>%
    .[timeout == FALSE, ] %>%
    .[, log_rt := log(rt)] %>%
    data.table::melt(.,
                     id.vars = c('participant_id',
                                 'group',
                                 'run',
                                 'trial',
                                 'trial_type',
                                 'comp'),
                     measure.vars = c('log_rt')) %>%
    .[trial_type == 'choice' & comp %in% c('1v2', '2v3')]
  
  # Get mean log(RT) of critical comparisons
  check_rt_mean_crit = check_rt_crit %>%
    .[, .(mean_log_rt = mean(value)),
      by = c('participant_id',
             'group',
             'run',
             'comp')]
  
  # Get difference 1v2 - 2v3 (within-subject comparison of which )
  check_rt_mean_crit_diff = check_rt_mean_crit %>%
    data.table::dcast(participant_id + group + run ~ paste0('mean_log_rt_', comp),
                      value.var = c('mean_log_rt')) %>%
    .[, mean_log_rt_1v2m2v3 := mean_log_rt_1v2 - mean_log_rt_2v3]
  
  # Prepare data for plot
  data_plot = Prepare_data_for_plot(check_rt_mean_crit_diff)
  
  # Get color schemes
  custom_guides = Get_plot_guides()
  
  # Plot (2x3 inches)
  p_rt_diff = ggplot(data = data_plot,
                     aes(x = group,
                         y = mean_log_rt_1v2m2v3,
                         color = group,
                         fill = group)) +
    geom_hline(yintercept = 0,
               size = 0.5,
               linetype = 'solid') +
    geom_point(size = 0.5,
               position = sdamr::position_jitternudge(jitter.width = 0.08,
                                                      jitter.height = 0,
                                                      nudge.x = -0.1)) +
    geom_boxplot(width = 0.2,
                 color = 'black',
                 outlier.shape = NA,
                 position = position_nudge(x = -0.1)) +
    stat_summary(fun = 'mean',
                 geom = 'point',
                 na.rm = TRUE,
                 shape = 23,
                 inherit.aes = TRUE,
                 fill = 'white',
                 size = 1.5,
                 stroke = 0.5,
                 position = position_nudge(x = -0.1)) +
    gghalves::geom_half_violin(side = 1,
                               color = NA,
                               width = 0.5,
                               alpha = 0.5,
                               position = position_nudge(x = 0.1),
                               show.legend = FALSE) +
    scale_color_manual(values = custom_guides) +
    scale_fill_manual(values = custom_guides) +
    labs(x = 'Age group',
         y = "Difference in mean log(RT)\n(LM - MH)")
  p_rt_diff = Neurocodify_plot(p_rt_diff) +
    theme(panel.grid = element_blank(),
          legend.position = 'None')
  
  return(p_rt_diff)
  
}

Figure_behav_pcrt = function(){
  
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
  
  # Get plots
  p_pc_overall = Figure_pc_overall() +
    theme(plot.margin = margin(0,10,0,0, 'pt'))
  p_rt_diff = Figure_behav_rt_diff() +
    theme(plot.margin = margin(0,0,0,10, 'pt'))
  
  # Combine plots
  behav_pcrt = cowplot::plot_grid(p_pc_overall, p_rt_diff,
                                  rel_widths = c(2,1),
                                  rel_heights = c(1,1),
                                  axis = 'tblr',
                                  align = 'hv',
                                  labels = c('A','B'))
  
  return(behav_pcrt)
  
}

