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

Figure_pc_learn = function(){
  
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
  
  data = Load_data() %>%
    Apply_exclusion_criteria(., choice_based_exclusion = TRUE) %>%
    Add_comp(.) %>%
    .[, run := as.factor(run)] %>%
    # Get trial counter
    .[, trial := seq(.N),
      by = c('participant_id', 'run')] %>%
    # Bin trials
    .[, bin := rep(seq(6), each = .N/length(seq(6))),
      by = c('participant_id', 'run')] %>%
    .[, bin := factor(bin)] %>%
    # Focus on free choice trials
    .[trial_type == 'choice',] %>%
    # Get if trial was answered correctly
    .[, correct_choice := if(option_left > option_right) 'left' else 'right',
      by = c('participant_id', 'run', 'trial')] %>%
    .[, correct := correct_choice == choice] %>%
    # Get percentage of correct choices (exclude timeouts from overall trials)
    .[, .(perc_correct = mean(correct, na.rm = TRUE)),
      by = c('participant_id', 'group', 'run', 'bin', 'comp')] %>%
    # Average across runs
    .[, .(perc_correct = mean(perc_correct, na.rm = TRUE)),
      by = c('participant_id', 'group', 'bin', 'comp')] %>%
    Prepare_data_for_plot(.)
  
  # level and rename bins
  data$bin = factor(data$bin)
  levels(data$bin) = c('1-40', '41-80', '81-120', '121-160', '161-200', '201-240')
  # level and rename comps
  data$comp = factor(data$comp)
  levels(data$comp) = c('Low-Mid', 'Mid-High', 'Low-High')
  
  # Sort age levels
  data$group = factor(data$group, levels = c('Older\nadults','Younger\nadults'))
  
  data_mean = data %>%
    .[, .(perc_correct = mean(perc_correct),
          sem = sd(perc_correct) / sqrt(.N)),
      by = c('bin', 'comp')] %>%
    .[, ':='(ymin = perc_correct - sem,
             ymax = perc_correct + sem)]
  
  dodge_width = 0.3
  
  p = ggplot(data = data,
         aes(x = bin,
             y = perc_correct,
             color = comp,
             fill = comp)) +
    # geom_boxplot(outlier.shape = NA,
    #              position = position_dodge(width = dodge_width),
    #              width = 0.8*dodge_width,
    #              color = 'black',
    #              alpha = 0.2,
    #              size = 0.1,
    #              show.legend = FALSE) +
    geom_hline(yintercept = 0.5,
               size = 0.5,
               linetype = 'dashed') +
    geom_line(data = Prepare_data_for_plot(data_mean),
              aes(group = comp),
              size = 0.3,
              position = position_dodge(width = dodge_width)) +
    geom_errorbar(data = Prepare_data_for_plot(data_mean),
                  aes(ymin = ymin,
                      ymax = ymax),
                  color = 'black',
                  width = 0.8 * dodge_width,
                  position = position_dodge(width = dodge_width),
                  size = 0.2) +
    # geom_point(data = Prepare_data_for_plot(data_mean),
    #            shape = 23,
    #            fill = 'white',
    #            size = 0.5,
    #            stroke = 0.5,
    #            position = position_dodge(width = dodge_width)) +
    labs(x = 'Trials',
         y = 'Percentage correct') +
    scale_y_continuous(limits = c(0.4,1),
                       breaks = seq(0.4,1,by = 0.1)) +
    viridis::scale_color_viridis(option = 'D', discrete = TRUE) +
    viridis::scale_fill_viridis(option = 'D', discrete = TRUE)
  
  p = Neurocodify_plot(p) +
    theme(panel.grid = element_blank(),
          axis.title.x = element_text(size = 10,
                                      face = 'bold',
                                      margin = margin(10,0,0,0,'pt')),
          axis.title.y = element_text(size = 10,
                                      face = 'bold',
                                      margin = margin(0,10,0,0,'pt')),
          legend.title = element_blank(),
          legend.direction = 'horizontal',
          legend.position = c(0.5,0.1),
          #legend.key.size = unit(5, 'pt'),
          legend.key = element_rect(fill = 'transparent'),
          legend.text = element_text(size = 8),
          legend.background = element_rect(fill = 'transparent'),
          plot.margin = margin(0,5,5,0,'pt'))
  
  return(p)
  
}

Figure_pc_run = function(){
  
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
  
  data = Load_data() %>%
    Apply_exclusion_criteria(., choice_based_exclusion = TRUE) %>%
    Add_comp(.) %>%
    .[, run := as.factor(run)] %>%
    # Get trial counter
    .[, trial := seq(.N),
      by = c('participant_id', 'run')] %>%
    # Focus on free choice trials
    .[trial_type == 'choice',] %>%
    # Get if trial was answered correctly
    .[, correct_choice := if(option_left > option_right) 'left' else 'right',
      by = c('participant_id', 'run', 'trial')] %>%
    .[, correct := correct_choice == choice] %>%
    # Get percentage of correct choices (exclude timeouts from overall trials)
    .[, .(perc_correct = mean(correct, na.rm = TRUE)),
      by = c('participant_id', 'group', 'run')] %>%
    Prepare_data_for_plot(.)
  
  # Sort age levels
  data$group = factor(data$group, levels = c('Older\nadults','Younger\nadults'))
  
  dodge_width = 0.5
  
  p = ggplot(data = data,
             aes(x = run,
                 y = perc_correct)) +
    geom_point(size = 0.4,
               position = position_jitter(width = 0.1,
                                          height = 0,
                                          seed = 666)) +
    geom_boxplot(width = 0.4,
                 color = 'black',
                 outlier.shape = NA) +
    stat_summary(fun = 'mean',
                 geom = 'line',
                 group = 'run',
                 na.rm = TRUE,
                 size = 0.5) +
    stat_summary(fun = 'mean',
                 geom = 'point',
                 na.rm = TRUE,
                 shape = 23,
                 fill = 'white',
                 size = 2,
                 stroke = 1) +
    labs(x = 'Run',
         y = 'Percentage correct')
  
  p = Neurocodify_plot(p) +
    theme(panel.grid = element_blank(),
          axis.title.x = element_text(size = 10,
                                      face = 'bold',
                                      margin = margin(10,0,0,0,'pt')),
          axis.title.y = element_text(size = 10,
                                      face = 'bold',
                                      margin = margin(0,10,0,0,'pt')),
          legend.title = element_blank(),
          legend.direction = 'vertical',
          legend.position = c(0.5,0.22),
          legend.key = element_rect(fill = 'transparent'),
          legend.background = element_rect(fill = 'transparent'),
          plot.margin = margin(0,0,5,5,'pt'))
  
  return(p)
  
}

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
    Apply_exclusion_criteria(., choice_based_exclusion = TRUE) %>%
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
      by = c('participant_id', 'group', 'comp')]
  
  # Prepare plotting data
  data_plot = Prepare_data_for_plot(check_noc)
  # Sort age levels
  data_plot$group = factor(data_plot$group, levels = c('Older\nadults','Younger\nadults'))
  
  # Rename plotted factor levels
  levels(data_plot$comp) = c('LM',
                             'MH',
                             'LH')
  
  dodge_width = 0.3
  
  # Plot (size 3x3 inches)
  p_pc_overall = ggplot(data = data_plot,
                        aes(x = comp,
                            y = perc_correct,
                            color = group,
                            fill = group)) +
    geom_point(size = 0.4,
               position = position_jitterdodge(dodge.width = dodge_width,
                                               jitter.width = dodge_width/2,
                                               jitter.height = 0,
                                               seed = 666)) +
    geom_boxplot(color = 'black',
                 outlier.shape = NA,
                 width = dodge_width,
                 position = position_dodge(width = dodge_width)) +
    stat_summary(fun = 'mean',
                 geom = 'point',
                 na.rm = TRUE,
                 shape = 23,
                 fill = 'white',
                 size = 2,
                 stroke = 0.5,
                 position = position_dodge(width = dodge_width),
                 show.legend = FALSE) +
    scale_color_manual(values = custom_guides) +
    scale_fill_manual(values = custom_guides) +
    scale_x_discrete(labels = c('Low\n-\nMid', 'Mid\n-\nHigh', 'Low\n-\nHigh')) +
    labs(x = 'Presented combination of bandits',
         y = 'Percentage correct') +
    theme(strip.text.y = element_text(angle = 0),
          axis.text.x = element_text(lineheight = 0.7),
          axis.title.x = element_text(size = 10,
                                      face = 'bold',
                                      margin = margin(10,0,0,0,'pt')),
          axis.title.y = element_text(size = 10,
                                      face = 'bold',
                                      margin = margin(0,10,0,0,'pt')))
  
  # Additional design
  p_pc_overall = Neurocodify_plot(p_pc_overall) +
    theme(panel.grid = element_blank(),
          legend.position = 'top',
          legend.title = element_blank(),
          legend.key.size = unit(10, 'pt'),
          legend.key = element_rect(fill = 'transparent'),
          legend.text = element_text(size = 8),
          legend.justification = c(0.5,0.5),
          legend.margin = margin(0,0,0,0,'pt'),
          legend.background = element_rect(fill = 'transparent'),
          plot.margin = margin(5,5,0,0,'pt'))
  
  return(p_pc_overall)
  
}

Figure_pc_diff = function(){
  
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
  
  file = file.path(base_path,
                   'derivatives',
                   'figures',
                   'f_pc_diff.tsv',
                   fsep = .Platform$file.sep)
  data = data.table::fread(file, sep = '\t', na.strings = 'n/a') %>%
    Prepare_data_for_plot(.)
  # Sort age levels
  data$group = factor(data$group, levels = c('Older\nadults','Younger\nadults'))
  
  nudge_with = 0.1
  
  p = ggplot(data = data,
             aes(x = group,
                 y = mean_bandit_effect,
                 color = group,
                 fill = group)) +
    geom_hline(yintercept = 0,
               size = 0.5) +
    geom_point(size = 0.5,
               position = sdamr::position_jitternudge(nudge.x = -nudge_with,
                                                      jitter.width = 0.05,
                                                      jitter.height = 0,
                                                      seed = 666),
               alpha = 0.5) +
    gghalves::geom_half_violin(side = 'r',
                               width = 0.5,
                               position = position_nudge(x = nudge_with),
                               alpha = 0.7,
                               color = NA) +
    geom_boxplot(outlier.shape = NA,
                 color = 'black',
                 width = 2*nudge_with,
                 position = position_nudge(x = -nudge_with)) +
    stat_summary(fun = 'mean',
                 geom = 'point',
                 na.rm = TRUE,
                 shape = 23,
                 inherit.aes = TRUE,
                 fill = 'white',
                 size = 2,
                 stroke = 0.5,
                 show.legend = FALSE,
                 position = position_nudge(x = -nudge_with)) +
    scale_color_manual(values = custom_guides) +
    scale_fill_manual(values = custom_guides) +
    labs(x = 'Age group',
         y = 'Relative error rate\nin Low-Mid\n(vs. Mid-High)')
  
  p = Neurocodify_plot(p) +
    theme(legend.position = 'none',
          axis.title.x = element_text(size = 10,
                                      face = 'bold',
                                      margin = margin(10,0,0,0,'pt')),
          axis.title.y = element_text(size = 9,
                                      face = 'bold',
                                      margin = margin(0,10,0,10,'pt')),
          panel.grid = element_blank())
  
  return(p)
  
  
}

Figure_rt_overall = function(){
  
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
  
  file = file.path(base_path,
                   'derivatives',
                   'figures',
                   'f_rt.tsv',
                   fsep = .Platform$file.sep)
  data = data.table::fread(file, sep = '\t', na.strings = 'n/a') %>%
    Prepare_data_for_plot(.)
  data$comp = factor(data$comp, levels = c('1v2', '2v3', '1v3'))
  levels(data$comp) = c('Low\n-\nMid',
                        'Mid\n-\nHigh',
                        'Low\n-\nHigh')
  
  # Sort age levels
  data$group = factor(data$group, levels = c('Older\nadults','Younger\nadults'))
  
  dodge_width = 0.3
  
  p = ggplot(data = data,
                     aes(x = comp,
                         y = mean_value,
                         color = group,
                         fill = group)) +
    geom_point(size = 0.5,
               position = position_jitterdodge(dodge.width = dodge_width,
                                               jitter.width = dodge_width/4,
                                               jitter.height = 0,
                                               seed = 666)) +
    geom_boxplot(outlier.shape = NA,
                 color = 'black',
                 width = dodge_width,
                 position = position_dodge(width = dodge_width)) +
    stat_summary(fun = 'mean',
                 geom = 'point',
                 na.rm = TRUE,
                 shape = 23,
                 inherit.aes = TRUE,
                 fill = 'white',
                 size = 2,
                 stroke = 0.5,
                 position = position_dodge(width = dodge_width),
                 show.legend = FALSE) +
    scale_color_manual(values = custom_guides) +
    scale_fill_manual(values = custom_guides) +
    labs(y = 'Mean log(RT)',
         x = 'Presented combination of bandits')
  p = Neurocodify_plot(p) +
    theme(axis.title.x = element_text(size = 10,
                                      face = 'bold',
                                      margin = margin(10,0,0,0,'pt')),
          axis.title.y = element_text(size = 10,
                                      face = 'bold',
                                      margin = margin(0,10,0,0,'pt')),
          axis.text.x = element_text(lineheight = 0.7),
          panel.grid = element_blank(),
          plot.margin = margin(5,0,0,0,'pt'),
          legend.position = 'top',
          legend.title = element_blank(),
          legend.key.size = unit(10, 'pt'),
          legend.key = element_rect(fill = 'transparent'),
          legend.text = element_text(size = 8),
          legend.justification = c(0.5,0.5),
          legend.margin = margin(0,0,0,0,'pt'),
          legend.background = element_rect(fill = 'transparent'))
  
  return(p)
  
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
    Apply_exclusion_criteria(., choice_based_exclusion = TRUE) %>%
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
  # Sort age levels
  data_plot$group = factor(data_plot$group, levels = c('Older\nadults','Younger\nadults'))
  
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
                 size = 2,
                 stroke = 0.5,
                 position = position_nudge(x = -0.1)) +
    gghalves::geom_half_violin(side = 1,
                               color = NA,
                               width = 0.5,
                               alpha = 0.7,
                               position = position_nudge(x = 0.1),
                               show.legend = FALSE) +
    scale_color_manual(values = custom_guides) +
    scale_fill_manual(values = custom_guides) +
    labs(x = 'Age group',
         y = "Relative mean log(RT)\nin Low-Mid\n(vs. Mid-High)")
  p_rt_diff = Neurocodify_plot(p_rt_diff) +
    theme(panel.grid = element_blank(),
          legend.position = 'None',
          axis.title.x = element_text(size = 10,
                                      face = 'bold',
                                      margin = margin(10,0,0,0,'pt')),
          axis.title.y = element_text(size = 9,
                                      face = 'bold',
                                      margin = margin(0,10,0,10,'pt')),
          plot.margin = margin(5,0,0,0,'pt'))
  
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
  p_pc_learning = Figure_pc_learn() +
    theme(plot.margin = margin(0,5,10,0,'pt'))
  p_pc_run = Figure_pc_run() +
    theme(plot.margin = margin(0,0,10,5,'pt'))
  p_pc_overall = Figure_pc_overall() +
    theme(plot.margin = margin(10,0,10,0,'pt'))
  p_pc_diff = Figure_pc_diff() +
    theme(plot.margin = margin(10,0,10,0,'pt'))
  p_rt_overall = Figure_rt_overall() +
    theme(plot.margin = margin(10,0,0,0,'pt'))
  p_rt_diff = Figure_behav_rt_diff() +
    theme(plot.margin = margin(10,0,0,0,'pt'))
  
  # Combine plots
  behav_pcrt = cowplot::plot_grid(p_pc_learning, p_pc_run,
                                  p_pc_overall, p_pc_diff,
                                  p_rt_overall, p_rt_diff,
                                  ncol = 2,
                                  nrow = 3,
                                  rel_widths = c(3,2),
                                  rel_heights = c(1,1),
                                  axis = 'tb',
                                  align = 'h',
                                  labels = c('A','B', 'C', 'D', 'E', 'F'),
                                  label_x = 0,
                                  label_y = 1)
  
  return(behav_pcrt)
  
}

