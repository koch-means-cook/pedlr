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



Figure_behav_rie = function(){
  
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
  file = file.path(base_path,
                   'derivatives',
                   'figures',
                   'f_rie.tsv',
                   fsep = .Platform$file.sep)
  data_plot = data.table::fread(file,
                           sep = '\t',
                           na.strings = 'n/a')
  file = file.path(base_path,
                   'derivatives',
                   'figures',
                   'f_rie_mean.tsv',
                   fsep = .Platform$file.sep)
  data_plot_mean = data.table::fread(file,
                           sep = '\t',
                           na.strings = 'n/a')
  
  dodge_width = 0.4
  
  p = ggplot(data = data_plot[window_relative %in% c('-1', '1', '2')],
             aes(x = window_relative,
                 y = accuracy,
                 fill = group,
                 color = group,
                 group = interaction(window_relative, group))) +
    geom_point(position = position_jitterdodge(dodge.width = dodge_width,
                                               jitter.width = 0.1,
                                               jitter.height = 0,
                                               seed = 666),
               size = 0.5,
               alpha = 0.5) +
    geom_vline(xintercept = 0,
               linetype = 'dashed') +
    geom_path(data = data_plot_mean[window_relative %in% c('-1', '1', '2')],
              size = 1,
              position = position_dodge(dodge_width),
              aes(group = group)) +
    geom_errorbar(data = data_plot_mean[window_relative %in% c('-1', '1', '2')],
                  aes(x = window_relative,
                      y = accuracy,
                      ymin = accuracy - sem_accuracy,
                      ymax = accuracy + sem_accuracy),
                  size = 0.5,
                  width = dodge_width/2,
                  position = position_dodge(dodge_width),
                  color = 'black') +
    geom_point(data = data_plot_mean[window_relative %in% c('-1', '1', '2')],
               aes(x = window_relative,
                   y = accuracy),
               size = 2.5,
               position = position_dodge(dodge_width),
               shape = 21,
               color = 'black') +
    scale_fill_manual(values = custom_guides) +
    scale_color_manual(values = custom_guides) +
    scale_x_continuous(breaks = seq(min(data_plot$window_relative),
                                    max(data_plot$window_relative))) +
    scale_y_continuous(limits = c(0,1)) +
    ggtext::geom_richtext(data = data_plot[window_relative %in% c('-1', '1', '2')][1],
                          x = 0,
                          y = 0.3,
                          label = 'Extreme outcome<br>in Mid bandit',
                          fill = 'white',
                          color = 'black',
                          angle = 90,
                          size = 3.5,
                          hjust = 0.5) +
    labs(x = 'Relative position of Low-Mid trial',
         y = 'Proportion of Mid choices\nin Low-Mid trials') +
    theme(legend.position = 'none',
          axis.text = element_text(size = 12),
          axis.title.x = element_text(size = 15,
                                      face = 'bold',
                                      margin = margin(10,0,0,0,'pt')),
          axis.title.y = element_text(size = 15,
                                      face = 'bold',
                                      margin = margin(0,10,0,0,'pt')))
  
  p = Neurocodify_plot(p) +
    theme(panel.grid = element_blank(),
          plot.margin = margin(0,10,0,0, 'pt'))
  
  return(p)
  
}

Figure_behav_ric = function(){
  
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
  file = file.path(base_path,
                   'derivatives',
                   'figures',
                   'f_ric.tsv',
                   fsep = .Platform$file.sep)
  data_plot = data.table::fread(file,
                                sep = '\t',
                                na.strings = 'n/a')
  file = file.path(base_path,
                   'derivatives',
                   'figures',
                   'f_ric_mean.tsv',
                   fsep = .Platform$file.sep)
  data_plot_mean = data.table::fread(file,
                                     sep = '\t',
                                     na.strings = 'n/a')
  
  dodge_width = 0.4
  
  p = ggplot(data = data_plot[window_relative %in% c('-1', '1', '2')],
             aes(x = window_relative,
                 y = accuracy,
                 fill = group,
                 color = group,
                 group = interaction(window_relative, group))) +
    geom_point(position = position_jitterdodge(dodge.width = dodge_width,
                                               jitter.width = 0.1,
                                               jitter.height = 0,
                                               seed = 666),
               size = 0.5,
               alpha = 0.5) +
    geom_vline(xintercept = 0,
               linetype = 'dashed') +
    geom_path(data = data_plot_mean[window_relative %in% c('-1', '1', '2')],
              size = 1,
              position = position_dodge(dodge_width),
              aes(group = group)) +
    geom_errorbar(data = data_plot_mean[window_relative %in% c('-1', '1', '2')],
                  aes(x = window_relative,
                      y = accuracy,
                      ymin = accuracy - sem_accuracy,
                      ymax = accuracy + sem_accuracy),
                  size = 0.5,
                  width = dodge_width/2,
                  position = position_dodge(dodge_width),
                  color = 'black') +
    geom_point(data = data_plot_mean[window_relative %in% c('-1', '1', '2')],
               aes(x = window_relative,
                   y = accuracy),
               size = 3,
               position = position_dodge(dodge_width),
               shape = 21,
               color = 'black') +
    scale_fill_manual(values = custom_guides) +
    scale_color_manual(values = custom_guides) +
    scale_x_continuous(breaks = seq(min(data_plot$window_relative),
                                    max(data_plot$window_relative))) +
    scale_y_continuous(limits = c(0,1)) +
    ggtext::geom_richtext(data = data_plot[window_relative %in% c('-1', '1', '2')][1],
                          x = 0,
                          y = 0.3,
                          label = '20-40th percentile<br>outcome in Mid bandit',
                          fill = 'white',
                          color = 'black',
                          angle = 90,
                          size = 3.5,
                          hjust = 0.5) +
    labs(x = 'Relative position of Low-Mid trial',
         y = '') +
    theme(legend.position = 'left',
          legend.title = element_blank(),
          legend.text = element_text(size = 7),
          legend.background = element_rect(color = 'transparent',
                                           fill = 'transparent',
                                           size = 0.5),
          legend.key = element_rect(fill = 'transparent'),
          legend.direction = 'vertical',
          legend.margin = margin(0,0,0,0, 'pt'),
          axis.text = element_text(size = 12),
          axis.title.x = element_text(size = 15,
                                      face = 'bold',
                                      margin = margin(10,0,0,0,'pt')),
          axis.title.y = element_text(size = 15,
                                      face = 'bold',
                                      margin = margin(0,10,0,0,'pt')))
  
  p = Neurocodify_plot(p) +
    theme(panel.grid = element_blank(),
          plot.margin = margin(0,0,0,10, 'pt'))
  
  return(p)
  
}

# Estimation distance plot
Figure_behav_ed = function(){
  
  base_path = here::here()
  
  # Load pre-written functions
  source_path = file.path(base_path, 'code', 'utils',
                          fsep = .Platform$file.sep)
  source_files = list.files(source_path, pattern = "[.][rR]$",
                            full.names = TRUE, recursive = TRUE)
  invisible(lapply(source_files, function(x) source(x)))
  
  # Get color schemes
  custom_guides = Get_plot_guides()
  
  # Get data
  file = file.path(base_path, 'derivatives', 'figures', 'f_ed.tsv',
                   fsep = .Platform$file.sep)
  data = data.table::fread(file, sep = '\t', na.strings = 'n/a') %>%
    .[variable == 'diff_EmR_2m1', variable := 'Low\n-\nMid'] %>%
    .[variable == 'diff_EmR_3m2', variable := 'Mid\n-\nHigh']
  
  dodge_width = 0.3
  
  p = ggplot(data = data,
             aes(x = variable,
                 y = mean_value,
                 color = group,
                 fill = group)) +
    geom_hline(yintercept = 0,
               linetype = 'solid',
               size = 0.4) +
    geom_point(size = 0.5,
               alpha = 0.8,
               position = position_jitterdodge(dodge.width = dodge_width,
                                               jitter.width = dodge_width/2,
                                               jitter.height = 0,
                                               seed = 666)) +
    geom_boxplot(color = 'black',
                 outlier.shape = NA,
                 position = position_dodge(width = dodge_width),
                 width = dodge_width) +
    stat_summary(fun = 'mean',
                 geom = 'point',
                 na.rm = TRUE,
                 shape = 23,
                 inherit.aes = TRUE,
                 fill = 'white',
                 size = 2,
                 stroke = 0.5,
                 show.legend = FALSE,
                 position = position_dodge(width = dodge_width)) +
    labs(x = 'Compared bandits',
         y = 'Distance of estimates\nvs. true bandit distance') +
    scale_y_continuous(breaks = seq(-40, 40, by = 10)) +
    scale_color_manual(values = custom_guides) +
    scale_fill_manual(values = custom_guides) +
    facet_grid(~group)
  p = Neurocodify_plot(p) +
    theme(legend.position = 'none',
          panel.grid = element_blank(),
          strip.text = element_text(size = 15),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12,
                                     lineheight = 0.6,
                                     margin = margin(5,0,0,0,'pt')),
          axis.title.x = element_text(size = 15,
                                      face = 'bold',
                                      margin = margin(10,0,0,0, 'pt')),
          axis.title.y = element_text(size = 15,
                                      face = 'bold',
                                      margin = margin(0,10,0,0, 'pt')),
          plot.margin = margin(0,0,0,0,'pt'))
  
  return(p)
  
}


Figure_behav_ried = function(){
  
  # critical behavioral effect
  p_rie = Figure_behav_rie() +
    labs(x = '') +
    theme(legend.position = 'none')
  # 20-40 percentile
  p_ric = Figure_behav_ric() +
    labs(x = '') +
    theme(legend.position = 'none',
          axis.title.y = element_blank(),
          plot.background = element_rect(fill = 'transparent',
                                         color = NA))
  # Isolate legend to plot for both plots
  legend = cowplot::get_legend(Figure_behav_ric() +
                                 theme(legend.position = 'top',
                                       legend.direction = 'horizontal',
                                       legend.text = element_text(size = 10),
                                       legend.key.height = unit(30, 'pt')))
  # Estimation plot
  p_ed = Figure_behav_ed()
  
  # Left side of plot
  p_behav_ri = cowplot::plot_grid(p_rie,NULL, p_ric,
                                  ncol = 3,
                                  rel_widths = c(1,0.1,0.8),
                                  rel_heights = c(1,1,1),
                                  axis = 'tb',
                                  align = 'h',
                                  labels = c('A', 'B', ''),
                                  label_y = 1.04,
                                  label_x = -0.09,
                                  label_size = 25) +
    annotation_custom(grid::textGrob(label = 'Relative position of Low-Mid trial',
                                     hjust = 0.5,
                                     vjust = 0,
                                     gp = grid::gpar(cex = 1.2,
                                                     fontface = 'bold')),
                      xmin = 0.6, xmax = 0.6, ymin = 0.01, ymax = 0.01)
  # Combine with right side
  p_behav_ried = cowplot::plot_grid(p_behav_ri, NULL, p_ed,
                                    nrow = 1,
                                    rel_widths = c(1,0.1,0.5),
                                    labels = c('', '', 'C'),
                                    label_size = 25,
                                    label_x = -0.05,
                                    label_y = 1.04) +
    theme(plot.margin = margin(5,5,2.5,10,'pt'))
  
  # Adjust legend for top across left plots
  legend = cowplot::plot_grid(NULL, legend, NULL,
                              ncol = 3,
                              rel_widths = c(0.1,1,0.8))
  # Combine with legend
  p = cowplot::plot_grid(legend, p_behav_ried,
                         ncol = 1,
                         rel_heights = c(0.1,1))
  
  return(p)
  
}

