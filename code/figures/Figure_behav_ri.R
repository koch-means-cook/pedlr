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
    ggtext::geom_richtext(x = 0,
                          y = 0.4,
                          label = 'Rare outcome<br>in mid bandit',
                          fill = 'white',
                          color = 'black',
                          angle = 90,
                          size = 3,
                          hjust = 0.5) +
    labs(x = 'Relative position of low-mid comparison',
         y = 'p(choice = Mid | Low-Mid)') +
    theme(legend.position = c(0.17,0.12),
          legend.title = element_blank(),
          legend.text = element_text(size = 7),
          legend.background = element_rect(color = 'transparent',
                                           fill = 'transparent',
                                           size = 0.5),
          legend.key = element_rect(fill = 'transparent'),
          legend.direction = 'vertical',
          legend.margin = margin(0,0,5,5, 'pt'),
          axis.title.x = element_text(size = 10,
                                      face = 'bold',
                                      margin = margin(10,0,0,0,'pt')),
          axis.title.y = element_text(size = 10,
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
    ggtext::geom_richtext(x = 0,
                          y = 0.4,
                          label = '20-40th percentile<br>outcome in mid bandit',
                          fill = 'white',
                          color = 'black',
                          angle = 90,
                          size = 3,
                          hjust = 0.5) +
    labs(x = 'Relative position of low-mid comparison',
         y = '') +
    theme(legend.position = 'none',
          axis.title.x = element_text(size = 10,
                                      face = 'bold',
                                      margin = margin(10,0,0,0,'pt')),
          axis.title.y = element_text(size = 10,
                                      face = 'bold',
                                      margin = margin(0,10,0,0,'pt')))
  
  p = Neurocodify_plot(p) +
    theme(panel.grid = element_blank(),
          plot.margin = margin(0,0,0,10, 'pt'))
  
  return(p)
  
}

Figure_behav_ri = function(){
  
  p_rie = Figure_behav_rie()
  p_ric = Figure_behav_ric()
  p_behav_ri = cowplot::plot_grid(p_rie, p_ric,
                                  ncol = 2,
                                  rel_widths = c(1,1),
                                  rel_heights = c(1,1),
                                  axis = 'tblr',
                                  align = 'hv',
                                  labels = c('A','B'))
  
  return(p_behav_ri)
  
}

