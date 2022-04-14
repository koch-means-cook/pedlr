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
library(latex2exp)

Figure_mw_cont = function(){
  
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
  file = file.path(base_path, 'derivatives', 'figures', 'f_mw.tsv',
                   fsep = .Platform$file.sep)
  data = data.table::fread(file, sep = '\t', na.strings = 'n/a')
  data$weight_high_pe = factor(data$weight_high_pe)
  levels(data$weight_high_pe) = c('Overweight', 'Underweight')
  
  dodge_width = 0.25
  jitter_width = dodge_width/4
  
  p = ggplot(data = data,
             aes(x = group,
                 y = diff_1m0,
                 color = group,
                 fill = group)) +
    geom_hline(yintercept = 0,
               size = 0.5) +
    geom_point(size = 0.8,
               position = position_jitter(width = jitter_width,
                                          height = 0,
                                          seed = 666),
               alpha = 0.5) +
    geom_boxplot(outlier.shape = NA,
                 color = 'black',
                 width = dodge_width) +
    gghalves::geom_half_violin(data = data[group == 'Older\nadults'],
                               side = 'l',
                               color = NA,
                               position = position_nudge(x = 0.5),
                               alpha = 0.8,
                               width = 0.3) +
    gghalves::geom_half_violin(data = data[group == 'Younger\nadults'],
                               side = 'r',
                               color = NA,
                               position = position_nudge(x = -0.5),
                               alpha = 0.8,
                               width = 0.3) +
    stat_summary(fun = 'mean',
                 geom = 'point',
                 na.rm = TRUE,
                 shape = 23,
                 fill = 'white',
                 size = 3,
                 stroke = 0.5) +
    scale_fill_manual(values = custom_guides) +
    scale_color_manual(values = custom_guides) +
    labs(x = '',
         y = bquote(bold(atop("Model-derived overweighting",
                              "("~alpha[1] - alpha[0]~")")))) +
    theme(legend.position = 'none')
  p = Neurocodify_plot(p) +
    theme(legend.position = 'none',
          panel.grid = element_blank(),
          axis.title.y = element_text(size = 10,
                                      face = 'bold',
                                      margin = margin(0,10,0,0,'pt')),
          plot.margin = margin(0,10,0,0, 'pt'))
  
  return(p)
  
}

Figure_mw_group = function(){
  
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
  file = file.path(base_path, 'derivatives', 'figures', 'f_mw.tsv',
                   fsep = .Platform$file.sep)
  data = data.table::fread(file, sep = '\t', na.strings = 'n/a')
  data$weight_high_pe = factor(data$weight_high_pe)
  levels(data$weight_high_pe) = c('Over-\nweight', 'Under-\nweight')
  
  p = ggplot(data = data,
             aes(x = weight_high_pe,
                 color = group,
                 fill = group)) +
    geom_bar() +
    facet_wrap(~group) +
    scale_fill_manual(values = custom_guides) +
    scale_color_manual(values = custom_guides) +
    labs(x = 'Relative weighting of rare outcomes',
         y = 'Number of participants')
  p = Neurocodify_plot(p) +
    theme(legend.position = 'none',
          panel.grid = element_blank(),
          axis.title.x = element_text(size = 10,
                                      face = 'bold',
                                      margin = margin(10,0,0,0,'pt')),
          axis.title.y = element_text(size = 10,
                                      face = 'bold',
                                      margin = margin(0,10,0,0,'pt')),
          plot.margin = margin(0,0,0,10, 'pt'),
          legend.title = element_blank())
  
  return(p)
  
}


Figure_model_wei = function(){

  p_mw_c = Figure_mw_cont()
  p_mw_g = Figure_mw_group()
  
  p = cowplot::plot_grid(p_mw_c, p_mw_g,
                         ncol = 2,
                         rel_widths = c(1,1),
                         labels = c('A', 'B'),
                         axis = 'bt',
                         align = 'h')
  
  return(p)
  
}

