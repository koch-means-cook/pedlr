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


Figure_mv_ri = function(){
  
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
  file = file.path(base_path, 'derivatives', 'figures', 'f_mv_ri.tsv',
                   fsep = .Platform$file.sep)
  data = data.table::fread(file, sep = '\t', na.strings = 'n/a')
  
  p = ggplot(data = data,
             aes(x = diff_1m0,
                 y = preminuspost,
                 color = group)) +
    geom_hline(yintercept = 0,
               size = 0.5) +
    geom_point() +
    geom_smooth(method = 'lm',
                formula = y ~ x,
                color = 'black') +
    scale_color_manual(values = custom_guides) +
    labs(x = 'Model derived overweighting',
         y = 'Influence of extreme outcomes') +
    scale_x_continuous(limits = c(-0.5,0.5),
                       breaks = seq(-0.5,0.5, by = 0.25)) +
    scale_y_continuous(limits = c(-0.8,0.8),
                       breaks = seq(-0.8,0.8, by = 0.2))
  p = Neurocodify_plot(p) +
    theme(legend.position = 'none',
          panel.grid = element_blank(),
          axis.title.x = element_text(size = 10,
                                      face = 'bold',
                                      margin = margin(10,0,0,0,'pt')),
          axis.title.y = element_text(size = 10,
                                      face = 'bold',
                                      margin = margin(0,10,0,0,'pt')),
          plot.margin = margin(0,10,0,0, 'pt'))
  
  return(p)
  
}

Figure_mv_ed = function(){
  
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
  file = file.path(base_path, 'derivatives', 'figures', 'f_mv_ed.tsv',
                   fsep = .Platform$file.sep)
  data = data.table::fread(file, sep = '\t', na.strings = 'n/a')
  
  p = ggplot(data = data,
             aes(x = diff_1m0,
                 y = diff_EmR_2m1,
                 color = group)) +
    geom_hline(yintercept = 0,
               size = 0.5) +
    geom_point() +
    geom_smooth(method = 'lm',
                formula = y ~ x,
                color = 'black') +
    scale_color_manual(values = custom_guides) +
    labs(x = 'Model derived overweighting',
         y = 'Bias in estimated distance\n(Low vs. Mid)') +
    scale_x_continuous(limits = c(-0.5,0.5),
                       breaks = seq(-0.5,0.5, by = 0.25)) +
    scale_y_continuous(limits = c(-30,30),
                       breaks = seq(-30,30, by = 10))
  p = Neurocodify_plot(p) +
    theme(legend.position = 'none',
          panel.grid = element_blank(),
          axis.title.x = element_text(size = 10,
                                      face = 'bold',
                                      margin = margin(10,0,0,0,'pt')),
          axis.title.y = element_text(size = 10,
                                      face = 'bold',
                                      margin = margin(0,10,0,0,'pt')),
          plot.margin = margin(0,0,0,10, 'pt'))
  
  return(p)
  
}

Figure_model_val = function(){

  p_mv_ri = Figure_mv_ri()
  p_mv_ed = Figure_mv_ed() +
    theme(plot.margin = margin(0,10,0,10,'pt'))
  
  p = cowplot::plot_grid(p_mv_ri, p_mv_ed,
                         ncol = 2,
                         rel_widths = c(1,1),
                         labels = c('A', 'B'),
                         align = 'h')
  
  return(p)
  
}

