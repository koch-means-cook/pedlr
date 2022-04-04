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
             aes(x = diff_1m0_z,
                 y = preminuspost_z,
                 color = group)) +
    geom_hline(yintercept = 0,
               size = 0.5) +
    geom_point() +
    geom_smooth(method = 'lm',
                formula = y ~ x,
                color = 'black') +
    scale_color_manual(values = custom_guides) +
    scale_y_continuous(limits = c(-4, 4)) +
    scale_x_continuous(limits = c(-4, 4)) +
    labs(x = 'Model derived overweighting\n(z-scored)',
         y = 'Influence of rare outcomes\n(z-scored)')
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
             aes(x = diff_1m0_z,
                 y = behav_contrast_z,
                 color = group)) +
    geom_hline(yintercept = 0,
               size = 0.5) +
    geom_point() +
    geom_smooth(method = 'lm',
                formula = y ~ x,
                color = 'black') +
    scale_color_manual(values = custom_guides) +
    scale_y_continuous(limit = c(-4,4)) +
    scale_x_continuous(limit = c(-4,4)) +
    labs(x = 'Model derived overweighting\n(z-scored)',
         y = 'Asymmetry in distance bias\n(Mid-High vs. Low-Mid, z-scored)')
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
  p_mv_ed = Figure_mv_ed()
  
  p = cowplot::plot_grid(p_mv_ri, p_mv_ed,
                         ncol = 2,
                         rel_widths = c(1,1),
                         labels = c('A', 'B'),
                         align = 'h')
  
  return(p)
  
}

