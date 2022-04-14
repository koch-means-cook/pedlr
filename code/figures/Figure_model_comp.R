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


Figure_mc_rel = function(){
  
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
  file = file.path(base_path, 'derivatives', 'figures', 'f_mc_rel.tsv',
                   fsep = .Platform$file.sep)
  data = data.table::fread(file, sep = '\t', na.strings = 'n/a') %>%
    .[, comp := 'one_group']
    # data.table::melt.data.table(id.vars = c('participant_id', 'group', 'best_fitting_model'),
    #                             measure.vars = c('Rw', 'Pedlr_step'),
    #                             value.name = 'AIC',
    #                             variable.name = 'model')
  
  dodge_width = 0.5
  jitter_width = dodge_width/4
  
  p = ggplot(data = data,
             aes(x = comp,
                 y = AIC_RWmPedlr_step)) +
    geom_hline(yintercept = 0,
               size = 0.35) +
    geom_point(position = position_jitternudge(jitter.width = jitter_width,
                                               jitter.height = 0,
                                               nudge.x = dodge_width/4,
                                               seed = 666),
               alpha = 0.5,
               size = 0.5) +
    geom_boxplot(outlier.shape = NA,
                 color = 'black',
                 width = dodge_width/2,
                 position = position_nudge(x = -dodge_width/4)) +
    stat_summary(fun = 'mean',
                 geom = 'point',
                 na.rm = TRUE,
                 shape = 23,
                 fill = 'white',
                 size = 3,
                 stroke = 0.5,
                 position = position_nudge(x = -dodge_width/4)) +
    labs(x = '',
         y = 'AIC difference\n(Rw - Pedlr_step)') +
    scale_y_continuous(limits = c(-20, 100),
                       breaks = seq(-20, 100, by = 20)) +
    theme(legend.position = 'none')
  p = Neurocodify_plot(p) +
    theme(axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_text(size = 10,
                                      face = 'bold',
                                      margin = margin(0,10,0,0,'pt')),
          panel.grid = element_blank(),
          plot.margin = margin(0,10,0,0,'pt'))
  
  return(p)
  
}

Figure_mc_abs = function(){
  
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
  file = file.path(base_path, 'derivatives', 'figures', 'f_mc_abs.tsv',
                   fsep = .Platform$file.sep)
  data = data.table::fread(file, sep = '\t', na.strings = 'n/a')
  
  data_plot = Prepare_data_for_plot(data)
  dodge_width = 0.5
  
  p = ggplot(data = data_plot,
                 aes(x = model,
                     color = group,
                     fill = group)) +
    geom_bar(stat = 'count',
             width = dodge_width * 0.9,
             position = position_dodge(width = dodge_width),
             color = 'transparent') +
    labs(x = 'Winning model',
         y = 'Number of participants') +
    scale_fill_manual(values = custom_guides) +
    scale_y_continuous(limits = c(0,40),
                       breaks = seq(0,40, by = 10))
  
  p = Neurocodify_plot(p) +
    theme(legend.title = element_blank(),
          axis.title.x = element_text(size = 10,
                                      face = 'bold',
                                      margin = margin(10,0,0,0,'pt')),
          axis.title.y = element_text(size = 10,
                                      face = 'bold',
                                      margin = margin(0,10,0,0,'pt')),
          panel.grid = element_blank(),
          plot.margin = margin(0,0,0,10,'pt'))
  
  return(p)
  
}

Figure_model_comp = function(){

  p_mc_rel = Figure_mc_rel()
  p_mc_abs = Figure_mc_abs()
  
  p = cowplot::plot_grid(p_mc_rel, p_mc_abs,
                         ncol = 2,
                         rel_widths = c(0.3,0.7),
                         labels = c('A', 'B'),
                         align = 'h')
  
  
  return(p)
  
}

