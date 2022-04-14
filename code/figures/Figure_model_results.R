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

# Model comparison
# AIC
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
               size = 0.5) +
    geom_point(position = position_jitternudge(jitter.width = jitter_width,
                                               jitter.height = 0,
                                               nudge.x = -dodge_width/4,
                                               seed = 666),
               alpha = 0.5,
               size = 0.5) +
    geom_boxplot(outlier.shape = NA,
                 color = 'black',
                 width = dodge_width/2,
                 position = position_nudge(x = dodge_width/4)) +
    stat_summary(fun = 'mean',
                 geom = 'point',
                 na.rm = TRUE,
                 shape = 23,
                 fill = 'white',
                 size = 3,
                 stroke = 1,
                 position = position_nudge(x = dodge_width/4)) +
    labs(x = '',
         y = 'AIC difference\n(Rw - PE-RW)') +
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
          panel.grid = element_blank())
  
  return(p)
  
}


# Model comparison
# Absolute/count data

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
         y = '\nNumber of participants') +
    scale_fill_manual(values = custom_guides) +
    scale_x_discrete(labels = c('RW', 'PE-RW')) +
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
          legend.position = 'top')
  
  return(p)
  
}


# Model validation
# Influence extreme outcomes

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
         y = 'Influence of\nextreme outcomes') +
    scale_x_continuous(limits = c(-0.5,0.5),
                       breaks = seq(-0.5,0.5, by = 0.25)) +
    scale_y_continuous(limits = c(-0.8,0.8),
                       breaks = seq(-0.8,0.8, by = 0.4))
  p = Neurocodify_plot(p) +
    theme(legend.position = 'none',
          panel.grid = element_blank(),
          axis.title.x = element_text(size = 10,
                                      face = 'bold',
                                      margin = margin(10,0,0,0,'pt')),
          axis.title.y = element_text(size = 10,
                                      face = 'bold',
                                      margin = margin(0,10,0,0,'pt')))
  
  return(p)
  
}


# Model validation
# Misjudgement of distance

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
                                      margin = margin(0,10,0,0,'pt')))
  
  return(p)
  
}


# Model weighting
# Absolute

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
    geom_bar(width = 0.8) +
    facet_wrap(~group) +
    scale_fill_manual(values = custom_guides) +
    scale_color_manual(values = custom_guides) +
    labs(x = 'Relative weighting of extreme outcomes',
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
          legend.title = element_blank())
  
  return(p)
  
}


# Model weighting
# AIC

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
                                      margin = margin(0,10,0,0,'pt')))
  
  return(p)
  
}

Figure_model_results = function(){
  
  # p_mc_aic = Figure_mc_rel() +
  #   theme(plot.margin = margin(0,10,20,0, 'pt'))
  # p_mc_abs = Figure_mc_abs() +
  #   theme(plot.margin = margin(0,0,20,10, 'pt'))
  # p_model_results_1 = cowplot::plot_grid(p_mc_aic, p_mc_abs,
  #                                        ncol = 2,
  #                                        rel_widths = c(1,1),
  #                                        rel_heights = c(1,1),
  #                                        axis = 'tb',
  #                                        align = 'h',
  #                                        labels = c('A','B'))
  # 
  # p_mv_ri = Figure_mv_ri() +
  #   theme(plot.margin = margin(20,10,20,0, 'pt'))
  # p_mv_ed = Figure_mv_ed() +
  #   theme(plot.margin = margin(20,0,20,10, 'pt'))
  # p_model_results_2 = cowplot::plot_grid(p_mv_ri, p_mv_ed,
  #                                        ncol = 2,
  #                                        rel_widths = c(1,1),
  #                                        rel_heights = c(1,1),
  #                                        axis = 'tb',
  #                                        align = 'h',
  #                                        labels = c('C','D'))
  # 
  # p_mw_group = Figure_mw_group() +
  #   theme(plot.margin = margin(20,10,0,0, 'pt'))
  # p_mw_cont = Figure_mw_cont() +
  #   theme(plot.margin = margin(20,0,0,10, 'pt'))
  # p_model_results_3 = cowplot::plot_grid(p_mw_group, p_mw_cont,
  #                                        ncol = 2,
  #                                        rel_widths = c(1,1),
  #                                        rel_heights = c(1,1),
  #                                        axis = 'tb',
  #                                        align = 'h',
  #                                        labels = c('E','F'))
  
  # p_model_results = cowplot::plot_grid(p_model_results_1, p_model_results_2, p_model_results_3,
  #                                      nrow = 3,
  #                                      rel_heights = c(1,0.8,1))
  
  p_mc_aic = Figure_mc_rel() +
    theme(plot.margin = margin(0,10,20,0, 'pt'))
  p_mv_ri = Figure_mv_ri() +
    theme(plot.margin = margin(20,10,20,0, 'pt'))
  p_mw_group = Figure_mw_group() +
    theme(plot.margin = margin(30,10,0,0, 'pt'))
  p_model_results_1 = cowplot::plot_grid(p_mc_aic, p_mv_ri,p_mw_group,
                                         ncol = 1,
                                         rel_heights = c(1,0.8,1),
                                         axis = 'bl',
                                         align = 'hv',
                                         labels = c('A','C','E'))
  
  p_mc_abs = Figure_mc_abs() +
    theme(plot.margin = margin(0,0,20,10, 'pt'))
  p_mv_ed = Figure_mv_ed() +
    theme(plot.margin = margin(20,20,20,10, 'pt'))
  p_mw_cont = Figure_mw_cont() +
    theme(plot.margin = margin(30,0,0,10, 'pt'))
  p_model_results_2 = cowplot::plot_grid(p_mc_abs, p_mv_ed,p_mw_cont,
                                         ncol = 1,
                                         rel_heights = c(1,0.8,1),
                                         axis = 'bl',
                                         align = 'hv',
                                         labels = c('B','D','F'))
  
  p_model_results = cowplot::plot_grid(p_model_results_1, p_model_results_2,
                                       ncol = 2,
                                       rel_widths = c(1,1))
  
  
  return(p_model_results)
  
}

