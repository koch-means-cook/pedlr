library(here)
library(data.table)
library(emojifont)
library(cowplot)
library(ggplot2)
library(magrittr)
library(viridis)

Figure_parameter_recovery = function(){
  
  # Get paths
  base_path = here::here()
  
  # Load pre-written functions
  source_path = file.path(base_path, 'code', 'utils',
                          fsep = .Platform$file.sep)
  source_files = list.files(source_path, pattern = "[.][rR]$",
                            full.names = TRUE, recursive = TRUE)
  invisible(lapply(source_files, function(x) source(x)))
  
  # Load data
  file = file.path(base_path, 'derivatives', 'parameter_recovery', 'param_recov.tsv',
                   fsep = .Platform$file.sep)
  data = data.table::fread(file, na.strings = 'n/a', sep = '\t') %>%
    # Order models
    .[, model := factor(model, levels = c('Rw',
                                          'Pedlr_simple',
                                          'Pedlr',
                                          'Pedlr_fixdep',
                                          'Pedlr_interdep'))] %>%
    # Rename alpha (Rw) to alpha0 (for convenience of plotting)
    .[para == 'alpha', para := 'alpha0']
  
  
  # Rw
  p_rw = Get_cormat_across(data = data,
                           model_name = 'Rw',
                           colnames = c('true_alpha0', 'true_temperature',
                                        'recov_alpha0', 'recov_temperature'),
                           plot_title = 'Rw')
  p_rw = p_rw +
    scale_x_discrete(labels = c(expression('\u03b1'['0']), expression('\u03c4'))) +
    scale_y_discrete(labels = c(expression('\u03b1'['0']), expression('\u03c4'))) +
    theme(axis.text = element_text(size = 10),
          plot.margin = margin(0,0,0,0,'pt'))
  
  # Pedlr_simple
  p_pedlr_simple = Get_cormat_across(data = data,
                                     model_name = 'Pedlr_simple',
                                     colnames = c('true_alpha1', 'true_temperature',
                                                  'recov_alpha1', 'recov_temperature'),
                                     plot_title = 'Pedlr_simple')
  p_pedlr_simple = p_pedlr_simple +
    scale_x_discrete(labels = c(expression('\u03b1'['1']), expression('\u03c4'))) +
    scale_y_discrete(labels = c(expression('\u03b1'['1']), expression('\u03c4'))) +
    theme(axis.text = element_text(size = 10),
          axis.title.y = element_blank(),
          plot.margin = margin(0,0,0,0,'pt'))
  
  # Pedlr
  p_pedlr = Get_cormat_across(data = data,
                         model_name = 'Pedlr',
                         colnames = c('true_alpha0', 'true_alpha1', 'true_temperature',
                                      'recov_alpha0', 'recov_alpha1', 'recov_temperature'),
                         plot_title = 'Pedlr')
  p_pedlr = p_pedlr +
    scale_x_discrete(labels = c(expression('\u03b1'['0']), expression('\u03b1'['1']), expression('\u03c4'))) +
    scale_y_discrete(labels = c(expression('\u03b1'['0']), expression('\u03b1'['1']), expression('\u03c4'))) +
    theme(axis.text = element_text(size = 10),
          axis.title.y = element_blank(),
          plot.margin = margin(0,0,0,0,'pt'))
  
  # Pedlr fixdep
  p_pedlr_fixdep = Get_cormat_across(data = data,
                                     model_name = 'Pedlr_fixdep',
                                     colnames = c('true_alpha0', 'true_alpha1', 'true_temperature',
                                                  'recov_alpha0', 'recov_alpha1', 'recov_temperature'),
                                     plot_title = 'Pedlr_fixdep')
  p_pedlr_fixdep = p_pedlr_fixdep +
    scale_x_discrete(labels = c(expression('\u03b1'['0']), expression('\u03b1'['1']), expression('\u03c4'))) +
    scale_y_discrete(labels = c(expression('\u03b1'['0']), expression('\u03b1'['1']), expression('\u03c4'))) +
    theme(axis.text = element_text(size = 10),
          axis.title.y = element_blank(),
          plot.margin = margin(0,0,0,0,'pt'))
  
  # Pedlr interdep
  p_pedlr_interdep = Get_cormat_across(data = data,
                                       model_name = 'Pedlr_interdep',
                                       colnames = c('true_alpha0', 'true_alpha1', 'true_interdep', 'true_temperature',
                                                    'recov_alpha0', 'recov_alpha1', 'recov_interdep', 'recov_temperature'),
                                       plot_title = 'Pedlr_interdep')
  p_pedlr_interdep = p_pedlr_interdep +
    scale_x_discrete(labels = c(expression('\u03b1'['0']), expression('\u03b1'['1']), expression('\u03c0'), expression('\u03c4'))) +
    scale_y_discrete(labels = c(expression('\u03b1'['0']), expression('\u03b1'['1']), expression('\u03c0'), expression('\u03c4'))) +
    
    theme(axis.text = element_text(size = 10),
          axis.title.y = element_blank(),
          legend.position = 'none',
          plot.margin = margin(0,0,0,0,'pt'))
  
  # extract the legend from one of the plots
  legend <- cowplot::get_legend(
    # create some space to the left of the legend
    p_pedlr_interdep +
      theme(#legend.box.margin = margin(6,6,12,12, unit = 'pt'),
            legend.position = 'right',
            legend.box.margin = margin(0,0,0,12, 'pt'),
            legend.key.width = unit(5, 'pt'),
            legend.key.height = unit(12, 'pt'),
            legend.title = element_blank(),
            legend.text = element_text(size = 6,
                                       hjust = -1))
  )
  
  p_plots = cowplot::plot_grid(p_rw, p_pedlr_simple, p_pedlr, p_pedlr_fixdep, p_pedlr_interdep,
                               ncol = 5,
                               rel_widths = c(1,1,1,1,1),
                               axis = 'tb',
                               align = 'v')
  p = cowplot::plot_grid(p_plots, legend,
                         nrow = 1,
                         rel_widths = c(5,.5),
                         axis = 'tb',
                         align = 'hv')
  return(p)
  
}