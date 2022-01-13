library(here)
library(data.table)
library(emojifont)
library(cowplot)
library(ggplot2)

figure_parameter_recovery = function(){
  
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
  
  # Function to plot single model recovery
  Get_cormat_across = function(data,
                               model_name,
                               colnames,
                               plot_title){
    # Prepare data for correlation
    corr_data = data %>%
      .[, recov := second_solution] %>%
      data.table::dcast(model + design + file + iter ~ para, value.var = c('true', 'recov'))
    
    # Specify data to plot correlation of
    data_plot = cor(corr_data[model == model_name, ..colnames]) %>%
      reshape2::melt(.) %>%
      as.data.table(.) %>%
      .[, row := seq(.N)] %>%
      # Only select correlation between true and recovered params (and not truth vs truth)
      .[, need := unlist(strsplit(as.character(Var1), '_'))[1] != unlist(strsplit(as.character(Var2), '_'))[1],
        by = 'row'] %>%
      .[need == TRUE, c('Var1', 'Var2', 'value')]
    # Eliminate duplicates (e.g. true_a1 vs. recov_a1 and recov_a1 vs. true_a1)
    data_plot = data_plot[1:(nrow(data_plot)/2)]
    # Eliminate levels
    data_plot$Var1 = as.character(data_plot$Var1)
    data_plot$Var2 = as.character(data_plot$Var2)
    
    
    # Plot correlation matrix
    p = ggplot(data_plot,
               aes(x = Var1, y = Var2, fill = value)) +
      theme_minimal()+ 
      geom_tile() +
      scale_fill_viridis(option='D', limits = c(-1,1)) +
      coord_fixed() +
      labs(title = plot_title,
           x = 'Recovery',
           y = 'True') +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = 'None',
            axis.title = element_text(size = 15, face = 'bold'),
            axis.text.x = element_text(angle = 0))
    
    return(p)
  }
  
  # Rw
  p_rw = Get_cormat_across(data = data,
                           model_name = 'Rw',
                           colnames = c('true_alpha0', 'true_temperature',
                                        'recov_alpha0', 'recov_temperature'),
                           plot_title = 'Rw: Recovery')
  p_rw = p_rw +
    scale_x_discrete(labels = c(expression('\u03b1'['0']), expression('\u03c4'))) +
    scale_y_discrete(labels = c(expression('\u03b1'['0']), expression('\u03c4'))) +
    theme(axis.text = element_text(size = 18))
  
  # Pedlr_simple
  p_pedlr_simple = Get_cormat_across(data = data,
                                     model_name = 'Pedlr_simple',
                                     colnames = c('true_alpha1', 'true_temperature',
                                                  'recov_alpha1', 'recov_temperature'),
                                     plot_title = 'Pedlr_simple: Recovery')
  p_pedlr_simple = p_pedlr_simple +
    scale_x_discrete(labels = c(expression('\u03b1'['1']), expression('\u03c4'))) +
    scale_y_discrete(labels = c(expression('\u03b1'['1']), expression('\u03c4'))) +
    theme(axis.text = element_text(size = 18))
  
  # Pedlr
  p_pedlr = Get_cormat_across(data = data,
                         model_name = 'Pedlr',
                         colnames = c('true_alpha0', 'true_alpha1', 'true_temperature',
                                      'recov_alpha0', 'recov_alpha1', 'recov_temperature'),
                         plot_title = 'Pedlr: Recovery')
  p_pedlr = p_pedlr +
    scale_x_discrete(labels = c(expression('\u03b1'['0']), expression('\u03b1'['1']), expression('\u03c4'))) +
    scale_y_discrete(labels = c(expression('\u03b1'['0']), expression('\u03b1'['1']), expression('\u03c4'))) +
    theme(axis.text = element_text(size = 18))
  
  # Pedlr fixdep
  p_pedlr_fixdep = Get_cormat_across(data = data,
                                     model_name = 'Pedlr_fixdep',
                                     colnames = c('true_alpha0', 'true_alpha1', 'true_temperature',
                                                  'recov_alpha0', 'recov_alpha1', 'recov_temperature'),
                                     plot_title = 'Pedlr_fixdep: Recovery')
  p_pedlr_fixdep = p_pedlr_fixdep +
    scale_x_discrete(labels = c(expression('\u03b1'['0']), expression('\u03b1'['1']), expression('\u03c4'))) +
    scale_y_discrete(labels = c(expression('\u03b1'['0']), expression('\u03b1'['1']), expression('\u03c4'))) +
    theme(axis.text = element_text(size = 18))
  
  # Pedlr interdep
  p_pedlr_interdep = Get_cormat_across(data = data,
                                       model_name = 'Pedlr_interdep',
                                       colnames = c('true_alpha0', 'true_alpha1', 'true_interdep', 'true_temperature',
                                                    'recov_alpha0', 'recov_alpha1', 'recov_interdep', 'recov_temperature'),
                                       plot_title = 'Pedlr_interdep: Recovery')
  p_pedlr_interdep = p_pedlr_interdep +
    scale_x_discrete(labels = c(expression('\u03b1'['0']), expression('\u03b1'['1']), expression('\u03c0'), expression('\u03c4'))) +
    scale_y_discrete(labels = c(expression('\u03b1'['0']), expression('\u03b1'['1']), expression('\u03c0'), expression('\u03c4'))) +
    
    theme(axis.text = element_text(size = 18),
          legend.position = 'right')
  
  p = cowplot::plot_grid(p_rw, p_pedlr_simple, p_pedlr, p_pedlr_fixdep, p_pedlr_interdep,
                     ncol = 5,
                     axis = 'tb',
                     align = 'v')
  p
  
}