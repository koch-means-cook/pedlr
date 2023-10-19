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
library(gghalves)
library(latex2exp)

# Model comparison
Figure_mc_nwinning = function(){
  
  # Get directory of repository
  base_path = here::here()
  
  # Load pre-written functions
  source_path = file.path(base_path, 'code', 'utils',
                          fsep = .Platform$file.sep)
  source_files = list.files(source_path, pattern = "[.][rR]$",
                            full.names = TRUE, recursive = TRUE)
  invisible(lapply(source_files, function(x) source(x)))
  
  source_path = file.path(base_path, 'code', 'model_fitting', 'LRfunction.R',
                          fsep = .Platform$file.sep)
  source(source_path)
  
  
  # Get plot colors/linetypes
  custom_guides = Get_plot_guides()
  
  # Load modelling results
  data = Load_model_fits_new() %>%
    .[starting_values == 'random'] %>%
    Apply_exclusion_criteria(., choice_based_exclusion = TRUE) %>%
    .[, ':='(participant_id = as.factor(participant_id),
             group = as.factor(group),
             sex = as.factor(sex),
             starting_values = as.factor(starting_values))] %>%
    Prepare_data_for_plot(.)
  # # Sort model levels by number of parameters
  # data$model = factor(data$model, levels = c('rw',
  #                                            'uncertainty',
  #                                            'surprise',
  #                                            'uncertainty_surprise'))
  
  # Create model comparison data
  data_model_comp = data %>%
    .[iter == 1] %>%
    .[, .(AIC = mean(AIC),
          sd_AIC = sd(AIC),
          AICc = mean(AICc),
          sd_AICc = sd(AICc),
          n_params = sum(variable == 'coefs')),
      by = c('participant_id', 'group', 'model')]

  # Get winning model within each participant
  data_counts = data_model_comp %>%
    data.table::melt(id.vars = c('participant_id', 'group', 'model'),
                     measure.vars = c('AIC', 'AICc')) %>%
    .[, ':='(lowest = min(value),
             loc_winning = value == min(value),
             # Name of winning model
             winning_model = model[value == min(value)]),
      by = c('participant_id', 'variable')] %>%
    # Only keep winning model
    .[loc_winning == TRUE]
  
  # Count winning models across participants
  data_counts_all = data_counts %>%
    .[, .(n_winning = .N),
      by = c('variable', 'model')] %>%
    .[variable == 'AICc'] %>%
    .[, size := n_winning == max(n_winning)]
    
  # Count winning models within age-groups
  data_counts_age = data_counts %>%
    .[, .(n_winning = .N),
      by = c('variable', 'group', 'model')] %>%
    .[variable == 'AICc'] %>%
    .[, size := n_winning == max(n_winning),
      by = c('group')]
  
  # Plot counts (for corrected AIC)
  p_all = ggplot(data = data_counts_all,
             aes(x = model,
                 y = n_winning,
                 fill = model,
                 linewidth = size)) +
    scale_fill_manual(values = custom_guides) +
    scale_linewidth_manual(values = c(NA,1)) +
    scale_y_continuous(limits = c(0,max(data_counts_all$n_winning)),
                       breaks = seq(0, round(max(data_counts_all$n_winning), -1), by = 10)) +
    geom_col(color = 'black') +
    labs(y = 'Number of best fits')
  p_all = Neurocodify_plot(p_all) +
    theme(axis.title.x = element_blank(),
          axis.text.y = element_text(size = 12, margin = margin(0,2.5,0,10, 'pt')),
          axis.title.y = element_text(size = 15,
                                      face = 'bold',
                                      margin = margin(0,0,0,0,'pt')),
          axis.text.x = element_text(size = 12,
                                     angle = 55,
                                     hjust = 1),
          legend.position = 'none',
          plot.margin = margin(0,0,0,5,'pt'),
          panel.grid = element_blank())
  
  p_age = ggplot(data = data_counts_age,
             aes(x = model,
                 y = n_winning,
                 fill = model,
                 size = size)) +
    scale_fill_manual(values = custom_guides) +
    scale_size_manual(values = c(0,1)) +
    scale_y_continuous(limits = c(0,max(data_counts_all$n_winning)),
                       breaks = seq(0, round(max(data_counts_all$n_winning), -1), by = 10)) +
    geom_col(color = 'black') +
    facet_grid(.~group, switch = 'x')
  p_age = Neurocodify_plot(p_age) +
    theme(panel.grid = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(color = 'transparent'),
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 12, margin = margin(0,2.5,0,0,'pt')),
          axis.line.x = element_line(color = 'transparent'),
          axis.ticks.x = element_line(color = 'transparent'),
          legend.position = 'none',
          plot.margin = margin(10,0,0,0,'pt'),
          strip.text.x.bottom = element_text(size = 12, face = 'bold', angle = 0))
  
  p = cowplot::plot_grid(p_all, NULL, p_age,
                         rel_widths = c(1,0.1,0.8),
                         axis = 'bt',
                         align = 'h',
                         ncol = 3)
  
  # Return plot
  return(p)
  
}

Figure_mc_rel_aic = function(){
  
  # Get directory of repository
  base_path = here::here()
  
  # Load pre-written functions
  source_path = file.path(base_path, 'code', 'utils',
                          fsep = .Platform$file.sep)
  source_files = list.files(source_path, pattern = "[.][rR]$",
                            full.names = TRUE, recursive = TRUE)
  invisible(lapply(source_files, function(x) source(x)))
  
  source_path = file.path(base_path, 'code', 'model_fitting', 'LRfunction.R',
                          fsep = .Platform$file.sep)
  source(source_path)
  
  
  # Get plot colors/linetypes
  custom_guides = Get_plot_guides()
  
  # Load modelling results
  data = Load_model_fits_new() %>%
    .[starting_values == 'random'] %>%
    Apply_exclusion_criteria(., choice_based_exclusion = TRUE) %>%
    .[, ':='(participant_id = as.factor(participant_id),
             group = as.factor(group),
             sex = as.factor(sex),
             starting_values = as.factor(starting_values))]
  
  # Create model comparison data
  data_model_comp = data %>%
    .[iter == 1] %>%
    .[, .(AIC = mean(AIC),
          sd_AIC = sd(AIC),
          AICc = mean(AICc),
          sd_AICc = sd(AICc),
          n_params = sum(variable == 'coefs')),
      by = c('participant_id', 'group', 'model')]
  
  # Relative AICc to RW model
  data_model_comp_rw = data_model_comp %>%
    data.table::dcast(participant_id + group ~ paste0('AICc_', model),
                      value.var = 'AICc') %>%
    .[, ':='(AICc_UmRW = AICc_uncertainty - AICc_rw,
             AICc_VmRW = AICc_seplr - AICc_rw,
             AICc_UVmRW = AICc_uncertainty_seplr - AICc_rw,
             AICc_SmRW = AICc_surprise - AICc_rw,
             AICc_USmRW = AICc_uncertainty_surprise - AICc_rw),
      by = c('participant_id', 'group')] %>%
    data.table::melt(id.vars = c('participant_id', 'group'),
                     measure.vars = c('AICc_UmRW',
                                      'AICc_VmRW',
                                      'AICc_UVmRW',
                                      'AICc_SmRW',
                                      'AICc_USmRW')) %>%
    Prepare_data_for_plot(.)
  levels(data_model_comp_rw$variable) = c('Uncertainty', 'Valence', 'Unc+Valence', 'Surprise', 'Unc+Surprise')
  
  data_model_comp_rw_mean = data_model_comp_rw %>%
    .[, .(value = mean(value),
          sd_value = sd(value),
          sem = sd(value) / sqrt(.N)),
      by = 'variable']
  
  # Plot counts (for corrected AIC)
  p = ggplot(data = data_model_comp_rw,
                 aes(x = variable,
                     y = value,
                     fill = variable,
                     color = variable)) +
    scale_fill_manual(values = custom_guides) +
    scale_color_manual(values = custom_guides) +
    geom_col(data = data_model_comp_rw_mean) +
    geom_hline(yintercept = 0,
               linewidth = 0.5) +
    geom_point(position = position_jitter(width = 0.1,
                                          height = 0,
                                          seed = 666),
               alpha = 0.5,
               size = 0.5,
               color = 'darkgrey') +
    geom_errorbar(data = data_model_comp_rw_mean,
                  aes(ymin = value - sem,
                      ymax = value + sem),
                  color = 'black',
                  width = 0.3) +
    labs(y = 'AICc relative to RW model') +
    scale_y_reverse()
  p = Neurocodify_plot(p) +
    coord_flip() +
    theme(axis.title.y = element_blank(),
          axis.text.x = element_text(size = 12, 
                                     margin = margin(2.5,0,0,0, 'pt'),
                                     angle = 40,
                                     hjust = 1),
          axis.title.x = element_text(size = 15,
                                      face = 'bold',
                                      margin = margin(10,0,0,0,'pt')),
          axis.text.y = element_text(size = 12),
          legend.position = 'none',
          plot.margin = margin(0,0,0,0,'pt'),
          panel.grid = element_blank())
  
  # Simple version of plot (no dots)
  p_simple = ggplot(data = data_model_comp_rw,
               aes(x = variable,
                   y = value,
                   fill = variable,
                   color = variable)) +
    scale_fill_manual(values = custom_guides) +
    scale_color_manual(values = custom_guides) +
    geom_col(data = data_model_comp_rw_mean) +
    geom_hline(yintercept = 0,
               linewidth = 0.5) +
    geom_errorbar(data = data_model_comp_rw_mean,
                  aes(ymin = value - sem,
                      ymax = value + sem),
                  color = 'black',
                  width = 0.3) +
    labs(y = 'AICc relative\nto RW model') +
    scale_y_continuous(limits = c(-6,0),
                       breaks = seq(-6,0, by = 1))
  p_simple = Neurocodify_plot(p_simple) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(size = 12, 
                                     margin = margin(2.5,0,0,0, 'pt'),
                                     angle = 55,
                                     hjust = 1),
          axis.title.y = element_text(size = 15,
                                      face = 'bold',
                                      margin = margin(0,10,0,0,'pt')),
          axis.text.y = element_text(size = 12),
          legend.position = 'none',
          plot.margin = margin(5,5,5,5,'pt'),
          panel.grid = element_blank())
  
  # Return plots
  res = list('p_full' = p,
             'p_simple' = p_simple)
  return(res)
  
}



Figure_mc_pxp = function(){
  
  # Get directory of repository
  base_path = here::here()
  
  # Load pre-written functions
  source_path = file.path(base_path, 'code', 'utils',
                          fsep = .Platform$file.sep)
  source_files = list.files(source_path, pattern = "[.][rR]$",
                            full.names = TRUE, recursive = TRUE)
  invisible(lapply(source_files, function(x) source(x)))
  
  source_path = file.path(base_path, 'code', 'model_fitting', 'LRfunction.R',
                          fsep = .Platform$file.sep)
  source(source_path)
  
  
  # Get plot colors/linetypes
  custom_guides = Get_plot_guides()
  
  # Load modelling results
  data = Load_model_fits_new() %>%
    .[starting_values == 'random'] %>%
    Apply_exclusion_criteria(., choice_based_exclusion = TRUE) %>%
    .[, ':='(participant_id = as.factor(participant_id),
             group = as.factor(group),
             sex = as.factor(sex),
             starting_values = as.factor(starting_values))]
  # # Sort model levels by number of parameters
  # data$model = factor(data$model, levels = c('rw',
  #                                            'uncertainty',
  #                                            'surprise',
  #                                            'uncertainty_surprise'))
  
  # Create model comparison data
  data_model_comp = data %>%
    .[iter == 1] %>%
    .[, .(AIC = mean(AIC),
          sd_AIC = sd(AIC),
          AICc = mean(AICc),
          sd_AICc = sd(AICc),
          n_params = sum(variable == 'coefs')),
      by = c('participant_id', 'group', 'model')]
  
  # Get winning model within each participant
  data_counts = data_model_comp %>%
    data.table::melt(id.vars = c('participant_id', 'group', 'model'),
                     measure.vars = c('AIC', 'AICc')) %>%
    .[, ':='(lowest = min(value),
             loc_winning = value == min(value),
             # Name of winning model
             winning_model = model[value == min(value)]),
      by = c('participant_id', 'variable')] %>%
    # Only keep winning model
    .[loc_winning == TRUE]
  
  # Count winning models across participants
  data_counts_all = data_counts %>%
    .[, .(n_winning = .N),
      by = c('variable', 'model')] %>%
    Prepare_data_for_plot(.)
  
  # Count winning models within age-groups
  data_counts_age = data_counts %>%
    .[, .(n_winning = .N),
      by = c('variable', 'group', 'model')] %>%
    Prepare_data_for_plot(.)
  
  # Exceedance probs
  data_ep = data_model_comp %>%
    .[, ':='(nAIC = -(AIC),
             nAICc = -(AICc))] %>%
    data.table::dcast(participant_id + group ~ model, value.var = 'nAICc') %>%
    Prepare_data_for_plot(.)
  # Set seed for calculating PXP
  set.seed(666)
  pxp = bmsR::VB_bms(cbind(data_ep$rw,
                           data_ep$uncertainty,
                           data_ep$seplr,
                           data_ep$uncertainty_seplr,
                           data_ep$surprise,
                           data_ep$uncertainty_surprise),
                     n_samples = 100000)$pxp
  model_names = c('RW', 'Uncertainty', 'Valence', 'Unc+Valence', 'Surprise', 'Unc+Surprise')
  data_plot = data.table(cbind(model_names,
                               pxp)) %>%
    .[, pxp := as.numeric(pxp)] %>%
    .[, model_names := factor(model_names, levels = c('RW', 'Uncertainty', 'Valence', 'Unc+Valence', 'Surprise', 'Unc+Surprise'))] %>%
    # Add frame for winning model
    .[, size := pxp == max(pxp)]
  colnames(data_plot) = c('model', 'pxp', 'size')
  
  p = ggplot(data = data_plot,
             aes(x = model,
                 y = pxp,
                 fill = model,
                 size = size)) +
    scale_fill_manual(values = custom_guides) +
    scale_size_manual(values = c(0,1)) +
    geom_col(color = 'black') +
    # geom_col(data = data_plot[model != 'Surprise']) +
    # geom_col(data = data_plot[model == 'Surprise'],
    #          color = 'black',
    #          size = 1)
    scale_y_continuous(limits = c(0,1)) +
    labs(y = 'Protected\nexceedance\nprobability')
  p = Neurocodify_plot(p) +
    theme(axis.title.x = element_blank(),
          axis.text.y = element_text(size = 12, margin = margin(0,2.5,0,10, 'pt')),
          axis.title.y = element_text(size = 15,
                                      face = 'bold',
                                      margin = margin(0,0,0,0,'pt')),
          axis.text.x = element_text(size = 12,
                                     margin = margin(2.5,0,0,0,'pt'),
                                     angle = 55,
                                     hjust = 1),
          legend.position = 'none',
          plot.margin = margin(0,0,0,5,'pt'),
          panel.grid = element_blank())

  # Return plot
  return(p)
  
}


Figure_model_comp = function(){
  
  # Create all sub figures
  p_nwin = Figure_mc_nwinning()
  p_aic = Figure_mc_rel_aic()
  p_pxp = Figure_mc_pxp()
  
  p1 = p_aic$p_simple
  p2 = p_pxp
  p3 = p_nwin
  
  p_top = cowplot::plot_grid(p1,NULL,p2,
                             nrow = 1,
                             rel_widths = c(1,0.2,1.2),
                             axis = 'bt',
                             align = 'h',
                             labels = c('A', '', 'B'),
                             label_size = 25,
                             label_y = 1.05,
                             label_x = -0.07)
  p_bottom = cowplot::plot_grid(NULL,p3,NULL,
                                nrow = 1,
                                rel_widths = c(0.1,1,0.1),
                                labels = c('','C', ''),
                                label_size = 25,
                                label_y = 1.1,
                                label_x = -0.03)
  p_model_comp = cowplot::plot_grid(p_top, NULL, p_bottom,
                                    ncol = 1,
                                    rel_heights = c(1,0.1,1),
                                    axis = 'lr',
                                    align = 'v') +
    theme(plot.margin = margin(5,5,5,5,'pt'))
  
  # p2 = cowplot::plot_grid(NULL, p2, NULL,
  #                         rel_widths = c(0.3,1,0.3),
  #                         ncol = 3,
  #                         labels = c('B','',''),
  #                         label_size = 25,
  #                         label_y = 1,
  #                         label_x = -0.02,
  #                         axis = 'b',
  #                         align = 'h')
  # 
  # p_right = cowplot::plot_grid(p2, p3,
  #                              ncol = 1,
  #                              labels = c('', 'C'),
  #                              rel_heights = c(1.4,1),
  #                              label_size = 25,
  #                              label_y = 1,
  #                              label_x = -0.02)
  # 
  # # p_left = p_left + theme(plot.margin = margin(30, 0, 0, 30, 'pt'))
  # # p_right = p_right + theme(plot.margin = margin(30, 0, 0, 30, 'pt'))
  # p_model_comp = cowplot::plot_grid(p1, p_right,
  #                    ncol = 2,
  #                    # axis = 't',
  #                    # align = 'v',
  #                    rel_widths = c(1, 1.5),
  #                    labels = c('A', ''),
  #                    label_size = 25,
  #                    label_y = 1,
  #                    label_x = -0.02)

  
  
  
  return(p_model_comp)
  
}