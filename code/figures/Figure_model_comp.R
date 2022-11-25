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
             starting_values = as.factor(starting_values))]
  # Sort model levels by number of parameters
  data$model = factor(data$model, levels = c('rw',
                                             'uncertainty',
                                             'surprise',
                                             'uncertainty_surprise'))
  
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
  levels(data_counts$model) = c('RW', 'Uncertainty', 'Surprise', 'Combined')
  
  # Count winning models across participants
  data_counts_all = data_counts %>%
    .[, .(n_winning = .N),
      by = c('variable', 'model')] %>%
    .[variable == 'AICc'] %>%
    Prepare_data_for_plot(.) %>%
    .[, size := n_winning == max(n_winning)]
    
  # Count winning models within age-groups
  data_counts_age = data_counts %>%
    .[, .(n_winning = .N),
      by = c('variable', 'group', 'model')] %>%
    .[variable == 'AICc'] %>%
    Prepare_data_for_plot(.) %>%
    .[, size := n_winning == max(n_winning),
      by = c('group')]
  
  # Plot counts (for corrected AIC)
  p_all = ggplot(data = data_counts_all,
             aes(x = model,
                 y = n_winning,
                 fill = model,
                 size = size)) +
    scale_fill_manual(values = custom_guides) +
    scale_size_manual(values = c(0,1)) +
    geom_col(color = 'black') +
    labs(y = 'Number of best fits')
  p_all = Neurocodify_plot(p_all) +
    theme(axis.title.x = element_blank(),
          axis.text.y = element_text(size = 12, margin = margin(0,2.5,0,10, 'pt')),
          axis.title.y = element_text(size = 15,
                                      face = 'bold',
                                      margin = margin(0,0,0,0,'pt')),
          axis.text.x = element_text(size = 12,
                                     angle = 40,
                                     hjust = 1),
          legend.position = 'none',
          plot.margin = margin(0,0,0,0,'pt'),
          panel.grid = element_blank())
  
  p_age = ggplot(data = data_counts_age,
             aes(x = model,
                 y = n_winning,
                 fill = model,
                 size = size)) +
    scale_fill_manual(values = custom_guides) +
    scale_size_manual(values = c(0,1)) +
    geom_col(color = 'black') +
    facet_wrap(~group)
  p_age = Neurocodify_plot(p_age) +
    theme(panel.grid = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(color = 'transparent'),
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 12, margin = margin(0,2.5,0,0,'pt')),
          axis.line.x = element_line(color = 'transparent'),
          axis.ticks.x = element_line(color = 'transparent'),
          legend.position = 'none',
          plot.margin = margin(0,0,0,0,'pt'),
          strip.text = element_text(size = 12))
  
  p = cowplot::plot_grid(p_all, p_age,
                         rel_widths = c(1,1),
                         ncol = 2,
                         axis = 'bl',
                         align = 'hv')
  
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
  # Sort model levels by number of parameters
  data$model = factor(data$model, levels = c('rw',
                                             'uncertainty',
                                             'surprise',
                                             'uncertainty_surprise'))
  
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
    .[, ':='(AICc_SmRW = AICc_surprise - AICc_rw,
             AICc_UmRW = AICc_uncertainty - AICc_rw,
             AICc_USmRW = AICc_uncertainty_surprise - AICc_rw),
      by = c('participant_id', 'group')] %>%
    data.table::melt(id.vars = c('participant_id', 'group'),
                     measure.vars = c('AICc_SmRW',
                                      'AICc_UmRW',
                                      'AICc_USmRW')) %>%
    Prepare_data_for_plot(.)
  levels(data_model_comp_rw$variable) = c('Surprise', 'Uncertainty', 'Combined')
  
  data_model_comp_rw_mean = data_model_comp_rw %>%
    .[, .(value =mean(value),
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
               size = 0.5) +
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
    # limit = -50 removes two outtliers in uncertainty & combined
    scale_y_reverse(limits = c(13,-50),
                    breaks = seq(10,-50,-10))
  p = Neurocodify_plot(p) +
    coord_capped_flip(left = 'both',
                      bottom = 'both') +
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
  p
  
  # Return plot
  return(p)
  
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
  # Sort model levels by number of parameters
  data$model = factor(data$model, levels = c('rw',
                                             'uncertainty',
                                             'surprise',
                                             'uncertainty_surprise'))
  
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
  levels(data_counts$model) = c('RW', 'Uncertainty', 'Surprise', 'Combined')
  
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
  
  pxp = bmsR::VB_bms(cbind(data_ep$rw,
                           data_ep$uncertainty,
                           data_ep$surprise,
                           data_ep$uncertainty_surprise),
                     n_samples = 100000)$pxp
  model_names = c('RW', 'Uncertainty', 'Surprise', 'Combined')
  data_plot = data.table(cbind(model_names,
                                  pxp)) %>%
    .[, pxp := as.numeric(pxp)] %>%
    .[, model_names := factor(model_names, levels = c('RW', 'Uncertainty', 'Surprise', 'Combined'))] %>%
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
    labs(y = 'Protected exceedance\nprobability')
  p = Neurocodify_plot(p) +
    theme(axis.title.x = element_blank(),
          axis.text.y = element_text(size = 12, margin = margin(0,2.5,0,10, 'pt')),
          axis.title.y = element_text(size = 15,
                                      face = 'bold',
                                      margin = margin(0,0,0,0,'pt')),
          axis.text.x = element_text(size = 12,
                                     angle = 40,
                                     hjust = 1),
          legend.position = 'none',
          plot.margin = margin(0,0,0,0,'pt'),
          panel.grid = element_blank())

    
  
  # Return plot
  return(p)
  
}


# Figure_mc_uml_groups = function(){
#   
#   # Get directory of repository
#   base_path = here::here()
#   
#   # Load pre-written functions
#   source_path = file.path(base_path, 'code', 'utils',
#                           fsep = .Platform$file.sep)
#   source_files = list.files(source_path, pattern = "[.][rR]$",
#                             full.names = TRUE, recursive = TRUE)
#   invisible(lapply(source_files, function(x) source(x)))
#   
#   source_path = file.path(base_path, 'code', 'model_fitting', 'LRfunction.R',
#                           fsep = .Platform$file.sep)
#   source(source_path)
#   
#   
#   # Get plot colors/linetypes
#   custom_guides = Get_plot_guides()
#   
#   # Load modelling results
#   data = Load_model_fits_new() %>%
#     .[starting_values == 'random'] %>%
#     Apply_exclusion_criteria(., choice_based_exclusion = TRUE) %>%
#     .[, ':='(participant_id = as.factor(participant_id),
#              group = as.factor(group),
#              sex = as.factor(sex),
#              starting_values = as.factor(starting_values))]
#   # Sort model levels by number of parameters
#   data$model = factor(data$model, levels = c('rw',
#                                              'uncertainty',
#                                              'surprise',
#                                              'uncertainty_surprise'))
#   
#   # get coefficients of winning model
#   data_surprise = data[model == 'surprise' & variable == 'coefs']
#   data_surprise[x == '(Intercept)']$x = 'intercept'
#   
#   data_param_corr = data_surprise %>%
#     data.table::dcast(participant_id + group + model + AIC + AICc ~ paste0('param_', x),
#                       value.var = 'value')
#   
#   data_surprise_param = data_param_corr %>%
#     .[, param_uml := param_u - param_l] %>%
#     .[, param_uml_dicho := as.factor(param_u > param_l)] %>%
#     Prepare_data_for_plot(.)
#   levels(data_surprise_param$param_uml_dicho) = c('Decreased', 'Increased')
#   
#   
#   p = ggplot(data = data_surprise_param,
#              aes(x = param_uml_dicho,
#                  fill = group)) +
#     geom_bar(position = 'dodge') +
#     scale_fill_manual(values = custom_guides) +
#     labs(y = 'Number of participants',
#          x = 'Updating from\nhigh surprise')
#   p = Neurocodify_plot(p) +
#     theme(axis.title.x = element_text(size = 15,
#                                       face = 'bold',
#                                       margin = margin(10,0,0,0,'pt')),
#           axis.text.x = element_text(size = 12, margin = margin(5,0,0,0, 'pt')),
#           axis.text.y = element_text(size = 12, margin = margin(0,2.5,0,10, 'pt')),
#           axis.title.y = element_text(size = 15,
#                                       face = 'bold',
#                                       margin = margin(0,0,0,0,'pt')),
#           legend.position = 'right',
#           legend.title = element_blank(),
#           legend.spacing.y = unit(10, 'pt'),
#           legend.text = element_text(size = 12),
#           legend.key.height = unit(20, 'pt'),
#           legend.key.width = unit(20, 'pt'),
#           plot.margin = margin(0,0,0,0,'pt'),
#           panel.grid = element_blank()) +
#     guides(fill = guide_legend(byrow = TRUE))
# 
#   
#   return(p)
#   
# }
# 
# 
# Figure_mc_params = function(){
#   
#   # Get directory of repository
#   base_path = here::here()
#   
#   # Load pre-written functions
#   source_path = file.path(base_path, 'code', 'utils',
#                           fsep = .Platform$file.sep)
#   source_files = list.files(source_path, pattern = "[.][rR]$",
#                             full.names = TRUE, recursive = TRUE)
#   invisible(lapply(source_files, function(x) source(x)))
#   
#   source_path = file.path(base_path, 'code', 'model_fitting', 'LRfunction.R',
#                           fsep = .Platform$file.sep)
#   source(source_path)
#   
#   
#   # Get plot colors/linetypes
#   custom_guides = Get_plot_guides()
#   
#   # Load modelling results
#   data = Load_model_fits_new() %>%
#     .[starting_values == 'random'] %>%
#     Apply_exclusion_criteria(., choice_based_exclusion = TRUE) %>%
#     .[, ':='(participant_id = as.factor(participant_id),
#              group = as.factor(group),
#              sex = as.factor(sex),
#              starting_values = as.factor(starting_values))]
#   # Sort model levels by number of parameters
#   data$model = factor(data$model, levels = c('rw',
#                                              'uncertainty',
#                                              'surprise',
#                                              'uncertainty_surprise'))
#   
#   # get coefficients of winning model
#   data_surprise = data[model == 'surprise' & variable == 'coefs']
#   data_surprise[x == '(Intercept)']$x = 'intercept'
#   
#   data_param_corr = data_surprise %>%
#     data.table::dcast(participant_id + group + model + AIC + AICc ~ paste0('param_', x),
#                       value.var = 'value')
#   
#   data_surprise_param = data_param_corr %>%
#     .[, param_uml := param_u - param_l] %>%
#     .[, param_uml_dicho := as.factor(param_u > param_l)] %>%
#     Prepare_data_for_plot(.) %>%
#     data.table::melt(id.vars = c('participant_id', 'group', 'model', 'AICc'),
#                      measure.vars = c('param_uml', 'param_s'))
#   levels(data_surprise_param$variable) = c(latex2exp::TeX('u \u2212 l'),
#                                            latex2exp::TeX('s'))
#   
#   p = ggplot(data = data_surprise_param,
#              aes(x = group,
#                  y = value,
#                  fill = group,
#                  color = group)) +
#     scale_color_manual(values = custom_guides) +
#     scale_fill_manual(values = custom_guides) +
#     geom_hline(yintercept = 0,
#                size = 0.5) +
#     geom_point(data = data_surprise_param[group == 'Older\nadults'],
#                alpha = 0.5,
#                position = sdamr::position_jitternudge(jitter.width = 0.2,
#                                           jitter.height = 0,
#                                           nudge.x = -0.2,
#                                           nudge.y = 0,
#                                           seed = 666)) +
#     geom_boxplot(data = data_surprise_param[group == 'Older\nadults'],
#                  width = 0.2,
#                  color = 'black',
#                  position = position_nudge(x = -0.2,
#                                            y = 0),) +
#     stat_summary(data = data_surprise_param[group == 'Older\nadults'],
#                  fun = 'mean',
#                  geom = 'point',
#                  shape = 23,
#                  size = 3,
#                  fill = 'white',
#                  color = 'black',
#                  stroke = 1,
#                  position = position_nudge(x = -0.2,
#                                            y = 0)) +
#     gghalves::geom_half_violin(data = data_surprise_param[group == 'Older\nadults'],
#                                side = 'l',
#                                position = position_nudge(x = 0.5,
#                                                          y = 0),
#                                width = 0.6,
#                                alpha = 0.7,
#                                color = NA) +
#     geom_point(data = data_surprise_param[group == 'Younger\nadults'],
#                alpha = 0.5,
#                position = sdamr::position_jitternudge(jitter.width = 0.2,
#                                                       jitter.height = 0,
#                                                       nudge.x = 0.2,
#                                                       nudge.y = 0,
#                                                       seed = 666)) +
#     geom_boxplot(data = data_surprise_param[group == 'Younger\nadults'],
#                  width = 0.2,
#                  color = 'black',
#                  position = position_nudge(x = 0.2,
#                                            y = 0),) +
#     stat_summary(data = data_surprise_param[group == 'Younger\nadults'],
#                  fun = 'mean',
#                  geom = 'point',
#                  shape = 23,
#                  size = 3,
#                  fill = 'white',
#                  color = 'black',
#                  stroke = 1,
#                  position = position_nudge(x = 0.2,
#                                            y = 0)) +
#     gghalves::geom_half_violin(data = data_surprise_param[group == 'Younger\nadults'],
#                                side = 'r',
#                                position = position_nudge(x = -0.5,
#                                                          y = 0),
#                                width = 0.6,
#                                alpha = 0.7,
#                                color = NA) +
#     
#     labs(y = 'Parameter value') +
#     facet_wrap(~variable,
#                scales = 'free_y')
#   
#   p = Neurocodify_plot(p) +
#     theme(axis.title.x = element_blank(),
#           axis.text.x = element_text(size = 12, margin = margin(5,0,0,0, 'pt')),
#           axis.text.y = element_text(size = 12, margin = margin(0,2.5,0,10, 'pt')),
#           axis.title.y = element_text(size = 15,
#                                       face = 'bold',
#                                       margin = margin(0,0,0,0,'pt')),
#           legend.position = 'none',
#           plot.margin = margin(0,0,0,0,'pt'),
#           panel.grid = element_blank(),
#           strip.text = element_text(size = 18))
#   
#   return(p)
#   
#   
# }
# 
# 
# Figure_mc_lrf = function(){
#   
#   # Get directory of repository
#   base_path = here::here()
#   
#   # Load pre-written functions
#   source_path = file.path(base_path, 'code', 'utils',
#                           fsep = .Platform$file.sep)
#   source_files = list.files(source_path, pattern = "[.][rR]$",
#                             full.names = TRUE, recursive = TRUE)
#   invisible(lapply(source_files, function(x) source(x)))
#   
#   source_path = file.path(base_path, 'code', 'model_fitting', 'LRfunction.R',
#                           fsep = .Platform$file.sep)
#   source(source_path)
#   
#   
#   # Get plot colors/linetypes
#   custom_guides = Get_plot_guides()
#   
#   # Load modelling results
#   data = Load_model_fits_new() %>%
#     .[starting_values == 'random'] %>%
#     Apply_exclusion_criteria(., choice_based_exclusion = TRUE) %>%
#     .[, ':='(participant_id = as.factor(participant_id),
#              group = as.factor(group),
#              sex = as.factor(sex),
#              starting_values = as.factor(starting_values))]
#   # Sort model levels by number of parameters
#   data$model = factor(data$model, levels = c('rw',
#                                              'uncertainty',
#                                              'surprise',
#                                              'uncertainty_surprise'))
#   
#   # get coefficients of winning model
#   data_surprise = data[model == 'surprise' & variable == 'coefs']
#   data_surprise[x == '(Intercept)']$x = 'intercept'
#   
#   data_param_corr = data_surprise %>%
#     data.table::dcast(participant_id + group + model + AIC + AICc ~ paste0('param_', x),
#                       value.var = 'value')
#   
#   data_surprise_param = data_param_corr %>%
#     .[, param_uml := param_u - param_l] %>%
#     .[, param_uml_dicho := as.factor(param_u > param_l)]
#   
#   # Get individual LR functions for surprise model
#   data_lrs = data[model == 'surprise' & variable == 'LRs' & !is.na(value)] %>%
#     .[, .SD, .SDcols = c('participant_id', 'group', 'model', 'AICc', 'variable',
#                          'x', 'value')] %>%
#     # Fuse encountered LRs with uml
#     data.table::merge.data.table(., data_surprise_param,
#                                  by = c('participant_id', 'group', 'model', 'AICc')) %>%
#     Prepare_data_for_plot(.)
#   
#   
#   p1 = ggplot(data = data_lrs[group == 'Older\nadults'],
#               aes(x = as.numeric(x),
#                   y = value,
#                   color = group,
#                   group = participant_id)) +
#     scale_color_manual(values = custom_guides) +
#     scale_x_continuous(limits = c(0,60)) +
#     scale_y_continuous(limits = c(0,1)) +
#     geom_line(alpha = 0.8) +
#     labs(y = latex2exp::TeX('$\\alpha*$'),
#          x = '|PE|',
#          title = 'Older adults') +
#     facet_wrap(~param_uml_dicho) +
#     coord_fixed(ratio = 1)
#   p1 = Neurocodify_plot(p1) +
#     theme(axis.title.x = element_text(size = 15,
#                                       face = 'bold',
#                                       margin = margin(10,0,0,0,'pt')),
#           axis.text.x = element_text(size = 12, margin = margin(5,0,0,0, 'pt')),
#           axis.text.y = element_text(size = 12, margin = margin(0,2.5,0,10, 'pt')),
#           axis.title.y = element_text(size = 18,
#                                       face = 'bold',
#                                       margin = margin(0,0,0,0,'pt')),
#           legend.position = 'none',
#           plot.margin = margin(0,10,0,0,'pt'),
#           panel.grid = element_blank(),
#           strip.text = element_blank(),
#           plot.title = element_text(size = 18,
#                                     hjust = 0.5,
#                                     face = 'bold'))
#     
#   p1
#   
#   p2 = ggplot(data = data_lrs[group == 'Younger\nadults'],
#               aes(x = as.numeric(x),
#                   y = value,
#                   color = group,
#                   group = participant_id)) +
#     scale_color_manual(values = custom_guides) +
#     scale_x_continuous(limits = c(0,60)) +
#     scale_y_continuous(limits = c(0,1)) +
#     geom_line(alpha = 0.8) +
#     labs(y = latex2exp::TeX('$\\alpha*$'),
#          x = '|PE|',
#          title = 'Younger adults') +
#     facet_wrap(~param_uml_dicho) +
#     coord_fixed(ratio = 1)
#   p2 = Neurocodify_plot(p2) +
#     theme(axis.title.x = element_text(size = 15,
#                                       face = 'bold',
#                                       margin = margin(10,0,0,0,'pt')),
#           axis.text.x = element_text(size = 12, margin = margin(5,0,0,0, 'pt')),
#           axis.text.y = element_blank(),
#           axis.title.y = element_blank(),
#           axis.line.y = element_line(color = 'transparent'),
#           axis.ticks.y = element_blank(),
#           legend.position = 'none',
#           plot.margin = margin(0,10,0,0,'pt'),
#           panel.grid = element_blank(),
#           strip.text = element_blank(),
#           plot.title = element_text(size = 18,
#                                     hjust = 0.5,
#                                     face = 'bold'))
#   
#   p2
#   
#   p = cowplot::plot_grid(p1,p2,
#                          rel_widths = c(1,1),
#                          ncol = 2,
#                          axis = 'bl',
#                          align = 'hv')
#   
#   return(p)
#   
#   
# }


Figure_model_comp = function(){
  
  # Create all sub figures
  p_nwin = Figure_mc_nwinning()
  p_aic = Figure_mc_rel_aic()
  p_pxp = Figure_mc_pxp()
  # p_para = Figure_mc_params()
  # p_uml = Figure_mc_uml_groups()
  # p_lrf = Figure_mc_lrf()

  # Adjust plots
  p_nwin = p_nwin +
    theme(plot.margin = margin(0,20,0,30,'pt'))
  p_pxp = p_pxp +
    theme(plot.margin = margin(0,10,0,30,'pt'))
  
  
  p1 = cowplot::plot_grid(p_nwin, p_pxp,
                          rel_widths = c(1,0.6),
                          ncol = 2,
                          #axis = 'tlr',
                          #align = 'v',
                          labels = c('A', 'B'),
                          label_size = 25,
                          label_y = 1.03,
                          label_x = -0.022)
  
  
  
  
  p_aic = p_aic +
    theme(plot.margin = margin(30,0,0,30,'pt'))
  
  p2 = cowplot::plot_grid(NULL, p_aic, NULL,
                          rel_widths = c(0.2,1,0.2),
                          ncol = 3,
                          axis = 'b',
                          align = 'h',
                          labels = c('', 'C', ''),
                          label_size = 25,
                          label_y = 1,
                          label_x = -0.02)
  
  p_model_comp = cowplot::plot_grid(p1,p2,
                          rel_heights = c(1,0.7),
                          nrow = 2,
                          axis = 'l',
                          align = 'h')

  
  
  
  return(p_model_comp)
  
}