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

Figure_sup_des = function(){
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
  
  # get coefficients of winning model
  data_surprise = data[model == 'surprise' & variable == 'coefs'] %>%
    Prepare_data_for_plot(.)
  data_surprise[x == '(Intercept)']$x = 'intercept'
  
  data_param_corr = data_surprise %>%
    data.table::dcast(participant_id + group + model + AIC + AICc ~ paste0('param_', x),
                      value.var = 'value')
  
  data_surprise_param = data_param_corr %>%
    .[, param_uml := param_u - param_l] %>%
    .[, param_uml_dicho := as.factor(param_u > param_l)] %>%
    Prepare_data_for_plot(.)
  levels(data_surprise_param$param_uml_dicho) = c('Decreased', 'Increased')
  
  suprise_cor = cor(data_surprise_param[, .SD, .SDcols = c('param_intercept',
                                                           'param_V1',
                                                           'param_V2',
                                                           'param_l',
                                                           'param_s',
                                                           'param_u')])
  suprise_cor[lower.tri(suprise_cor)] = NA
  suprise_cor = reshape2::melt(suprise_cor, na.rm = TRUE)
  suprise_cor$Var1 = factor(suprise_cor$Var1)
  suprise_cor$Var2 = factor(suprise_cor$Var2)
  levels(suprise_cor$Var1) = c('beta0',
                               'beta1',
                               'beta2',
                               'l',
                               's',
                               'u')
  levels(suprise_cor$Var2) = c('beta0',
                               'beta1',
                               'beta2',
                               'l',
                               's',
                               'u')
  suprise_cor = as.data.table(suprise_cor)
  suprise_cor$equal = suprise_cor$Var1 == suprise_cor$Var2
  #levels(suprise_cor$Var2) = rev(levels(suprise_cor$Var2))
  
  p1 = ggplot(data = suprise_cor,
             aes(x = Var1,
                 y = Var2,
                 fill = value,
                 label = sub("^(-?)0.", "\\1.", sprintf("%.2f", round(value, 2))))) +
    geom_tile() +
    geom_label(data = suprise_cor[!suprise_cor$equal,],
               fill = 'white',
               size = 2.5) +
    viridis::scale_fill_viridis(option = 'D',
                                limits = c(-1,1),
                                breaks = c(-1,0,1)) +
    scale_x_discrete(labels = list('beta0' = bquote(beta[0]),
                                   'beta1' = bquote(beta[1]),
                                   'beta2' = bquote(beta[2]),
                                   'l',
                                   's',
                                   'u')) +
    scale_y_discrete(labels = list('beta0' = bquote(beta[0]),
                                   'beta1' = bquote(beta[1]),
                                   'beta2' = bquote(beta[2]),
                                   'l',
                                   's',
                                   'u'))
  p1 = Neurocodify_plot(p1) +
    theme(panel.grid = element_blank(),
          axis.title = element_blank(),
          legend.title = element_blank(),
          axis.text = element_text(size = 12, hjust = 0.5, vjust = 0.5),
          axis.ticks = element_blank(),
          axis.line = element_line(color = 'transparent'),
          legend.direction = 'horizontal',
          legend.position = c(0.7,0.15),
          legend.key.height = unit(10,'pt'),
          legend.key.width = unit(15,'pt'))
  
  data_plot = data_surprise
  data_plot$x = factor(data_plot$x)
  levels(data_plot$x) = c('beta[0]',
                          'l',
                          's',
                          'u',
                          'beta[1]',
                          'beta[2]')
  data_plot$x = factor(data_plot$x, levels = c('beta[0]',
                                               'beta[1]',
                                               'beta[2]',
                                               'l',
                                               's',
                                               'u'))
  
  p2 = ggplot(data = data_plot[x %in% c('l', 's', 'u')],
              aes(x = value,
                  fill = group)) +
    scale_fill_manual(values = custom_guides,
                      labels = c('Younger',
                                 'Older')) +
    geom_histogram(bins = 10,
                   alpha = 0.6,
                   color = 'black',
                   size = 0.3,
                   position = 'identity') +
    facet_wrap(~x, scales = 'free_x', nrow = 1, labeller = label_parsed) +
    scale_x_continuous(n.breaks = 3) +
    labs(y = 'Count')
  p2 = Neurocodify_plot(p2) +
    theme(legend.position = 'right',
          legend.spacing.x = unit(15, 'pt'),
          legend.title = element_blank(),
          legend.text = element_text(size = 15),
          strip.text = element_text(size = 15),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 15, face = 'bold'),
          axis.text.x = element_text(size = 12,
                                     angle = 45,
                                     hjust = 1),
          axis.text.y = element_text(size = 12),
          panel.spacing.x = unit(20, 'pt'),
          panel.grid = element_blank())
  
  p2 = p2 +
    theme(plot.margin = margin(0,20,0,0,'pt'))
  
  p = cowplot::plot_grid(p2,p1,
                     rel_widths = c(7,3),
                     ncol = 2,
                     axis = 'bt',
                     align = 'h')
  return(p)
  
}

Figure_mc_uml_groups = function(){

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

  # get coefficients of winning model
  data_surprise = data[model == 'surprise' & variable == 'coefs']
  data_surprise[x == '(Intercept)']$x = 'intercept'

  data_param_corr = data_surprise %>%
    data.table::dcast(participant_id + group + model + AIC + AICc ~ paste0('param_', x),
                      value.var = 'value')

  data_surprise_param = data_param_corr %>%
    .[, param_uml := param_u - param_l] %>%
    .[, param_uml_dicho := as.factor(param_u > param_l)] %>%
    Prepare_data_for_plot(.)
  levels(data_surprise_param$param_uml_dicho) = c('Decreased', 'Increased')
  
  suprise_cor = cor(data_surprise_param[, .SD, .SDcols = c('param_intercept',
                                             'param_V1',
                                             'param_V2',
                                             'param_l',
                                             'param_s',
                                             'param_u')])
  suprise_cor[lower.tri(suprise_cor)] = NA
  suprise_cor = reshape2::melt(suprise_cor, na.rm = TRUE)
  suprise_cor$Var1 = factor(suprise_cor$Var1)
  suprise_cor$Var2 = factor(suprise_cor$Var2)
  levels(suprise_cor$Var1) = c('beta0',
                               'beta1',
                               'beta2',
                               'l',
                               's',
                               'u')
  levels(suprise_cor$Var2) = c('beta0',
                               'beta1',
                               'beta2',
                               'l',
                               's',
                               'u')
  suprise_cor = as.data.table(suprise_cor)
  suprise_cor$equal = suprise_cor$Var1 == suprise_cor$Var2
  #levels(suprise_cor$Var2) = rev(levels(suprise_cor$Var2))
  
  p = ggplot(data = suprise_cor,
             aes(x = Var1,
                 y = Var2,
                 fill = value,
                 label = sub("^(-?)0.", "\\1.", sprintf("%.2f", round(value, 2))))) +
    geom_tile() +
    geom_label(data = suprise_cor[!suprise_cor$equal,],
               fill = 'white',
               size = 3) +
    viridis::scale_fill_viridis(option = 'D',
                                limits = c(-1,1),
                                breaks = c(-1,0,1)) +
  scale_x_discrete(labels = list('beta0' = bquote(beta[0]),
                              'beta1' = bquote(beta[1]),
                              'beta2' = bquote(beta[2]),
                              'l',
                              's',
                              'u')) +
    scale_y_discrete(labels = list('beta0' = bquote(beta[0]),
                                   'beta1' = bquote(beta[1]),
                                   'beta2' = bquote(beta[2]),
                                   'l',
                                   's',
                                   'u'))
  Neurocodify_plot(p) +
    theme(panel.grid = element_blank(),
          axis.title = element_blank(),
          legend.title = element_blank(),
          axis.text = element_text(size = 15),
          axis.ticks = element_blank(),
          axis.line = element_line(color = 'transparent'),
          legend.direction = 'horizontal',
          legend.position = c(0.7,0.15))
  
  # p = GGally::ggpairs(data_surprise_param,
  #                     columns = which(colnames(data_surprise_param) %in% c('param_l',
  #                                                                'param_s',
  #                                                                'param_u')),
  #                     mapping = ggplot2::aes(color = group),
  #                     upper = 'blank',
  #                     lower = list(continuous = GGally::wrap('cor', stars = FALSE, color = 'black')),
  #                     diag = list(continuous = GGally::wrap('densityDiag', alpha = 0.7)))
  #   #scale_color_manual(custom_guides)
  # Neurocodify_plot(p) +
  #   scale_fill_manual(values = custom_guides[c(1:2)]) +
  #   scale_color_manual(values = custom_guides[c(1:2)]) +
  #   theme(panel.grid = element_blank(),
  #         axis.line.y = element_line(size = NA),
  #         axis.text.y = element_blank(),
  #         axis.ticks.y = element_blank())


  p = ggplot(data = data_surprise_param,
             aes(x = param_uml_dicho,
                 fill = group)) +
    geom_bar(position = 'dodge') +
    scale_fill_manual(values = custom_guides) +
    labs(y = 'Number of participants',
         x = 'Updating from\nhigh surprise')
  p = Neurocodify_plot(p) +
    theme(axis.title.x = element_text(size = 15,
                                      face = 'bold',
                                      margin = margin(10,0,0,0,'pt')),
          axis.text.x = element_text(size = 12, margin = margin(5,0,0,0, 'pt')),
          axis.text.y = element_text(size = 12, margin = margin(0,2.5,0,10, 'pt')),
          axis.title.y = element_text(size = 15,
                                      face = 'bold',
                                      margin = margin(0,0,0,0,'pt')),
          legend.position = 'right',
          legend.title = element_blank(),
          legend.spacing.y = unit(10, 'pt'),
          legend.text = element_text(size = 12),
          legend.key.height = unit(20, 'pt'),
          legend.key.width = unit(20, 'pt'),
          plot.margin = margin(0,0,0,0,'pt'),
          panel.grid = element_blank()) +
    guides(fill = guide_legend(byrow = TRUE))


  return(p)

}


Figure_mc_params = function(){

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

  # get coefficients of winning model
  data_surprise = data[model == 'surprise' & variable == 'coefs']
  data_surprise[x == '(Intercept)']$x = 'intercept'

  data_param_corr = data_surprise %>%
    data.table::dcast(participant_id + group + model + AIC + AICc ~ paste0('param_', x),
                      value.var = 'value')

  data_surprise_param = data_param_corr %>%
    .[, param_uml := param_u - param_l] %>%
    .[, param_uml_dicho := as.factor(param_u > param_l)] %>%
    Prepare_data_for_plot(.) %>%
    data.table::melt(id.vars = c('participant_id', 'group', 'model', 'AICc'),
                     measure.vars = c('param_uml', 'param_s'))
  levels(data_surprise_param$variable) = c(latex2exp::TeX('u \u2212 l'),
                                           latex2exp::TeX('s'))

  p = ggplot(data = data_surprise_param,
             aes(x = group,
                 y = value,
                 fill = group,
                 color = group)) +
    scale_color_manual(values = custom_guides) +
    scale_fill_manual(values = custom_guides) +
    geom_hline(yintercept = 0,
               size = 0.5) +
    geom_point(data = data_surprise_param[group == 'Older\nadults'],
               alpha = 0.5,
               position = sdamr::position_jitternudge(jitter.width = 0.2,
                                          jitter.height = 0,
                                          nudge.x = -0.2,
                                          nudge.y = 0,
                                          seed = 666)) +
    geom_boxplot(data = data_surprise_param[group == 'Older\nadults'],
                 width = 0.2,
                 color = 'black',
                 position = position_nudge(x = -0.2,
                                           y = 0),) +
    stat_summary(data = data_surprise_param[group == 'Older\nadults'],
                 fun = 'mean',
                 geom = 'point',
                 shape = 23,
                 size = 3,
                 fill = 'white',
                 color = 'black',
                 stroke = 1,
                 position = position_nudge(x = -0.2,
                                           y = 0)) +
    gghalves::geom_half_violin(data = data_surprise_param[group == 'Older\nadults'],
                               side = 'l',
                               position = position_nudge(x = 0.5,
                                                         y = 0),
                               width = 0.6,
                               alpha = 0.7,
                               color = NA) +
    geom_point(data = data_surprise_param[group == 'Younger\nadults'],
               alpha = 0.5,
               position = sdamr::position_jitternudge(jitter.width = 0.2,
                                                      jitter.height = 0,
                                                      nudge.x = 0.2,
                                                      nudge.y = 0,
                                                      seed = 666)) +
    geom_boxplot(data = data_surprise_param[group == 'Younger\nadults'],
                 width = 0.2,
                 color = 'black',
                 position = position_nudge(x = 0.2,
                                           y = 0),) +
    stat_summary(data = data_surprise_param[group == 'Younger\nadults'],
                 fun = 'mean',
                 geom = 'point',
                 shape = 23,
                 size = 3,
                 fill = 'white',
                 color = 'black',
                 stroke = 1,
                 position = position_nudge(x = 0.2,
                                           y = 0)) +
    gghalves::geom_half_violin(data = data_surprise_param[group == 'Younger\nadults'],
                               side = 'r',
                               position = position_nudge(x = -0.5,
                                                         y = 0),
                               width = 0.6,
                               alpha = 0.7,
                               color = NA) +

    labs(y = 'Parameter value') +
    facet_wrap(~variable,
               scales = 'free_y')

  p = Neurocodify_plot(p) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(size = 12, margin = margin(5,0,0,0, 'pt')),
          axis.text.y = element_text(size = 12, margin = margin(0,2.5,0,10, 'pt')),
          axis.title.y = element_text(size = 15,
                                      face = 'bold',
                                      margin = margin(0,0,0,0,'pt')),
          legend.position = 'none',
          plot.margin = margin(0,0,0,0,'pt'),
          panel.grid = element_blank(),
          strip.text = element_text(size = 18))

  return(p)


}


Figure_mc_lrf = function(){

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

  # get coefficients of winning model
  data_surprise = data[model == 'surprise' & variable == 'coefs']
  data_surprise[x == '(Intercept)']$x = 'intercept'

  data_param_corr = data_surprise %>%
    data.table::dcast(participant_id + group + model + AIC + AICc ~ paste0('param_', x),
                      value.var = 'value')

  data_surprise_param = data_param_corr %>%
    .[, param_uml := param_u - param_l] %>%
    .[, param_uml_dicho := as.factor(param_u > param_l)]

  # Get individual LR functions for surprise model
  data_lrs = data[model == 'surprise' & variable == 'LRs' & !is.na(value)] %>%
    .[, .SD, .SDcols = c('participant_id', 'group', 'model', 'AICc', 'variable',
                         'x', 'value')] %>%
    # Fuse encountered LRs with uml
    data.table::merge.data.table(., data_surprise_param,
                                 by = c('participant_id', 'group', 'model', 'AICc')) %>%
    Prepare_data_for_plot(.)


  p1 = ggplot(data = data_lrs[group == 'Older\nadults'],
              aes(x = as.numeric(x),
                  y = value,
                  color = group,
                  group = participant_id)) +
    scale_color_manual(values = custom_guides) +
    scale_x_continuous(limits = c(0,60)) +
    scale_y_continuous(limits = c(0,1)) +
    geom_line(alpha = 0.8) +
    labs(y = latex2exp::TeX('$\\alpha*$'),
         x = 'Surprise (|PE|)',
         title = 'Older adults') +
    facet_wrap(~param_uml_dicho) +
    coord_fixed(ratio = 1)
  p1 = Neurocodify_plot(p1) +
    theme(axis.title.x = element_text(size = 15,
                                      face = 'bold',
                                      margin = margin(10,0,0,0,'pt')),
          axis.text.x = element_text(size = 12, margin = margin(5,0,0,0, 'pt')),
          axis.text.y = element_text(size = 12, margin = margin(0,2.5,0,10, 'pt')),
          axis.title.y = element_text(size = 18,
                                      face = 'bold',
                                      margin = margin(0,0,0,0,'pt')),
          legend.position = 'none',
          plot.margin = margin(0,10,0,0,'pt'),
          panel.grid = element_blank(),
          strip.text = element_blank(),
          plot.title = element_text(size = 18,
                                    hjust = 0.5,
                                    face = 'bold'))

  p2 = ggplot(data = data_lrs[group == 'Younger\nadults'],
              aes(x = as.numeric(x),
                  y = value,
                  color = group,
                  group = participant_id)) +
    scale_color_manual(values = custom_guides) +
    scale_x_continuous(limits = c(0,60)) +
    scale_y_continuous(limits = c(0,1)) +
    geom_line(alpha = 0.8) +
    labs(y = latex2exp::TeX('$\\alpha*$'),
         x = 'Surprise (|PE|)',
         title = 'Younger adults') +
    facet_wrap(~param_uml_dicho) +
    coord_fixed(ratio = 1)
  p2 = Neurocodify_plot(p2) +
    theme(axis.title.x = element_text(size = 15,
                                      face = 'bold',
                                      margin = margin(10,0,0,0,'pt')),
          axis.text.x = element_text(size = 12, margin = margin(5,0,0,0, 'pt')),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.line.y = element_line(color = 'transparent'),
          axis.ticks.y = element_blank(),
          legend.position = 'none',
          plot.margin = margin(0,10,0,0,'pt'),
          panel.grid = element_blank(),
          strip.text = element_blank(),
          plot.title = element_text(size = 18,
                                    hjust = 0.5,
                                    face = 'bold'))

  p = cowplot::plot_grid(p1,p2,
                         rel_widths = c(1,1),
                         ncol = 2,
                         axis = 'bl',
                         align = 'hv')

  return(p)


}


Figure_model_surprise = function(){
  
  # Create all sub figures
  p_des = Figure_sup_des()
  p_para = Figure_mc_params()
  p_uml = Figure_mc_uml_groups()
  p_lrf = Figure_mc_lrf()

  # Adjust plots
  p_des = p_des +
    theme(plot.margin = margin(0,5,30,30,'pt'),
          legend.position = 'right')
  p1 = p_des
  
  p_lrf = p_lrf +
    theme(plot.margin = margin(0,0,30,30,'pt'))
  p2 = p_lrf
  
  p_uml = p_uml +
    theme(plot.margin = margin(0,10,0,30,'pt'),
          legend.position = 'top')
  p_para = p_para +
    theme(plot.margin = margin(0,0,0,30,'pt'))
  
  p3 = cowplot::plot_grid(p_uml, p_para,
                          rel_widths = c(3,5),
                          ncol = 2,
                          axis = 'bt',
                          align = 'h',
                          labels = c('C', 'D'),
                          label_size = 25,
                          label_y = 1,
                          label_x = c(-0.04, 0.02))
  p3
  
  
  p_model_sup = cowplot::plot_grid(p1,p2,p3,
                     rel_heights = c(1,1,1),
                     ncol = 1,
                     labels = c('A', 'B', ''),
                     label_size = 25,
                     label_x = -0.015)
  p_model_sup


  
  
  
  return(p_model_sup)
  
}