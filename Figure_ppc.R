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

Figure_ppc = function(){
  
  # Get directory of repository
  base_path = here::here()
  
  # Load pre-written functions
  source_path = file.path(base_path, 'code', 'utils',
                          fsep = .Platform$file.sep)
  source_files = list.files(source_path, pattern = "[.][rR]$",
                            full.names = TRUE, recursive = TRUE)
  invisible(lapply(source_files, function(x) source(x)))
  
  # Load plotting function of cbe
  source_path = file.path(base_path, 'code', 'figures', 'Figure_behav_ried.R',
                          fsep = .Platform$file.sep)
  source(source_path)
  
  
  # Get color schemes
  custom_guides = Get_plot_guides()
  
  # Get real data plot for reference
  p_cbe = Figure_behav_rie() +
    labs(y = 'Proportion of Mid choices\nin Low-Mid trials',
         x = 'Relative position of Low-Mid trial',
         title = 'Behavior of sample') +
    facet_wrap(~., labeller = as_labeller(c(' '))) +
    theme(plot.title = element_text(size = 15,
                                    face = 'bold',
                                    hjust = 0.5),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 12,
                                      face = 'bold',
                                      margin = margin(10,0,0,0,'pt')),
          strip.text = element_text(size = 12,
                                    face = 'bold',
                                    color = 'transparent'))
    
  # Load modeldata (from model fitting)
  files_pattern = file.path(base_path, 'derivatives', 'model_fitting', 'modeldata-*_sv-random.tsv',
                            fsep = .Platform$file.sep)
  files = Sys.glob(files_pattern)
  # Function to load text files (.tsv)
  Load_tsv = function(file_path){
    tsv = data.table::fread(file_path,
                            sep = '\t',
                            na.strings = 'n/a')
    return(tsv)
  }
  # Get list of all text files using parallel processing
  data_list = parallel::mclapply(X = files,
                                 FUN = Load_tsv,
                                 mc.cores = 4)
  # Bind individual list entries (loaded text files) to single data frame
  # (using optimized bind function by data.table package)
  modeldata_fit = data.table::rbindlist(data_list)
  
  
  # Load windowrized simulation data
  file_pattern = file.path(base_path, 'derivatives', 'posterior_pred_checks', 
                           paste0('windowrizepred-*.tsv'),
                           fsep = .Platform$file.sep)
  files = Sys.glob(file_pattern)
  window_data_run = data.table::data.table()
  for(file in files){
    temp = data.table::fread(file, sep = '\t', na.strings = 'n/a')
    window_data_run = rbind(window_data_run, temp)
  }
  
  # Add group information
  groupinfo = modeldata_fit[, .(group = unique(group)),
                            by = c('participant_id')]
  window_data_run = data.table::merge.data.table(window_data_run,
                                                 groupinfo)
  
  # Get mean across runs
  window_data_participant = window_data_run %>%
    .[, .(accuracy = mean(mean_accuracy, na.rm = TRUE)),
      by = c('participant_id', 'group', 'model', 'window_relative')]
  
  # Get age group specific mean and sd
  window_data_group = window_data_participant %>%
    .[, .(mean_accuracy = mean(accuracy, na.rm = TRUE),
          sem_accuracy = sd(accuracy, na.rm = TRUE)/sqrt(.N)),
      by = c('group', 'model', 'window_relative')] %>%
    .[order(rank(group), rank(model), rank(window_relative)),]
  
  # Prepare data for plot
  data_plot = Prepare_data_for_plot(window_data_participant[window_relative != 0,]) %>%
    .[, group := factor(group, levels = rev(levels(group)))] %>%
    .[model %in% c('Valence', 'Surprise')]
  data_plot_mean = Prepare_data_for_plot(window_data_group[window_relative != 0,]) %>%
    .[, group := factor(group, levels = rev(levels(group)))] %>%
    .[model %in% c('Valence', 'Surprise')]
  
  dodge_width = 0.4
  
  # Plot windowrized data
  p_cbe_sim = ggplot(data = data_plot,
             aes(x = window_relative,
                 y = accuracy,
                 color = group)) +
    # Add data points of individual participant
    geom_point(position = position_jitterdodge(jitter.width = 0.1,
                                               jitter.height = 0,
                                               dodge.width = dodge_width,
                                               seed = 666),
               size = 0.5,
               alpha = 0.5) +
    # Dashed horizontal line
    geom_vline(xintercept = 0,
               linetype = 'dashed') +
    # Line connecting groups
    geom_path(data = data_plot_mean,
              linewidth = 1,
              position = position_dodge(dodge_width),
              aes(y = mean_accuracy,
                  group = group)) +
    # Errorbars (SEM) of group means
    geom_errorbar(data = data_plot_mean,
                  inherit.aes = FALSE,
                  aes(x = window_relative,
                      ymin = mean_accuracy - sem_accuracy,
                      ymax = mean_accuracy + sem_accuracy,
                      group = group),
                  size = 0.5,
                  color = 'black',
                  width = dodge_width/2,
                  position = position_dodge(width = dodge_width)) +
    # Group means
    geom_point(data = data_plot_mean,
               aes(y = mean_accuracy,
                   fill = group),
               shape = 21,
               size = 2.5,
               color = 'black',
               position = position_dodge(width = dodge_width)) +
    # Textbox
    ggtext::geom_richtext(data = rbind(data_plot[model == 'Valence'][1,],
                                       data_plot[model == 'Surprise'][1,]),
                          x = 0,
                          y = 0.3,
                          label = 'Surprising outcome<br>in Mid bandit',
                          fill = 'white',
                          color = 'black',
                          angle = 90,
                          size = 3.5,
                          hjust = 0.5) +
    labs(y = 'Proportion of Mid choices\nin Low-Mid trials',
         x = 'Relative position of Low-Mid trial',
         title = 'Behavior simulated with models') +
    # Scale group colors according to predefined guides
    scale_color_manual(values = custom_guides) +
    scale_fill_manual(values = custom_guides) +
    scale_y_continuous(limits = c(0,1)) +
    facet_wrap(~model) +
    theme(legend.position = 'none')
  
  p_cbe_sim = Neurocodify_plot(p_cbe_sim) +
    theme(panel.spacing.x = unit(30, 'pt'),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          strip.text = element_text(size = 12,
                                    face = 'plain'),
          plot.title = element_text(size = 15,
                                    face = 'bold',
                                    hjust = 0.5),
          axis.text = element_text(size = 12),
          axis.title.x = element_text(size = 12,
                                      face = 'bold',
                                      margin = margin(10,0,0,0,'pt')),
          axis.title.y = element_text(size = 12,
                                      face = 'bold',
                                      margin = margin(0,10,0,0,'pt')))
  
  p_cbe_comp = cowplot::plot_grid(p_cbe_sim, NULL, p_cbe,
                                  ncol = 3,
                                  rel_widths = c(2,0.2,0.8),
                                  align = 'h',
                                  axis = 'tb')
  
  # Age comparison of effect
  # Real data
  # Load real behavioral data
  file = file.path(base_path, 'derivatives', 'analysis', 'windowrize.tsv',
                   fsep = .Platform$file.sep)
  window_real = data.table::fread(file, sep = '\t', na.strings = 'n/a')
  # Summarize data
  window_real_run = window_real %>%
    # Get mean accuracy across all relative window positions (-2 to +3)
    .[, .(mean_accuracy = mean(correct_choice, na.rm = TRUE),
          n_data = sum(!is.na(correct_choice))),
      by = c('participant_id', 'group', 'run', 'window_relative')]
  # Get mean across runs
  window_real_participant = window_real_run %>%
    .[, .(accuracy = mean(mean_accuracy, na.rm = TRUE)),
      by = c('participant_id', 'group', 'window_relative')] %>%
    # add faux model grouping for easier data table merge
    .[, model := 'real_data']
  
  # Simulated data
  # Same summary for predicted data
  window_model_participant = window_data_run %>%
    .[, .(accuracy = mean(mean_accuracy, na.rm = TRUE)),
      by = c('participant_id', 'group', 'model', 'window_relative')] %>%
    # Restrict model to pxp-relevant models
    .[model %in% c('seplr', 'surprise')]

  # Merge real and model data
  data_cbe_comp = rbind(window_real_participant, window_model_participant) %>%
    # restrict data to pre/post
    .[window_relative %in% c(-1, 1)] %>%
    # Convert id vars to factors
    .[, c('participant_id', 'group', 'model') := lapply(.SD, factor),
      .SDcols = c('participant_id', 'group', 'model')] %>%
    # rename relative window to pre and post for wide format
    .[, window_relative := as.character(window_relative)] %>%
    .[window_relative == -1, window_relative := 'pre'] %>%
    .[window_relative == 1, window_relative := 'post'] %>%
    # wide format for pre and post accuracy
    data.table::dcast(participant_id + group + model ~ window_relative,
                      value.var = 'accuracy') %>%
    # Get critical behavioral effect (pre minus post)
    .[, cbe := pre - post] %>%
    # wide format for cbe
    data.table::dcast(participant_id + group ~ paste0('cbe_', model), value.var = 'cbe')
  
  # CBE for all types (surprise, seplr, real data) in long format 
  data_cbe_comp_all = data_cbe_comp %>%
    data.table::melt(id.vars = c('participant_id', 'group'))
  
  # Plot for simulated data
  data_plot = Prepare_data_for_plot(data_cbe_comp_all) %>%
    # Select simulated data
    .[, variable := factor(variable, labels = c('Real data',
                                                'Valence',
                                                'Surprise'))]
  
  # Plot for simulated data
  p_sim = ggplot(data = data_plot[variable %in% c('Valence', 'Surprise')],
             aes(x = group,
                 y = value,
                 color = group,
                 fill = group)) +
    scale_fill_manual(values = custom_guides) +
    scale_color_manual(values = custom_guides) +
    # Line at 0
    geom_hline(yintercept = 0,
               linewidth = 0.5) +
    # Plot data
    geom_point(position = position_jitter(width = 0.1,
                                          height = 0,
                                          seed = 666),
               alpha = 0.5) +
    geom_boxplot(outlier.shape = NA,
                 color = 'black',
                 alpha = 0.5,
                 width = 0.4) +
    # Summary statistics (mean as diamond and line)
    stat_summary(fun = mean,
                 geom = 'path',
                 group = 'variable',
                 color = 'black',
                 linewidth = 1) +
    stat_summary(fun = mean,
                 geom = 'point',
                 color = 'black',
                 fill = 'white',
                 shape = 23,
                 stroke = 1.5,
                 size = 3) +
    labs(y = 'Difference pre-post\nsurprising outcome') +
    scale_y_continuous(limits = c(-0.5,1)) +
    # Separate plot by model
    facet_wrap(~variable)
  p_sim = Neurocodify_plot(p_sim) +
    theme(panel.grid = element_blank(),
          legend.position = 'none',
          panel.spacing.x = unit(30, 'pt'),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.y = element_text(size = 12, 
                                      face = 'bold'),
          axis.title.x = element_blank(),
          axis.text = element_text(size = 12),
          strip.text = element_text(size = 12,
                                    face = 'plain'))
  
  # Plot real data
  p_real = ggplot(data = data_plot[variable %in% c('Real data')],
                 aes(x = group,
                     y = value,
                     color = group,
                     fill = group)) +
    scale_fill_manual(values = custom_guides) +
    scale_color_manual(values = custom_guides) +
    # Line at 0
    geom_hline(yintercept = 0,
               linewidth = 0.5) +
    # Plot data
    geom_point(position = position_jitter(width = 0.1,
                                          height = 0,
                                          seed = 666),
               alpha = 0.5) +
    geom_boxplot(outlier.shape = NA,
                 color = 'black',
                 alpha = 0.5,
                 width = 0.4) +
    # Summary statistics (mean as diamond and line)
    stat_summary(fun = mean,
                 geom = 'path',
                 group = 'variable',
                 color = 'black',
                 linewidth = 1) +
    stat_summary(fun = mean,
                 geom = 'point',
                 color = 'black',
                 fill = 'white',
                 shape = 23,
                 stroke = 1.5,
                 size = 3) +
    scale_y_continuous(limits = c(-0.5,1)) +
    # Separate plot by model
    facet_wrap(~.)
  p_real = Neurocodify_plot(p_real) +
    theme(panel.grid = element_blank(),
          legend.position = 'none',
          panel.spacing.x = unit(30, 'pt'),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title = element_blank(),
          axis.text = element_text(size = 12),
          strip.text = element_text(size = 12,
                                    face = 'plain',
                                    color = 'transparent'),
          plot.margin = margin(0,12,0,0,'pt'))
  
  
  p_comp = cowplot::plot_grid(p_sim, NULL, p_real,
                                  ncol = 3,
                                  rel_widths = c(2,0.2,0.8),
                                  align = 'h',
                                  axis = 'tb')
  
  p_legend = Figure_behav_rie() +
    theme(legend.position = 'bottom',
          legend.title = element_blank(),
          legend.key = element_rect(fill = NA,
                                    color = NA),
          legend.text = element_text(size = 12),
          legend.direction = 'horizontal')
  legend = cowplot::get_legend(p_legend)
  
  p = cowplot::plot_grid(p_cbe_comp, legend, NULL, p_comp,
                         nrow = 4,
                         rel_heights = c(4.5,0.5,0.4,4),
                         align = 'h',
                         axis = 'lr',
                         labels = c('A', '', '', 'B'),
                         label_size = 25,
                         label_x = -0.02,
                         label_y = 1.04) +
    theme(plot.margin = margin(5,40,5,5,'pt'))
  
  return(p)
  
}

