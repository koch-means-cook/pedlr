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
library(latex2exp)
library(grid)
library(gridExtra)
library(ggcorrplot)
library(corrr)
library(stringr)


Figure_model_recov = function(){
  
  base_path = here::here()
  
  # Load pre-written functions
  source_path = file.path(base_path, 'code', 'utils',
                          fsep = .Platform$file.sep)
  source_files = list.files(source_path, pattern = "[.][rR]$",
                            full.names = TRUE, recursive = TRUE)
  invisible(lapply(source_files, function(x) source(x)))
  
  # Get color schemes
  custom_guides = Get_plot_guides()
  
  # Glob files based on naming pattern
  load_path = file.path(here::here(),
                        'derivatives',
                        'parameter_recovery',
                        fsep = .Platform$file.sep)
  # Load data with fixed betas during simulation
  files = Sys.glob(file.path(load_path,
                             'modelrecov_base-*_randips-TRUE_randbetas-FALSE_randsvs-TRUE.tsv',
                             fsep = .Platform$file.sep))
  
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
  data = data.table::rbindlist(data_list) %>%
    # Rename to avoid 'x' as column name
    data.table::setnames(., old = 'x', new = 'params')
  
  # Prepare data
  data_recovery = data %>%
    # For each model that generated data, get AICc for all fitted models
    .[, .(AICc = unique(AICc)),
      by = c('participant_id',
             'generating_model',
             'iter',
             'model',
             'generating_x1',
             'generating_x2',
             'generating_x3',
             'generating_x4')] %>%
    # Get winning model of recovery (lowest AICc)
    .[, recovered_model := model[which(AICc == min(AICc))],
      by = c('participant_id',
             'generating_model',
             'iter')] %>%
    # Get match between generating/recovered model
    .[, correct_recovery := generating_model == recovered_model] %>%
    # Factorize variables and sort factor levels in matching manner
    .[, c('generating_model', 'recovered_model') := lapply(.SD, factor),
      .SDcols = c('generating_model', 'recovered_model')] %>%
    .[, recovered_model := factor(recovered_model, levels = levels(generating_model))]
  
  # Plot recovery as histogram
  data_hist = data_recovery %>%
    # Reduce to row for each attempted recovery
    .[, .(recovered_model = unique(recovered_model)),
      by = c('participant_id', 'generating_model', 'iter')] %>%
    # Get matching model for each generative model to mark it in plot
    .[, correct_model := as.character(recovered_model) == as.character(generating_model)] %>%
    # Adjust names
    Prepare_data_for_plot(.)
  
  # Plot
  p_hist = ggplot(data = data_hist,
             aes(x = recovered_model,
                 color = correct_model)) +
    scale_color_manual(values = c('transparent', 'black')) +
    scale_y_continuous(breaks = c(0,400)) +
    geom_bar(fill = 'lightgrey') +
    labs(x = 'Fit model') + 
    facet_grid(generating_model ~ .,
               switch = 'y')
  p_hist = Neurocodify_plot(p_hist) +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(size = 12,
                                     angle = 45,
                                     hjust = 1,
                                     vjust = 1),
          axis.text.y = element_text(size = 10),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 15,
                                      face = 'bold',
                                      margin = margin(5,0,0,0,'pt')),
          strip.placement = 'outside',
          strip.text.y.left = element_text(size = 12,
                                           face = 'plain',
                                           angle = 0,
                                           hjust = 1),
          legend.position = 'none')
  
  # Plot recovery as CM
  data_cm = data_recovery %>%
    # Reduce to row for each attempted recovery
    .[, .(recovered_model = unique(recovered_model)),
      by = c('participant_id', 'generating_model', 'iter')] %>%
    .[, .(count = .N),
      by = c('recovered_model', 'generating_model')] %>%
    data.table::setorder('generating_model') %>%
    # Get percentage for recovery
    .[, perc := count/sum(count),
      by = 'generating_model'] %>%
    # Check if percentage within one model sums up to one
    .[, sum_perc := sum(perc),
      by = 'generating_model'] %>%
    # Text fill format e.g. ".37"
    .[, label_short := substr(format(round(perc, 2), nsmall = 2), 2,4)] %>%
    # Color variable to change text color if background is too dark
    .[, is_low := perc < 0.4] %>%
    # Adjust names
    Prepare_data_for_plot(.)
  
  # Plot
  p_cm = ggplot(data = data_cm,
             aes(x = recovered_model,
                 y = generating_model,
                 fill = perc,
                 color = is_low,
                 label = label_short)) +
    geom_tile(color = 'black') +
    geom_text(size = 2.5) +
    labs(x = 'Fit model',
         y = 'Simulated model') +
    theme(panel.background = element_rect(fill = NA,
                                          color = NA),
          panel.grid = element_blank(),
          legend.position = 'none',
          axis.title = element_text(size = 12,
                                    face = 'bold'),
          axis.ticks = element_blank(),
          strip.text = element_blank(),
          strip.background = element_blank(),
          plot.title = element_text(hjust = 0.5,
                                    vjust = 0),
          #axis.text = element_text(size = 12),
          plot.margin = margin(0,0,0,0,'pt')) +
    scale_y_discrete(limits = rev) +
    scale_color_manual(values = c('black', 'white')) +
    scale_fill_gradient(low = 'black', high = 'white')
  p_cm = Neurocodify_plot(p_cm) +
    theme(panel.grid = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_text(size = 12,
                                     angle = 45,
                                     hjust = 1,
                                     vjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 15,
                                      face = 'bold',
                                      margin = margin(0,5,0,0,'pt')),
          axis.title.x = element_text(size = 15,
                                      face = 'bold',
                                      margin = margin(5,0,0,0,'pt')),
          legend.position = 'none')
  
  
  p = cowplot::plot_grid(p_cm, NULL, p_hist,
                         ncol = 3,
                         rel_widths = c(0.95,0.1,1),
                         labels = c('A', '', 'B'),
                         label_size = 25,
                         label_x = -0.05,
                         label_y = 1.045,
                         align = 'h',
                         axis = 'tb') +
    theme(plot.margin = margin(5,0,0,5,'pt'))
  p

  return(p)
}


