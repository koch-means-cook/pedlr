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


Figure_behav_ed = function(){
  
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
  file = file.path(base_path, 'derivatives', 'figures', 'f_ed.tsv',
                   fsep = .Platform$file.sep)
  data = data.table::fread(file, sep = '\t', na.strings = 'n/a') %>%
    .[variable == 'diff_EmR_2m1', variable := 'Low\n-\nMid'] %>%
    .[variable == 'diff_EmR_3m2', variable := 'Mid\n-\nHigh']
  
  dodge_width = 0.3
  
  p = ggplot(data = data,
             aes(x = variable,
                 y = mean_value,
                 color = group,
                 fill = group)) +
    geom_hline(yintercept = 0,
               linetype = 'solid',
               size = 0.4) +
    geom_point(size = 0.5,
               alpha = 0.8,
               position = position_jitterdodge(dodge.width = dodge_width,
                                               jitter.width = dodge_width/2,
                                               jitter.height = 0,
                                               seed = 666)) +
    geom_boxplot(color = 'black',
                 outlier.shape = NA,
                 position = position_dodge(width = dodge_width),
                 width = dodge_width) +
    stat_summary(fun = 'mean',
                 geom = 'point',
                 na.rm = TRUE,
                 shape = 23,
                 inherit.aes = TRUE,
                 fill = 'white',
                 size = 2,
                 stroke = 0.5,
                 show.legend = FALSE,
                 position = position_dodge(width = dodge_width)) +
    labs(x = 'Compared bandits',
         y = 'Bias in estimated distance') +
    scale_y_continuous(breaks = seq(-40, 40, by = 10)) +
    scale_color_manual(values = custom_guides) +
    scale_fill_manual(values = custom_guides) +
    facet_grid(~group)
  p = Neurocodify_plot(p) +
    theme(legend.position = 'none',
          panel.grid = element_blank(),
          axis.text = element_text(lineheight = 0.6),
          axis.title.x = element_text(size = 10,
                                      face = 'bold',
                                      margin = margin(10,0,0,0, 'pt')),
          axis.title.y = element_text(size = 10,
                                      face = 'bold',
                                      margin = margin(0,10,0,0, 'pt')),
          plot.margin = margin(0,0,0,0,'pt'))
  
  return(p)
  
}

