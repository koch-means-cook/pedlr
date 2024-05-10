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

Figure_dist_schema = function(){

  # Get directory of repository
  base_path = here::here()
  
  # Load pre-written functions
  source_path = file.path(base_path, 'code', 'utils',
                          fsep = .Platform$file.sep)
  source_files = list.files(source_path, pattern = "[.][rR]$",
                            full.names = TRUE, recursive = TRUE)
  invisible(lapply(source_files, function(x) source(x)))
  
  # Get area of distributions
  sd = (100 * 1/6) / 3
  lowest_gauss_mean = 1/6 * 100
  highest_gauss_mean = 5/6 * 100
  distance = 1/6 * 100
  bimodal_mode_distance = -35
  bimodal_rel_proportion = 0.2
  reward_space_lb = 0
  reward_space_ub = 100
  
  # Low
  d_outcome = dnorm(seq(from=reward_space_lb,
                        to=reward_space_ub,
                        by = 0.1),
                    2/6*100,
                    sd)
  outcome = d_outcome / sum(d_outcome)
  low = data.table(x = seq(from=reward_space_lb,
                           to=reward_space_ub,
                           by = 0.1),
                   y = outcome,
                   stim = 'Low')
  # Mid
  rel_proportion = 0.2
  distance = -35
  main.mean = 3/6 * 100 - ((rel_proportion * distance) / (rel_proportion + 1))
  second.mean = main.mean + distance
  main.sd = second.sd = sd
  main.outcome = dnorm(seq(from=reward_space_lb,to=reward_space_ub, by = 0.1), mean = main.mean, sd=main.sd)
  second.outcome = dnorm(seq(from=reward_space_lb,to=reward_space_ub, by = 0.1), mean=second.mean, sd=second.sd)
  d_outcome = main.outcome + rel_proportion * second.outcome
  outcome = d_outcome/sum(d_outcome)
  mid = data.table(x = seq(from=reward_space_lb,
                           to=reward_space_ub,
                           by = 0.1),
                   y = outcome,
                   stim = 'Mid')
  mid = mid[y > 0.000001,]
  # High
  d_outcome = dnorm(seq(from=reward_space_lb,
                        to=reward_space_ub,
                        by = 0.1),
                    4/6*100,
                    sd)
  outcome = d_outcome / sum(d_outcome)
  high = data.table(x = seq(from=reward_space_lb,
                            to=reward_space_ub,
                            by = 0.1),
                    y = outcome,
                    stim = 'High')
  data = rbind(low,mid)
  data = rbind(data, high)
  data$stim = factor(data$stim, levels = c('Low', 'Mid', 'High'))
  
  # Get color schemes
  guides = Get_plot_guides()
  
  # Add annotation data for text boxes and hiragana symbols
  annot = data.table(x = c(2/6*100, 3/6*100, 4/6*100),
                     y = c(0.01, 0.01, 0.01),
                     stim = c('Low', 'Mid', 'High'),
                     label = c('Mean\nLow', 'Mean\nMid', 'Mean\nHigh'))
  annot_hira = data.table(x = c(2/6*100, 3/6*100, 4/6*100),
                          y = annot$y - (annot$y - max(data$y))/2,
                          stim = c('Low', 'Mid', 'High'),
                          label = c('\u3072', '\u307f', '\u307a'))
  
  # Plot
  p_dist_schema = ggplot(data = data,
                         aes(x = x,
                             y = y,
                             fill = stim)) +
    geom_segment(x = 2/6*100,
                 xend = 2/6*100,
                 y = data[which.min(abs(data[stim == 'Low']$x - 2/6*100)),]$y,
                 yend = annot[stim == 'Low']$y,
                 color = guides['Low'],
                 size = 0.5) +
    geom_segment(x = 3/6*100,
                 xend = 3/6*100,
                 y = data[stim == 'Mid' & x == 3/6*100]$y,
                 yend = annot[stim == 'Mid']$y,
                 color = guides['Mid'],
                 size = 0.5) +
    geom_segment(x = 4/6*100,
                 xend = 4/6*100,
                 y = data[stim == 'High'][which.min(abs(data[stim == 'High']$x - 4/6*100)),]$y,
                 yend = annot[stim == 'High']$y,
                 color = guides['High'],
                 size = 0.5) +
    geom_label(data = annot,
               aes(label = label,
                   color = stim),
               fill = 'white',
               size = 3,
               label.size = 0.5,
               label.padding = unit(0.2, units = 'lines'),
               show.legend = FALSE,
               vjust = 0.7) +
    geom_label(data = annot_hira,
              aes(label = label),
              size = 3,
              color = 'black',
              fill = 'white',
              vjust = 1,
              show.legend = FALSE) +
    geom_area(data = data[stim == 'Low'],
              alpha = 0.5) +
    geom_area(data = data[stim == 'High'],
              alpha = 0.5) +
    geom_area(data = data[stim == 'Mid'],
              alpha = 0.8) +
    scale_color_manual(values = guides) +
    scale_fill_manual(values = guides) +
    labs(x = 'Reward',
         y = 'Probability density')
  
  # Design plot
  p_dist_schema = Neurocodify_plot(p_dist_schema) +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          legend.position = 'none',
          panel.grid = element_blank(),
          #axis.text = element_text(size = 15),
          axis.title.x = element_text(size = 10,
                                      face = 'bold',
                                      margin = margin(10,0,0,0, unit = 'pt')),
          axis.title.y = element_text(size = 10,
                                      face = 'bold',
                                      margin = margin(0,10,0,0, unit = 'pt')),
          axis.ticks.length = unit(4, units = 'pt'),
          plot.margin = margin(0,0,0,10))
  
  return(p_dist_schema)
    
}

Figure_task = function(){
  
  # Get directory of repository
  base_path = here::here()
  
  # Load pre-written functions
  source_path = file.path(base_path, 'code', 'utils',
                          fsep = .Platform$file.sep)
  source_files = list.files(source_path, pattern = "[.][rR]$",
                            full.names = TRUE, recursive = TRUE)
  invisible(lapply(source_files, function(x) source(x)))
  
  # Load task scheme as plot (handmade pdf, should be in datalad repo)
  file = file.path(base_path, 'derivatives', 'figures', 'task.pdf',
                   fsep = .Platform$file.sep)
  if(file.exists(file)){
    task = magick::image_read_pdf(file)
    p_task = magick::image_ggplot(task, interpolate = FALSE) +
      theme(plot.margin = margin(0,10,0,0))
    # If handmade plot is not present, skip
  } else{
    p_task = NULL
  }
  
  
  p_dist_schema = Figure_dist_schema()
  
  # p_task_complete = p_task + p_dist_schema +
  #   patchwork::plot_annotation(tag_levels = 'A')
  
  p_task_complete = cowplot::plot_grid(p_task, p_dist_schema,
                         ncol = 2,
                         rel_widths = c(1,1),
                         labels = c('A', 'B'))


  return(p_task_complete)
  
}
