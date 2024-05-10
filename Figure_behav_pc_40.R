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

Figure_behav_pc_40 = function(){
  
  # Get directory of repository
  base_path = here::here()
  
  # Load pre-written functions
  source_path = file.path(base_path, 'code', 'utils',
                          fsep = .Platform$file.sep)
  source_files = list.files(source_path, pattern = "[.][rR]$",
                            full.names = TRUE, recursive = TRUE)
  invisible(lapply(source_files, function(x) source(x)))
  
  # Get color schemes
  custom_guides = Get_plot_guides()
  
  data = Load_data() %>%
    Apply_exclusion_criteria(., choice_based_exclusion = TRUE) %>%
    Add_comp(.) %>%
    .[, run := as.factor(run)] %>%
    # Get trial counter
    .[, trial := seq(.N),
      by = c('participant_id', 'run')] %>%
    # Bin trials
    .[, bin := rep(seq(6), each = .N/length(seq(6))),
      by = c('participant_id', 'run')] %>%
    .[, bin := factor(bin)] %>%
    # Focus on free choice trials
    .[trial_type == 'choice',] %>%
    # Get if trial was answered correctly
    .[, correct_choice := if(option_left > option_right) 'left' else 'right',
      by = c('participant_id', 'run', 'trial')] %>%
    .[, correct := correct_choice == choice] %>%
    # Get percentage of correct choices (exclude timeouts from overall trials)
    .[, .(perc_correct = mean(correct, na.rm = TRUE)),
      by = c('participant_id', 'group', 'run', 'bin', 'comp')] %>%
    # Average across runs
    .[, .(perc_correct = mean(perc_correct, na.rm = TRUE)),
      by = c('participant_id', 'group', 'bin', 'comp')] %>%
    Prepare_data_for_plot(.)
  
  # level and rename bins
  data$bin = factor(data$bin)
  levels(data$bin) = c('1-40', '41-80', '81-120', '121-160', '161-200', '201-240')
  # level and rename comps
  data$comp = factor(data$comp)
  levels(data$comp) = c('Low-Mid', 'Mid-High', 'Low-High')
  
  # Sort age levels
  data$group = factor(data$group, levels = c('Older\nadults','Younger\nadults'))
  
  data_mean = data %>%
    .[, .(perc_correct = mean(perc_correct),
          sem = sd(perc_correct) / sqrt(.N)),
      by = c('group', 'bin', 'comp')] %>%
    .[, ':='(ymin = perc_correct - sem,
             ymax = perc_correct + sem)]
  
  dodge_width = 0.5
  
  data_plot = data[bin == '1-40']
  data_plot_mean = data_mean[bin == '1-40']
  
  p = ggplot(data = data_plot,
         aes(x = group,
             y = perc_correct,
             color = comp,
             fill = comp)) +
    geom_point(size = 0.8,
               alpha = 0.5,
               position = position_jitterdodge(jitter.width = 0.08,
                                               jitter.height = 0,
                                               dodge.width = dodge_width,
                                               seed = 666)) +
    geom_boxplot(outlier.shape = NA,
                 position = position_dodge(width = dodge_width),
                 width = 0.8*dodge_width,
                 color = 'black',
                 alpha = 0.5,
                 size = 0.3) +
    geom_hline(yintercept = 0.5,
               size = 0.5,
               linetype = 'dashed') +
    stat_summary(aes(group = comp),
                 geom = 'point',
                 fun = mean,
                 color = 'black',
                 fill = 'white',
                 shape = 23,
                 stroke = 1,
                 size = 2,
                 position = position_dodge(width = dodge_width)) +
    labs(y = 'Percentage correct\n(trials 1-40)') +
    # scale_y_continuous(limits = c(0.4,1),
    #                    breaks = seq(0.4,1,by = 0.1)) +
    viridis::scale_color_viridis(option = 'D', discrete = TRUE) +
    viridis::scale_fill_viridis(option = 'D', discrete = TRUE)
  
  p = Neurocodify_plot(p) +
    theme(panel.grid = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12,
                                      face = 'bold',
                                      margin = margin(0,10,0,0,'pt')),
          axis.text = element_text(size = 10,
                                      face = 'plain',
                                      margin = margin(0,10,0,0,'pt')),
          legend.title = element_blank(),
          legend.position = 'right',
          legend.key = element_rect(fill = 'transparent'),
          legend.text = element_text(size = 10),
          legend.background = element_rect(fill = 'transparent'),
          plot.margin = margin(5,5,5,5,'pt'))
  
  return(p)
  
}

