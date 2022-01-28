library(ggplot2)
library(here)

CreateDistributionPictograms = function(){

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
  
  # Get color schemes for plot
  guides = Get_plot_guides()
  
  # Plot pictogram of low/mid comparison
  p_picto_low_mid = ggplot(data = data,
                           aes(x = x,
                               y = y,
                               fill = stim)) +
    geom_area(data = data[stim == 'Low'],
              alpha = 0.5) +
    geom_area(data = data[stim == 'Mid'],
              alpha = 0.8) +
    scale_color_manual(values = guides) +
    scale_fill_manual(values = guides) +
    scale_x_continuous(limits = as.numeric(c(min(data$x),max(data$x))),
                       expand = c(0,0)) +
    scale_y_continuous(limits = c(0,max(data$y)),
                       expand = c(0,0)) +
    theme_minimal() +
    theme(axis.line = element_blank(),
          legend.position = 'None',
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.margin = margin(0,0,0,0, 'in'))
  
  # Pictogram of mid/high comparison
  p_picto_mid_high = ggplot(data = data,
                           aes(x = x,
                               y = y,
                               fill = stim)) +
    geom_area(data = data[stim == 'High'],
              alpha = 0.5) +
    geom_area(data = data[stim == 'Mid'],
              alpha = 0.8) +
    scale_color_manual(values = guides) +
    scale_fill_manual(values = guides) +
    scale_x_continuous(limits = as.numeric(c(min(data$x),max(data$x))),
                       expand = c(0,0)) +
    scale_y_continuous(limits = c(0,max(data$y)),
                       expand = c(0,0)) +
    theme_minimal() +
    theme(axis.line = element_blank(),
          legend.position = 'None',
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.margin = margin(0,0,0,0, 'in'))
  
  # Pictogram of low/high comparison
  p_picto_low_high = ggplot(data = data,
                            aes(x = x,
                                y = y,
                                fill = stim)) +
    geom_area(data = data[stim == 'Low'],
              alpha = 0.5) +
    geom_area(data = data[stim == 'High'],
              alpha = 0.5) +
    scale_color_manual(values = guides) +
    scale_fill_manual(values = guides) +
    scale_x_continuous(limits = as.numeric(c(min(data$x),max(data$x))),
                       expand = c(0,0)) +
    scale_y_continuous(limits = c(0,max(data$y)),
                       expand = c(0,0)) +
    theme_minimal() +
    theme(axis.line = element_blank(),
          legend.position = 'None',
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.margin = margin(0,0,0,0, 'in'))
  
  # Save pictograms
  # Low/Mid
  out_file = file.path(base_path, 'derivatives', 'figures', 'picto_lm.pdf',
                       fsep = .Platform$file.sep)
  ggsave(out_file, p_picto_low_mid, width = 3, height = 3)
  
  # Mid/High
  out_file = file.path(base_path, 'derivatives', 'figures', 'picto_mh.pdf',
                       fsep = .Platform$file.sep)
  ggsave(out_file, p_picto_mid_high, width = 3, height = 3)
  
  # Low/High
  out_file = file.path(base_path, 'derivatives', 'figures', 'picto_lh.pdf',
                       fsep = .Platform$file.sep)
  ggsave(out_file, p_picto_low_high, width = 3, height = 3)
  
  
}