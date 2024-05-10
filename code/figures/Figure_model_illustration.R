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
library(grid)

Figure_model_illustration = function(){
  
  # Get git repo root
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
  
  # Simulate data
  # Constant LR
  data_lr_const = data.table::data.table('pe' = seq(0,50,by = 0.1),
                                         'alpha_star' = 0.5)
  # Step LR
  data_lr_seplr = data.table::data.table('pe' = seq(-50,50,by = 0.1),
                                         'alpha_star' = 0.5) %>%
    .[pe >= 0, alpha_star := 0.2]
  # LR function Surprise
  data_lr_surprise_u = data.table::data.table('u' = seq(0,1, by = 0.1)) %>%
    .[, .(pe = seq(0,50,by = 0.05)),
      by = 'u'] %>%
    .[, alpha_star := LRfunction(low = 0.5,
                                slope = 3,
                                up = u,
                                PE = pe,
                                tau = 0.2),
      by = 'u'] %>%
    .[, u := factor(u)]
  data_lr_surprise_s = data.table::data.table('s' = seq(1,7, by = 1)) %>%
    .[, .(pe = seq(0,50, by = 0.05)),
      by = 's'] %>%
    .[, alpha_star := LRfunction(low = 0.1,
                                 slope = s,
                                 up = 0.8,
                                 PE = pe,
                                 tau = 0.2),
      by = 's'] %>%
    .[, s := factor(s)]
  # Standard choice probability
  data_prob_const = data.table::data.table('vl' = seq(-50,50, length.out = 1000),
                                           'vr' = 0) %>%
    .[, vrmvl := vr - vl] %>%
    .[, ':='(beta_0 = 0,
             beta_1 = -0.1,
             beta_2 = 0.1,
             beta_3 = 0,
             beta_4 = 0)] %>%
    .[, prob_right := 1/(1 + exp(-(beta_0 + beta_1*vl + beta_2*vr)))]
  # Choice probability variation with uncertainty
  data_prob_unc = data.table::data.table('beta_0' = 0,
                                         'beta_1' = -0.1,
                                         'beta_2' = 0.1,
                                         'beta_4' = seq(0.2, -0.2, length.out = 10)) %>%
    .[, .(beta_0 = unique(beta_0),
          beta_1 = unique(beta_1),
          beta_2 = unique(beta_2),
          vl = seq(-50,50, length.out = 1000),
          vr = 0),
      by = 'beta_4'] %>%
    .[, ':='(vrmvl = vr - vl,
             U_r = 10)] %>%
    .[, prob_right := 1/(1 + exp(-(beta_0 + beta_1*vl + beta_2*vr + beta_4*U_r)))] %>%
    .[, beta_4 := factor(beta_4)]
  
  # Set shared variables
  # Text size of annotation layers (e.g. alpha on plot line)
  annotate_text_size = 3.5
  # Textsize for plot titles
  textsize_title = 13
  # Textsize for axis text and title
  axis_size = 11
  # Aspet ratio of all plots
  aspect_ratio = 0.8
  # Create color palatte to pick values in between red and orange for annotations (e.g. beta4 = 0.2)
  col_pallette = colorRampPalette(c('red', 'orange'))
  # Headlines for subplots
  headline_label_rw = 'A'
  headline_label_unc = 'B'
  headline_label_seplr = 'C'
  headline_label_surprise = 'D'
  # Subplot Headline position
  headline_y = 1.13
  headline_x = 0
  headline_hjust = 0
  # Subplot label size
  headline_label_size = 20
  
  
  # Plot: LR no variation
  p_lr_const = ggplot(data = data_lr_const,
                      aes(x = pe,
                          y = alpha_star)) +
    scale_y_continuous(limit = c(0,1),
                       breaks = c(0,1)) +
    geom_line() +
    labs(x = 'Surprise (|PE|)',
         y = latex2exp::TeX(r'(Learning rate $\alpha^{*}$)'),
         title = ' ') +
    # display alpha
    annotate('label',
             x = 25,
             y = 0.5,
             label = latex2exp::TeX(r'(\textit{$\alpha = 0.5$})',
                                    output = 'character'),
             parse = TRUE,
             label.size = 0,
             family = "serif",
             size = annotate_text_size,
             fill = 'white')
  p_lr_const = Neurocodify_plot(p_lr_const) +
    theme(axis.text.x = element_text(size = axis_size, 
                                     margin = margin(2.5,0,0,0, 'pt')),
          axis.title.x = element_text(size = axis_size,
                                      margin = margin(10,0,0,0,'pt')),
          axis.title.y = element_text(size = axis_size,
                                      margin = margin(0,5,0,0,'pt')),
          axis.text.y = element_text(size = axis_size),
          legend.position = 'none',
          plot.margin = margin(0,0,0,0,'pt'),
          panel.grid = element_blank(),
          aspect.ratio = aspect_ratio,
          plot.title = element_text(size = textsize_title,
                                    margin = margin(0,0,10,0, 'pt'),
                                    hjust = 0.5))
  
  # Plot: Prob no variation
  p_prob_const = ggplot(data = data_prob_const,
                        aes(x = vrmvl,
                            y = prob_right)) +
    scale_x_continuous(breaks = seq(-40, 40, by = 20)) +
    scale_y_continuous(limits = c(0,1),
                       breaks = seq(0, 1)) +
    labs(x = 'Value right \u2212 Value left',
         y = 'Probability to choose right',
         title = ' ') +
    geom_line() +
    # display beta4
    annotate('label',
             x = 0,
             y = 0.5,
             label = latex2exp::TeX(r'(\textit{$\beta_{4} = 0$})',
                                    output = 'character'),
             parse = TRUE,
             label.size = 0,
             family = "serif",
             size = annotate_text_size,
             fill = 'white') +
    # display beta0
    annotate('label',
             x = 18,
             y = 0.29,
             label = latex2exp::TeX(r'(\textit{$\beta_{0} = 0$})',
                                    output = 'character'),
             parse = TRUE,
             label.size = 0,
             family = "serif",
             size = annotate_text_size,
             fill = 'white',
             hjust = 0) +
    # display beta1
    annotate('label',
             x = 18,
             y = 0.16,
             label = latex2exp::TeX(r'(\textit{$\beta_{1} = -0.1$})',
                                    output = 'character'),
             parse = TRUE,
             label.size = 0,
             family = "serif",
             size = annotate_text_size,
             fill = 'white',
             hjust = 0) +
    # display beta2
    annotate('label',
             x = 18,
             y = 0.03,
             label = latex2exp::TeX(r'(\textit{$\beta_{2} = 0.1$})',
                                    output = 'character'),
             parse = TRUE,
             label.size = 0,
             family = "serif",
             size = annotate_text_size,
             fill = 'white',
             hjust = 0) +
    coord_cartesian(clip = 'off')
    
    p_prob_const = Neurocodify_plot(p_prob_const) +
      theme(axis.text.x = element_text(size = axis_size, 
                                       margin = margin(2.5,0,0,0, 'pt')),
            axis.title.x = element_text(size = axis_size,
                                        margin = margin(10,0,0,0,'pt')),
            axis.title.y = element_text(size = axis_size,
                                        margin = margin(0,5,0,0,'pt')),
            axis.text.y = element_text(size = axis_size),
            legend.position = 'none',
            plot.margin = margin(0,0,0,0,'pt'),
            panel.grid = element_blank(),
            aspect.ratio = aspect_ratio,
            plot.title = element_text(size = textsize_title,
                                      margin = margin(0,0,10,0, 'pt'),
                                      hjust = 0.5))
    
    # Plot: Varying prob
    p_prob_var = ggplot(data = data_prob_unc,
                          aes(x = vrmvl,
                              y = prob_right,
                              color = as.numeric(beta_4),
                              group = beta_4)) +
      scale_x_continuous(breaks = seq(-40, 40, by = 20)) +
      scale_y_continuous(limits = c(0,1),
                         breaks = seq(0, 1)) +
      labs(x = 'Value right \u2212 Value left',
           y = 'Probability to choose right',
           title = latex2exp::TeX(r'(Variation in uncertainty effect \textit{$\beta_{4}$})')) +
      geom_line() +
      # Display high value of beta4
      annotate('label',
               x = data_prob_unc[round(prob_right,2) == 0.50 & beta_4 == '0.2'][1]$vrmvl,
               y = 0.6,
               label = latex2exp::TeX(r'(\textit{$\beta_{4} = .2$})',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               family = "serif",
               size = annotate_text_size,
               fill = 'white',
               color = 'orange',
               hjust = 1) +
      # Display low value of beta4
      annotate('label',
               x = data_prob_unc[round(prob_right,2) == 0.50 & beta_4 == '-0.2'][1]$vrmvl,
               y = 0.5,
               label = latex2exp::TeX(r'(\textit{$\beta_{4} = -.2$})',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               family = "serif",
               size = annotate_text_size,
               fill = 'white',
               color = 'red',
               hjust = -0.1) +
      # Display beta0
      annotate('label',
               x = 18,
               y = 0.29,
               label = latex2exp::TeX(r'(\textit{$\beta_{0} = 0$})',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               family = "serif",
               size = annotate_text_size,
               fill = 'white',
               hjust = 0) +
      # Display beta1
      annotate('label',
               x = 18,
               y = 0.16,
               label = latex2exp::TeX(r'(\textit{$\beta_{1} = -0.1$})',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               family = "serif",
               size = annotate_text_size,
               fill = 'white',
               hjust = 0) +
      # Display beta2
      annotate('label',
               x = 18,
               y = 0.03,
               label = latex2exp::TeX(r'(\textit{$\beta_{2} = 0.1$})',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               family = "serif",
               size = annotate_text_size,
               fill = 'white',
               hjust = 0) +
      # Textbox "uncertainty"
      annotate('label',
               x = -50,
               y = 0.95,
               label = latex2exp::TeX(r'(Uncertainty:)',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               family = "serif",
               size = annotate_text_size,
               fill = 'white',
               hjust = 0,
               vjust = 0) +
      # Textbox U = 10
      annotate('label',
               x = -50,
               y = 0.81,
               label = latex2exp::TeX(r'(\textit{$\U_{\textrm{right}} = 10$})',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               family = "serif",
               size = annotate_text_size,
               fill = 'white',
               hjust = 0,
               vjust = 0) +
      scale_color_gradient(low = 'red', high = 'orange') +
      coord_cartesian(clip = 'off')
    
    p_prob_var = Neurocodify_plot(p_prob_var) +
      theme(axis.text.x = element_text(size = axis_size, 
                                       margin = margin(2.5,0,0,0, 'pt')),
            axis.title.x = element_text(size = axis_size,
                                        margin = margin(10,0,0,0,'pt')),
            axis.title.y = element_text(size = axis_size,
                                        margin = margin(0,5,0,0,'pt')),
            axis.text.y = element_text(size = axis_size),
            legend.position = 'none',
            plot.margin = margin(0,0,0,0,'pt'),
            panel.grid = element_blank(),
            aspect.ratio = aspect_ratio,
            plot.title = element_text(size = textsize_title,
                                      margin = margin(0,0,10,0, 'pt'),
                                      hjust = 0.7))
    
    # Plot: LR varying by u
    p_lr_var_u = ggplot(data = data_lr_surprise_u,
                        aes(x = pe,
                            y = alpha_star,
                            color = as.numeric(u),
                            group = u)) +
      scale_y_continuous(limit = c(0,1),
                         breaks = c(0,1)) +
      geom_line() +
      labs(x = 'Surprise (|PE|)',
           y = latex2exp::TeX(r'(Learning rate $\alpha^{*}$)'),
           title = latex2exp::TeX(r'(Variation in upper LR \textit{$u$})')) +
      # Display u = 0
      annotate('label',
               x = 50,
               y = data_lr_surprise_u[pe == 50 & u == 0]$alpha_star,
               label = latex2exp::TeX(r'(\textit{$u = 0$})',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               family = "serif",
               size = annotate_text_size,
               fill = 'white',
               hjust = 1,
               color = 'red') +
      # Display u = 0.4
      annotate('label',
               x = 50,
               y = data_lr_surprise_u[pe == 50 & u == 0.4]$alpha_star,
               label = latex2exp::TeX(r'(\textit{$u = 0.4$})',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               family = "serif",
               size = annotate_text_size,
               fill = 'white',
               hjust = 1,
               color = col_pallette(10)[4]) +
      # Display u = 6
      annotate('label',
               x = 50,
               y = data_lr_surprise_u[pe == 50 & u == 0.6]$alpha_star,
               label = latex2exp::TeX(r'(\textit{$u = 0.6$})',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               family = "serif",
               size = annotate_text_size,
               fill = 'white',
               hjust = 1,
               color = col_pallette(10)[6]) +
      # Display u = 1
      annotate('label',
               x = 50,
               y = data_lr_surprise_u[pe == 50 & u == 1]$alpha_star,
               label = latex2exp::TeX(r'(\textit{$u = 1$})',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               family = "serif",
               size = annotate_text_size,
               fill = 'white',
               hjust = 1,
               color = 'orange') +
      scale_color_gradient(low = 'red', high = 'orange') +
      # Display textbox "lower LR"
      annotate('label',
               x = 0,
               y = 0.8,
               label = latex2exp::TeX(r'(lower LR:)',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               family = "serif",
               size = annotate_text_size,
               fill = 'white',
               vjust = 0,
               hjust = 0) +
      # Display l = 0.5
      annotate('label',
               x = 0,
               y = 0.7,
               label = latex2exp::TeX(r'(\textit{$l = 0.5$})',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               family = "serif",
               size = annotate_text_size,
               fill = 'white',
               vjust = 0,
               hjust = 0) +
      # Arrow from textbox (l) to line
      geom_segment(aes(x = 4, y = 0.7, xend = 4, yend = 0.52),
                   arrow = arrow(length = unit(0.03, "npc"),
                                 angle = 30,
                                 type = 'closed'),
                   linewidth = 0.4,
                   color = 'black') +
      # Display textbox "slope:"
      annotate('label',
               x = 0,
               y = 0.13,
               label = latex2exp::TeX(r'(slope:)',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               family = "serif",
               size = annotate_text_size,
               fill = 'white',
               vjust = 0,
               hjust = 0) +
      # Display s = 3
      annotate('label',
               x = 0,
               y = 0.03,
               label = latex2exp::TeX(r'(\textit{$s = 3$})',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               family = "serif",
               size = annotate_text_size,
               fill = 'white',
               vjust = 0,
               hjust = 0) +
      # Arrow from textbox (s) to line
      geom_segment(aes(x = 11, y = 0.13, xend = 15, yend = 0.3),
                   arrow = arrow(length = unit(0.03, "npc"),
                                 angle = 30,
                                 type = 'closed'),
                   color = 'black',
                   linewidth = 0.4)
    p_lr_var_u = Neurocodify_plot(p_lr_var_u) +
      theme(axis.text.x = element_text(size = axis_size, 
                                       margin = margin(2.5,0,0,0, 'pt')),
            axis.title.x = element_text(size = axis_size,
                                        margin = margin(10,0,0,0,'pt')),
            axis.title.y = element_text(size = axis_size,
                                        margin = margin(0,5,0,0,'pt')),
            axis.text.y = element_text(size = axis_size),
            legend.position = 'none',
            plot.margin = margin(0,0,0,0,'pt'),
            panel.grid = element_blank(),
            aspect.ratio = aspect_ratio,
            plot.title = element_text(size = textsize_title,
                                      margin = margin(0,0,10,0, 'pt'),
                                      hjust = 0.5))
  
    # Plot: LR varying by s
    p_lr_var_s = ggplot(data = data_lr_surprise_s,
                        aes(x = pe,
                            y = alpha_star,
                            color = as.numeric(s),
                            group = s)) +
      scale_y_continuous(limit = c(0,1),
                         breaks = c(0,1)) +
      geom_line() +
      labs(x = 'Surprise (|PE|)',
           y = latex2exp::TeX(r'(Learning rate $\alpha^{*}$)'),
           title = latex2exp::TeX(r'(Variation in slope \textit{$s$})')) +
      # Display s = 1
      annotate('label',
               x = 10,
               y = 0.6,
               label = latex2exp::TeX(r'(\textit{$s = 1$})',
                                      output = 'character'),
               parse = TRUE,
               family = "serif",
               label.size = 0,
               size = annotate_text_size,
               fill = 'white',
               color = 'red') +
      # Display s = 4
      annotate('label',
               x = 23,
               y = 0.4,
               label = latex2exp::TeX(r'(\textit{$s = 4$})',
                                      output = 'character'),
               parse = TRUE,
               family = "serif",
               label.size = 0,
               size = annotate_text_size,
               fill = 'white',
               color = col_pallette(6)[4]) +
      # Display s = 7
      annotate('label',
               x = 36,
               y = 0.2,
               label = latex2exp::TeX(r'(\textit{$s = 7$})',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               family = "serif",
               size = annotate_text_size,
               fill = 'white',
               color = 'orange') +
      scale_color_gradient(low = 'red', high = 'orange') +
      # Display textbox "upper LR: u = 0.8"
      annotate('label',
               x = 15,
               y = 0.9,
               label = latex2exp::TeX(r'(upper LR:   \textit{$u = 0.8$})',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               family = "serif",
               size = annotate_text_size,
               fill = 'white',
               vjust = 0,
               hjust = 0) +
      # Arrow from textbox (upper LR) to line
      geom_segment(aes(x = 35, y = 0.9, xend = 35, yend = 0.82),
                   arrow = arrow(length = unit(0.03, "npc"),
                                 angle = 30,
                                 type = 'closed'),
                   color = 'black',
                   linewidth = 0.4)
    
    p_lr_var_s = Neurocodify_plot(p_lr_var_s) +
      theme(axis.text.x = element_text(size = axis_size, 
                                       margin = margin(2.5,0,0,0, 'pt')),
            axis.title.x = element_text(size = axis_size,
                                        margin = margin(10,0,0,0,'pt')),
            axis.title.y = element_text(size = axis_size,
                                        margin = margin(0,5,0,0,'pt')),
            axis.text.y = element_text(size = axis_size),
            legend.position = 'none',
            plot.margin = margin(0,0,0,0,'pt'),
            panel.grid = element_blank(),
            aspect.ratio = aspect_ratio,
            plot.title = element_text(size = textsize_title,
                                      margin = margin(0,0,10,0, 'pt'),
                                      hjust = 0.5))
    
    # Plot: Step function
    p_lr_step = ggplot(data = data_lr_seplr,
                       aes(x = pe,
                           y = alpha_star)) +
      geom_line() +
      scale_y_continuous(limits = c(0,1),
                         breaks = c(0,1)) +
      labs(x = 'PE',
           y = latex2exp::TeX(r'(Learning rate $\alpha^{*}$)'),
           title = ' ') +
      # Display alpha(neg)
      annotate('label',
               x = -25,
               y = 0.5,
               label = latex2exp::TeX(r'(\textit{$\alpha_{\textrm{neg}} = 0.5$})',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               family = "serif",
               size = annotate_text_size*0.9,
               fill = 'white') +
      # Display alpha(pos)
      annotate('label',
               x = 25,
               y = 0.2,
               label = latex2exp::TeX(r'(\textit{$\alpha_{\textrm{pos}} = 0.2$})',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               family = "serif",
               size = annotate_text_size*0.9,
               fill = 'white')
    p_lr_step = Neurocodify_plot(p_lr_step) +
      theme(axis.text.x = element_text(size = axis_size, 
                                       margin = margin(2.5,0,0,0, 'pt')),
            axis.title.x = element_text(size = axis_size,
                                        margin = margin(10,0,0,0,'pt')),
            axis.title.y = element_text(size = axis_size,
                                        margin = margin(0,5,0,0,'pt')),
            axis.text.y = element_text(size = axis_size),
            legend.position = 'none',
            plot.margin = margin(0,0,0,0,'pt'),
            panel.grid = element_blank(),
            aspect.ratio = aspect_ratio,
            plot.title = element_text(size = textsize_title,
                                      margin = margin(0,0,10,0, 'pt'),
                                      hjust = 0.5))
    
    # Combine plots: RW
    p1 = p_lr_const + theme(plot.margin = margin(5,5,5,0,'pt'))
    p2 = p_prob_const + theme(plot.margin = margin(5,5,5,0,'pt'))
    p_rw = cowplot::plot_grid(p1, NULL, p2,
                              ncol = 1,
                              rel_heights = c(1,0.05,1),
                              axis = 'btlr',
                              align = 'vh',
                              labels = c(headline_label_rw, ''),
                              label_size = headline_label_size,
                              label_x = headline_x,
                              hjust = headline_hjust,
                              label_y = headline_y) +
      theme(panel.border = element_rect(fill = NA,
                                        color = 'black',
                                        linewidth = 1),
            plot.margin = margin(15,0,0,0,'pt')) +
      # Annotate model name above subplot-frame
      annotation_custom(grob = grid::textGrob(label = 'Rescorla-Wagner Model',
                                              hjust = 0.5,
                                              vjust = 0,
                                              gp = grid::gpar(cex = 1.2)),
                        ymin = 1.014,
                        ymax = 1.014,
                        xmin = 0.5,
                        xmax = 0.5)
    
    # Combine plots: Unc
    p1 = p_lr_const + theme(plot.margin = margin(5,5,5,0,'pt'))
    p2 = p_prob_var + theme(plot.margin = margin(5,5,5,0,'pt'))
    p_unc = cowplot::plot_grid(p1, NULL, p2,
                               ncol = 1,
                               rel_heights = c(1,0.05,1),
                               axis = 'btrl',
                               align = 'hv',
                               labels = c(headline_label_unc, ''),
                               label_size = headline_label_size,
                               label_x = headline_x,
                               hjust = headline_hjust,
                               label_y = headline_y) +
      theme(panel.border = element_rect(fill = NA,
                                        color = 'black',
                                        linewidth = 1),
            plot.margin = margin(15,0,0,0,'pt')) +
      # Annotate model name above subplot-frame
      annotation_custom(grob = grid::textGrob(label = 'Uncertainty Model',
                                              hjust = 0.5,
                                              vjust = 0,
                                              gp = grid::gpar(cex = 1.2)),
                        ymin = 1.014,
                        ymax = 1.014,
                        xmin = 0.5,
                        xmax = 0.5)
    
    # Combine plots: Valence
    p1 = p_lr_step + theme(plot.margin = margin(5,5,5,0,'pt'))
    p2 = p_prob_const + theme(plot.margin = margin(5,5,5,0,'pt'))
    p_val = cowplot::plot_grid(p1, NULL, p2,
                               ncol = 1,
                               rel_heights = c(1,0.05,1),
                               axis = 'btrl',
                               align = 'hv',
                               labels = c(headline_label_seplr, ''),
                               label_size = headline_label_size,
                               label_x = headline_x,
                               hjust = headline_hjust,
                               label_y = headline_y) +
      theme(panel.border = element_rect(fill = NA,
                                        color = 'black',
                                        linewidth = 1),
            plot.margin = margin(15,0,0,0,'pt')) +
      # Annotate model name above subplot-frame
      annotation_custom(grob = grid::textGrob(label = 'Valence Model',
                                              hjust = 0.5,
                                              vjust = 0,
                                              gp = grid::gpar(cex = 1.2)),
                        ymin = 1.014,
                        ymax = 1.014,
                        xmin = 0.5,
                        xmax = 0.5)
    
    # Combine plots: Surprise
    p1 = p_lr_var_u + theme(plot.margin = margin(5,5,5,0,'pt'))
    p2 = p_lr_var_s + theme(plot.margin = margin(5,5,5,0,'pt'))
    p3 = p_prob_const + theme(plot.margin = margin(5,5,5,0,'pt'))
    p_surprise = cowplot::plot_grid(NULL,p1,NULL, p2,NULL, p3,NULL,
                                    nrow = 1,
                                    rel_widths = c(0.02,1,0.1,1,0.1,1,0.02),
                                    axis = 'btrl',
                                    align = 'hv',
                                    labels = c(headline_label_surprise ,'','', '', '', '', ''),
                                    label_size = headline_label_size,
                                    label_x = headline_x,
                                    hjust = headline_hjust,
                                    label_y = headline_y - 0.015) +
      theme(panel.border = element_rect(fill = NA,
                                        color = 'black',
                                        linewidth = 1),
            plot.margin = margin(20,5,5,5,'pt')) +
      # Annotate model name above subplot-frame
      annotation_custom(grob = grid::textGrob(label = 'Surprise Model',
                                              hjust = 0.5,
                                              vjust = 0,
                                              gp = grid::gpar(cex = 1.2)),
                        ymin = 1.032,
                        ymax = 1.032,
                        xmin = 0.5,
                        xmax = 0.5)
    
    # Form top row (RW, Uncertainty, Valence)
    p_top = cowplot::plot_grid(p_rw, NULL, p_unc, NULL, p_val,
                               rel_widths = c(1,0.05,1,0.05,1),
                               nrow = 1) +
      theme(plot.margin = margin(5,5,5,5,'pt'))
    
    # Combine top row with surprise plot
    p = cowplot::plot_grid(p_top, NULL, p_surprise,
                           rel_heights = c(1,0.02,0.6),
                           ncol = 1)
    
    return(p)
}