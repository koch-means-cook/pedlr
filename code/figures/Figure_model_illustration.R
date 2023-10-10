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
  data_lr_const = data.table::data.table('pe' = seq(0,50,length.out = 1000),
                                         'alpha_star' = 0.5)
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
  data_prob_const = data.table::data.table('vl' = seq(-50,50, length.out = 1000),
                                           'vr' = 0) %>%
    .[, vlmvr := vl - vr] %>%
    .[, ':='(beta_0 = 0,
             beta_1 = 0.1,
             beta_2 = -0.1,
             beta_3 = 0,
             beta_4 = 0)] %>%
    .[, prob := 1/(1 + exp(-(beta_0 + beta_1*vl + beta_2*vr)))]
  data_prob_unc = data.table::data.table('beta_0' = 0,
                                         'beta_1' = 0.1,
                                         'beta_2' = -0.1,
                                         'beta_3' = seq(0.2, -0.2, length.out = 10)) %>%
    .[, .(beta_0 = unique(beta_0),
          beta_1 = unique(beta_1),
          beta_2 = unique(beta_2),
          vl = seq(-50,50, length.out = 1000),
          vr = 0),
      by = 'beta_3'] %>%
    .[, ':='(vlmvr = vl-vr,
             U_l = 10)] %>%
    .[, prob := 1/(1 + exp(-(beta_0 + beta_1*vl + beta_2*vr + beta_3*U_l)))] %>%
    .[, beta_3 := factor(beta_3)]
  
  # Set shared variables
  annotate_text_size = 7
  col_pallette = colorRampPalette(c('red', 'orange'))
  
  # Plot: LR no variation
  p_lr_const = ggplot(data = data_lr_const,
                      aes(x = pe,
                          y = alpha_star)) +
    scale_y_continuous(limit = c(0,1),
                       breaks = c(0,1)) +
    geom_line() +
    labs(x = 'Surprise (|PE|)',
         y = latex2exp::TeX(r'(Learning rate $\alpha^{*}$)')) +
    annotate('label',
             x = 25,
             y = 0.5,
             label = latex2exp::TeX(r'($\alpha = 0.5$)',
                                    output = 'character'),
             parse = TRUE,
             label.size = 0,
             size = annotate_text_size,
             fill = 'white')
  p_lr_const = Neurocodify_plot(p_lr_const) +
    theme(axis.text.x = element_text(size = 15, 
                                     margin = margin(2.5,0,0,0, 'pt')),
          axis.title.x = element_text(size = 15,
                                      margin = margin(10,0,0,0,'pt')),
          axis.title.y = element_text(size = 15,
                                      margin = margin(0,10,0,0,'pt')),
          axis.text.y = element_text(size = 15),
          legend.position = 'none',
          plot.margin = margin(0,0,0,0,'pt'),
          panel.grid = element_blank(),
          aspect.ratio = 1)
  p_lr_const
  
  # Plot: Prob no variation
  p_prob_const = ggplot(data = data_prob_const,
                        aes(x = vlmvr,
                            y = prob)) +
    scale_x_continuous(breaks = seq(-40, 40, by = 20)) +
    scale_y_continuous(limits = c(0,1),
                       breaks = seq(0, 1)) +
    labs(x = 'Value left \u2212 Value right',
         y = 'Probability to choose left') +
    geom_line() +
    annotate('label',
             x = 0,
             y = 0.5,
             label = latex2exp::TeX(r'($\beta_{3} = 0$)',
                                    output = 'character'),
             parse = TRUE,
             label.size = 0,
             size = annotate_text_size,
             fill = 'white') +
    annotate('label',
             x = 22,
             y = 0.23,
             label = latex2exp::TeX(r'($\beta_{0} = 0$)',
                                    output = 'character'),
             parse = TRUE,
             label.size = 0,
             size = annotate_text_size,
             fill = 'white',
             hjust = 0) +
    annotate('label',
             x = 22,
             y = 0.13,
             label = latex2exp::TeX(r'($\beta_{1} = 0.1$)',
                                    output = 'character'),
             parse = TRUE,
             label.size = 0,
             size = annotate_text_size,
             fill = 'white',
             hjust = 0) +
    annotate('label',
             x = 22,
             y = 0.03,
             label = latex2exp::TeX(r'($\beta_{2} = -0.1$)',
                                    output = 'character'),
             parse = TRUE,
             label.size = 0,
             size = annotate_text_size,
             fill = 'white',
             hjust = 0)
    
    p_prob_const = Neurocodify_plot(p_prob_const) +
      theme(axis.text.x = element_text(size = 15, 
                                       margin = margin(2.5,0,0,0, 'pt')),
            axis.title.x = element_text(size = 15,
                                        margin = margin(10,0,0,0,'pt')),
            axis.title.y = element_text(size = 15,
                                        margin = margin(0,10,0,0,'pt')),
            axis.text.y = element_text(size = 15),
            legend.position = 'none',
            plot.margin = margin(0,0,0,0,'pt'),
            panel.grid = element_blank(),
            aspect.ratio = 1)
    p_prob_const
    
    # Plot: Varying prob
    p_prob_var = ggplot(data = data_prob_unc,
                          aes(x = vlmvr,
                              y = prob,
                              color = as.numeric(beta_3),
                              group = beta_3)) +
      scale_x_continuous(breaks = seq(-40, 40, by = 20)) +
      scale_y_continuous(limits = c(0,1),
                         breaks = seq(0, 1)) +
      labs(x = 'Value left \u2212 Value right',
           y = 'Probability to choose left') +
      geom_line() +
      annotate('label',
               x = data_prob_unc[round(prob,2) == 0.50 & beta_3 == '0.2'][1]$vlmvr,
               y = 0.5,
               label = latex2exp::TeX(r'($\beta_{3} = .2$)',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               size = annotate_text_size,
               fill = 'white',
               color = 'orange',
               hjust = 0.8) +
      annotate('label',
               x = data_prob_unc[round(prob,2) == 0.50 & beta_3 == '-0.2'][1]$vlmvr,
               y = 0.5,
               label = latex2exp::TeX(r'($\beta_{3} = -.2$)',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               size = annotate_text_size,
               fill = 'white',
               color = 'red',
               hjust = 0.2) +
      annotate('label',
               x = 22,
               y = 0.23,
               label = latex2exp::TeX(r'($\beta_{0} = 0$)',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               size = annotate_text_size,
               fill = 'white',
               hjust = 0) +
      annotate('label',
               x = 22,
               y = 0.13,
               label = latex2exp::TeX(r'($\beta_{1} = 0.1$)',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               size = annotate_text_size,
               fill = 'white',
               hjust = 0) +
      annotate('label',
               x = 22,
               y = 0.03,
               label = latex2exp::TeX(r'($\beta_{2} = -0.1$)',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               size = annotate_text_size,
               fill = 'white',
               hjust = 0) +
      annotate('label',
               x = -50,
               y = 0.9,
               label = latex2exp::TeX(r'(\underline{Uncertainty:})',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               size = annotate_text_size,
               fill = 'white',
               hjust = 0,
               vjust = 0) +
      annotate('label',
               x = -50,
               y = 0.8,
               label = latex2exp::TeX(r'($\U = 10$)',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               size = annotate_text_size,
               fill = 'white',
               hjust = 0,
               vjust = 0) +
      scale_color_gradient(low = 'red', high = 'orange')
    
    p_prob_var = Neurocodify_plot(p_prob_var) +
      theme(axis.text.x = element_text(size = 15, 
                                       margin = margin(2.5,0,0,0, 'pt')),
            axis.title.x = element_text(size = 15,
                                        margin = margin(10,0,0,0,'pt')),
            axis.title.y = element_text(size = 15,
                                        margin = margin(0,10,0,0,'pt')),
            axis.text.y = element_text(size = 15),
            legend.position = 'none',
            plot.margin = margin(0,0,0,0,'pt'),
            panel.grid = element_blank(),
            aspect.ratio = 1)
    p_prob_var
    
    # Plot: LR varying
    p_lr_var_u = ggplot(data = data_lr_surprise_u,
                        aes(x = pe,
                            y = alpha_star,
                            color = as.numeric(u),
                            group = u)) +
      scale_y_continuous(limit = c(0,1),
                         breaks = c(0,1)) +
      geom_line() +
      labs(x = 'Surprise (|PE|)',
           y = latex2exp::TeX(r'(Learning rate $\alpha^{*}$)')) +
      annotate('label',
               x = 50,
               y = data_lr_surprise_u[pe == 50 & u == 0]$alpha_star,
               label = latex2exp::TeX(r'($u = 0$)',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               size = annotate_text_size,
               fill = 'white',
               hjust = 1,
               color = 'red') +
      annotate('label',
               x = 50,
               y = data_lr_surprise_u[pe == 50 & u == 0.4]$alpha_star,
               label = latex2exp::TeX(r'($u = 0.4$)',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               size = annotate_text_size,
               fill = 'white',
               hjust = 1,
               color = col_pallette(10)[4]) +
      annotate('label',
               x = 50,
               y = data_lr_surprise_u[pe == 50 & u == 0.6]$alpha_star,
               label = latex2exp::TeX(r'($u = 0.6$)',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               size = annotate_text_size,
               fill = 'white',
               hjust = 1,
               color = col_pallette(10)[6]) +
      annotate('label',
               x = 50,
               y = data_lr_surprise_u[pe == 50 & u == 1]$alpha_star,
               label = latex2exp::TeX(r'($u = 1$)',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               size = annotate_text_size,
               fill = 'white',
               hjust = 1,
               color = 'orange') +
      scale_color_gradient(low = 'red', high = 'orange') +
      annotate('label',
               x = 0,
               y = 0.8,
               label = latex2exp::TeX(r'(lower LR:)',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               size = annotate_text_size,
               fill = 'white',
               vjust = 0,
               hjust = 0) +
      annotate('label',
               x = 0,
               y = 0.7,
               label = latex2exp::TeX(r'($l = 0.5$)',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               size = annotate_text_size,
               fill = 'white',
               vjust = 0,
               hjust = 0) +
      geom_segment(aes(x = 4, y = 0.7, xend = 4, yend = 0.52),
                   arrow = arrow(),
                   color = 'black') +
      annotate('label',
               x = 0,
               y = 0.13,
               label = latex2exp::TeX(r'(slope:)',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               size = annotate_text_size,
               fill = 'white',
               vjust = 0,
               hjust = 0) +
      annotate('label',
               x = 0,
               y = 0.03,
               label = latex2exp::TeX(r'($s = 3$)',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               size = annotate_text_size,
               fill = 'white',
               vjust = 0,
               hjust = 0) +
      geom_segment(aes(x = 11, y = 0.13, xend = 15, yend = 0.3),
                   arrow = arrow(),
                   color = 'black')
    p_lr_var_u = Neurocodify_plot(p_lr_var_u) +
      theme(axis.text.x = element_text(size = 15, 
                                       margin = margin(2.5,0,0,0, 'pt')),
            axis.title.x = element_text(size = 15,
                                        margin = margin(10,0,0,0,'pt')),
            axis.title.y = element_text(size = 15,
                                        margin = margin(0,10,0,0,'pt')),
            axis.text.y = element_text(size = 15),
            legend.position = 'none',
            plot.margin = margin(0,0,0,0,'pt'),
            panel.grid = element_blank(),
            aspect.ratio = 1)
    p_lr_var_u
  
    # Plot: LR varying
    p_lr_var_s = ggplot(data = data_lr_surprise_s,
                        aes(x = pe,
                            y = alpha_star,
                            color = as.numeric(s),
                            group = s)) +
      scale_y_continuous(limit = c(0,1),
                         breaks = c(0,1)) +
      geom_line() +
      labs(x = 'Surprise (|PE|)',
           y = latex2exp::TeX(r'(Learning rate $\alpha^{*}$)')) +
      annotate('label',
               x = 10,
               y = 0.6,
               label = latex2exp::TeX(r'($s = 1$)',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               size = annotate_text_size,
               fill = 'white',
               color = 'red') +
      annotate('label',
               x = 23,
               y = 0.4,
               label = latex2exp::TeX(r'($s = 4$)',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               size = annotate_text_size,
               fill = 'white',
               color = col_pallette(6)[4]) +
      annotate('label',
               x = 36,
               y = 0.2,
               label = latex2exp::TeX(r'($s = 7$)',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               size = annotate_text_size,
               fill = 'white',
               color = 'orange') +
      scale_color_gradient(low = 'red', high = 'orange') +
      # annotate('label',
      #          x = 30,
      #          y = 1,
      #          label = latex2exp::TeX(r'(upper LR:)',
      #                                 output = 'character'),
      #          parse = TRUE,
      #          label.size = 0,
      #          size = annotate_text_size,
      #          fill = 'white',
      #          vjust = 0,
      #          hjust = 0) +
      annotate('label',
               x = 15,
               y = 0.9,
               label = latex2exp::TeX(r'(upper LR:   $u = 0.8$)',
                                      output = 'character'),
               parse = TRUE,
               label.size = 0,
               size = annotate_text_size,
               fill = 'white',
               vjust = 0,
               hjust = 0) +
      geom_segment(aes(x = 35, y = 0.9, xend = 35, yend = 0.82),
                   arrow = arrow(),
                   color = 'black')
    
    p_lr_var_s = Neurocodify_plot(p_lr_var_s) +
      theme(axis.text.x = element_text(size = 15, 
                                       margin = margin(2.5,0,0,0, 'pt')),
            axis.title.x = element_text(size = 15,
                                        margin = margin(10,0,0,0,'pt')),
            axis.title.y = element_text(size = 15,
                                        margin = margin(0,10,0,0,'pt')),
            axis.text.y = element_text(size = 15),
            legend.position = 'none',
            plot.margin = margin(0,0,0,0,'pt'),
            panel.grid = element_blank(),
            aspect.ratio = 1)
    p_lr_var_s
    
    # Plot: Step function
}