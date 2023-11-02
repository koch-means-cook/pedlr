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


Figure_param_recov = function(){
  
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
  # Glob files based on naming pattern
  load_path = file.path(here::here(),
                        'derivatives',
                        'parameter_recovery',
                        fsep = .Platform$file.sep)
  # Load data with fixed betas during simulation
  files = Sys.glob(file.path(load_path,
                             'paramrecov_base-*_randips-TRUE_randbetas-FALSE_randsvs-TRUE.tsv',
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
    data.table::setnames(., old = 'x', new = 'params') %>%
    # Delete betas based on z-scored predictor values from recovery (data can not
    # be generated based these z_betas since they require a complete data set to
    # be calculated)
    .[!params %in% c('z_(Intercept)', 'z_V1', 'z_V2', 'z_V1u', 'z_V2u'),]
  
  data_recov = data %>%
    .[params %in% c('alpha', 'pi', 'alpha_pos', 'alpha_neg', 'l', 'u', 's'),] %>%
    .[variable %in% c('coefs', 'input_params')] %>%
    data.table::dcast(.,
                      participant_id + model + AIC + AICc + iter + params ~ variable,
                      value.var = 'value') %>%
    data.table::setorder(., 'participant_id', 'iter', 'params') %>%
    .[, input_s := input_params[params == 's'],
      by = c('participant_id', 'model', 'iter')] %>%
    .[, inbounds := data.table::between(input_s, 1, 7)]
  
  # Datatable to display cutoffs for s parameter
  cutoffs = data.table::data.table(cbind(rep('s', 2),
                                         c(1,7)))
  colnames(cutoffs) = c('params', 'x')
  
  # Function to render simple recov plots per parameter
  Plot_recov = function(data,
                        model,
                        param,
                        limits,
                        title_expr,
                        add_s_bounds = FALSE){
    
    # Constrain data
    data_plot = data[model == model & params == param]
    
    # Plot
    p = ggplot(data = data_plot,
               aes(x = input_params,
                   y = coefs)) +
      geom_point(alpha = 0.2,
                 size = 0.1) +
      # Correlation line
      geom_smooth(method = 'lm',
                  formula = y ~ x,
                  linewidth = 0.5,
                  color = 'black',
                  fill = 'black') +
      # Minimal axis text
      scale_y_continuous(expand = c(0,0),
                         limits = limits,
                         breaks = limits) +
      scale_x_continuous(expand = c(0,0),
                         limits = limits,
                         breaks = limits) +
      # Add latex expression for parameter titles
      labs(title = title_expr) +
      # One facet (specified parameter)
      facet_wrap(~ params,
                 nrow = 1) +
      # Use plot title as facet label
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5,
                                      vjust = 0))
    
    p = Neurocodify_plot(p) +
      theme(panel.grid = element_blank(),
            plot.margin = margin(2,2,2,2,'pt'),
            strip.text = element_blank(),
            strip.background = element_blank())
    
    if(add_s_bounds){
      p = p + geom_smooth(data = data_plot[inbounds == TRUE],
                          method = 'lm',
                          formula = y ~ x,
                          linewidth = 0.5,
                          color = 'yellow',
                          fill = 'black')
    }
    
    return(p)
  }
  
  # RW
  p_rw_a = Plot_recov(data = data_recov,
                      model = 'rw',
                      param = 'alpha',
                      limits = c(0,1),
                      title_expr = latex2exp::TeX(r'($\alpha$)'))
  
  # Unc
  p_unc_a = Plot_recov(data = data_recov,
                       model = 'uncertainty',
                       param = 'alpha',
                       limits = c(0,1),
                       title_expr = latex2exp::TeX(r'($\alpha$)'))
  p_unc_pi = Plot_recov(data = data_recov,
                        model = 'uncertainty',
                        param = 'pi',
                        limits = c(0,1),
                        title_expr = latex2exp::TeX(r'($\pi$)'))
  
  # Valence
  p_val_ap = Plot_recov(data = data_recov,
                        model = 'seplr',
                        param = 'alpha_pos',
                        limits = c(0,1),
                        title_expr = latex2exp::TeX(r'($\alpha_{\textit{pos}}$)'))
  p_val_an = Plot_recov(data = data_recov,
                        model = 'seplr',
                        param = 'alpha_neg',
                        limits = c(0,1),
                        title_expr = latex2exp::TeX(r'($\alpha_{\textit{neg}}$)'))
  
  # Surprise
  p_surprise_l = Plot_recov(data = data_recov,
                                model = 'surprise',
                                param = 'l',
                                limits = c(0,1),
                                add_s_bounds = TRUE,
                            title_expr = latex2exp::TeX(r'($\textit{l}$)'))
  p_surprise_u = Plot_recov(data = data_recov,
                                model = 'surprise',
                                param = 'u',
                                limits = c(0,1),
                                add_s_bounds = TRUE,
                            title_expr = latex2exp::TeX(r'($\textit{u}$)'))
  p_surprise_s = Plot_recov(data = data_recov,
                                model = 'surprise',
                                param = 's',
                                limits = c(0,10),
                                add_s_bounds = TRUE,
                            title_expr = latex2exp::TeX(r'($\textit{s}$)')) +
    geom_vline(data = cutoffs,
                        aes(xintercept = as.numeric(x)),
                        linetype = 'dashed')
  
  # Valence+Unc
  p_val_unc_ap = Plot_recov(data = data_recov,
                        model = 'uncertainty_seplr',
                        param = 'alpha_pos',
                        limits = c(0,1),
                        title_expr = latex2exp::TeX(r'($\alpha_{\textit{pos}}$)'))
  p_val_unc_an = Plot_recov(data = data_recov,
                        model = 'uncertainty_seplr',
                        param = 'alpha_neg',
                        limits = c(0,1),
                        title_expr = latex2exp::TeX(r'($\alpha_{\textit{neg}}$)'))
  p_val_unc_pi = Plot_recov(data = data_recov,
                            model = 'uncertainty_seplr',
                            param = 'pi',
                            limits = c(0,1),
                            title_expr = latex2exp::TeX(r'($\pi$)'))
  
  # Surprise+Unc
  p_surprise_unc_l = Plot_recov(data = data_recov,
                                model = 'uncertainty_surprise',
                                param = 'l',
                                limits = c(0,1),
                                add_s_bounds = TRUE,
                                title_expr = latex2exp::TeX(r'($\textit{l}$)'))
  p_surprise_unc_u = Plot_recov(data = data_recov,
                                model = 'uncertainty_surprise',
                                param = 'u',
                                limits = c(0,1),
                                add_s_bounds = TRUE,
                                title_expr = latex2exp::TeX(r'($\textit{u}$)'))
  p_surprise_unc_s = Plot_recov(data = data_recov,
                                model = 'uncertainty_surprise',
                                param = 's',
                                limits = c(0,10),
                                add_s_bounds = TRUE,
                                title_expr = latex2exp::TeX(r'($\textit{s}$)')) +
    geom_vline(data = cutoffs,
               aes(xintercept = as.numeric(x)),
               linetype = 'dashed')
  p_surprise_unc_pi = Plot_recov(data = data_recov,
                                model = 'uncertainty_surprise',
                                param = 'pi',
                                limits = c(0,1),
                                add_s_bounds = TRUE,
                                title_expr = latex2exp::TeX(r'($pi$)'))
  
  # Combine first colummn
  p_1 = cowplot::plot_grid(p_rw_a, p_unc_a, p_val_ap, p_surprise_l, p_val_unc_ap, p_surprise_unc_l,
                         ncol = 1,
                         rel_heights = c(1,1,1,1,1,1),
                         align = 'v',
                         axis = 'tb')
  # Second column
  p_2 = cowplot::plot_grid(NULL, p_unc_pi, p_val_an, p_surprise_u, p_val_unc_an, p_surprise_unc_u,
                           ncol = 1,
                           rel_heights = c(1,1,1,1,1,1),
                           align = 'v',
                           axis = 'tb')
  p_3 = cowplot::plot_grid(NULL, NULL, NULL, p_surprise_s, p_val_unc_pi, p_surprise_unc_s,
                           ncol = 1,
                           rel_heights = c(1,1,1,1,1,1),
                           align = 'v',
                           axis = 'tb') +
    theme(plot.margin = margin(0,5,0,0,'pt'))
  p_4 = cowplot::plot_grid(NULL, NULL, NULL, NULL, NULL, p_surprise_unc_pi,
                           ncol = 1,
                           rel_heights = c(1,1,1,1,1,1),
                           align = 'v',
                           axis = 'tb')
  p = cowplot::plot_grid(p_1, NULL, p_2, NULL, p_3, NULL, p_4,
                         ncol = 7,
                         rel_widths = c(1,0,1,0,1.1,0,1),
                         align = 'v',
                         axis = 'tb')
  
  title_y = textGrob("Estimate (Recovery)", 
                     gp = gpar(fontface = "bold",
                               fontsize = 15),
                     rot = 90)
  
  title_x = textGrob("Input (Simulation)", 
                     gp = gpar(fontface = "bold",
                               fontsize = 15))
  p = grid.arrange(arrangeGrob(p,
                               left = title_y,
                               bottom = title_x))
  
  p
  
  # Prepare data for correlation plot
  data_corrplot = data_recov %>%
    .[, sim_id := paste0(participant_id, '_', iter)] %>%
    data.table::setnames(., old = c('coefs', 'input_params'),
                         new = c('est', 'in'),
                         skip_absent=TRUE) %>%
    data.table::dcast(sim_id + model ~ params, value.var = c('est', 'in'))
  
  Plot_corr = function(data,
                       model_name){
    
    data_corr = data %>%
      .[, sim_id := paste0(participant_id, '_', iter)] %>%
      .[model == model_name,] %>%
      data.table::setnames(., old = c('coefs', 'input_params'),
                           new = c('est', 'in'),
                           skip_absent=TRUE) %>%
      data.table::dcast(sim_id + model ~ params, value.var = c('est', 'in'))
    
    # Select estimation column names to only use selected columns during plotting
    est_cols = colnames(data_corr)[grep('est_', colnames(data_corr))]
    
    corr_mat  = correlate(data_corr,
                             method = "spearman",
                             diagonal = 1) %>%
      corrr::focus(all_of(est_cols)) %>%
      as.data.table(.) %>%
      data.table::setnames(., old = 'term', new = 'x') %>%
      data.table::melt(id.vars = 'x',
                       variable.name = 'y')
    
    p = ggplot(data = corr_mat,
               aes(x = x, y = y, fill = value, label = round(value, 2))) +
      geom_tile() +
      geom_text(color = 'black') +
      theme(panel.background = element_rect(fill = NA,
                                            color = NA),
            panel.grid = element_blank(),
            legend.position = 'none',
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            #axis.text = element_text(size = 12),
            plot.margin = margin(0,0,0,0,'pt')) +
      scale_fill_viridis(option = 'D')
    return(p)
  }
  
  p_c_rw = Plot_corr(data = data_recov,
                      model = 'rw') +
    scale_x_discrete(labels = c(latex2exp::TeX(r'($\textit{$\alpha$}$)'))) +
    scale_y_discrete(labels = c(latex2exp::TeX(r'($\textit{$\alpha$}$)')))
  
  p_c_unc = Plot_corr(data = data_recov,
                     model = 'uncertainty') +
    scale_x_discrete(labels = c(latex2exp::TeX(r'($\textit{$\alpha$}$)'),
                                latex2exp::TeX(r'($\textit{$\pi$}$)'))) +
    scale_y_discrete(labels = c(latex2exp::TeX(r'($\textit{$\alpha$}$)'),
                                latex2exp::TeX(r'($\textit{$\pi$}$)')))
  
  p_c_val = Plot_corr(data = data_recov,
                      model = 'seplr') +
    scale_x_discrete(labels = c(latex2exp::TeX(r'($\textit{\alpha_{\textit{pos}}}$)'),
                                latex2exp::TeX(r'($\textit{\alpha_{\textit{neg}}}$)'))) +
    scale_y_discrete(labels = c(latex2exp::TeX(r'($\textit{\alpha_{\textit{pos}}}$)'),
                                latex2exp::TeX(r'($\textit{\alpha_{\textit{neg}}}$)')))
  
  p_c_sup = Plot_corr(data = data_recov[inbounds == TRUE],
                      model = 'surprise') +
    scale_x_discrete(labels = c(latex2exp::TeX(r'($l$)'),
                                latex2exp::TeX(r'($s$)'),
                                latex2exp::TeX(r'($u$)'))) +
    scale_y_discrete(labels = c(latex2exp::TeX(r'($l$)'),
                                latex2exp::TeX(r'($s$)'),
                                latex2exp::TeX(r'($u$)')))
  
  p_c_val_unc = Plot_corr(data = data_recov,
                      model = 'uncertainty_seplr') +
    scale_x_discrete(labels = c(latex2exp::TeX(r'($\textit{\alpha_{\textit{pos}}}$)'),
                                latex2exp::TeX(r'($\textit{\alpha_{\textit{neg}}}$)'),
                                latex2exp::TeX(r'($\pi$)'))) +
    scale_y_discrete(labels = c(latex2exp::TeX(r'($\textit{\alpha_{\textit{pos}}}$)'),
                                latex2exp::TeX(r'($\textit{\alpha_{\textit{neg}}}$)'),
                                latex2exp::TeX(r'($\pi$)')))
  
  p_c_sup_unc = Plot_corr(data = data_recov[inbounds == TRUE],
                      model = 'uncertainty_surprise') +
    scale_x_discrete(labels = c(latex2exp::TeX(r'($l$)'),
                                latex2exp::TeX(r'($s$)'),
                                latex2exp::TeX(r'($u$)'),
                                latex2exp::TeX(r'($\pi$)'))) +
    scale_y_discrete(labels = c(latex2exp::TeX(r'($l$)'),
                                latex2exp::TeX(r'($s$)'),
                                latex2exp::TeX(r'($u$)')))
  
  p_c = cowplot::plot_grid(p_c_rw, p_c_unc, p_c_val, p_c_sup, p_c_val_unc, p_c_sup_unc,
                           ncol = 1,
                           rel_heights = c(1,1,1,1,1,1),
                           align = 'vh',
                           axis = 'tbrl')
  
  cowplot::plot_grid(p_c, p,
                     ncol = 2,
                     axis = 'bt',
                     align = 'h')
}


