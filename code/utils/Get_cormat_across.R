library(data.table)
library(magrittr)
library(ggplot2)

Get_cormat_across = function(data,
                             model_name,
                             colnames,
                             plot_title){
  # Prepare data for correlation
  corr_data = data %>%
    .[, recov := second_solution] %>%
    .[,real_iter := seq(.N),
      by = c('model', 'design_run1', 'design_run2', 'para')] %>%
    data.table::dcast(model + design_run1 + design_run2 + real_iter ~ para, value.var = c('true', 'recov'))
  
  # Specify data to plot correlation of
  data_plot = cor(corr_data[model == model_name, ..colnames]) %>%
    reshape2::melt(.) %>%
    as.data.table(.) %>%
    .[, row := seq(.N)] %>%
    # Only select correlation between true and recovered params (and not truth vs truth)
    .[, need := unlist(strsplit(as.character(Var1), '_'))[1] != unlist(strsplit(as.character(Var2), '_'))[1],
      by = 'row'] %>%
    .[need == TRUE, c('Var1', 'Var2', 'value')]
  # Eliminate duplicates (e.g. true_a1 vs. recov_a1 and recov_a1 vs. true_a1)
  data_plot = data_plot[1:(nrow(data_plot)/2)]
  # Eliminate levels
  data_plot$Var1 = as.character(data_plot$Var1)
  data_plot$Var2 = as.character(data_plot$Var2)
  
  
  # Plot correlation matrix
  p = ggplot(data_plot,
             aes(x = Var1, y = Var2, fill = value, label = round(value, 2))) +
    theme_minimal()+ 
    geom_tile() +
    geom_label(fill = 'white', color = 'black') +
    scale_fill_viridis(option='D') +
    coord_fixed() +
    labs(title = plot_title) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = 'None',
          axis.title = element_blank(),
          axis.text.x = element_text(angle = 90))
  
  return(p)
}