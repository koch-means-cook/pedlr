library(ggplot2)
library(lemon)

Neurocodify_plot = function(input_plot){
  
  res = input_plot +
    # Use capped coordinate system (liney will not touch extremes)
    coord_capped_cart(left='both',
                      bottom='both',
                      expand = TRUE) +
    # Set theme for plot
    theme(panel.border=element_blank(),
          axis.line=element_line(),
          panel.background = element_rect(fill = 'white'),
          panel.grid = element_line(color = '#f5f5f5'),
          strip.background = element_rect(fill = 'transparent',
                                          color = 'transparent'),
          strip.text = element_text(face = 'bold'))
  
  return(res)
  
}