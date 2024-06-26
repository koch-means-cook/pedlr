Get_plot_guides = function(){
  
  # Set scalings manual
  res = c('Younger\nadults' = '#999999',
          'Older\nadults' = '#56B4E9',
          'Low' = '#0072B2',
          'Mid' = '#009E73',
          'High' = '#E69F00',
          'Free\nchoices' = 'solid',
          'Guided\nchoices' = 'dashed',
          'RW' = '#A36A2C',
          'Uncertainty' = '#D2C08E',
          'Valence' = '#2586A0',
          'Uncertainty +\nValence' = '#A7544B',
          'Unc+Valence' = '#A7544B',
          'Surprise' = '#B7C7B8',
          'Uncertainty +\nSurprise' = '#2F4858',
          'Unc+Surprise' = '#2F4858',
          'Valence+Surprise' = '#bf9fcc')
  
  return(res)
  
}