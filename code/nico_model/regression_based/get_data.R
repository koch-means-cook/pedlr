get_data = function(data_path) {
  file = file.path(data_path, '*.tsv', fsep = .Platform$file.sep)
  files = Sys.glob(file)
  exclfile = grep('exclusions', files)
  temp = data.table::fread(files[exclfile], sep = '\t', na.strings = 'n/a')
  exclids = temp$participant_id[temp$mod == 'choice_performance']

  files = files[-exclfile]
  files = files[-sapply(exclids, function(x) grep(x,files))]

  DATA = data.table()
  for(i in seq(length(files))){
    temp = data.table::fread(files[i], sep = '\t', na.strings = 'n/a')
    temp$fit = i
    DATA = rbind(DATA, temp)
  }

  # calculate if choice was correct or not
  DATA$correct = c('left', 'right')[(DATA$option_right > DATA$option_left)*1+1]
  DATA$correct_choice = DATA$correct == DATA$choice
  DATA$correct_choice[DATA$forced_left == 1] = DATA$choice[DATA$forced_left == 1] == 'left'
  DATA$correct_choice[DATA$forced_right == 1] = DATA$choice[DATA$forced_right == 1] == 'right'

  ids = unique(DATA$participant_id)

  # Get chosen option (1 = left, 2 = right, 3 = none/timeout) (ERROR: this seems to default choice to left in case of NA)
  tmpidx = cbind(1:(length(ids) * 480), factor(DATA$choice, c('left', 'right'), c(1, 2)))
  # Get chosen bandit (option choice col)
  cmat = cbind(DATA$option_left, DATA$option_right)
  DATA$chosen_bandit = cmat[tmpidx]

  # make bandit variable (labeling 12 and 21 the same)
  DATA$bandit = paste(DATA$option_left, DATA$option_right, sep = '')
  return(DATA)
}
