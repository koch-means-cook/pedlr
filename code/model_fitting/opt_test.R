library(optparse)

bla = function(x){
  print(x)
}

split_list = function(object,
                      flag,
                      value,
                      parser){
  x = as.numeric(unlist(strsplit(value, ',')))
  return(x)
}

option_list = list(
  make_option(c('-x', '--x'),
              type='character',
              default = NULL,
              help = 'Path to data file model should be fit to',
              metavar = 'X',
              callback = split_list))
# provide options in list to be callable by script
opt_parser = OptionParser(option_list = option_list)
opt = parse_args(opt_parser)


bla(opt$x)