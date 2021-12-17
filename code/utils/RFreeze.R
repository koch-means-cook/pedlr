library(pacman)
library(plyr)
library(here)
library(optparse)

RFreeze = function(path,
                   save_to_path = FALSE){
  
  # Get installed packages
  ps = pacman::p_lib()
  
  # Get package versions
  vs = lapply(ps, packageVersion)
  
  # Store package with version in df
  res = plyr::ldply(vs, data.frame)
  colnames(res) = 'version'
  res$pkg = ps
  # Switch columns
  res = res[,c(2,1)]
  
  # If output should be saved instead of returned
  if(save_to_path){
    file = file.path(path,
                     'requirements_r.txt',
                     fsep = .Platform$file.sep)
    write.table(res,
               file = file,
               sep = '==',
               quote = FALSE,
               row.names = FALSE,
               col.names = FALSE)
  } else{
    return(res) 
  }
  
}

# Create options to pass to script
option_list = list(
  make_option(c('-p', '--path'),
              type='character',
              default = NULL,
              help = 'Path to output dir (file will always be called r_requirements.txt)',
              metavar = 'PATH'),
  make_option(c('-s', '--save_to_path'),
              type='character',
              default = NULL,
              help = 'TRUE if file should be saved',
              metavar = 'SAVE'))

# provide options in list to be callable by script
opt_parser = OptionParser(option_list = option_list)
opt = parse_args(opt_parser)

# Cal wrapper with command line inputs
RFreeze(path = opt$path,
        save_to_path = opt$save_to_path)


