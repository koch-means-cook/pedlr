library(optparse)
library(here)
library(jsonlite)
library(data.table)
library(magrittr)

# input_path = '/Volumes/MPRG-Neurocode/Data/pedlr_2021_koch/20210929_prolific_pedlr-pilot-03/raw/2021-10-07 16_49_20.json'
# demo_path = '/Volumes/MPRG-Neurocode/Data/pedlr_2021_koch/20210929_prolific_pedlr-pilot-03/raw/pedlr-pilot-03_demographic.csv'

# Wrapper for preprocessing function
Preprocess_arc_data_wrapper = function(input_path,
                                       demo_path,
                                       out_dir){
  
  # Load preprocessing function
  source_path = file.path(here::here(),
                          'code',
                          'preprocessing',
                          'Preprocess_arc_data.R',
                          fsep = .Platform$file.sep)
  source(source_path)
  
  # Call function
  Preprocess_arc_data(input_path = input_path,
                      demo_path = demo_path,
                      out_dir = out_dir)
  
  
}

# Create options to pass to script
option_list = list(
  make_option(c('-i', '--input_path'),
              type='character',
              default = NULL,
              help = 'Path to experiment result file that should be preprocessed',
              metavar = 'INPUT_PATH'),
  make_option(c('-d', '--demo_path'),
              type='character',
              default = NULL,
              help = 'Path to demographic data given by prolific',
              metavar = 'DEMO_PATH'),
  make_option(c('-o', '--out_dir'),
              type='character',
              default = NULL,
              help = 'Path to output directory',
              metavar = 'OUT_DIR'))

# provide options in list to be callable by script
opt_parser = OptionParser(option_list = option_list)
opt = parse_args(opt_parser)

# Call main function
Preprocess_arc_data_wrapper(input_path = opt$input_path,
                            demo_path = opt$demo_path,
                            out_dir = opt$out_dir)


