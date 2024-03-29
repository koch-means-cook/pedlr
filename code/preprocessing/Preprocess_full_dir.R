library(optparse)
library(here)
library(jsonlite)
library(data.table)
library(magrittr)

# input_path = '/Volumes/MPRG-Neurocode/Data/pedlr_2021_koch/20210929_prolific_pedlr-pilot-03/raw'
# demo_path = '/Volumes/MPRG-Neurocode/Data/pedlr_2021_koch/20210929_prolific_pedlr-pilot-03/raw/pedlr-pilot-03_demographic.csv'
# out_dir = '/Volumes/MPRG-Neurocode/Data/pedlr_2021_koch/20210929_prolific_pedlr-pilot-03'


Preprocess_full_dir = function(input_path,
                               demo_path,
                               out_dir){
  
  # Load conversion function
  source_path = file.path(here::here(),
                          'code',
                          'preprocessing',
                          'Preprocess_arc_data.R',
                          fsep = .Platform$file.sep)
  source(source_path)
  
  files = file.path(input_path, '*.json', fsep = .Platform$file.sep)
  files = Sys.glob(files)
  
  # Run preprocessing for complete folder
  for(i in files){
    Preprocess_arc_data(input_path = i,
                        demo_path = demo_path,
                        out_dir = out_dir)
  }

}

# Create options to pass to script
option_list = list(
  make_option(c('-i', '--input_path'),
              type='character',
              default = NULL,
              help = 'Path to directory containing raw experiment results ',
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
Preprocess_full_dir(input_path = opt$input_path,
                    demo_path = opt$demo_path,
                    out_dir = opt$out_dir)


