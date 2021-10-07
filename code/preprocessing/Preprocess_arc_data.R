library(optparse)
library(here)
library(jsonlite)
library(data.table)
library(magrittr)

# input_path = '/Volumes/MPRG-Neurocode/Data/pedlr_2021_koch/20210929_prolific_pedlr-pilot-03/raw/2021-10-07 16_49_20.json'
# demo_path = '/Volumes/MPRG-Neurocode/Data/pedlr_2021_koch/20210929_prolific_pedlr-pilot-03/raw/pedlr-pilot-03_demographic.csv'


Preprocess_arc_data = function(input_path,
                               demo_path,
                               out_dir){
  
  # Load conversion function
  source_path = file.path(here::here(),
                          'code',
                          'preprocessing',
                          'Raw_to_data.R',
                          fsep = .Platform$file.sep)
  source(source_path)
  
  # Load json file of participant
  data = data.table(jsonlite::fromJSON(input_path))
  
  # Load demo data
  demo_data = data.table(read.table(file = demo_path,
                                    header = TRUE,
                                    sep = ',',
                                    na.strings = ''))
  
  # Convert data
  data = Raw_to_data(data = data,
                     demo_data = demo_data,
                     add_demo = TRUE,
                     delete_prolific = TRUE)
  
  # Get name to save data as
  out_name = paste(unique(data$participant_id),
                   '_exp_data.tsv',
                   sep = '')
  out_file = file.path(out_dir,
                       out_name,
                       fsep = .Platform$file.sep)
  
  # Save preprocessed data
  write.table(data,
              file = out_file,
              sep = '\t',
              na = 'n/a',
              row.names = FALSE)
  
  # Give message to user:
  msg = paste('Saved as: ', out_name)
  message(msg)
  
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
Preprocess_arc_data(input_path = opt$input_path,
                    demo_path = opt$demo_path,
                    out_dir = opt$out_dir)


