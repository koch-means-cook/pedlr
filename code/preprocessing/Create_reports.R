library(rmarkdown)
library(here)
library(optparse)

# input_path = '/Volumes/MPRG-Neurocode/Data/pedlr_2021_koch/20210929_prolific_pedlr-pilot-03/raw'

Create_reports = function(input_path){
  
  # Get report script to render
  rmd_file = file.path(here::here(),
                       'code',
                       'preprocessing',
                       'Batch_report.Rmd',
                       fsep = .Platform$file.sep)
  
  # Get all experiment result .json to get report of (based on input folder)
  files = file.path(input_path,
                    '*.json',
                    fsep = .Platform$file.sep)
  files = Sys.glob(files)
  
  # Set up message batch for end of script
  final_msg = list()
  
  # For each data file
  for(i in files){
    
    # Define name of output html (same as input file but as .html)
    output_path = paste(substr(i, 0, (nchar(i) - 5)),
                    '.html',
                    sep = '')
    
    # Render report by passing participant-specific arguments to render command
    rmarkdown::render(rmd_file,
                      params = list(base_path = here::here(),
                                    input = i),
                      output_file = output_path)
    
    final_msg = append(final_msg, paste('\t', basename(i), sep = ''))
    
  }
  
  # Give message to user:
  message('\n')
  message('-------------------------------------------')
  message('Created reports for:')
  for(msg in final_msg){
    message(msg)
  }
  message('-------------------------------------------')
  message('\n')
  
}

# Create options to pass to script
option_list = list(
  make_option(c('-i', '--input_path'),
              type='character',
              default = NULL,
              help = 'Path to results folder to create report for each data file',
              metavar = 'INPUT_PATH'))

# provide options in list to be callable by script
opt_parser = OptionParser(option_list = option_list)
opt = parse_args(opt_parser)

# Call main function
Create_reports(input_path = opt$input_path)

