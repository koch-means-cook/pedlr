library(rmarkdown)
library(here)
library(optparse)

# input_path = '/Volumes/MPRG-Neurocode/Data/pedlr_2021_koch/20211130_prolific_pedlr-main-younger-01/raw'

Create_reports = function(input_path,
                          all = FALSE){
  
  # Get report script to render
  rmd_file = file.path(here::here(),
                       'code',
                       'preprocessing',
                       'Batch_report.Rmd',
                       fsep = .Platform$file.sep)
  
  # Get all experiment result .json to get report of (based on input folder)
  file_pattern = file.path(input_path,
                           '*.json',
                           fsep = .Platform$file.sep)
  files_json = Sys.glob(file_pattern)
  file_pattern = file.path(input_path,
                           '*.html',
                           fsep = .Platform$file.sep)
  files_html = Sys.glob(file_pattern)
  
  # Check if all files should be processed or only the new files for which there
  # are no html reports so far
  if(all){
    files = files_json
  } else{
    jsons = tools::file_path_sans_ext(basename(files_json))
    htmls = tools::file_path_sans_ext(basename(files_html))
    new_idx = !jsons %in% htmls
    files = files_json[new_idx]
  }
  
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
  if(length(final_msg) == 0){
    message('No new files detected. Use -a TRUE to create reports for all files.')
  } else{
    message('Created reports for:')
    for(msg in final_msg){
      message(msg)
    } 
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
              metavar = 'INPUT_PATH'),
  make_option(c('-a', '--all'),
              type='character',
              default = NULL,
              help = 'If TRUE recreate reports for all file in the given input path, even if they already exist',
              metavar = 'ALL'))

# provide options in list to be callable by script
opt_parser = OptionParser(option_list = option_list)
opt = parse_args(opt_parser)

# Call main function
Create_reports(input_path = opt$input_path,
               all = opt$all)

