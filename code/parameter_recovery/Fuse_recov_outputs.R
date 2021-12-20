library(here)
library(data.table)
library(tools)
library(optparse)

# Load functions
source_path = file.path(here::here(), 'code', 'utils',
                        fsep = .Platform$file.sep)
source_files = list.files(source_path, pattern = "[.][rR]$",
                          full.names = TRUE, recursive = TRUE)
invisible(lapply(source_files, function(x) source(x)))

Fuse_recov_outputs = function(delete_source = FALSE){
  
  # Get repo dir
  base_path = here::here()
  # Get dir of data to fuse
  fuse_path = file.path(base_path, 'derivatives', 'parameter_recovery',
                        fsep = .Platform$file.sep)
  
  # Get designs
  path = file.path(here::here(), 'pedlr-task', 'client', 'public', 'designs', '*.tsv')
  designs = tools::file_path_sans_ext(basename(Sys.glob(path)))
  
  # Create name of outfile
  out_file = file.path(fuse_path,
                       'param_recov.tsv',
                       fsep = .Platform$file.sep)
  out = data.table()
  
  # Loop over data of each design
  for(des in designs){
    # Loop over different models
    for(model in c('Rw', 'Pedlr_simple', 'Pedlr', 'Pedlr_fixdep', 'Pedlr_interdep')){
      
      # Get data for design and model
      pattern = paste(des,'*', model, '-*', '.tsv', sep = '')
      data = Sys.glob(file.path(fuse_path, pattern, fsep = .Platform$file.sep))

      # If data is not empty
      if(length(data) != 0){
        # Fuse all data files
        for(i in data){
          temp = data.table::fread(i,
                                   sep = '\t',
                                   header = TRUE,
                                   na.strings = 'n/a')
          # Add column giving file count
          file_count = unlist(strsplit(basename(i), '-'))
          file_count = file_count[length(file_count)]
          file_count = as.numeric(substr(file_count, 1,3))
          temp$file = file_count
          # Add model as column
          temp$model = model
          # Bind individual file to complete file of participant
          out = rbind(out, temp)
        }
        
        # If source files should be deleted
        if(delete_source){
          unlink(data)
        }
        
      }
    }
  }
  
  # Write fused table to output file
  data.table::fwrite(out,
                     out_file,
                     na = 'n/a',
                     row.names = FALSE,
                     sep = '\t')
  
  # Give message to user
  message(paste('Created ', out_file, '...', sep = ''))
  # Give message to user
  message(paste('   Deleted source for ', basename(out_file), '...', sep = ''))
  message('...done')
  
}


# Create options to pass to script
option_list = list(
  make_option(c('-d', '--delete_source'),
              type='character',
              default = FALSE,
              help = 'TRUE if files that are fused together to create output should be deleted',
              metavar = 'DELETE_SOURCE'))

# provide options in list to be callable by script
opt_parser = OptionParser(option_list = option_list)
opt = parse_args(opt_parser)

# Call function
Fuse_recov_outputs(delete_source = opt$delete_source)

