library(here)
library(data.table)
library(optparse)

# Load functions
source_path = file.path(here::here(), 'code', 'utils',
                        fsep = .Platform$file.sep)
source_files = list.files(source_path, pattern = "[.][rR]$",
                          full.names = TRUE, recursive = TRUE)
invisible(lapply(source_files, function(x) source(x)))

Fuse_fitting_outputs = function(delete_source = FALSE){
  
  # Get repo dir
  base_path = here::here()
  # Get dir of data to fuse
  fuse_path = file.path(base_path, 'derivatives', 'model_fitting',
                        fsep = .Platform$file.sep)
  
  # Get participant IDs
  participants = Get_participants()
  
  # Loop over data of each participant
  for(id in participants){
    # Loop over different models
    for(model in c('Rw', 'Pedlr_simple', 'Pedlr_simple_const', 'Pedlr', 'Pedlr_step', 'Pedlr_fixdep', 'Pedlr_interdep')){
      # Get data for participant and model
      pattern = paste(id,'*', model, '-*', '.tsv', sep = '')
      data = Sys.glob(file.path(fuse_path, pattern, fsep = .Platform$file.sep))
      # Get name of finished fused output file
      out_file = file.path(fuse_path,
                           paste(id, '-fit-', model, '.tsv', sep = ''),
                           fsep = .Platform$file.sep)
      # In case output file is in list of files that should create the output,
      # delete it from the files that will be fused
      if(out_file %in% data){
        data = data[which(data != out_file)]
      }
      # If data is not empty
      if(length(data) != 0){
        # Fuse all data files
        out = data.table()
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
        # Write fused table to output file
        data.table::fwrite(out,
                           out_file,
                           na = 'n/a',
                           col.names = TRUE,
                           row.names = FALSE,
                           sep = '\t')
        
        # Give message to user
        message(paste('Created ', out_file, '...', sep = ''))
        
        # If source files should be deleted
        if(delete_source){
          unlink(data)
          
          # Give message to user
          message(paste('   Deleted source for ', basename(out_file), '...', sep = ''))
        }
        
      }
    }
  }
  
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
Fuse_fitting_outputs(delete_source = opt$delete_source)

