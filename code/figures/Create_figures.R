library(here)

Create_figures = function(){
  
  # Get path
  base_path = here::here()
  
  # Load pre_written functions
  source_path = file.path(base_path, 'code', 'utils',
                          fsep = .Platform$file.sep)
  source_files = list.files(source_path, pattern = "[.][rR]$",
                            full.names = TRUE, recursive = TRUE)
  invisible(lapply(source_files, function(x) source(x)))
  source_path = file.path(base_path, 'code', 'figures',
                          fsep = .Platform$file.sep)
  source_files = list.files(source_path, pattern = "[.][rR]$",
                            full.names = TRUE, recursive = TRUE)
  source_files = source_files[-which(basename(source_files) == 'Create_figures.R')]
  invisible(lapply(source_files, function(x) source(x)))
  
  
  # Parameter recovery
  p_pr = Figure_parameter_recovery()
  file = file.path(base_path, 'derivatives', 'figures', 'f_pr.pdf',
                   fsep = .Platform$file.sep)
  ggsave(file, p_pr, width = 6, height = 2)
  
  
  # Task figure
  p_t = Figure_task()
  out_file = file.path(base_path, 'derivatives', 'figures', 'f_t.pdf',
                       fsep = .Platform$file.sep)
  ggsave(filename = out_file,
         plot = p_t,
         device = 'pdf',
         width = 7,
         height = 3)
  
  
}