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
  
  # Behavioral data (Overall correct and RT diff)
  p_pcrt = Figure_behav_pcrt()
  out_file = file.path(base_path, 'derivatives', 'figures', 'f_pcrt.pdf',
                       fsep = .Platform$file.sep)
  ggsave(filename = out_file,
         plot = p_pcrt,
         device = 'pdf',
         width = 6,
         height = 6)
  
  # Behavioral data (rare influence)
  p_ri = Figure_behav_ri()
  out_file = file.path(base_path, 'derivatives', 'figures', 'f_ri.pdf',
                       fsep = .Platform$file.sep)
  ggsave(filename = out_file,
         plot = p_ri,
         device = 'pdf',
         width = 7,
         height = 3)
  
  # Behavioral data (Estimation distance bias)
  p_ed = Figure_behav_ed()
  out_file = file.path(base_path, 'derivatives', 'figures', 'f_ed.pdf',
                       fsep = .Platform$file.sep)
  ggsave(filename = out_file,
         plot = p_ed,
         device = 'pdf',
         width = 2.5,
         height = 3)
  
  # Model (model comparison)
  p_mc = Figure_model_comp()
  out_file = file.path(base_path, 'derivatives', 'figures', 'f_mc.pdf',
                       fsep = .Platform$file.sep)
  ggsave(filename = out_file,
         plot = p_mc,
         device = 'pdf',
         width = 6,
         height = 3)
  
  # Model (model validation)
  p_mv = Figure_model_val()
  out_file = file.path(base_path, 'derivatives', 'figures', 'f_mv.pdf',
                       fsep = .Platform$file.sep)
  ggsave(filename = out_file,
         plot = p_mv,
         device = 'pdf',
         width = 6,
         height = 3)
  
  # Model (weighting analysis)
  p_mw = Figure_model_wei()
  out_file = file.path(base_path, 'derivatives', 'figures', 'f_mw.pdf',
                       fsep = .Platform$file.sep)
  ggsave(filename = out_file,
         plot = p_mw,
         device = 'pdf',
         width = 6,
         height = 3)
  
  
}
