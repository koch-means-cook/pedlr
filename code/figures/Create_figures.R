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
  
  
  # Task figure
  message("Creating task figure...\n")
  p_t = Figure_task()
  out_file = file.path(base_path, 'derivatives', 'figures', 'f_t.pdf',
                       fsep = .Platform$file.sep)
  ggsave(filename = out_file,
         plot = p_t,
         device = 'pdf',
         width = 7,
         height = 3)
  
  # Behavioral data (Overall correct and RT diff)
  message("Creating behavioral figure...\n")
  p_pcrt = Figure_behav_pcrt() +
    theme(plot.margin = margin(0,0,0,5,'pt'))
  out_file = file.path(base_path, 'derivatives', 'figures', 'f_pcrt.pdf',
                       fsep = .Platform$file.sep)
  ggsave(filename = out_file,
         plot = p_pcrt,
         device = 'pdf',
         width = 6,
         height = 8)
  
  # Behavioral data (rare influence)
  message("Creating surprise figure...\n")
  p_ri = Figure_behav_ri() +
    theme(plot.margin = margin(0,0,0,5,'pt'))
  out_file = file.path(base_path, 'derivatives', 'figures', 'f_ri.pdf',
                       fsep = .Platform$file.sep)
  ggsave(filename = out_file,
         plot = p_ri,
         device = 'pdf',
         width = 7,
         height = 3)
  
  # Behavioral data (Estimation distance bias)
  message("Creating estimation figure...\n")
  p_ed = Figure_behav_ed() +
    theme(plot.margin = margin(0,0,0,5,'pt'))
  out_file = file.path(base_path, 'derivatives', 'figures', 'f_ed.pdf',
                       fsep = .Platform$file.sep)
  ggsave(filename = out_file,
         plot = p_ed,
         device = 'pdf',
         width = 2.5,
         height = 3)
  
  # Model comp
  message("Creating model comparison figure...\n")
  p_mc = Figure_model_comp()
  out_file = file.path(base_path, 'derivatives', 'figures', 'f_mc.pdf',
                       fsep = .Platform$file.sep)
  ggsave(filename = out_file,
         plot = p_mc,
         device = 'pdf',
         width = 8,
         height = 5)
  
  # Model surprsie
  message("Creating surprise model figure...\n")
  p_ms = Figure_model_surprise()
  #base_path = here::here()
  out_file = file.path(base_path, 'derivatives', 'figures', 'f_ms.pdf',
                       fsep = .Platform$file.sep)
  ggsave(filename = out_file,
         plot = p_ms,
         device = 'pdf',
         width = 8,
         height = 9)
  
  
}

Create_figures()
