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
  message("Creating task figure...")
  p_t = Figure_task()
  out_file = file.path(base_path, 'derivatives', 'figures', 'f_t.pdf',
                       fsep = .Platform$file.sep)
  ggsave(filename = out_file,
         plot = p_t,
         device = 'pdf',
         width = 7,
         height = 3)
  
  # Model illustration figure
  message("Creating model illustration...")
  p_mi = Figure_model_illustration()
  out_file = file.path(base_path, 'derivatives', 'figures', 'f_mi.pdf',
                       fsep = .Platform$file.sep)
  ggsave(filename = out_file,
         plot = p_mi,
         device = 'pdf',
         width = 9,
         height = 9)
  
  # Behavioral data (Overall correct and RT diff)
  message("Creating behavioral figure...")
  p_pcrt = Figure_behav_pcrt() +
    theme(plot.margin = margin(0,0,0,5,'pt'))
  out_file = file.path(base_path, 'derivatives', 'figures', 'f_pcrt.pdf',
                       fsep = .Platform$file.sep)
  ggsave(filename = out_file,
         plot = p_pcrt,
         device = 'pdf',
         width = 6,
         height = 8)
  
  # Behavioral data (rare influence and estimation)
  message("Creating surprise and estimation figure...")
  p_ried = Figure_behav_ried()
  out_file = file.path(base_path, 'derivatives', 'figures', 'f_ried.pdf',
                       fsep = .Platform$file.sep)
  ggsave(filename = out_file,
         plot = p_ried,
         device = 'pdf',
         width = 9,
         height = 4)
  
  # Model comp
  message("Creating model comparison figure...")
  p_mc = Figure_model_comp()
  out_file = file.path(base_path, 'derivatives', 'figures', 'f_mc.pdf',
                       fsep = .Platform$file.sep)
  ggsave(filename = out_file,
         plot = p_mc,
         device = 'pdf',
         width = 6,
         height = 7)
  
  # Supplement: Relative model comparison
  message("Creating supplementary figure relative AICc...")
  sp_mc = Figure_mc_rel_aic()$p_full
  out_file = file.path(base_path, 'derivatives', 'figures', 'sf_mc.pdf',
                       fsep = .Platform$file.sep)
  ggsave(filename = out_file,
         plot = sp_mc,
         device = 'pdf',
         width = 9,
         height = 2)
  
  # Model surprise
  message("Creating surprise model figure...")
  p_ms = Figure_model_surprise()
  #base_path = here::here()
  out_file = file.path(base_path, 'derivatives', 'figures', 'f_ms.pdf',
                       fsep = .Platform$file.sep)
  ggsave(filename = out_file,
         plot = p_ms,
         device = 'pdf',
         width = 8,
         height = 9)
  
  # Model parameter recovery
  message("Creating parameter recovery figure...")
  p_pr = Figure_param_recov()
  #base_path = here::here()
  out_file = file.path(base_path, 'derivatives', 'figures', 'f_pr.pdf',
                       fsep = .Platform$file.sep)
  ggsave(filename = out_file,
         plot = p_pr,
         device = 'pdf',
         width = 9.5,
         height = 7.5)
  
}

Create_figures()
