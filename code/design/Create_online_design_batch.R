
require(jsonlite)
require(stringr)

# For knitting:
source_path = file.path(here::here(), 'code', fsep = .Platform$file.sep)

# Source functions required for this script
source(file.path(source_path, 'design', 'Beta_pseudo_sim.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'design', 'Gaussian_pseudo_sim.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'design', 'Bimodal_pseudo_sim.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'design', 'Uniform_pseudo_sim.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'design', 'Create_plan.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'design', 'Create_block.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'design', 'Create_design.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'design', 'Transform_design_online.R', fsep = .Platform$file.sep))

# Create Design with parameters
n_blocks = 4
perc_forced = 20
blocks_per_task = 2
dist_list = list(c('bimodal', 30, 30, 80, 10, 0.2),
                 c('beta', 3, 2),
                 c('gaussian', 2*100/3, 30),
                 c('beta', 3, 2),
                 c('bimodal', 30, 0.2, 40, 10, 10),
                 c('beta', 2, 3))

# Twenty different designs
for(i in seq(20)){
  
  # Set seed for each iteration of design creation
  set.seed(i)
  
  # Get task plan
  plan = Create_plan(n_blocks,
                     perc_forced,
                     blocks_per_task,
                     dist_list)
  
  # Get task design
  design = Create_design(n_blocks,
                         perc_forced,
                         blocks_per_task,
                         dist_list)
  
  # Port design for online task
  design_online = Transform_design_online(design)
  
  # Put different runs into json format
  json_list = lapply(design_online, FUN = jsonlite::toJSON,
                     dataframe = 'rows',
                     pretty = TRUE,
                     na = 'string')
  
  # Save json files for each run (in pedlr-task submodule)
  file = file.path(here::here(),
                   'pedlr-task',
                   'client',
                   'public',
                   'designs',
                   paste('design-',
                         str_pad(as.character(i), width = 2, pad = '0'),
                         sep = ''))
  for(json_count in seq(length(json_list))){
    name = paste(file,
                 paste('_run-', as.character(json_count), sep = ''),
                 '.json',
                 sep = '')
    write(json_list[[json_count]], file = name)
  }
}


