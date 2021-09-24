
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
dist_list = list(c('gaussian', 100 * 2/6, (100 * 1/6) / 3),
                 c('bimodal', 100 * 3/6, 0.2, -35, (100 * 1/6) / 3, (100 * 1/6) / 3),
                 c('gaussian', 100 * 4/6, (100 * 1/6) / 3),
                 c('gaussian', 100 * 3/6, (100 * 1/6) / 3),
                 c('bimodal', 100 * 4/6, 0.2, -35, (100 * 1/6) / 3, (100 * 1/6) / 3),
                 c('gaussian', 100 * 5/6, (100 * 1/6) / 3))

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
                         dist_list,
                         prop_rare = 0.2,
                         min_forced_with_rare_per_block = 2,
                         min_rate_after_rare_forced_per_block = 2)
  
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
    
    # Save design alongside (in pedlr-task submodule)
    name = paste(file,
                 paste('_run-', as.character(json_count), sep = ''),
                 '.tsv',
                 sep = '')
    write.table(design[design$task_version == json_count,],
                file = name,
                sep = '\t',
                na = 'n/a',
                row.names = FALSE)
  }
}


