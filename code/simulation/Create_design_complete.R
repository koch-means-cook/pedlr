
# For knitting:
source_path = '/Volumes/MPRG-Neurocode/Users/christoph/pedlr/code/simulation/'

# Source functions required for this script
source(file.path(source_path, 'Create_miniblock.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'Create_miniblock_var_forced.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'Beta_pseudo_sim.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'Gaussian_pseudo_sim.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'Bimodal_pseudo_sim.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'Create_plan.R', fsep = .Platform$file.sep))

Create_design <- function(n_blocks,
                          perc_forced,
                          blocks_per_task,
                          dist_list){
  
  # Build plan mapping blocks, versions, distributions, and parameters
  plan = Create_plan(n_blocks,
                     perc_forced,
                     blocks_per_task,
                     dist_list)
  
  # Sample task version
  
}