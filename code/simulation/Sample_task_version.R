
# For knitting:
source_path = '/Volumes/MPRG-Neurocode/Users/christoph/pedlr/code/simulation/'

# Source functions required for this script
source(file.path(source_path, 'Create_miniblock.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'Create_miniblock_var_forced.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'Beta_pseudo_sim.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'Gaussian_pseudo_sim.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'Bimodal_pseudo_sim.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'Create_plan.R', fsep = .Platform$file.sep))

Sample_task_version = function(plan,
                               ){
  
}