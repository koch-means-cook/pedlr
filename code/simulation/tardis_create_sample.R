
# For knitting:
base_path = file.path(here::here(), fsep = .Platform$file.sep)
source_path = file.path(base_path, 'code', fsep = .Platform$file.sep)
save_path = file.path(base_path, 'derivatives', 'simulation',
                      fsep = .Platform$file.sep)

# Source functions required for this script
source(file.path(source_path, 'design', 'Create_design_complete.R',
                 fsep = .Platform$file.sep))

# Function to sample design for specific number of simulated participants. 
# Option to set seed.
main = function(n_subjects,
                set_seed,
                n_blocks,
                perc_forced,
                blocks_per_task,
                dist_list){
  
  # Create template to append simulated subjects
  template = Create_design(n_blocks = n_blocks,
                           perc_forced = perc_forced,
                           blocks_per_task = blocks_per_task,
                           dist_list = dist_list)
  # Add column indicating subject
  template$sim_subject = NA
  # Cut template for appending
  template = template[0,]
  
  # For each subject
  for(sub_count in seq(n_subjects)){
    
    # Set seed in case option was selected
    if(set_seed){
      set.seed(sub_count)
    }
    
    # Get design for participant
    design = Create_design(n_blocks = n_blocks,
                           perc_forced = perc_forced,
                           blocks_per_task = blocks_per_task,
                           dist_list = dist_list)
    # Add column for subject
    design$sub_id = sub_count
    
    # Append subjects design to template
    template = rbind(template, design)
  }
  
  return(template)
  
}

# Reward lower and upper boundary
reward_space_lb = 1
reward_space_ub = 100

# Gaussian
# SD similar for all Gaussians
gaussian_sd = (100 * 1/6) / 3
# Mean of different Gaussians
# Lower end
gaussian_le_mean = (100 * 1/6) * 1
# Mid
gaussian_mid_mean = (100 * 1/6) * 3
# Upper end
gaussian_ue_mean = (100 * 1/6) * 5

# Betas
# Set up "skewdness" parameter for easier change of slope
beta_skewedness = 5
# Gain Beta
beta_a_gain = beta_skewedness/3
beta_b_gain = 2*beta_skewedness/3
# Loss Beta
beta_a_loss = 2*beta_skewedness/3
beta_b_loss = beta_skewedness/3

# Uniform
uniform_min = reward_space_lb
uniform_max = reward_space_ub

# Bimodal
# Same sd for each mode
bimodal_sd = gaussian_sd
# Same relative proportion (between modes) for both bimodals
bimodal_rel_proportion = 0.2
## Overall mean bimodal distributions
bimodal_mean_gain = (100 * 1/6) * 2
bimodal_mean_loss = (100 * 1/6) * 4
# Distance between modes in gain and loss bimodal
bimodal_distance_gain = 40
bimodal_distance_loss = -40

# Number of subjects
n_subjects = 40

# Set seed for each subject
set_seed = TRUE

# Number of overall blocks
n_blocks = 6

# Number of blocks dedicated to each task version
blocks_per_task = 3

# Percentage of forced choice trials
perc_forced = 20

# List of different distributions with parameters (three distributions for each 
# task version)
dist_list = list(c('gaussian', gaussian_le_mean, gaussian_sd),
                 c('bimodal',
                   bimodal_mean_gain,
                   bimodal_rel_proportion,
                   bimodal_distance_gain,
                   bimodal_sd,
                   bimodal_sd),
                 c('gaussian', gaussian_mid_mean, gaussian_sd),
                 c('gaussian', gaussian_mid_mean, gaussian_sd),
                 c('bimodal',
                   bimodal_mean_loss,
                   bimodal_rel_proportion,
                   bimodal_distance_loss,
                   bimodal_sd,
                   bimodal_sd),
                 c('gaussian', gaussian_ue_mean, gaussian_sd))

# Create designs for simulated participants
design = main(n_subjects = n_subjects,
              set_seed = set_seed,
              n_blocks = n_blocks,
              perc_forced = perc_forced,
              blocks_per_task = blocks_per_task,
              dist_list = dist_list)

# Save design 
file = file.path(save_path, 'simulated_participants.tsv')
write.table(design, file = file, sep = '\t', row.names = FALSE)

