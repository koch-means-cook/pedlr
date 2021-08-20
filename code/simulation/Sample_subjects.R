
# For knitting:
source_path = file.path(here::here(), 'code', fsep = .Platform$file.sep)

# Source functions required for this script
source(file.path(source_path, 'design', 'Create_design.R',
                 fsep = .Platform$file.sep))

# Function to sample design for specific number of simulated participants. 
# Option to set seed.
Sample_subjects = function(n_subjects,
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