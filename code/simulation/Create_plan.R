
Create_plan <- function(n_blocks,
                          perc_forced,
                          blocks_per_task,
                          dist_list){
  
  # Check inputs
  if(length(dist_list)/3 != n_blocks/blocks_per_task){
    stop(paste("Number of provided distributions: ",
               length(dist_list),
               " (",
               length(dist_list)/3,
               " tasks).\n",
               "With three distributions per block and ",
               blocks_per_task,
               " blocks per task the number of blocks can only be ",
               length(dist_list)/3*blocks_per_task,
               " (and not ",
               n_blocks,
               ").",
               sep=''))
  }
  
  # Number of specified distributions
  n_dist = length(dist_list)
  # Number of task versions (one per three distributions, since three dists in one task version)
  n_vers = n_dist/3
  # number of assigned dist
  n_plan = 3*n_blocks
  
  # Get general assignment of blocks, task versions, and distributions
  plan = data.frame(matrix(NA,n_plan,9))
  colnames(plan) = c('block',
                     'version',
                     'dist_nr',
                     'dist_name',
                     'arg_1',
                     'arg_2', 
                     'arg_3',
                     'arg_4',
                     'arg_5')
  # Fill plan with blocks, versions, and assigned distributions
  plan$block = rep(c(1:n_blocks), each=3)
  plan$version = rep(c(1:n_vers), each=3*blocks_per_task)
  plan$dist_nr = rep(c(1:3), times=n_blocks) + (plan$version - 1) * 3
  # Fill plan with distributions
  for(dist_count in 1:n_dist){
    dist_vec = dist_list[[dist_count]]
    dist_type = dist_vec[1]
    
    # Set name of distribution
    plan$dist_name[plan$dist_nr == dist_count] = dist_type
    
    # Depending on dist type, fill arguments in plan
    if(dist_type == 'beta'){
      # Read out alpha parameter
      plan$arg_1[plan$dist_nr == dist_count] = dist_vec[2]
      # Read out beta parameter
      plan$arg_2[plan$dist_nr == dist_count] = dist_vec[3]
    }
    if(dist_type == 'bimodal'){
      # Read out parameters of first distribution (mean, sd)
      plan$arg_1[plan$dist_nr == dist_count] = dist_vec[2]
      plan$arg_2[plan$dist_nr == dist_count] = dist_vec[3]
      # Read out parameters of second distribution (mean, sd)
      plan$arg_3[plan$dist_nr == dist_count] = dist_vec[4]
      plan$arg_4[plan$dist_nr == dist_count] = dist_vec[5]
      # Read out proportion of distributions
      plan$arg_5[plan$dist_nr == dist_count] = dist_vec[6]
    }
    if(dist_type == 'gaussian'){
      # Read mean parameter
      plan$arg_1[plan$dist_nr == dist_count] = dist_vec[2]
      # Read SD parameter
      plan$arg_2[plan$dist_nr == dist_count] = dist_vec[3]
    }
    if(dist_type == 'uniform'){
      # Read out min parameter
      plan$arg_1[plan$dist_nr == dist_count] = dist_vec[2]
      # Read out max parameter
      plan$arg_2[plan$dist_nr == dist_count] = dist_vec[3]
    }
  }
  
  # Assign random stimuli to distributions for each task version
  stimuli = sample(seq(n_dist))
  plan$stimuli = unlist(lapply(plan$dist_nr, function(x) stimuli[x]))
  
  # Set data types for arguments
  plan$arg_1 = as.numeric(plan$arg_1)
  plan$arg_2 = as.numeric(plan$arg_2)
  plan$arg_3 = as.numeric(plan$arg_3)
  plan$arg_4 = as.numeric(plan$arg_4)
  plan$arg_5 = as.numeric(plan$arg_5)
  plan$stimuli = as.numeric(plan$stimuli)
  
  # Return plan
  return(plan)
}