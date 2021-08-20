

library(reshape2)
library(ggplot2)
library(plotly)
library(plyr)
library(Rfast)
library(data.table)
library(knitr)
#library(rstudioapi)
#library(here)
library(viridis)
library(cowplot)
library(here)
library(optparse)

# Set paths
base_path = file.path(here::here(), fsep = .Platform$file.sep)
derivatives_path = file.path(base_path, 'derivatives', 'simulation',
                             fsep = .Platform$file.sep)
source_path = file.path(base_path, 'code', fsep = .Platform$file.sep)

# Source functions required for this script
source(file.path(source_path, 'simulation', 'Sample_subjects.R',
                 fsep = .Platform$file.sep))
source(file.path(source_path, 'models', 'Pedlr.R',
                 fsep = .Platform$file.sep))
source(file.path(source_path, 'models', 'Pedlr_interdep.R',
                 fsep = .Platform$file.sep))
source(file.path(source_path, 'simulation', 'Apply_model.R',
                 fsep = .Platform$file.sep))
source(file.path(source_path, 'simulation', 'Model_results.R',
                 fsep = .Platform$file.sep))

# Start function
Free_bimodal_modes = function(n_subjects,
                              set_seed,
                              n_blocks,
                              perc_forced,
                              blocks_per_task,
                              dist_list,
                              model,
                              parameters,
                              init_values,
                              shrink_distance_vec,
                              save_data,
                              save_file,
                              load_data,
                              load_file,
                              mode_distance_change_vec){
  
  # In case data should be loaded, just read table of saved outcome
  if(load_data){
    outcome = setDT(read.table(load_file, header = TRUE, sep = '\t'))
  }
  
  
  # In case data should not be loaded, calculate outcome
  if(!load_data){
    
    
    for(change_count in seq(length(mode_distance_change_vec))){
      
      # Get distance change between both modes of bimodal distribution for each 
      # iteration
      distance_change = mode_distance_change_vec[change_count]
      adjusted_dist_list = dist_list
      
      # Apply distance change to bimodal distributions
      for(i in seq(length(adjusted_dist_list))){
        if(adjusted_dist_list[[i]][1] == 'bimodal'){
          # Enhance distance between modes of bimoadl dist
          adjusted_dist_list[[i]][4] = (
            (abs(as.numeric(adjusted_dist_list[[i]][4])) + distance_change) * 
            ((as.numeric(as.numeric(adjusted_dist_list[[i]][4]) > 0) * 2) - 1)
            )
          #print(adjusted_dist_list[[i]][4])
        }
      }
      
      # In first iteration create template to append data to
      if(change_count == 1){
        # Assess bias in correct choices for specific distance of modes
        outcome = Model_bias_correct(n_subjects,
                                     set_seed,
                                     n_blocks,
                                     perc_forced,
                                     blocks_per_task,
                                     adjusted_dist_list,
                                     model,
                                     parameters,
                                     init_values,
                                     shrink_distance_vec,
                                     save_data,
                                     save_file,
                                     load_data,
                                     load_file)$data_bias_correct
        
        # Add column stating mode distance change
        outcome$bimodal_distance_change = distance_change
      }
      
      data = Model_bias_correct(n_subjects,
                                set_seed,
                                n_blocks,
                                perc_forced,
                                blocks_per_task,
                                adjusted_dist_list,
                                model,
                                parameters,
                                init_values,
                                shrink_distance_vec,
                                save_data,
                                save_file,
                                load_data,
                                load_file)$data_bias_correct
      data$bimodal_distance_change = distance_change
      outcome = rbind(outcome, data)
    }
    
  }
  
  # If specified save outcome
  if(save_data){
    write.table(outcome, save_file, sep='\t', row.names = FALSE)
  }
  
  
  return(outcome)
  
}
