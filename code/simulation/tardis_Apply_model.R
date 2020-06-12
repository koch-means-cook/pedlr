
# Load required libraries
library(reshape2)
library(ggplot2)
library(plotly)
library(plyr)
library(Rfast)
library(data.table)
library(knitr)
library(viridis)
library(cowplot)
library(here)
library(optparse)


# Source required functions
source_path = file.path(here::here(), 'code', fsep = .Platform$file.sep)
source(file.path(source_path, 'models', 'Pedlr.R', fsep = .Platform$file.sep))
source(file.path(source_path, 'models', 'Pedlr_interdep.R',
                 fsep = .Platform$file.sep))
source(file.path(source_path, 'models', 'Softmax_choice.R',
                 fsep = .Platform$file.sep))


# Main function to apply model to design
Apply_model = function(data_file,
                       model,
                       parameters,
                       init_values,
                       distance_shrinkage,
                       out_file){
  
  # Load pre-created designs for participants
  sample = read.table(data_file, header = TRUE, sep = '\t')
  
  # Create matrix to store model data
  data_model = matrix(NA,0,20)
  # Convert to data frame
  data_model = data.frame(data_model)
  colnames(data_model) = c('trial',
                           'stim_1',
                           'stim_2',
                           'reward_stim_1',
                           'reward_stim_2',
                           'comp_number',
                           'choice',
                           'choice_prob',
                           'forced_choice',
                           'v_stim_1',
                           'v_stim_2',
                           'v_stim_3',
                           'pe_stim_1',
                           'pe_stim_2',
                           'pe_stim_3',
                           'fpe_stim_1',
                           'fpe_stim_2',
                           'fpe_stim_3',
                           'task_version',
                           'sub_id')
  
  # Apply model to each participant
  for(participant in unique(sample$sub_id)){
    
    # Restrict data to participant
    design = subset(sample, sub_id == participant)
    
    # Move samples of edge distributions closer to center by amount of 
    # shrinkage
    design$reward_stim_1[design$option_left == 1] = (
      design$reward_stim_1[design$option_left == 1] + distance_shrinkage)
    design$reward_stim_1[design$option_left == 3] = (
      design$reward_stim_1[design$option_left == 3] - distance_shrinkage)
    design$reward_stim_2[design$option_right == 1] = (
      design$reward_stim_2[design$option_right == 1] + distance_shrinkage)
    design$reward_stim_2[design$option_right == 3] = (
      design$reward_stim_2[design$option_right == 3] - distance_shrinkage)
    
    # Apply model to each task version
    for(version_count in unique(design$task_version)){
      
      # Restrict data to specific task version
      design_version = subset(design, task_version == version_count)
      
      # Create template to append data
      data = data.frame(matrix(NA,nrow(design_version),ncol(data_model)))
      colnames(data) = colnames(data_model)
      
      # Add data important for model behavior
      data$trial = c(1:nrow(design_version))
      data$stim_1 = design_version$option_left
      data$stim_2 = design_version$option_right
      data$reward_stim_1 = design_version$reward_stim_1
      data$reward_stim_2 = design_version$reward_stim_2
      data$comp_number = design_version$comp_number
      data$sub_id = participant
      
      # Apply specified model to data
      if(model == 'Pedlr'){
        sim = Pedlr(design = design_version,
                    params.alpha0 = parameters$alpha0,
                    params.alpha1 = parameters$alpha1,
                    params.temperature = parameters$temperature,
                    params.reward_space_ub = parameters$reward_space_ub,
                    choice_policy = parameters$choice_policy,
                    init_values = init_values[[version_count]])
      } else if(model == 'Pedlr_interdep'){
        sim = Pedlr_interdep(design = design_version,
                             params.alpha0 = parameters$alpha0,
                             params.alpha1 = parameters$alpha1,
                             params.interdep = parameters$interdep,
                             params.temperature = parameters$temperature,
                             params.reward_space_ub = parameters$reward_space_ub,
                             choice_policy = parameters$choice_policy,
                             init_values = init_values[[version_count]])
      }
      
      # Save model values, PE and fPE for ech trial and subject
      data$choice = sim$choices$choice
      data$choice_prob = sim$choices$choice_prob
      data$forced_choice = sim$choices$forced_choice
      data$v_stim_1 = sim$values$stim_1
      data$v_stim_2 = sim$values$stim_2
      data$v_stim_3 = sim$values$stim_3
      data$pe_stim_1 = sim$PE$stim_1
      data$pe_stim_2 = sim$PE$stim_2
      data$pe_stim_3 = sim$PE$stim_3
      data$fpe_stim_1 = sim$fPE$stim_1
      data$fpe_stim_2 = sim$fPE$stim_2
      data$fpe_stim_3 = sim$fPE$stim_3
      data$task_version = version_count
      
      # Append model data for each task_version
      data_model = rbind(data_model, data)
    }
    
  }
  
  # Fill output with constant parameters
  data_model$alpha0 = parameters$alpha0
  data_model$alpha1 = parameters$alpha1
  data_model$temperature = parameters$temperature
  data_model$reward_space_ub = parameters$reward_space_ub
  data_model$choice_policy = parameters$choice_policy
  data_model$interdep = parameters$interdep
  data_model$distance_shrinkage = distance_shrinkage
  data_model$model = model
  
  # Save data to output file
  write.table(data_model, out_file, sep = '\t', row.names = FALSE)
  
}

# Create options to pass to script
option_list = list(
  make_option(c('--data_file'),
              type='character',
              default = NULL,
              help = 'path to simulated subject designs',
              metavar = 'DATA_FILE'),
  make_option(c('--model'),
              type='character',
              default = NULL,
              help = 'model to use on sample',
              metavar = 'MODEL'),
  make_option(c('--alpha0'),
              type='numeric',
              default = NULL,
              help = 'learning rate unrelated to rare events',
              metavar = 'ALPHA_0'),
  make_option(c('--alpha1'),
              type='numeric',
              default = NULL,
              help = 'learning rate related to rare events',
              metavar = 'ALPHA_1'),
  make_option(c('--temperature'),
              type='numeric',
              default = NULL,
              help = 'temperature of softmax choice function',
              metavar = 'TEMPERATURE'),
  make_option(c('--reward_space_ub'),
              type='numeric',
              default = NULL,
              help = 'upper boundary of possible rewards',
              metavar = 'REWARD_SPACE'),
  make_option(c('--choice_policy'),
              type='character',
              default = NULL,
              help = 'choice policy of model ("softmax" or "greedy")',
              metavar = 'POLICY'),
  make_option(c('--interdep'),
              type='numeric',
              default = NULL,
              help = 'interdependency parameter (only for model "Pedlr_interdep")',
              metavar = 'POLICY'),
  make_option(c('--init_values'),
              type='character',
              default = NULL,
              help = 'values to initiate model, three inputs for each task version',
              metavar = 'INIT_VALUES'),
  make_option(c('--distance_shrinkage'),
              type='numeric',
              default = NULL,
              help = 'value by which to move the edge distributions towards the middle',
              metavar = 'DISTANCE_SHRINKAGE'),
  make_option(c('--out_file'),
              type='character',
              default = NULL,
              help = 'file to save output table to (.tsv)',
              metavar = 'OUT_FILE'))

# provide options in list to be callable by script
opt_parser = OptionParser(option_list = option_list)
opt = parse_args(opt_parser)

# Convert input list data frame to pass arguments to function
parameters = data.frame(matrix(NA, 1, 6))
colnames(parameters) = c('alpha0',
                         'alpha1',
                         'temperature',
                         'reward_space_ub',
                         'choice_policy',
                         'interdep')
parameters$alpha0 = opt$alpha0
parameters$alpha1 = opt$alpha1
parameters$temperature = opt$temperature
parameters$reward_space_ub = opt$reward_space_ub
parameters$choice_policy = opt$choice_policy
parameters$interdep = opt$interdep

# Convert initializing values to appropriate format
input = unlist(strsplit(opt$init_values, ' '))
init_values = list()
for(task_version in seq(length(input)/3)){
  init_values[[task_version]] = as.numeric(
    input[c(1:3)+(3*(task_version - 1))]
    )  
}

# Call main function
Apply_model(data_file = opt$data_file,
            model = opt$model,
            parameters = parameters,
            init_values = init_values,
            distance_shrinkage = opt$distance_shrinkage,
            out_file = opt$out_file)
