
require(jsonlite)
require(stringr)
require(data.table)
require(magrittr)
require(ggplot2)

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

# Get area of distributions
sd = (100 * 1/6) / 3
lowest_gauss_mean = 1/6 * 100
highest_gauss_mean = 5/6 * 100
distance = 1/6 * 100
bimodal_mode_distance = -35
bimodal_rel_proportion = 0.2

# Save all designs in one data frame
# all_designs = data.table()

# Twenty different designs
for(i in seq(20)){
  
  # Set seed for each iteration of design creation
  set.seed(i * 35)
  
  # Create distributions
  # run 1
  mid_mean_1 = round(runif(n = 1,
                           min = 42, # 42 ensures that the 2nd mode of the bimodal distribution does not touch the edges of the reward space 1-100
                           max = mean(c(42, highest_gauss_mean - distance)) - distance/2))
  low_mean_1 = mid_mean_1 - distance
  high_mean_1 = mid_mean_1 + distance
  # run 2
  mid_mean_2 = round(runif(n = 1,
                           min = mean(c(42, highest_gauss_mean - distance)) + distance/2,
                           max = floor(highest_gauss_mean - distance)))
  low_mean_2 = mid_mean_2 - distance
  high_mean_2 = mid_mean_2 + distance
  # Pass distribution parameters to sampling
  dist_list = list(c('gaussian', low_mean_1, sd),
                   c('bimodal', mid_mean_1, bimodal_rel_proportion, bimodal_mode_distance, sd, sd),
                   c('gaussian', high_mean_1, sd),
                   c('gaussian', low_mean_2, sd),
                   c('bimodal', mid_mean_2, bimodal_rel_proportion, bimodal_mode_distance, sd, sd),
                   c('gaussian', high_mean_2, sd))
  
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
  
  # Save all designs in one data frame
  # design$n_design = i
  # all_designs = rbind(all_designs, design)
  
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

# # Get max and min points of each design
# all_designs = all_designs %>%
#   .[, trial:=seq(.N),
#     by = n_design] %>%
#   .[trial_type == 'choice',
#     ':='(max_choice = max(c(reward_stim_1, reward_stim_2)),
#          min_choice = min(c(reward_stim_1, reward_stim_2))),
#     by = c('trial', 'n_design')] %>%
#   .[, ':='(max_points = sum(max_choice, na.rm = TRUE),
#            min_points = sum(min_choice, na.rm = TRUE)),
#     by = n_design]
# 
# data_plot = all_designs[trial == 1] %>%
#   data.table::melt(., id.vars = 'n_design', measure.vars = c('max_points', 'min_points')) %>%
#   .[]
# ggplot(data = data_plot, aes(x = variable, y = value)) +
#   geom_point()

# # Plot distributions of each design
# data_plot = all_designs %>%
#   .[, .(option = c(option_left, option_right),
#         reward = c(reward_stim_1, reward_stim_2)),
#     by = c('n_design', 'task_version')] %>%
#   .[, option := as.factor(option)]
# 
# ggplot(data_plot,
#        aes(x = reward,
#            fill = option,
#            color = option)) +
#   geom_bar() +
#   scale_x_binned(limits = c(1,100), n.breaks = 100) +
#   facet_grid(n_design ~ task_version)
  

