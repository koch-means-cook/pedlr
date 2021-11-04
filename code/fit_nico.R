library(data.table)
library(here)
library(magrittr)
library(nloptr)
library(optparse)

# out_file = '/Users/koch/Docs/pedlr/derivatives/model_fitting/nico/test.tsv'
# random_x0 = TRUE

fit_nico = function(out_file,
                    random_x0){
  
  base_path = here::here()
  data_dir = file.path(base_path, 'data', fsep = .Platform$file.sep)
  # List of all data points
  data_list = Sys.glob(file.path(data_dir, '*.tsv', fsep = .Platform$file.sep))
  ids = unname(sapply(data_list, function(x) unlist(strsplit(basename(x), split = '_'))[1]))[1:2]
  nid = length(ids)
  # Load pre-written functions
  source_path = file.path(base_path, 'code', 'utils',
                          fsep = .Platform$file.sep)
  source_files = list.files(source_path, pattern = "[.][rR]$",
                            full.names = TRUE, recursive = TRUE)
  invisible(lapply(source_files, function(x) source(x)))
  
  source_path = file.path(base_path, 'code', 'RW_nico.R', fsep = .Platform$file.sep)
  source(source_path)
  source_path = file.path(base_path, 'code', 'PEDLR_nico.R', fsep = .Platform$file.sep)
  source(source_path)
  source_path = file.path(base_path, 'code', 'LL_nico.R', fsep = .Platform$file.sep)
  source(source_path)
  
  #opts = list('algorithm'='NLOPT_GN_CRS2_LM', 'xtol_rel'=1.0e-4, 'maxeval'= 5000)
  opts = list('algorithm'='NLOPT_GN_DIRECT_L', 'xtol_rel'=1.0e-4, 'maxeval'= 5000)
  #opts = list('algorithm'='NLOPT_LN_BOBYQA', 'xtol_rel'=1.0e-8, 'maxeval'= 5000)
  
  # Set lower and upper boundaries
  ub_pedlr =  c(1, 1, 10)
  lb_pedlr = c(0, 0, 1)
  ub_rw = c(1, 10)
  lb_rw = c(0, 1)
  
  # Load data of participants
  data_all = Load_data() %>%
    Add_comp(.) %>%
    Prepare_data_for_fit(.)
  
  output = data.table()
  
  # For each run
  for(ctv in seq(2)){
    modeldf = data.frame(matrix(NA, nid, 2+7+2+2+6+5))
    colnames(modeldf) = c('RW_LL', 'RW_alpha', 'RW_temp', 'RW_init_values',
                          'RW_value1', 'RW_value2', 'RW_value3', 'PEDLR_LL',
                          'PEDLR_alpha0', 'PEDLR_alpha1', 'PEDLR_temp',
                          'PEDLR_init_values',  'PEDLR_value1', 'PEDLR_value2',
                          'PEDLR_value3', 'Choice1v2', 'Choice2v3', 'Rating2',
                          'RatingSD2', 'RW_x0alpha', 'RW_x0temp',
                          'PEDLR_x0alpha0', 'PEDLR_x0alpha1', 'PEDLR_x0temp')
    
    # For each participant
    for (cid in 1:nid) {
      
      # Set starting values for optimizer (either random or fixed)
      if(random_x0){
        x0_pedlr = round(c(runif(1,0,1), runif(1,0,1), runif(1, 1, 10)), 2)
        x0_rw = round(c(runif(1,0,1), runif(1, 1, 10)), 2)
      } else{
        x0_pedlr = c(0.2, 0.2, 1)
        x0_rw = c(0.2, 1)
      }
      
      message(paste('\n', 'x0_rw: ', paste(x0_rw, collapse = ' ', sep = ''),
                    ' | x0_pedlr: ', paste(x0_pedlr, collapse = ' ', sep = ''),
                    sep = ''))
      
      # Message to user
      cat(paste('Fitting Sub ', sprintf("%02d", cid), ' ', sep = ''))
      data = data_all %>%
        .[participant_id == ids[cid] & task_version == ctv, ]
      
      RWfit = nloptr(x0=x0_rw,
                     eval_f=LL_nico,
                     lb=lb_rw,
                     ub=ub_rw,
                     opts=opts,
                     data=data,
                     model = 'RW')
      cparams = NULL
      modeldf$RW_LL[cid] = RWfit$objective
      cparams$alpha = modeldf$RW_alpha[cid] = RWfit$solution[1]
      cparams$temp = modeldf$RW_temp[cid] = RWfit$solution[2]
      cparams$init_values = modeldf$RW_init_values[cid] = 50
      cdf = RW_nico(data, cparams)
      modeldf$RW_value1[cid] = mean(cdf$value1, na.rm = TRUE)
      modeldf$RW_value2[cid] = mean(cdf$value2, na.rm = TRUE)
      modeldf$RW_value3[cid] = mean(cdf$value3, na.rm = TRUE)
      
      PEDLRfit = nloptr(x0=x0_pedlr,
                        eval_f=LL_nico,
                        lb=lb_pedlr,
                        ub=ub_pedlr,
                        opts=opts,
                        data=data,
                        model = 'PEDLR')
      
      #LL_nico(data = data, x = x0_pedlr, model = 'PEDLR')
      
      cparams = NULL
      modeldf$PEDLR_LL[cid] = PEDLRfit$objective
      cparams$alpha0 = modeldf$PEDLR_alpha0[cid] = PEDLRfit$solution[1]
      cparams$alpha1 = modeldf$PEDLR_alpha1[cid] = PEDLRfit$solution[2]
      cparams$temp = modeldf$PEDLR_temp[cid] = PEDLRfit$solution[3]
      cparams$init_values = modeldf$PEDLR_init_values[cid] = 50
      cdf = PEDLR_nico(data, cparams)
      modeldf$PEDLR_value1[cid] = mean(cdf$value1, na.rm = TRUE)
      modeldf$PEDLR_value2[cid] = mean(cdf$value2, na.rm = TRUE)
      modeldf$PEDLR_value3[cid] = mean(cdf$value3, na.rm = TRUE)
      
      X = tapply(data$choice == 2, list(data$comp, data$free_choice), mean, na.rm = TRUE)[c('1v2', '2v3'),'1']
      modeldf$Choice1v2[cid]  = 1 - X[1]
      modeldf$Choice2v3[cid]  = X[2]
      modeldf$Rating2[cid] = mean(data$est_2_reward, na.rm = TRUE)
      modeldf$RatingSD2[cid] = mean(data$est_2_range, na.rm = TRUE)
      cat(paste(' | RW ', sprintf("%01.2f", round(modeldf$RW_LL[cid], 3)), ' - PEDLR ', sprintf("%01.2f", round(modeldf$PEDLR_LL[cid], 3)), sep = ''))
      cat(paste(' | LR RW ', sprintf("%01.2f", round(modeldf$RW_alpha[cid], 3)),  ' - LR0 PEDLR ', sprintf("%01.2f", round(modeldf$PEDLR_alpha0[cid], 3)), ' - LR1 PEDLR ', sprintf("%01.2f", round(modeldf$PEDLR_alpha1[cid], 3)), sep = ''))
      cat(paste(' | Done\n', sep = '.'))
      
      # If all participants are fitted, append output across runs
      if(cid == nid){
        modeldf$participant_id = ids
        modeldf$task_version = ctv
        modeldf$random_x0 = random_x0
        modeldf$RW_x0alpha = x0_rw[1]
        modeldf$RW_x0temp = x0_rw[2]
        modeldf$PEDLR_x0alpha0 = x0_pedlr[1]
        modeldf$PEDLR_x0alpha1 = x0_pedlr[2]
        modeldf$PEDLR_x0temp = x0_pedlr[3]
        output = rbind(output, modeldf)
      }
    }
  }
  
  
  # Write Fit output
  data.table::fwrite(output,
                     file = out_file,
                     sep = '\t',
                     na = 'n/a',
                     col.names = TRUE,
                     row.names = FALSE)
}

# Make function command line callable with options
option_list = list(
  make_option(c('-o', '--out_file'),
              type='character',
              default = NULL,
              help = 'Path to data file fitting results should be saved to (includes name of target file)',
              metavar = 'OUTPUT_PATH'),
  make_option(c('-r', '--random_x0'),
              type='character',
              default = NULL,
              help = 'TRUE for random starting values for optimizer',
              metavar = 'RANDOM_X0'))

# provide options in list to be callable by script
opt_parser = OptionParser(option_list = option_list)
opt = parse_args(opt_parser)

# Call function with options
fit_nico(out_file = opt$out_file,
         random_x0 = opt$random_x0)

# # does a model with higher alpha 1 have lower value estimates for bandit 2?
# cor.test(modeldf$PEDLR_value2, modeldf$PEDLR_alpha1)
# # do subjs with more evidence for the PEDLR model make more mistakes in bandit 2?
# cor.test(modeldf$RW_LL - modeldf$PEDLR_LL, modeldf$Choice1v2)
# cor.test(modeldf$RW_LL - modeldf$PEDLR_LL, modeldf$Choice1v2 - modeldf$Choice2v3)
# 
# # do subjs with more evidence for the PEDLR model have higher alpha1 learnig rates
# cor.test(modeldf$RW_LL - modeldf$PEDLR_LL, modeldf$PEDLR_alpha1)
# # are differenes in estimates values for bandit 2 related to ratings ?
# cor.test(modeldf$RW_value2 - modeldf$PEDLR_value2, modeldf$Rating2)
# cor.test(modeldf$PEDLR_value2, modeldf$Rating2)
# cor.test(modeldf$RW_value2, modeldf$Rating2)
