library(data.table)
library(here)
library(magrittr)
library(nloptr)
library(optparse)

# participant_id = '09RI1ZH'
# out_file = '/Users/koch/Docs/pedlr/derivatives/model_fitting/nico/test.tsv'
# random_x0 = TRUE

fit_nico = function(participant_id,
                    out_file,
                    random_x0){
  
  base_path = here::here()
  data_dir = file.path(base_path, 'data', fsep = .Platform$file.sep)
  # List of all data points
  data_list = Sys.glob(file.path(data_dir, '*.tsv', fsep = .Platform$file.sep))
  ids = unname(sapply(data_list, function(x) unlist(strsplit(basename(x), split = '_'))[1]))
  ids = ids[ids == participant_id]
  nid = length(ids)
  # Load pre-written functions
  source_path = file.path(base_path, 'code', 'utils',
                          fsep = .Platform$file.sep)
  source_files = list.files(source_path, pattern = "[.][rR]$",
                            full.names = TRUE, recursive = TRUE)
  invisible(lapply(source_files, function(x) source(x)))
  
  source_path = file.path(base_path, 'code', 'RW_nico.R', fsep = .Platform$file.sep)
  source(source_path)
  source_path = file.path(base_path, 'code', 'PEDLR_simple_nico.R', fsep = .Platform$file.sep)
  source(source_path)
  source_path = file.path(base_path, 'code', 'PEDLR_nico.R', fsep = .Platform$file.sep)
  source(source_path)
  source_path = file.path(base_path, 'code', 'PEDLR_fixdep_nico.R', fsep = .Platform$file.sep)
  source(source_path)
  source_path = file.path(base_path, 'code', 'LL_nico.R', fsep = .Platform$file.sep)
  source(source_path)
  
  #opts = list('algorithm'='NLOPT_GN_CRS2_LM', 'xtol_rel'=1.0e-4, 'maxeval'= 5000)
  opts = list('algorithm'='NLOPT_GN_DIRECT_L', 'xtol_rel'=1.0e-4, 'maxeval'= 5000)
  #opts = list('algorithm'='NLOPT_LN_BOBYQA', 'xtol_rel'=1.0e-8, 'maxeval'= 5000)
  
  # Set lower and upper boundaries
  ub_rw = c(1, 10)
  lb_rw = c(0, 1)
  ub_pedlr_simple =  c(1, 10)
  lb_pedlr_simple = c(0, 1)
  ub_pedlr =  c(1, 1, 10)
  lb_pedlr = c(0, 0, 1)
  ub_pedlr_fixdep =  c(1, 1, 10)
  lb_pedlr_fixdep = c(0, 0, 1)
  
  
  # Load data of participants
  data_all = Load_data() %>%
    Add_comp(.) %>%
    Prepare_data_for_fit(.)
  
  output = data.table()
  
  # For each run
  for(ctv in seq(2)){
    modeldf = data.frame(matrix(NA, nid, 44))
    colnames(modeldf) = c('RW_LL', 'RW_alpha', 'RW_temp', 'RW_init_values', 'RW_value1', 'RW_value2', 'RW_value3',
                          'PEDLRSIMP_LL', 'PEDLRSIMP_alpha1', 'PEDLRSIMP_temp', 'PEDLRSIMP_init_values',  'PEDLRSIMP_value1', 'PEDLRSIMP_value2', 'PEDLRSIMP_value3',
                          'PEDLR_LL', 'PEDLR_alpha0', 'PEDLR_alpha1', 'PEDLR_temp', 'PEDLR_init_values',  'PEDLR_value1', 'PEDLR_value2', 'PEDLR_value3',
                          'PEDLRFIX_LL', 'PEDLRFIX_alpha0', 'PEDLRFIX_alpha1', 'PEDLRFIX_temp', 'PEDLRFIX_init_values',  'PEDLRFIX_value1', 'PEDLRFIX_value2', 'PEDLRFIX_value3',
                          'Choice1v2', 'Choice2v3',
                          'Rating2', 'RatingSD2',
                          'RW_x0alpha', 'RW_x0temp',
                          'PEDLRSIMP_x0alpha1', 'PEDLRSIMP_x0temp',
                          'PEDLR_x0alpha0', 'PEDLR_x0alpha1', 'PEDLR_x0temp',
                          'PEDLRFIX_x0alpha0', 'PEDLRFIX_x0alpha1', 'PEDLRFIX_x0temp')
    
    # For each participant
    for (cid in 1:nid) {
      
      # Set starting values for optimizer (either random or fixed)
      if(random_x0){
        x0_rw = round(c(runif(1,0,1), runif(1, 1, 10)), 2)
        x0_pedlr_simple = round(c(runif(1,0,1), runif(1, 1, 10)), 2)
        x0_pedlr = round(c(runif(1,0,1), runif(1,0,1), runif(1, 1, 10)), 2)
        x0_pedlr_fixdep = round(c(runif(1,0,1), runif(1,0,1), runif(1, 1, 10)), 2)
        
      } else{
        x0_rw = c(0.2, 1)
        x0_pedlr_simple = c(0.2, 1)
        x0_pedlr = c(0.2, 0.2, 1)
        x0_pedlr_fixdep = c(0.2, 0.2, 1)
        
      }
      
      message(paste('\n',
                    'x0_rw: ', paste(x0_rw, collapse = ' ', sep = ''),
                    '\n',
                    'x0_pedlr_simple: ', paste(x0_pedlr_simple, collapse = ' ', sep = ''),
                    '\n',
                    'x0_pedlr: ', paste(x0_pedlr, collapse = ' ', sep = ''),
                    '\n',
                    'x0_pedlr_fixdep: ', paste(x0_pedlr_fixdep, collapse = ' ', sep = ''),
                    sep = ''))
      
      # Message to user
      cat(paste('Fitting ', participant_id, sep = ''))
      data = data_all %>%
        .[participant_id == ids[cid] & run == ctv, ]
      
      # RW
      message('\n   Fitting RW...')
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
      
      # Pedlr_simple
      message('   Fitting PEDLRSIMP...')
      PEDLRSIMPfit = nloptr(x0=x0_rw,
                     eval_f=LL_nico,
                     lb=lb_pedlr_simple,
                     ub=ub_pedlr_simple,
                     opts=opts,
                     data=data,
                     model = 'PEDLRSIMP')
      cparams = NULL
      modeldf$PEDLRSIMP_LL[cid] = PEDLRSIMPfit$objective
      cparams$alpha1 = modeldf$PEDLRSIMP_alpha1[cid] = PEDLRSIMPfit$solution[1]
      cparams$temp = modeldf$PEDLRSIMP_temp[cid] = PEDLRSIMPfit$solution[2]
      cparams$init_values = modeldf$PEDLRSIMP_init_values[cid] = 50
      cdf = PEDLR_simple_nico(data, cparams)
      modeldf$PEDLRSIMP_value1[cid] = mean(cdf$value1, na.rm = TRUE)
      modeldf$PEDLRSIMP_value2[cid] = mean(cdf$value2, na.rm = TRUE)
      modeldf$PEDLRSIMP_value3[cid] = mean(cdf$value3, na.rm = TRUE)
      
      # Pedlr
      message('   Fitting PEDLR...')
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
      
      # Pedlr_fixdep
      message('   Fitting PEDLRFIX...')
      PEDLRFIXfit = nloptr(x0=x0_pedlr,
                        eval_f=LL_nico,
                        lb=lb_pedlr_fixdep,
                        ub=ub_pedlr_fixdep,
                        opts=opts,
                        data=data,
                        model = 'PEDLRFIX')
      cparams = NULL
      modeldf$PEDLRFIX_LL[cid] = PEDLRFIXfit$objective
      cparams$alpha0 = modeldf$PEDLRFIX_alpha0[cid] = PEDLRFIXfit$solution[1]
      cparams$alpha1 = modeldf$PEDLRFIX_alpha1[cid] = PEDLRFIXfit$solution[2]
      cparams$temp = modeldf$PEDLRFIX_temp[cid] = PEDLRFIXfit$solution[3]
      cparams$init_values = modeldf$PEDLRFIX_init_values[cid] = 50
      cdf = PEDLR_fixdep_nico(data, cparams)
      modeldf$PEDLRFIX_value1[cid] = mean(cdf$value1, na.rm = TRUE)
      modeldf$PEDLRFIX_value2[cid] = mean(cdf$value2, na.rm = TRUE)
      modeldf$PEDLRFIX_value3[cid] = mean(cdf$value3, na.rm = TRUE)
      
      # General, model-free information
      X = tapply(data$choice == 2, list(data$comp, data$free_choice), mean, na.rm = TRUE)[c('1v2', '2v3'),'1']
      modeldf$Choice1v2[cid]  = 1 - X[1]
      modeldf$Choice2v3[cid]  = X[2]
      modeldf$Rating2[cid] = mean(data$est_2_reward, na.rm = TRUE)
      modeldf$RatingSD2[cid] = mean(data$est_2_range, na.rm = TRUE)
      
      # Add x0 (minimization starting values)
      modeldf$RW_x0alpha[cid] = x0_rw[1]
      modeldf$RW_x0temp[cid] = x0_rw[2]
      modeldf$PEDLRSIMP_x0alpha1[cid] = x0_pedlr_simple[1]
      modeldf$PEDLRSIMP_x0temp[cid] = x0_pedlr_simple[2]
      modeldf$PEDLR_x0alpha0[cid] = x0_pedlr[1]
      modeldf$PEDLR_x0alpha1[cid] = x0_pedlr[2]
      modeldf$PEDLR_x0temp[cid] = x0_pedlr[3]
      modeldf$PEDLRFIX_x0alpha0[cid] = x0_pedlr_fixdep[1]
      modeldf$PEDLRFIX_x0alpha1[cid] = x0_pedlr_fixdep[2]
      modeldf$PEDLRFIX_x0temp[cid] = x0_pedlr_fixdep[3]
      
      # Give message to user
      cat(paste(' | RW ', sprintf("%01.2f", round(modeldf$RW_LL[cid], 3)),
                ' - PEDLRSIMP ', sprintf("%01.2f", round(modeldf$PEDLRSIMP_LL[cid], 3)),
                ' - PEDLR ', sprintf("%01.2f", round(modeldf$PEDLR_LL[cid], 3)),
                ' - PEDLRFIX ', sprintf("%01.2f", round(modeldf$PEDLRFIX_LL[cid], 3)),
                sep = ''))
      cat(paste(' | LR RW ', sprintf("%01.2f", round(modeldf$RW_alpha[cid], 3)),
                ' - LR1 PEDLRSIMP ', sprintf("%01.2f", round(modeldf$PEDLRSIMP_alpha1[cid], 3)),
                ' - LR0 PEDLR ', sprintf("%01.2f", round(modeldf$PEDLR_alpha0[cid], 3)),
                ' - LR1 PEDLR ', sprintf("%01.2f", round(modeldf$PEDLR_alpha1[cid], 3)),
                ' - LR0 PEDLRFIX ', sprintf("%01.2f", round(modeldf$PEDLRFIX_alpha0[cid], 3)),
                ' - LR1 PEDLRFIX ', sprintf("%01.2f", round(modeldf$PEDLRFIX_alpha1[cid], 3)),
                sep = ''))
      cat(paste(' | Done\n', sep = '.'))
      
      # If all participants are fitted, append output across runs
      if(cid == nid){
        modeldf$participant_id = ids
        modeldf$run = ctv
        modeldf$random_x0 = random_x0
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
  make_option(c('-p', '--participant_id'),
              type='character',
              default = NULL,
              help = 'ID of participant to process',
              metavar = 'PARTICIPANT_ID'),
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
fit_nico(participant_id = opt$participant_id,
         out_file = opt$out_file,
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
