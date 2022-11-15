library(here)
library(data.table)
library(nloptr)
library(zoo)
library(bmsR)
library(beeswarm)

# Needed for function std.error?
library(plotrix)

base_path = here::here()

source(file.path(base_path, 'code', 'utils', 'Load_data.R'))
source(file.path(base_path, 'code', 'utils', 'Apply_exclusion_criteria.R'))
source(file.path(base_path, 'code', 'utils', 'Add_comp.R'))
source(file.path(base_path, 'code', 'utils', 'LL_reg.R'))
source(file.path(base_path, 'code', 'model_fitting', 'Regression_model.R'))
source(file.path(base_path, 'code', 'model_fitting', 'Compute_value.R'))
source(file.path(base_path, 'code', 'model_fitting', 'LRfunction.R'))

# Load participant data
bla = Load_data()
bla = Apply_exclusion_criteria(bla, choice_based_exclusion = TRUE)

# Get correct choices
# Find correct choice
bla$correct = c('left', 'right')[(bla$option_right > bla$option_left)*1+1]
# See if choice was correct
bla$correct_choice = bla$correct == bla$choice
# Get correct choices for forced choices
bla$correct_choice[bla$forced_left == 1] = bla$choice[bla$forced_left == 1] == 'left'
bla$correct_choice[bla$forced_right == 1] = bla$choice[bla$forced_right == 1] == 'right'
# Duplicate chosen bandit
bla$chosen_bandit = bla$option_choice
# Get bandit comparison for each trial
bla = Add_comp(bla)
# Delete 'v' in middle of comparison
bla = bla %>%
  .[, bandit := paste(substr(comp, 1, 1), substr(comp, 3,3), sep = '')]


#source(paste(code_path, '/../../../../R_tools/basic_functions.R', sep = ''))
#source(paste(code_path, '/get_data.R', sep = ''))
#source(paste(code_path, '/LRfunction.R', sep = ''))
#source(paste(code_path, '/comp_value.R', sep = ''))
#source(paste(code_path, '/reg_model.R', sep = ''))

# Get data overview
n_id = length(unique(bla$participant_id))
agegroup = tapply(bla$group, bla$participant_id, function(x) which(unique(x) == c('younger', 'older')))
# Performance = average over correct choices (including forced choices)
perf = tapply(bla$correct_choice, bla$participant_id, mean, na.rm = TRUE)

# # Log likelihood function based on regression
# LL_reg = function(x,
#                   cdf,
#                   model) {
#   
#   # Run regression
#   cres = Regression_model(x = x,
#                    cdf = cdf,
#                    model = model)
#   
#   # Return negative LL
#   return(-logLik(cres[[1]])[1])
# }

# Define fitting parameters
copts = list('algorithm'='NLOPT_GN_DIRECT_L',
             'xtol_rel'=1.0e-5,
             'maxeval'= 100)

# Set number of models
n_models = 4
# 1 parameter models: learning rate
# 3 parameter models: low, high, slope
x0 = list(0.2,
          0.2,
          c(0.2, 0.5, 1),
          c(0.2, 0.5, 1))
lb = list(0.01,
          0.01,
          c(exp(-5), exp(-5), -20),
          c(exp(-5), exp(-5), -20))
ub = list(1,
          1,
          c(1, 1, 20),
          c(1, 1, 20))
#x0 = list(0.2, 0.2, c(0.2, 1), c(0.2, 1))
#lb = list(0.01, 0.01, c(0.01, -3), c(0.01, -3))
#ub = list(1, 1, c(1, 3), c(1, 3))

# Get formulas for regression
glmods = list(
  rw = as.formula('choice ~ V1 + V2'),
  uncertainty = as.formula('choice ~ V1 + V2 + V1u + V2u'),
  surprise = as.formula('choice ~ V1 + V2'),
  uncertainty_surprise = as.formula('choice ~ V1 + V2 + V1u + V2u'))

# Loop over participants
for (id_count in 1:n_id) {
  # Get participant
  id = unique(bla$participant_id)[id_count]
  
  # initialize data arrays on first participant
  if (id_count == 1) {
    ps = array(NA, dim = c(2, n_id, n_models))
    coefs = array(NA, dim = c(8, n_id, n_models))
    AICs = array(NA, dim = c(n_id, n_models))
    LRs = array(NA, dim = c(100, n_id, n_models))
  }
  
  # Send message to user
  message(paste('Starting ID', id, '...'), appendLF = FALSE)
  
  # Get data of participant
  data = bla[participant_id == id]
  
  # Loop over models
  for (model_count in 1:n_models) {
    # Fit model
    cmod = nloptr(x0 = x0[[model_count]],
                  eval_f = LL_reg,
                  lb = lb[[model_count]],
                  ub = ub[[model_count]],
                  opts = copts,
                  glmods = glmods,
                  data = data,
                  model = model_count)
    
    # Run regression with best fitting parameters
    cres = Regression_model(x = cmod$solution,
                     cdf = data,
                     model = model_count)
    # Get regression report
    cglm = cres[[1]]
    cres[[2]]
    # Learning rate on each trial
    lr = cres[[4]][2,] #colSums(cres[[4]], na.rm = TRUE)
    lr = lr[-which(is.na(lr))]
    # PE on each trial
    pe = cres[[3]][2,which(!is.na(cres[[4]][2,]))]
    
    # Enter variables into output array
    # LR
    LRs[, id_count, model_count] = tapply(c(lr, rep(NA, 100)), c(round(abs(pe)), seq(0, 99)), mean, na.rm = TRUE)
    # P values
    ps[, id_count, model_count] = summary(cglm)$coefficients[2:3,4]
    # Betas
    coefs[1:length(coef(cglm)), id_count, model_count] = coef(cglm)
    if (coefs[2, id_count, model_count] < -200) {browser()}
    #plot(LRfunction(cmod$solution[1], cmod$solution[2], -cmod$solution[3]/3, 1:50)[[1]], ylim = c(0, 1))
    
    # Add model prediction
    cres[[2]]$model_p = predict(cglm, type = 'response')
    # Add betas to output
    coefs[6:(5+length(x0[[model_count]])), id_count, model_count] = cmod$solution
    # Add AICs to output
    AICs[id_count, model_count] = AIC(cglm) + 2*length(x0[[model_count]])
  }
  message(paste('done!'), appendLF = FALSE)
  cAICs = round(AICs[id_count,], 1)
  cAICs[2:4] = cAICs[2:4] - cAICs[1]
  message(paste(' || \t AIC1:', sprintf("%+.1f", cAICs[1]), '\t AIC2:', sprintf("%+.1f", cAICs[2]), '\t AIC3:', sprintf("%+.1f", cAICs[3]), '\t AIC4:', sprintf("%+.1f", cAICs[4])), appendLF = TRUE)
}

# Figure 1: beta's of RW model
# younger
# Test if V1 and V2 sig from 0
cmat = coefs[c(2:3, 6),agegroup==1,1]
t.test(cmat[1,])
t.test(cmat[2,])

cmeans = apply(cmat, 1, mean)
ccols = rep(hcl.colors(6, 'Earth', alpha = 0.8)[1], 3)
k = barplot(cmeans, beside = TRUE, col = ccols, border = NA, ylim = c(-1, 1))
abline(h = 0)
imat = matrix(c(k), dim(cmat)[1], dim(cmat)[2], byrow = FALSE)
beeswarm(cmat~imat, at = c(k), add = TRUE, pch = 21, col = '#555555BB', bg = ccols, cex = 0.5, lwd = 0.3, corral = 'wrap', method = 'hex')



# older
# Test if V1 and V2 sig from 0
cmat = coefs[c(2:3, 6),agegroup==2,1]
t.test(cmat[1,])
t.test(cmat[2,])

cmeans = apply(cmat, 1, mean)
ccols = rep(hcl.colors(6, 'Earth', alpha = 0.8)[1], 3)
k = barplot(cmeans, beside = TRUE, col = ccols, border = NA, ylim = c(-1, 1))
abline(h = 0)
imat = matrix(c(k), dim(cmat)[1], dim(cmat)[2], byrow = FALSE)
beeswarm(cmat~imat, at = c(k), add = TRUE, pch = 21, col = '#555555DD', bg = ccols, cex = 0.5, lwd = 0.3, corral = 'wrap', method = 'hex')

# correlation analyses across age groups
# V1, V2, alpha from rw model
cmat = coefs[c(2:3, 6),,1]
apply(cmat, 1, mean)
# V1 and performance (average of correct choices, including forced_choices)
cor.test(perf, cmat[1,])
# V2 and performance
cor.test(perf, cmat[2,])
# alpha (?) and performance
cor.test(perf, cmat[3,])

# test model frequencies between age groups (models = rw, uncertainty, surprise)
tab1 = tabulate(apply(AICs[agegroup == 1,1:3], 1, function(x) which.min(x)))
tab2 = tabulate(apply(AICs[agegroup == 2,1:3], 1, function(x) which.min(x)))
chisq.test(cbind(tab1, tab2))


res = bmsR::VB_bms(-AICs, 1000)
res1 = bmsR::VB_bms(-AICs[agegroup == 1,1:3], 1000)
res2 = bmsR::VB_bms(-AICs[agegroup == 2,1:3], 1000)

x1 = rnorm(1000, mean=60, sd=10)
x2 = rnorm(1000, mean=0, sd=10)
x3 = rnorm(1000, mean=30, sd=10)

h1 = hist(x1, breaks = seq(-36, 101, by = 5), plot = FALSE)
h2 = hist(x2, breaks = seq(-36, 101, by = 5), plot = FALSE)
h3 = hist(x3, breaks = seq(-36, 101, by = 5), plot = FALSE)

barplot(h1$counts,
        border = NA,
        col = hcl.colors(3, 'Dark3', alpha = 0.6)[1],
        bty = 'n')
barplot(h2$counts,
        border = NA,
        col = hcl.colors(3, 'Dark3', alpha = 0.6)[2],
        bty = 'n',
        add = TRUE)
barplot(h3$counts,
        border = NA,
        col = hcl.colors(3, 'Dark3', alpha = 0.6)[3],
        bty = 'n',
        add = TRUE)

opar = par(lwd = 2)
k = barplot(cbind(tab1, tab2),
            space = c(rep(0.1, 3),1, rep(0.1, 2)),
            beside = TRUE,
            col = hcl.colors(4, 'Earth')[1:3],
            border = c(c('white', 1, NA), c('white', 'white', 1)),
            ylim = c(0, 25),
            cex.axis = 1.2,
            cex.lab = 1.2,
            names.arg = c('Younger\n Adults', 'Older\n Adults'))

k = barplot(cbind(res1$pxp, res2$pxp),
            space = c(rep(0.1, 3), 1, rep(0.1, 2)),
            beside = TRUE,
            col = hcl.colors(4, 'Earth')[1:3],
            border = c(c('white', 1, NA), c('white', 'white', 1)),
            ylim = c(0, 1),
            cex.axis = 1.2,
            cex.lab = 1.2,
            names.arg = c('Younger\n Adults', 'Older\n Adults'),
            yaxt = 'n')
axis(2,
     at = c(0, 0.5, 1),
     cex.axis = 1.2)


# panel 2: AICs
cAICs = AICs - AICs[,rep(1, 4)]
cmeans = cbind(apply(cAICs[agegroup == 1,2:4], 2, mean, na.rm = TRUE),
               apply(cAICs[agegroup == 2,2:4], 2, mean, na.rm = TRUE))
csds = cbind(apply(cAICs[agegroup == 1,2:4], 2, std.error),
             apply(cAICs[agegroup == 2,2:4], 2, std.error))
cmat1 = t(cAICs[agegroup == 1,2:4])
cmat2 = t(cAICs[agegroup == 2,2:4])



ccols = hcl.colors(4, 'Earth')[2:4]

k = barplot(cmeans,
            beside = TRUE,
            col = ccols, border = 'white',
            ylim = c(-25, 10),
            cex.axis = 1.2,
            cex.lab = 1.2,
            names.arg = c('Younger\n Adults', 'Older\n Adults'))
abline(h = 0)
imat1 = matrix(c(k[,1]), dim(cmat1)[1], dim(cmat1)[2], byrow = FALSE)
imat2 = matrix(c(k[,2]), dim(cmat2)[1], dim(cmat2)[2], byrow = FALSE)
beeswarm(cmat1~imat1, 
         at = c(k[,1]),
         add = TRUE,
         pch = 21,
         col = '#888888',
         bg = hcl.colors(4, 'Earth', alpha = 0.8)[2:4],
         cex = 0.5,
         corral = 'wrap',
         method = 'hex')
beeswarm(cmat2~imat2,
         at = c(k[,2]),
         add = TRUE,
         pch = 21,
         col = '#888888',
         bg = hcl.colors(4, 'Earth', alpha = 0.8)[2:4],
         cex = 0.5,
         corral = 'wrap',
         method = 'hex')

#se_bars(k, cmeans, csds)


# AIC scores for both age groups
X2 = AICs[agegroup == 2,]
X1 = AICs[agegroup == 1,]

# Younger, RW vs. others
t.test(X1[,1], X1[,2], paired = TRUE)
t.test(X1[,1], X1[,3], paired = TRUE)
t.test(X1[,1], X1[,4], paired = TRUE)

# Older, RW vs. others
t.test(X2[,1], X2[,2], paired = TRUE)
t.test(X2[,1], X2[,3], paired = TRUE)
t.test(X2[,1], X2[,4], paired = TRUE)

# Younger, uncertainty (?) vs. rest
t.test(X1[,2], X1[,4], paired = TRUE)
t.test(X1[,2], X1[,3], paired = TRUE)

# Older, uncertainty (?) vs. rest
t.test(X2[,2], X2[,3], paired = TRUE)
t.test(X2[,2], X2[,4], paired = TRUE)

# AIC ratio uncertainty/surprise; younger vs older
t.test(X1[,2]/X1[,3], X2[,2]/X2[,3])
# AIC difference uncertainty (?) - surprise; younger vs. older
t.test(X1[,2]-X1[,4], X2[,2]-X2[,4])
t.test(X1[,2]-X1[,4], X2[,2]-X2[,4])

# coef comparison
sapply(c(1:8),
       function(x) t.test(coefs[x,agegroup == 1,4],
                          coefs[x,agegroup == 2,4])$p.value
       )
t.test(coefs[7,agegroup == 1,4],
       coefs[7,agegroup == 2,4])
apply(AICs, 2, sum)

# coefs of uncertanty in model 4
cmat = coefs[c(4:5),agegroup==1,4]
t.test(cmat[1,])
t.test(cmat[2,])

cmat = coefs[c(4:5),agegroup==2,4]
t.test(cmat[1,])
t.test(cmat[2,])

t.test(colMeans(coefs[c(4:5),agegroup==2,4]), colMeans(coefs[c(4:5),agegroup==1,4]))

# coefs of learning rate function in model 4
cmat1 = coefs[c(6:8),agegroup==1,4]
t.test(cmat1[1,]-cmat1[2,])
t.test(cmat1[3,]-1)

cmat2 = coefs[c(6:8),agegroup==2,4]
t.test(cmat2[1,]-cmat2[2,])
t.test(cmat2[3,]-1)

t.test(cmat1[1,], cmat2[1,])
t.test(cmat1[1,] - cmat1[2,], cmat2[1,] - cmat2[2,])
t.test(exp(cmat1[3,]), exp(cmat2[3,]))


cmat = coefs[c(4:5),agegroup==2,4]
t.test(cmat[1,])
t.test(cmat[2,])

t.test(colMeans(coefs[c(4:5),agegroup==2,4]), colMeans(coefs[c(4:5),agegroup==1,4]))

#### not shown: illustrations of learning rate

LRfunmat3 = sapply(1:nid, function(x) LRfunction(coefs[6,x,3], coefs[7,x,3], coefs[8,x,3], 1:60)[[1]])
LRfunmat4 = sapply(1:nid, function(x) LRfunction(coefs[6,x,4], coefs[7,x,4], coefs[8,x,4], 1:60)[[1]])

LRfunners3 = apply(AICs, 1, function(x) which.min(x))==3
LRfunners4 = apply(AICs, 1, function(x) which.min(x))==4
LRfunners = (LRfunners3 | LRfunners4)

LRsign3 = (coefs[7,,3] - coefs[6,,3])
LRsign4 = (coefs[7,,4] - coefs[6,,4])


LRfunmat = LRfunmat3
LRfunmat[,LRfunners4] = LRfunmat4[,LRfunners4]

LRsign = LRsign3
LRsign[LRfunners4] = LRsign4[LRfunners4]

cidx1 = which(LRfunners & agegroup == 1 & LRsign < 0)
cidx2 = which(LRfunners & agegroup == 1 & LRsign > 0)
matplot(LRfunmat[1:60,cidx1], lty = 1, type = 'l', lwd = 1, col = '#FF99FF', bty = 'n', cex.lab = 1.2, cex.axis = 1.2, ylab = '', xlab = '', ylim = c(0, 1), yaxt = 'n')
matlines(LRfunmat[1:60,cidx2], lty = 1, type = 'l', lwd = 1, col = 'darkorange')
axis(2, at = c(0, 1), cex.axis = 1.2)

cidx1 = which(LRfunners & agegroup == 2 & LRsign < 0)
cidx2 = which(LRfunners & agegroup == 2 & LRsign > 0)
matplot(LRfunmat[1:60,cidx1], lty = 1, type = 'l', lwd = 1, col = '#FF99FF', bty = 'n', cex.lab = 1.2, cex.axis = 1.2, ylab = '', xlab = '', ylim = c(0, 1), yaxt = 'n')
matlines(LRfunmat[1:60,cidx2], lty = 1, type = 'l', lwd = 1, col = 'darkorange')
axis(2, at = c(0, 1), cex.axis = 1.2)
