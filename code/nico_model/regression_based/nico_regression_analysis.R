packages = c('data.table', 'nloptr', 'zoo', 'bmsR', 'beeswarm')
pksload = unlist(lapply(packages, require, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))

#data_path = '/Users/schuck/data/pedlr-main-data'
#code_path = '/Users/schuck/code/pedlr/code/nico_model/regression_based'
data_path = file.path(here::here(), 'data',
                      fsep = .Platform$file.sep)
code_path = file.path(here::here(), 'code', 'nico_model', 'regression_based',
                      fsep = .Platform$file.sep)

#source(paste(code_path, '/../../../../R_tools/basic_functions.R', sep = ''))
source(paste(code_path, '/get_data.R', sep = ''))
source(paste(code_path, '/LRfunction.R', sep = ''))
source(paste(code_path, '/comp_value.R', sep = ''))
source(paste(code_path, '/reg_model.R', sep = ''))

DATA = get_data(data_path)

ids = unique(DATA$participant_id)
nid = length(ids)
agegroup = tapply(DATA$group, DATA$participant_id, function(x) which(unique(x) == c('younger', 'older')))
perf = tapply(DATA$correct_choice, DATA$participant_id, mean, na.rm = TRUE)

LL_reg = function(x, cdf, model) {
  cres = reg_model(x, cdf, model)
  return(-logLik(cres[[1]])[1])
}

copts = list('algorithm'='NLOPT_GN_DIRECT_L',
             'xtol_rel'=1.0e-5,
             'maxeval'= 100)


nmodels = 4
# 1 parameter models: learning rate
# 3 parameter models: low, high, slope
x0 = list(0.2, 0.2, c(0.2, 0.5, 1), c(0.2, 0.5, 1))
lb = list(0.01, 0.01, c(exp(-5), exp(-5), -20), c(exp(-5), exp(-5), -20))
ub = list(1, 1, c(1, 1, 20), c(1, 1, 20))


#x0 = list(0.2, 0.2, c(0.2, 1), c(0.2, 1))
#lb = list(0.01, 0.01, c(0.01, -3), c(0.01, -3))
#ub = list(1, 1, c(1, 3), c(1, 3))


glmods = list(
  as.formula('choice ~ V1 + V2'),
  as.formula('choice ~ V1 + V2 + V1u + V2u'),
  as.formula('choice ~ V1 + V2'),
  as.formula('choice ~ V1 + V2 + V1u + V2u'))

cid = 1
for (cid in 1:nid) {
  if (cid == 1) {
    # initialize data arrays
    ps = array(NA, dim = c(2, nid, nmodels))
    coefs = array(NA, dim = c(8, nid, nmodels))
    AICs = array(NA, dim = c(nid, nmodels))
    LRs = array(NA, dim = c(100, nid, nmodels))
  }
  message(paste('Starting ID', cid, '...'), appendLF = FALSE)
  cDATA = subset(DATA, DATA$participant_id == ids[cid])
  for (cmodel in 1:nmodels) {
    cmod = nloptr(x0=x0[[cmodel]],eval_f=LL_reg, lb=lb[[cmodel]], ub=ub[[cmodel]], opts=copts, cdf=cDATA, model = cmodel)
    cres = reg_model(cmod$solution, cDATA, model = cmodel)
    cglm = cres[[1]]
    cres[[2]]
    lr = cres[[4]][2,] #colSums(cres[[4]], na.rm = TRUE)
    lr = lr[-which(is.na(lr))]
    pe = cres[[3]][2,which(!is.na(cres[[4]][2,]))]
    LRs[, cid, cmodel] = tapply(c(lr, rep(NA, 100)), c(round(abs(pe)), seq(0, 99)), mean, na.rm = TRUE)
    ps[, cid, cmodel] = summary(cglm)$coefficients[2:3,4]
    coefs[1:length(coef(cglm)), cid, cmodel] = coef(cglm)
    if (coefs[2,cid,cmodel] < -200) {browser()}
    #plot(LRfunction(cmod$solution[1], cmod$solution[2], -cmod$solution[3]/3, 1:50)[[1]], ylim = c(0, 1))
    cres[[2]]$model_p = predict(cglm, type = 'response')
    coefs[6:(5+length(x0[[cmodel]])), cid, cmodel] = cmod$solution
    AICs[cid, cmodel] = AIC(cglm) + 2*length(x0[[cmodel]])
  }
  message(paste('done!'), appendLF = FALSE)
  cAICs = round(AICs[cid,], 1)
  cAICs[2:4] = cAICs[2:4] - cAICs[1]
  message(paste(' || \t AIC1:', sprintf("%+.1f", cAICs[1]), '\t AIC2:', sprintf("%+.1f", cAICs[2]), '\t AIC3:', sprintf("%+.1f", cAICs[3]), '\t AIC4:', sprintf("%+.1f", cAICs[4])), appendLF = TRUE)
}

# Put data in similar format to mine
# AIC
model_names = c('rw', 'uncertainty', 'surprise', 'uncertainty_surprise')
data_aic_nico = as.data.table(AICs)
colnames(data_aic_nico) = model_names
data_aic_nico$participant_id = unique(DATA$participant_id)
data_aic_nico = data.table::melt(data_aic_nico,
                                 id.vars = 'participant_id',
                                 variable.name = 'model',
                                 value.name = 'AIC')

# Coefs
coef_names = c('intercept', 'V1', 'V2', 'V1u', 'V2u', 'alpha/l', 'u', 's')
# Expand for each participant
data_coefs_nico = data.table()
for(p_count in seq(length(coefs[1,,1]))){
  temp = as.data.table(coefs[,p_count,])
  colnames(temp) = model_names
  temp$coefs = coef_names
  temp$participant_id = unique(data_aic$participant_id)[p_count]
  data_coefs_nico = rbind(data_coefs_nico, temp)
}
data_coefs_nico = data.table::melt(data_coefs_nico,
                                   id.vars = c('participant_id', 'coefs'),
                                   measure.vars = model_names,
                                   variable.name = 'model',
                                   value.name = 'value')
setcolorder(data_coefs_nico, c('participant_id', 'model', 'coefs'))

data_coefs_nico = data_coefs_nico[!is.na(value)]
data_coefs_nico[model %in% c('rw', 'uncertainty') & coefs == 'alpha/l']$coefs = 'alpha'
data_coefs_nico[coefs == 'alpha/l']$coefs = 'l'

data_nico = data.table::merge.data.table(data_aic_nico,
                                         data_coefs_nico,
                                         by = c('participant_id',
                                                'model'))
data_nico$variable = 'coefs'
colnames(data_nico)[colnames(data_nico) == 'coefs'] = 'x'

x0_names = c('alpha', 'l', 'u', 's')
data_x0 = as.data.table(t(c(x0[[1]], x0[[3]])))
data_x0 = data_x0[rep(seq_len(nrow(data_x0)), each = 4*length(ids))]
colnames(data_x0) = x0_names
data_x0$participant_id = rep(ids, each = 4)
data_x0$model = rep(model_names, length(ids))
data_x0 = data.table::melt(data_x0,
                           id.vars = c('participant_id', 'model'),
                           variable.name = 'x')
data_x0$variable = 'x0'
data_x0[model %in% c('rw', 'uncertainty') & x != 'alpha'] = NA
data_x0[!model %in% c('rw', 'uncertainty') & x == 'alpha'] = NA
data_x0 = data_x0[!is.na(variable)]
data_x0 = data.table::merge.data.table(data_x0, data_aic,
                                       by = c('participant_id', 'model'))
data_nico = rbind(data_nico, data_x0)
setcolorder(data_nico, c('participant_id', 'model', 'AIC', 'variable', 'x', 'value'))

# Save nicos output in long format
save_path = file.path(here::here(), 'derivatives', 'model_fitting', 'fitting_nico.tsv',
                      fsep = .Platform$file.sep)
data.table::fwrite(data_nico, file = save_path, sep = '\t', na = 'n/a')

# Figure 1: beta's of RW model
# younger
# test if V1 and V2 are significantly different from 0
cmat = coefs[c(2:3, 6),agegroup==1,1]
t.test(cmat[1,]) # V1
t.test(cmat[2,]) # V2

cmeans = apply(cmat, 1, mean)
ccols = rep(hcl.colors(6, 'Earth', alpha = 0.8)[1], 3)
k = barplot(cmeans, beside = TRUE, col = ccols, border = NA, ylim = c(-1, 1))
abline(h = 0)
imat = matrix(c(k), dim(cmat)[1], dim(cmat)[2], byrow = FALSE)
beeswarm(cmat~imat, at = c(k), add = TRUE, pch = 21, col = '#555555BB', bg = ccols, cex = 0.5, lwd = 0.3, corral = 'wrap', method = 'hex')



# older
# test if V1 and V2 are significantly different from 0
cmat = coefs[c(2:3, 6),agegroup==2,1]
t.test(cmat[1,]) # V1
t.test(cmat[2,]) # V2

cmeans = apply(cmat, 1, mean)
ccols = rep(hcl.colors(6, 'Earth', alpha = 0.8)[1], 3)
k = barplot(cmeans, beside = TRUE, col = ccols, border = NA, ylim = c(-1, 1))
abline(h = 0)
imat = matrix(c(k), dim(cmat)[1], dim(cmat)[2], byrow = FALSE)
beeswarm(cmat~imat, at = c(k), add = TRUE, pch = 21, col = '#555555DD', bg = ccols, cex = 0.5, lwd = 0.3, corral = 'wrap', method = 'hex')

# correlation analyses across age groups
cmat = coefs[c(2:3, 6),,1]
apply(cmat, 1, mean)
cor.test(perf, cmat[1,])
cor.test(perf, cmat[2,])
cor.test(perf, cmat[3,])


tab1 = tabulate(apply(AICs[agegroup == 1,1:3], 1, function(x) which.min(x)))
tab2 = tabulate(apply(AICs[agegroup == 2,1:3], 1, function(x) which.min(x)))
chisq.test(cbind(tab1, tab2))


res = VB_bms(-AICs, 1000)
res1 = VB_bms(-AICs[agegroup == 1,1:3], 1000)
res2 = VB_bms(-AICs[agegroup == 2,1:3], 1000)

x1 = rnorm(1000, mean=60, sd=10)
x2 = rnorm(1000, mean=0, sd=10)
x3 = rnorm(1000, mean=30, sd=10)

h1 = hist(x1, breaks = seq(-36, 101, by = 5), plot = FALSE)
h2 = hist(x2, breaks = seq(-36, 101, by = 5), plot = FALSE)
h3 = hist(x3, breaks = seq(-36, 101, by = 5), plot = FALSE)

barplot(h1$counts, border = NA, col = hcl.colors(3, 'Dark3', alpha = 0.6)[1], bty = 'n')
barplot(h2$counts, border = NA, col = hcl.colors(3, 'Dark3', alpha = 0.6)[2], bty = 'n', add = TRUE)
barplot(h3$counts, border = NA, col = hcl.colors(3, 'Dark3', alpha = 0.6)[3], bty = 'n', add = TRUE)

opar = par(lwd = 2)
k = barplot(cbind(tab1, tab2), space = c(rep(0.1, 3), 1, rep(0.1, 2)), beside = TRUE, col = hcl.colors(4, 'Earth')[1:3], border = c(c('white', 1, NA), c('white', 'white', 1)), ylim = c(0, 25), cex.axis = 1.2, cex.lab = 1.2, names.arg = c('Younger\n Adults', 'Older\n Adults'))

k = barplot(cbind(res1$pxp, res2$pxp), space = c(rep(0.1, 3), 1, rep(0.1, 2)), beside = TRUE, col = hcl.colors(4, 'Earth')[1:3], border = c(c('white', 1, NA), c('white', 'white', 1)), ylim = c(0, 1), cex.axis = 1.2, cex.lab = 1.2, names.arg = c('Younger\n Adults', 'Older\n Adults'), yaxt = 'n')
axis(2, at = c(0, 0.5, 1), cex.axis = 1.2)


# panel 2: AICs
cAICs = AICs - AICs[,rep(1, 4)]
cmeans = cbind(apply(cAICs[agegroup == 1,2:4], 2, mean, na.rm = TRUE), apply(cAICs[agegroup == 2,2:4], 2, mean, na.rm = TRUE))
csds = cbind(apply(cAICs[agegroup == 1,2:4], 2, std.error), apply(cAICs[agegroup == 2,2:4], 2, std.error))
cmat1 = t(cAICs[agegroup == 1,2:4])
cmat2 = t(cAICs[agegroup == 2,2:4])



ccols = hcl.colors(4, 'Earth')[2:4]

k = barplot(cmeans, beside = TRUE, col = ccols, border = 'white', ylim = c(-25, 10), cex.axis = 1.2, cex.lab = 1.2, names.arg = c('Younger\n Adults', 'Older\n Adults'))
abline(h = 0)
imat1 = matrix(c(k[,1]), dim(cmat1)[1], dim(cmat1)[2], byrow = FALSE)
imat2 = matrix(c(k[,2]), dim(cmat2)[1], dim(cmat2)[2], byrow = FALSE)
beeswarm(cmat1~imat1, at = c(k[,1]), add = TRUE, pch = 21, col = '#888888', bg = hcl.colors(4, 'Earth', alpha = 0.8)[2:4], cex = 0.5, corral = 'wrap', method = 'hex')
beeswarm(cmat2~imat2, at = c(k[,2]), add = TRUE, pch = 21, col = '#888888', bg = hcl.colors(4, 'Earth', alpha = 0.8)[2:4], cex = 0.5, corral = 'wrap', method = 'hex')

#se_bars(k, cmeans, csds)



X2 = AICs[agegroup == 2,]
X1 = AICs[agegroup == 1,]


t.test(X1[,1], X1[,2], paired = TRUE)
t.test(X1[,1], X1[,3], paired = TRUE)
t.test(X1[,1], X1[,4], paired = TRUE)

t.test(X2[,1], X2[,2], paired = TRUE)
t.test(X2[,1], X2[,3], paired = TRUE)
t.test(X2[,1], X2[,4], paired = TRUE)

t.test(X1[,2], X1[,4], paired = TRUE)
t.test(X1[,2], X1[,3], paired = TRUE)

t.test(X2[,2], X2[,3], paired = TRUE)
t.test(X2[,2], X2[,4], paired = TRUE)

t.test(X1[,2]/X1[,3], X2[,2]/X2[,3])
t.test(X1[,2]-X1[,4], X2[,2]-X2[,4])
t.test(X1[,2]-X1[,4], X2[,2]-X2[,4])

# coef comparison
sapply(c(1:8), function(x) t.test(coefs[x,agegroup == 1,4], coefs[x,agegroup == 2,4])$p.value)
t.test(coefs[7,agegroup == 1,4], coefs[7,agegroup == 2,4])
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
