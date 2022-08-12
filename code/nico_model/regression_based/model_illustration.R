code_path = '/Users/schuck/code/pedlr/code/nico_model/regression_based'

source(paste(code_path, '/LRfunction.R', sep = ''))

#### MODEL ILLUISTRATION
# fixes end value, fixed slope, var starting
cPE = seq(0, 50, length.out = 50)
ccols = hcl.colors(15, 'Oranges')[1:10]
ahat = seq(0.05, 0.95, length.out = 10)
cLR = LRfunction(ahat[1], 0.5, 1, cPE)[[1]]
plot(cPE, cLR, col = ccols[1], type = 'l', lwd = 1.5, bty = 'n', cex.lab = 1.2, cex.axis = 1.2, ylab = '', xlab = '', ylim = c(0, 1), yaxt = 'n')
axis(2, at = c(0, 1), cex.axis = 1.2)
for (i in 2:10) {
  cLR = LRfunction(ahat[i], 0.5, 1, cPE)[[1]]
  lines(cPE, cLR, col = ccols[i], type = 'l', lwd = 1.5)
}

# fixes start value, fixed slope, end starting

ahat = seq(0.00, 1, length.out = 10)
cLR = LRfunction(0.5, ahat[1], 1, cPE)[[1]]
plot(cPE, cLR, col = ccols[1], type = 'l', lwd = 1.5, bty = 'n', cex.lab = 1.2, cex.axis = 1.2, ylab = '', xlab = '', ylim = c(0, 1), yaxt = 'n')
axis(2, at = c(0, 1), cex.axis = 1.2)
for (i in 2:10) {
  cLR = LRfunction(0.5, ahat[i], 1, cPE)[[1]]
  lines(cPE, cLR, col = ccols[i], type = 'l', lwd = 1.5)
}

cPE = seq(0, 50, length.out = 50)
ccols = hcl.colors(15, 'Oranges')[1:10]
slope = seq(-2.5, 2.5, length.out = 10)
cLR = LRfunction(0.1, 0.9, slope[1], cPE)[[1]]
plot(cPE, cLR, col = ccols[1], type = 'l', lwd = 1.5, bty = 'n', cex.lab = 1.2, cex.axis = 1.2, ylab = '', xlab = '', ylim = c(0, 1), yaxt = 'n')
axis(2, at = c(0, 1), cex.axis = 1.2)
for (i in 2:10) {
  cLR = LRfunction(0.1, 0.9, slope[i], cPE)[[1]]
  lines(cPE, cLR, col = ccols[i], type = 'l', lwd = 1.5)
}


# logistic function of uncertainty

b0 = 0
b1 = 0.1
b2 = -0.1
b3 = seq(-0.2, 0.2, length.out = 10)

vals1 = seq(1, 100, by = 2)
vals2 = rep(50, 50)
uncertainty = 10

x = b0 + b1*vals1 + b2*vals2 + b3[1]*uncertainty
cp = exp(x)/(1 + exp(x))
plot(vals1-vals2, cp, col = ccols[1], type = 'l', lwd = 1.5, bty = 'n', cex.lab = 1.2, cex.axis = 1.2, ylab = '', xlab = '', ylim = c(0, 1), yaxt = 'n')
axis(2, at = c(0, 1), cex.axis = 1.2)
for (i in 2:10) {
  x = b0 + b1*vals1 + b2*vals2 + b3[i]*uncertainty
  cp = exp(x)/(1 + exp(x))
  lines(vals1-vals2, cp, col = ccols[i], type = 'l', lwd = 1.5)
}

b0 = 0
b1 = 0.1
b2 = -0.1
b3 = 0

x = b0 + b1*vals1 + b2*vals2 + b3[1]*uncertainty
cp = exp(x)/(1 + exp(x))
plot(vals1-vals2, cp, col = '#333333', type = 'l', lwd = 1.5, bty = 'n', cex.lab = 1.2, cex.axis = 1.2, ylab = '', xlab = '', ylim = c(0, 1), yaxt = 'n')
axis(2, at = c(0, 1), cex.axis = 1.2)
