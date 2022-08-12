
X = tapply(DATA$outcome, list(DATA$participant_id, DATA$block, DATA$chosen_bandit), function(x) rbind(hist(x, plot = FALSE)$counts, hist(x, plot = FALSE)$counts/sum(hist(x, plot = FALSE)$counts)))
Xdist = matrix(NA, 8, 3)
rownames(Xdist) = seq(0, 0.7, 0.1)
for (i in 1:3) {
  cX = matrix(unlist(X[,,i]), nrow = 2)
  cX = tapply(cX[1,], round(cX[2,]*10)/10, sum)
  Xdist[names(cX),i] = cX
}

Xdist = apply(Xdist, 2, function(x) x/sum(x, na.rm = TRUE))

matplot(Xdist, type = 'l', col = hcl.colors(3, 'Set3'), lty = 1, lwd = 2)


plot(tapply(X2[1,], round(X2[2,], 1), sum), type = 'l')
