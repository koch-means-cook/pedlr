data_path = '/Users/schuck/data/magic_behavior_nico'

DATA = as.data.frame(data.table::fread(paste(data_path,'ChoiceData_082022_all_Flip.csv',sep=.Platform$file.sep),header = TRUE))
# only perceptual sessions
DATA = subset(DATA, DATA$ses %in% c(1,4))
# drop 1 D trials
DATA = DATA[!((DATA$X1t==0) | (DATA$Y1t==0)),]
# exclude bad performers
exclids = c('BE806','BE808','BE703')
DATA = subset(DATA, !(DATA$sub %in% exclids))
DATA = DATA[DATA$sub%in%unique(DATA$sub[DATA$ses==4]),]# &DATA2$Rtype==Rtype,]
DATA$sub = as.factor(DATA$sub)

nids = length(unique(DATA$sub))
# recode response variable
DATA$resp = NaN
DATA$resp[DATA$response=='h' | DATA$response=='g']=1 # left, A, X1.Y1
DATA$resp[DATA$response=='k' | DATA$response=='b']=0 # right, B, X3,Y3
# drop no answer trials
DATA = DATA[!is.nan(DATA$resp),]


# Recode Quadrants
# 0: Top left
# 1: Top right
# 2: Bottom right
# 3: Bottom left
# 4: Cross Quadrant
# check: tapply(DATA$X1t, DATA$Quantile, function(x) range(x))
# check: tapply(DATA$Y1t, DATA$Quantile, function(x) range(x))

DATA$quadrant = factor(DATA$Quantile)
levels(DATA$quadrant)  = c('TL', 'TR', 'BR', 'BL', 'XQ')

# make variable indicating whether we are in the rewarded quadrant
DATA$Rquadrant = DATA$quadrant == DATA$Rtype

# distances of target to X1 and X3
DATA$dist1t = sqrt((DATA$X1t - DATA$X2t)^2 + (DATA$Y1t - DATA$Y2t)^2)
DATA$dist3t = sqrt((DATA$X3t - DATA$X2t)^2 + (DATA$Y3t - DATA$Y2t)^2)

DATA$dist1OG = sqrt((DATA$X1OG - DATA$X2OG)^2 + (DATA$Y1OG - DATA$Y2OG)^2)
DATA$dist3OG = sqrt((DATA$X3OG - DATA$X2OG)^2 + (DATA$Y3OG - DATA$Y2OG)^2)

# diff of distances
DATA$distDt = round((DATA$dist1t - DATA$dist3t))
DATA$distDOG = round((DATA$dist1OG - DATA$dist3OG))

# caculate correctness for diff distance metrics
DATA$correctt = 0
DATA$correctt[DATA$resp == 1 & DATA$distDt < 0] = 1
DATA$correctt[DATA$resp == 0 & DATA$distDt > 0] = 1
DATA$correctt[DATA$distDt == 0] = NA

DATA$correctOG = 0
DATA$correctOG[DATA$resp == 1 & DATA$distDOG < 0] = 1
DATA$correctOG[DATA$resp == 0 & DATA$distDOG > 0] = 1
DATA$correctOG[DATA$distDOG == 0] = NA

cmat = tapply(DATA$correctt, list(DATA$Rquadrant, DATA$ses, DATA$sub), mean, na.rm = TRUE)
# difference in sessions
cmat = apply(cmat, c(1, 3), diff, na.rm = TRUE)*100
t.test(cmat[1,], cmat[2,], paired = TRUE)


cmat = tapply(DATA$correctOG, list(DATA$Rquadrant, DATA$ses, DATA$sub), mean, na.rm = TRUE)
# difference in sessions
cmat = apply(cmat, c(1, 3), diff, na.rm = TRUE)*100

t.test(cmat[1,], cmat[2,], paired = TRUE)
