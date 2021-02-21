# trade <- read_csv("TradeOut.csv")
# 
# 
# dsub <- dfull[dfull$terriss == 1, ]


# Claim year
# ch_ind <- dfull["dyad" == 710750, ]
# # duplicated(dfull[, "year"])
# ch_ind_agg <- ch_ind %>% group_by(year) %>% summarize(
#   pcmsum = sum(attemptsp),
#   midsum = sum(midissyr)
# )


## Glms
a <- brglm::brglm(attanyp ~ a + icowsal + caprat + demdy + (conttype == 1), data = dfull, family = binomial(link = "logit")); summary(a)
a <- logistf::logistf(attanyp ~ a + icowsal + caprat + demdy + (conttype == 1), data = dfull, family = binomial(link = "logit")); summary(a)

### amen
fit_SRM<-ame(Y,model="bin")

library(amen)
library(statnet)

dfull <- left_join(icowpartdy, trade)
clist <- with(dfull, unique(dyad))
nnodes <- length(a)
b <- matrix(nrow = nnodes, ncol = nnodes)
diag(b) <- 0
as.network(b, directed = F, loops = F, matrix.type = "adjacency")


dutchcollege$Y #32 x 32 matrix of student dvs, 7 time points (3rd dimension of array)
dutchcollege$X #32 x 3 matrix of student ivs (no tvcs)
Y<-1*( dutchcollege$Y >= 2 )[,,2:7]

#TV matrix n * n * k vars * t

# 
mat <- matrix(1:15,ncol=3)
mat[,3] <- c(1,1,1,2,2)
lapply( split( mat[,1:2], mat[,3] ), matrix, ncol=2)
# arg 1 - matrix to split, arg2 - factor to split by (time), ncol = ncol
# dim 1 = students, dim2 = vars, dim 3 = time points


dsub <- as.matrix(dfull %>% select(dyad, year, npeace, bpeace, nmids, bmids, totsalmax, chalsalmax, tgtsalmax, ugtpred, ugtobs))
ab <- lapply(split(dsub[, 3:ncol(dsub)], dsub[, "year"]), matrix, ncol = 9)
ab <- split(dsub[, 3:ncol(dsub)], dsub[, "year"])
ab <- array(ab)
ab[]

# each element of this list is a matrix
ab <- lapply(split(dsub, dsub[, "year"]), matrix, ncol = 11)
ab[1]

#ncase rows, nvar cols, additional for time points
n <- 
nvar
t
d <- array(dim = (n, ))

# Questions
# Consistency of estimator - n at each t?
# Missing data - can it accomodate
# Does each case need to be in every time point