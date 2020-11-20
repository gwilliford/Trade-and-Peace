################################################################################
##### Analysis_Ch3_ClaimRecurrence
# Analyzes whether ICOW claims recur after being resolved once
# Findings: Number of recurrenc claims is too small to do any kind of analysis
################################################################################
setwd("C:/Users/gwill/Dropbox/Research/Dissertation/chapter3")

library(readstata13)
library(tvcure)
library(doSNOW)
library(compiler)
library(beepr)

options(scipen = 999)

# Precompile tvcure functions
cmpfun(tvcure)
enableJIT(3)

# Parallel processing
cl <- makeCluster(4, "SOCK")
registerDoSNOW(cl)


################################################################################
# Analysis
################################################################################
icowrecur = read.dta13("C:/Users/gwill/Dropbox/Research/Dissertation/Data Management/outputdata/ICOWClaimRecurrenceDataset.dta")
icowrecur = rename(icowrecur, "st" = "_st", "event" = "_d", "stop" = "_t", "start" = "_t0")

m1 = tvcure(Surv(start, stop, event) ~ recmidwt + recnowt,
       cureform = ~ icowsal,
       data = icowrecur, 
       var = F, nboot = 100, brglm = T)

m2 = coxph(Surv(start, stop, event) ~ recmidwt + recnowt + icowsal, data = icowrecur)
