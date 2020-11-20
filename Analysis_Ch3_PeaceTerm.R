################################################################################
# Setup
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
# Data Management
################################################################################

peaceterm = read.dta13("C:/Users/gwill/Dropbox/Research/Dissertation/Data Management/outputdata/ICOWPeaceTerminationData.dta")
peaceterm = rename(peaceterm, "st" = "_st", "event" = "_d", "stop" = "_t", "start" = "_t0")


################################################################################
# Descriptive Statistics
################################################################################

# Number of claims peacefully resolved
table(icow_claimdy$pterm2)

# Kaplan-Meier plot for peaceful termination
a = with(peaceterm, survfit(Surv(start, stop, event) ~ 1))

with(peaceterm, plot(survfit(Surv(start, stop, event) ~ 1)))
pterm_med = median(peaceterm$stop)
pterm_sd = sd(peaceterm$stop)
pterm_sdabove = pterm_med + pterm_sd * 2
pterm_sdbelow = pterm_med - pterm_sd * 2
abline(v = pterm_med, lty = 2)
abline(v = pterm_sdabove, lty = 3)
abline(v = pterm_sdbelow, lty = 3)


################################################################################
# Analysis
################################################################################
m2 = coxph(Surv(start, stop, event) ~ ltradedepmin_logc + recmidwt + recnowt + icowsal, data = peaceterm); summary(m2)
m2 = tvcure(Surv(start, stop, event) ~ ltradedepmin_logc + lpchcap + recmidwt + recnowt + recyeswt + bdymid,
            cureform = ~ ltradedepmin_logc + caprat + icowsal + riveriss + mariss + demdy + autdy + contdir + defense + igosum, 
            data = peaceterm, 
            brglm = T); summary(m2)
m2 = tvcure(Surv(start, stop, event) ~ lpchcap + recmidwt + recnowt + recyeswt + bdymid,
            cureform = ~ ltradedepmin_logc + icowsal + riveriss + mariss + demdy + autdy + contdir + defense + igosum, 
            data = peaceterm, 
            brglm = T); summary(m2)
m2 = tvcure(Surv(start, stop, event) ~ ltradedepmin_logc + lpchcap + recmidwt + recnowt + recyeswt + bdymid,
            cureform = ~ icowsal + riveriss + mariss + demdy + autdy + contdir + defense + igosum, 
            data = peaceterm, 
            brglm = F); summary(m2)
################################################################################
# Results of testing
################################################################################
# Tradedepmin
  # Both looks good at .1 level
  # cureonly looks good at .1 level
  # hazard only looks bad
  # use brglm for best results

# ltradedepmin
  # looks like it could reach .05 in glm equation
  # null in cure only
  # null for hazard only

# ltradedepmin_sqrt
  # looks like it could reach .1 in glm equation
  # null in cure only
  # null for hazard only

# ltradedepminbc
  # null for both, 
  # null for cureonly, 
  # null for hazard only

# ltradedepmin_c
  # p = .13 in glm
  # null for cure only
  # null for hazard only

# ltradedepmin_logc
  # both hull
  # cure only null
  # hazard only null