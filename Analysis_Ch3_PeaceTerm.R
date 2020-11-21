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

peaceatt = read.dta13("C:/Users/gwill/Dropbox/Research/Dissertation/Data Management/outputdata/ICOWPeaceAttemptData.dta")
peaceatt = rename(peaceatt, "st" = "_st", "event" = "_d", "stop" = "_t", "start" = "_t0")

peaceagr = read.dta13("C:/Users/gwill/Dropbox/Research/Dissertation/Data Management/outputdata/ICOWPeaceAgreementData.dta")
peaceagr = rename(peaceagr, "st" = "_st", "event" = "_d", "stop" = "_t", "start" = "_t0")

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


att_cox = coxph(Surv(start, stop, event) ~ ltradedep_geomean + lpchcap + caprat +
                  icowsal + riveriss + mariss + 
                  recmidwt + recnowt + recyeswt + bdymid + 
                  demdy + autdy + contdir + defense + igosum,
                data = peaceatt); summary(att_cox)
att_coxphf = coxphf(Surv(start, stop, event) ~ ltradedep_geomean + lpchcap + caprat +
                  icowsal + riveriss + mariss + 
                  recmidwt + recnowt + recyeswt + bdymid + 
                  demdy + autdy + contdir + defense + igosum,
                data = peaceatt); summary(att_coxphf)

att_cure = tvcure(Surv(start, stop, event) ~ lpchcap + recmidwt + recnowt + recyeswt + bdymid,
            cureform = ~ ltradedep_geomean + caprat +
              icowsal + riveriss + mariss +
              demdy + autdy + contdir + defense + igosum, 
            data = peaceatt, 
            link = "probit",
            brglm = T); summary(att_cure)

agr_cox = coxph(Surv(start, stop, event) ~ ltradedep_geomean + lpchcap + caprat +
                       icowsal + riveriss + mariss + 
                       recmidwt + recnowt + recyeswt + bdymid + 
                       demdy + autdy + contdir + defense + igosum,
                     data = peaceatt); summary(agr_cox)

agr_cure = tvcure(Surv(start, stop, event) ~ lpchcap + recmidwt + recnowt + recyeswt + bdymid,
                  cureform = ~ ltradedep_geomean + caprat +
                    icowsal + riveriss + mariss +
                    demdy + autdy + contdir + defense + igosum, 
                  data = peaceagr, 
                  link = "probit",
                  brglm = T); summary(agr_cure)

set_cox = coxph(Surv(start, stop, event) ~ ltradedep_geomean + lpchcap + caprat +
                  icowsal + riveriss + mariss + 
                  recmidwt + recnowt + recyeswt + bdymid + 
                  demdy + autdy + contdir + defense + igosum,
                data = peaceterm); summary(set_cox)

set_cure = tvcure(Surv(start, stop, event) ~ lpchcap + recmidwt + recnowt + recyeswt + bdymid,
                  cureform = ~ ltradedep_geomean + caprat +
                    icowsal + riveriss + mariss +
                    demdy + autdy + contdir + defense + igosum, 
                  data = peaceterm, 
                  link = "probit",
                  brglm = T); summary(set_cure)


################################################################################
# Analysis
################################################################################
m1 = coxph(Surv(start, stop, event) ~ ltradedep_geomean + lpchcap + caprat +
             icowsal + riveriss + mariss + 
             recmidwt + recnowt + recyeswt + bdymid + 
             demdy + autdy + contdir + defense + igosum,
           data = peaceterm); summary(m1)
m2 = tvcure(Surv(start, stop, event) ~ ltradedep_geomean +
              lpchcap + recmidwt + recnowt + recyeswt + bdymid,
            cureform = ~ ltradedep_geomean + caprat + icowsal + riveriss + mariss + demdy + contdir + defense, 
            data = peaceterm,
            link = "probit",
            brglm = T); summary(m2)
m4 = tvcure(Surv(start, stop, event) ~ ltradedep_geomean + lpchcap + recmidwt + recnowt + recyeswt + bdymid,
            cureform = ~ icowsal + riveriss + mariss + demdy + autdy + contdir + defense + igosum, 
            data = peaceterm, 
            brglm = F); summary(m4)

# Theory - public pushes policymakers to take action - leads to settlement attempts and settlements
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
  # both null
  # cure only null
  # hazard only null

# ltradedepmin_logc1
  # both null
  # cure only null
  # hazard only null

# ltradedep_geomean
  # both - significant at .05 in glm equation - separation
  # glm only - could be significant at .01 level, separation
  # hazard only - insignificant
