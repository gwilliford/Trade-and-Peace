################################################################################
# Setup
################################################################################
setwd("C:/Users/gwill/Dropbox/Research/Dissertation/chapter3")

library(readstata13)
library(dplyr)
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
peaceatt$tradedep_geomean = peaceatt$tradedep_geomean/.102

peaceagr = read.dta13("C:/Users/gwill/Dropbox/Research/Dissertation/Data Management/outputdata/ICOWPeaceAgreementData.dta")
peaceagr = rename(peaceagr, "st" = "_st", "event" = "_d", "stop" = "_t", "start" = "_t0")
peaceagr$lnt = log(peaceagr$stop)
peaceagr$tradedep_geomean = peaceagr$tradedep_geomean/max(peaceagr$tradedep_geomean, na.rm = T)

peaceterm = read.dta13("C:/Users/gwill/Dropbox/Research/Dissertation/Data Management/outputdata/ICOWPeaceTerminationData2.dta")
peaceterm = rename(peaceterm, "st" = "_st", "event" = "_d", "stop" = "_t", "start" = "_t0")
peaceterm$lnt = log(peaceterm$stop)
peaceterm$tradedep_geomean = peaceterm$tradedep_geomean/max(peaceterm$tradedep_geomean, na.rm = T)

################################################################################
# Descriptive Statistics
################################################################################

# Number of claims peacefully resolved
# table(icow_claimdy$pterm3)

# Kaplan-Meier plot for peaceful termination
a = with(peaceatt, survfit(Surv(start, stop, event) ~ 1))
plot(a)
a = with(peaceagr, survfit(Surv(start, stop, event) ~ 1))
plot(a)

# Kaplan-Meier plot for peaceful termination
a = with(peaceterm, survfit(Surv(start, stop, event) ~ 1))
plot(a)
with(peaceterm, plot(survfit(Surv(start, stop, event) ~ 1)))
pterm_med = median(peaceterm$stop)
pterm_sd = sd(peaceterm$stop)
pterm_sdabove = pterm_med + pterm_sd * 2
pterm_sdbelow = pterm_med - pterm_sd * 2
abline(v = pterm_med, lty = 2)
abline(v = pterm_sdabove, lty = 3)
abline(v = pterm_sdbelow, lty = 3)

# Number of interstate acd conflicts
a = read.dta13("C:/Users/gwill/Dropbox/Data/UCDP/Conflict Year Dataset/ucdp-prio-acd-171.dta")
a$ccodeb = as.numeric(a$gwnob)
b = a %>% filter(!is.na(ccodeb))

length(unique(b$conflictid))

################################################################################
# Analysis
################################################################################

# Peaceful settlement attempts -------------------------------------------------
att_cox = coxph(Surv(start, stop, event) ~ dee2 + lpchcap + caprat +
                  icowsal + riveriss + mariss + 
                  recmidwt + recnowt + recyeswt + bdymid + 
                  demdy + autdy + contdir + defense + igosum,
                data = peaceatt, x = T); summary(att_cox)
att_x = as.data.frame(cbind(att_cox$y, att_cox$x))
att_x = rename(att_x, "event" = "status")
att_coxphf = coxphf(Surv(start, stop, event) ~ ltradedep_geomean + lpchcap + caprat +
                  icowsal + riveriss + mariss + 
                  recmidwt + recnowt + recyeswt + bdymid + 
                  demdy + autdy + contdir + defense + igosum,
                data = att_x, 
                penalty = .1, 
                maxit = 100); summary(att_coxphf)

att_logit = glmer(event ~ dee2 + lpchcap + caprat +
                  icowsal + riveriss + mariss + 
                  recmidwt + recnowt + recyeswt + bdymid + 
                  demdy + autdy + contdir + defense + igosum +
                  stop + I(stop^2) + I(stop^3) + (1 | claimdy) + (1 | year), family = binomial(link = "logit"),
                data = peaceatt); summary(att_logit)

att_cure = tvcure(Surv(start, stop, event) ~ lpchcap + recmidwt + recnowt + recyeswt + bdymid,
            cureform = ~ ltradedep_geomean + caprat +
              icowsal + riveriss + mariss +
              demdy + autdy + contdir + defense + igosum, 
            data = peaceatt, 
            link = "probit",
            brglm = T); summary(att_cure)

# Peace agreements -------------------------------------------------------------
agr_cox = coxph(Surv(start, stop, event) ~ ltradedep_geomean + lpchcap + caprat +
                       icowsal + riveriss + mariss + 
                       recmidwt + recnowt + recyeswt + bdymid + 
                       demdy + autdy + contdir + defense + igosum,
                     data = peaceatt, x = T); summary(agr_cox)
agr_x = as.data.frame(cbind(agr_cox$y, agr_cox$x))
agr_x = rename(agr_x, "event" = "status")
agr_coxphf = coxphf(Surv(start, stop, event) ~ ltradedep_geomean + lpchcap + caprat +
                      icowsal + riveriss + mariss + 
                      recmidwt + recnowt + recyeswt + bdymid + 
                      demdy + autdy + contdir + defense + igosum,
                    data = agr_x); summary(agr_coxphf)

agr_cure = tvcure(Surv(start, stop, event) ~ lagdee2 + recmidwt + recnowt + recyeswt + bdymid,
                  cureform = ~ caprat + icowsal + 
                    demdy + autdy + contdir + defense + igosum, 
                  data = peaceagr, subset = peaceagr$terriss == 1,
                  link = "probit",
                  brglm = T); summary(agr_cure)
agr_cure_sch = sch(agr_cure)
plotsch(agr_cure_sch, "lagdee2")
plotsch(agr_cure_sch, "lpchcap")
plotsch(agr_cure_sch, "recnowt")
plotsch(agr_cure_sch, "lpchcap")
plotsch(agr_cure_sch, "recyeswt")
plotsch(agr_cure_sch, "bdymid")


# Peaceful termination -----------------------------------------------------------
set_cox = coxph(Surv(start, stop, event) ~ ltradedep_geomean + lpchcap + caprat +
                  icowsal + riveriss + mariss + 
                  recmidwt + recnowt + recyeswt + bdymid + 
                  demdy + autdy + contdir + defense + igosum,
                data = peaceterm, x = T); summary(set_cox)
set_x = as.data.frame(cbind(set_cox$y, set_cox$x))
set_x = rename(set_x, "event" = "status")
set_coxphf = coxphf(Surv(start, stop, event) ~ ltradedep_geomean + lpchcap + caprat +
                      icowsal + riveriss + mariss + 
                      recmidwt + recnowt + recyeswt + bdymid + 
                      demdy + autdy + contdir + defense + igosum,
                    data = set_x); summary(set_coxphf)

set_cure = tvcure(Surv(start, stop, event) ~ ltradedep_geomean + lpchcap + recmidwt + recnowt + recyeswt + bdymid,
                  cureform = ~ ltradedep_geomean + caprat +
                    icowsal + riveriss + mariss +
                    demdy + autdy + contdir + defense + igosum, 
                  data = peaceterm, 
                  link = "probit",
                  brglm = T); summary(set_cure)
set_cure = tvcure(Surv(start, stop, event) ~ ltradedep_geomean + lpchcap + recmidwt + recnowt + recyeswt + bdymid,
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
m1 = tvcure(Surv(start, stop, event) ~ ltradedep_geomean + ltradedep_geomean:lnt + 
              lpchcap + recmidwt + recnowt + recyeswt + bdymid,
            cureform = ~ ltradedep_geomean + caprat + icowsal + riveriss + mariss + demdy + contdir + defense, 
            data = peaceterm,
            link = "probit",
            brglm = T, var = T)
m2_sch = sch(m2)
plotsch(m2_sch, "ltradedep_geomean")
plotsch(m2_sch, "recyeswt")

m4 = tvcure(Surv(start, stop, event) ~ ltradedep_geomean + lpchcap + recmidwt + recnowt + recyeswt + bdymid,
            cureform = ~ icowsal + riveriss + mariss + demdy + autdy + contdir + defense + igosum, 
            data = peaceterm, 
            brglm = F); summary(m4)



m2 = tvcure(Surv(start, stop, event) ~ lpchcap + recmidwt + bdymid,
            cureform = ~ ltradedep_geomean + caprat + icowsal + demdy + contdir + defense, 
            data = peaceterm, 
            link = "probit",
            brglm = T, var = T)

m2 = tvcure(Surv(start, stop, event) ~ ltradedep_geomean + lpchcap + recmidwt + bdymid,
            cureform = ~ caprat + icowsal + demdy + contdir + defense, 
            data = peaceterm, 
            link = "probit",
            brglm = T, var = T)

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
