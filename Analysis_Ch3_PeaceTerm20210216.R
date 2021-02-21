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
# peaceatt = peaceatt %>% filter(terriss == 1)
peaceatt = rename(peaceatt, "st" = "_st", "event" = "_d", "stop" = "_t", "start" = "_t0")
peaceatt$tradedep_geomean = peaceatt$tradedep_geomean/.102
peaceatt$stop2 = peaceatt$stop^2 / 1000
peaceatt$stop3 = peaceatt$stop^3 / 1000

peaceconc = read.dta13("C:/Users/gwill/Dropbox/Research/Dissertation/Data Management/outputdata/ICOWPeaceConcessionData.dta")
peaceconc = rename(peaceconc, "st" = "_st", "event" = "_d", "stop" = "_t", "start" = "_t0")
peaceconc$lnt = log(peaceconc$stop)
peaceconc$tradedep_geomean = peaceconc$tradedep_geomean/max(peaceconc$tradedep_geomean, na.rm = T)

peaceagr = read.dta13("C:/Users/gwill/Dropbox/Research/Dissertation/Data Management/outputdata/ICOWPeaceAgreementData.dta")
peaceagr = rename(peaceagr, "st" = "_st", "event" = "_d", "stop" = "_t", "start" = "_t0")
peaceagr$lnt = log(peaceagr$stop)
peaceagr$tradedep_geomean = peaceagr$tradedep_geomean/max(peaceagr$tradedep_geomean, na.rm = T)

peaceterm = read.dta13("C:/Users/gwill/Dropbox/Research/Dissertation/Data Management/outputdata/ICOWPeaceTerminationData2.dta")
peaceterm = rename(peaceterm, "st" = "_st", "event" = "_d", "stop" = "_t", "start" = "_t0")
peaceterm$lnt = log(peaceterm$stop)
peaceterm$tradedep_geomean = peaceterm$tradedep_geomean/max(peaceterm$tradedep_geomean, na.rm = T)

################################################################################
# Kaplan-Meier plots
################################################################################

# Number of claims peacefully resolved
# table(icow_claimdy$pterm3)

png("./figures/kmplots.png", width = 6, height = 3, res = 72, units = "in")
par(mfrow = c(1, 3))
a = with(peaceatt, survfit(Surv(start, stop, event) ~ 1))
plot(a, main = "Settlement Attempts", ylab = c("Survival Probability"))
a = with(peaceagr, survfit(Surv(start, stop, event) ~ 1))
plot(a, main = "Agreements", xlab = "Time (years)")
a = with(peaceterm, survfit(Surv(start, stop, event) ~ 1))
plot(a, main = "Claim Resolution")
dev.off()
# Add standard deviations
# with(peaceterm, plot(survfit(Surv(start, stop, event) ~ 1)))
# pterm_med = median(peaceterm$stop)
# pterm_sd = sd(peaceterm$stop)
# pterm_sdabove = pterm_med + pterm_sd * 2
# pterm_sdbelow = pterm_med - pterm_sd * 2
# abline(v = pterm_med, lty = 2)
# abline(v = pterm_sdabove, lty = 3)
# abline(v = pterm_sdbelow, lty = 3)

# Number of interstate acd conflicts
# a = read.dta13("C:/Users/gwill/Dropbox/Data/UCDP/Conflict Year Dataset/ucdp-prio-acd-171.dta")
# a$ccodeb = as.numeric(a$gwnob)
# b = a %>% filter(!is.na(ccodeb))
# 
# length(unique(b$conflictid))

################################################################################
# Analysis
################################################################################

# Peaceful settlement attempts -------------------------------------------------
att_cure = tvcure(Surv(start, stop, event) ~ lpchcap + recnowt + recyeswt + bdymid,
            cureform = ~ ltradedep_geomean, #+ caprat +
             # icowsal + demdy + contdir + defense + igosum, 
            data = peaceatt, 
            link = "logit",
            brglm = T)

att_logit = brglm(event ~ ltradedep_geomean + lpchcap + recmidwt + recnowt + recyeswt + bdymid + caprat +
                     icowsal + demdy + autdy + contdir + defense + igosum,
                   data = peaceatt, family = binomial(link = "logit")); summary(att_logit)

a = prediction4(att_cure, "uncureprob", "ltradedep_geomean", seq(0, .1, .01), CI = F)
# a = a + geom_vline(aes(xintercept = mean(peaceterm$ltradedep_geomean, na.rm = T) + 2 * sd(peaceterm$ltradedep_geomean, na.rm = T)))
# a = a + 
a = prediction4(set_cure, "uncureprob", "ltradedep_geomean", seq(0, .1, .01), CI = F, internals = T)
a$uncureprob


# Peace agreements -------------------------------------------------------------
agr_cure = tvcure(Surv(start, stop, event) ~ recmidwt + recnowt + recyeswt + bdymid,
                  cureform = ~ ltradedep_geomean + caprat + icowsal +
                    demdy + contdir + defense + igosum,
                  data = peaceagr,
                  link = "logit",
                  brglm = T)
# agr_cure_sch = sch(agr_cure)
# plotsch(agr_cure_sch, "lagdee2")
# plotsch(agr_cure_sch, "lpchcap")
# plotsch(agr_cure_sch, "recnowt")
# plotsch(agr_cure_sch, "lpchcap")
# plotsch(agr_cure_sch, "recyeswt")
# plotsch(agr_cure_sch, "bdymid")

agr_logit = brglm(event ~ ltradedep_geomean + lpchcap + recmidwt + recnowt + recyeswt + bdymid + caprat +
                    icowsal + demdy + autdy + contdir + defense + igosum,
                  data = peaceagr, family = binomial(link = "logit")); summary(agr_logit)

# Concessions ------------------------------------------------------------------

conc_cure = tvcure(Surv(start, stop, event) ~ recmidwt + recnowt + recyeswt + bdymid,
                  cureform = ~ ltradedep_geomean + caprat + icowsal +
                    demdy + autdy + contdir + defense + igosum,
                  data = peaceagr,
                  link = "logit",
                  brglm = T)

conc_logit = brglm(event ~ ltradedep_geomean + lpchcap + recmidwt + recnowt + recyeswt + bdymid + caprat +
                    icowsal + demdy + autdy + contdir + defense + igosum,
                  data = peaceconc, family = binomial(link = "logit"), pl = T); summary(conc_logit)


# Peaceful termination ---------------------------------------------------------
set_cure = tvcure(Surv(start, stop, event) ~  lpchcap + recmidwt + recnowt + recyeswt + bdymid,
                  cureform = ~ ltradedep_geomean + caprat +
                    icowsal + riveriss + mariss +
                    demdy + autdy + contdir + defense + igosum, 
                  data = peaceterm, 
                  link = "logit",
                  brglm = T)

a = prediction4(set_cure, "uncureprob", "ltradedep_geomean", seq(0, .1, .01), CI = F)
# a = a + geom_vline(aes(xintercept = mean(peaceterm$ltradedep_geomean, na.rm = T) + 2 * sd(peaceterm$ltradedep_geomean, na.rm = T)))
a = prediction4(set_cure, "uncureprob", "ltradedep_geomean", seq(0, .1, .01), CI = F, internals = T)
a$uncureprob
# b = rbind(a$uncureprob, seq(0, 1, .1))
# plot(x = b[, 2], y = b[, 1], type = "l")
# set_logit = brglm(event ~ lpchcap + recmidwt + recnowt + recyeswt + bdymid + lagdee2 + caprat +
#                    icowsal + demdy + autdy + contdir + defense + igosum, 
#                  data = peaceterm, family = binomial(link = "logit")); summary(set_logit)

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
