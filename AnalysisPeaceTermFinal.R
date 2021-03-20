setwd("C:/Users/gwill/Dropbox/Research/Dissertation/chapter3")

library(readstata13)
library(dplyr)
library(tvcure)
library(brglm)
library(doSNOW)
library(compiler)
library(beepr)
library(xtable)

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

peaceterm = read.dta13("C:/Users/gwill/Dropbox/Research/Dissertation/Data Management/outputdata/ICOWPeaceTerminationData2.dta")
peaceterm = rename(peaceterm, "st" = "_st", "event" = "_d", "stop" = "_t", "start" = "_t0")
peaceterm$lnt = log(peaceterm$stop)
peaceterm$ltradedep_geomean = peaceterm$ltradedep_geomean/max(peaceterm$ltradedep_geomean, na.rm = T)


################################################################################
# Kaplan-Meier plot
################################################################################

png("./figures/kmplots.png", width = 4, height = 4, res = 72, units = "in")
a = with(peaceterm, survfit(Surv(start, stop, event) ~ 1))
plot(a)#, main = "Claim Resolution")
dev.off()


################################################################################
# Analysis
################################################################################
set_cox = coxph(Surv(start, stop, event) ~ ltradedep_geomean + recmidwt +
                  recnowt + recyeswt + caprat +
                  icowsal + riveriss + mariss +
                  demdy + autdy + contdir,
                data = peaceterm); summary(set_cox)
set.seed(1681435)
set_cure = tvcure(Surv(start, stop, event) ~ ltradedep_geomean +
                    recmidwt + recnowt + recyeswt + bdymid,
                  cureform = ~ icowsal + riveriss + mariss +
                    caprat + demdy + autdy + contdir,
                  data = peaceterm,
                  link = "logit",
                  brglm = T, var = F, parallel = F, nboot = 500)


################################################################################
#  Plot
################################################################################
b = predict(set_cure, "suncure", "ltradedep_geomean", c(0, 1), CI = F, internals = T)
b = b$splot +
  scale_linetype(labels = c(0, 1), name = "Average Trade Dependence") +
  scale_color_discrete(labels = c(0, 1), name = "Average Trade Dependence")
ggsave("./figures/trade_suncure.png", b, width = 6, height = 4, units = "in")


################################################################################
# Results table
################################################################################
varlist = list("Average Trade Dependence" = "ltradedep_geomean",
               "Recent MIDs" = "recmidwt",
               "Recent Failed CM Attempts" = "recnowt",
               "Recent Successful CM Attempts" = "recyeswt",
               "Ongoing MID" = "bdymid",
               "Issue Salience" = "icowsal",
               "River Claim" = "riveriss",
               "Maritime Claim" = "mariss",
               "Joint Democracy" = "demdy",
               "Joint Autocracy" = "autdy",
               "Contiguity" = "contdir",
               "Alliance" = "defense",
               "Joint IGO Membership" = "igosum",
               "Capability Ratio" = "caprat")

rtab = xtable(tvtable(set_cox, set_cure,
                      varlist = varlist, siglevel = c('**' = 0.05, '*' = 0.10)))
print(rtab,
      booktabs = F,
      sanitize.text.function = identity,
      include.rownames = F,
      include.colnames = F)
