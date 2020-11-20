################################################################################
# Setup
################################################################################
setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis")

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

icow <- readRDS("./data/ICOWFinal.RDS")
icow$updown <- ifelse(icow$lagdee2 > mean(icow$lagdee2, na.rm = T), 1, 0)
icow = icow %>% 
  mutate(
    pchdee2 = (dee2 - lagdee2)/lagdee2
  )

################################################################################
# Kaplan-Meier plots
################################################################################

# Kaplan-Meier plot of cm attempt
plot(with(icow, survfit(Surv(clstart, clstop, attanyp) ~ 1)))

# Kaplan-Meier plot of any agreement
plot(with(icow, survfit(Surv(clstart, clstop, agree) ~ 1)))

# Kaplan-Meier plot of substantive agreement
plot(with(icow, survfit(Surv(clstart, clstop, agreeiss) ~ 1)))

# Kaplan-Meier plot of concessions
plot(with(icow, survfit(Surv(clstart, clstop, conc) ~ 1)))

# Kaplan-Meier plot of claim termination
plot(with(icow, survfit(Surv(clstart, clstop, clterm) ~ 1)))

################################################################################
# Analysis
################################################################################

### Create generic formulas
hazform <- as.formula( ~ pchdee2 + 
                         lgovcrisesdy + 
                         updown + icowsal + 
                         bdymid + recmidwt + recnowt + recyeswt)

cureform <- as.formula( ~ icowsal + riveriss + mariss +
                          demdy + autdy +
                          lcaprat + ldefense + contdir + igosum)

fullform <- as.formula( ~ icowsal + riveriss + mariss +
                          recmidwt + recfatwt + recnowt + recyeswt +
                          lag_pch_gdpmax + lgovcrisesdy + midissyr +  
                          demdy + autdy +
                          lcaprat + ldefense + contdir + igosum)

### Claim termination
at1 <- tvcure(update.formula(hazform, Surv(clstart, clstop, clterm) ~ .), 
              cureform = update.formula(cureform,  ~ updown  + .),
              data = icow, 
              var = T, nboot = 100, brglm = F); summary(at1)
