setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis")
library(readstata13)
library(dplyr)
library(tvcure)
library(brglm)
library(ggplot2)
library(ggeffects)
options(scipen = 999)


cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
agr8 <- tvcure(Surv(clstart, clstop, agree) ~ dee +
                 icowsal + riveriss + mariss +
                 recmidwt + recfatwt + recnowt + recyeswt +
                 lag_pch_gdp_min,
               cureform =  ~ dee +
                 demdy + autdy +
                 lcaprat + ldefense + contdir + igosum, 
               data = icow_part_cyr, 
               var = T, nboot = 100, brglm = T); summary(agr8)
agr8 <- tvcure(Surv(clstart, clstop, agree) ~ dee,               cureform =  ~ dee +
                 icowsal + riveriss + mariss,               data = icow_part_cyr, 
               var = T, nboot = 30, brglm = T); summary(agr8)
agr8 <- tvcure(Surv(clstart, clstop, agree) ~ dee + ponderosa +
                 icowsal + riveriss + mariss +
                 recmidwt + recfatwt + recnowt + recyeswt +
                 demdy + autdy +
                 lag_pch_gdp_min +
               lcaprat + ldefense + contdir + igosum, 
               cureform =  ~ dee + ponderosa +
                 icowsal + riveriss + mariss +
                 recmidwt + recfatwt + recnowt + recyeswt +
                 demdy + autdy +
                 lag_pch_gdp_min +
                 lcaprat + ldefense + contdir + igosum, 
               data = icow_part_cyr, 
               var = T, nboot = 100, brglm = T); summary(agr8)
agr8 <- tvcure(Surv(clstart, clstop, agree) ~ dee + fattymagoo +
                 icowsal + riveriss + mariss +
                 recmidwt + recfatwt + recnowt + recyeswt +
                 lag_pch_gdp_min,
               cureform =  ~ dee + fattymagoo +
                 demdy + autdy +
                 lag_pch_gdp_min +
                 lcaprat + ldefense + contdir + igosum, 
               data = icow_part_cyr, 
               var = T, nboot = 100, brglm = T); summary(agr8)
agr8 <- tvcure(Surv(clstart, clstop, agree) ~ dee + fattymagoo +
                 icowsal + riveriss + mariss +
                 recmidwt + recfatwt + recnowt + recyeswt +
                 lag_pch_gdp_min,
               cureform =  ~ dee + fattymagoo +
                 demdy + autdy +
                 lag_pch_gdp_min +
                 lcaprat + ldefense + contdir + igosum, 
               data = icow_part_cyr, 
               var = T, nboot = 100, brglm = T); summary(agr8)
agr9 <- tvcure(Surv(clstart, clstop, agreeiss) ~ dee + fattymagoo +
                 icowsal + riveriss + mariss +
                 recmidwt + recfatwt + recnowt + recyeswt +
                 lag_pch_gdp_min,
               cureform =  ~ dee + fattymagoo +
                 demdy + autdy +
                 lcaprat + ldefense + contdir + igosum, 
               data = icow_part_cyr, 
               var = T, nboot = 100, brglm = T); summary(agr9)
deemin <- min(icow_part_cyr$dee, na.rm = T)
deemax <- max(icow_part_cyr$dee, na.rm = T)
prediction3(agr9, "dee", c(deemin, deemax), "spop", CI = T, ylab = "Probability of Cure")
prediction3(agr8, "dee", c(deemin, deemax), "suncure", CI = T, ylab = "Probability of Cure")
summary(agr8)
prediction3(agr9, "dee", c(deemin, deemax), "basesurv", CI = T, ylab = "Probability of Cure")

l1 <- glm(agree ~ dee + ponderosa +
                 icowsal + riveriss + mariss +
                 recmidwt + recfatwt + recnowt + recyeswt +
                 demdy + autdy +
                 lag_pch_gdp_min +
                 lcaprat + ldefense + contdir + igosum, 
          family = binomial,
               data = icow_part_cyr); summary(l1)

l3 <- glm(agreeiss ~ dee + fattymagoo +
            icowsal + riveriss + mariss +
            recmidwt + recfatwt + recnowt + recyeswt +
            demdy + autdy +
            lag_pch_gdp_min +
            lcaprat + ldefense + contdir + igosum + c, 
          family = binomial,
          data = icow_part_cyr); summary(l3)
plot(ggpredict(l3, "dee"))
l3 <- glmer(agreeiss ~ dee + fattymagoo +
            icowsal + riveriss + mariss +
            recmidwt + recfatwt + recnowt + recyeswt +
            demdy + autdy +
            lag_pch_gdp_min +
            lcaprat + ldefense + contdir + igosum + c + (1 | dyad), 
          family = binomial,
          data = icow_part_cyr, 
          control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                 optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))); summary(l3)
plot(ggpredict(l3, "dee"))

