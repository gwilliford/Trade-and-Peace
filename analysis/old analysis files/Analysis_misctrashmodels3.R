setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis")
library(readstata13)
library(dplyr)
library(tvcure)
library(brglm)
library(ggplot2)
library(ggeffects)
options(scipen = 999)

b <- read.dta13("./data/icow_final.dta")
b <- b %>% rename("agstart1" = "_t0", "agstop2" = "_t", "agfail3" = "_d")
b$lncaprat <- log(b$lcaprat)
b$post45 <- ifelse(b$year > 1945, 1, 0)
b$igosum <- ifelse(is.na(b$igosum), 0, b$igosum)

maragfail  <- tvcure(formula  = Surv(agstart, agstop, midissyr) ~ lcaprat + recmidwt,
                     cureform = ~ dee + fattymagoo +
                       icowsal + riveriss + mariss +
                       recmidwt + recfatwt + recnowt + recyeswt +
                       demdy + autdy +
                       lag_pch_gdp_min +
                       lcaprat + ldefense + contdir + igosum,
                     data = icow_part_cyr, subset = icow_part_cyr$mariss == 1,
                     brglm = T, var = T, nboot = 30, link = "probit");summary(maragfail)   #### SOMething isn't happening with these bootstrap replications

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
b$pikachu2 <- ifelse(b$pikachu > 1, 1, icow_part_cyr$pikachu)

cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
cx1 <- coxph(Surv(agstart1, agstop2, agfail3) ~ dee + fattymagoo +
                  lag_pch_gdp_min + icowsal + riveriss + mariss + 
                  lcaprat + I(lcaprat^2) + igosum, 
                data = b); summary(cx1)
comp1 <- tvcure(Surv(agstart1, agstop2, agfail3) ~  lag_pch_gdp_min,
               cureform =  ~ dee + 
                 + dee +
                 icowsal + riveriss + mariss + 
                 lncaprat + igosum, 
               data = b, 
               var = T, nboot = 30, brglm = T); summary(comp1)
comp1 <- tvcure(Surv(agstart1, agstop2, agfail3) ~  pikachu2,
                cureform =  ~ pikachu2 + dee +
                  icowsal + riveriss + mariss + 
                  lncaprat + igosum, 
                data = b, 
                var = T, nboot = 30, brglm = T); summary(comp1)
comp1 <- tvcure(Surv(agstart1, agstop2, agfail3) ~  pikachu2,
                cureform =  ~ pikachu2 + dee +
                  icowsal + riveriss + mariss + 
                  lncaprat + igosum, 
                data = b, 
                var = T, nboot = 30, brglm = T); summary(comp1)
comp1 <- tvcure(Surv(clstart, clstop, midissyr) ~  dee + fattymagoo,
                cureform =  ~ dee + fattymagoo +
                  icowsal + riveriss + mariss + 
                  lncaprat + igosum, 
                data = b, 
                var = T, nboot = 30, brglm = T); summary(comp1)

comp1 <- tvcure(Surv(agstart1, agstop2, agfail3) ~  lag_pch_gdp_min + pchcaprat,
                cureform =  ~ dee + fattymagoo + icowsal + riveriss + mariss + 
                  lncaprat + igosum, 
                data = b, 
                var = T, nboot = 30, brglm = T); summary(comp1)

comp1 <- tvcure(Surv(clstart, clstop, clterm) ~  lag_pch_gdp_min + pchcaprat,
                cureform =  ~ dee +
                  icowsal + riveriss + mariss + 
                  lncaprat + igosum, 
                data = b, 
                var = T, nboot = 30, brglm = T); summary(comp1)
comp1 <- tvcure(Surv(clstart, clstop, clterm) ~  lag_pch_gdp_min + pchcaprat,
                cureform =  ~ dee + frank +
                  icowsal + riveriss + mariss + 
                  lncaprat + igosum, 
                data = b, 
                var = T, nboot = 30, brglm = T); summary(comp1)
comp1 <- tvcure(Surv(clstart, clstop, clterm) ~  lag_pch_gdp_min + pchcaprat,
                cureform =  ~ pikachu + squirtle + bulbasaur + 
                  icowsal + riveriss + mariss + 
                  lncaprat + igosum, 
                data = b, 
                var = T, nboot = 30, brglm = T); summary(comp1)
comp1 <- tvcure(Surv(clstart, clstop, clterm) ~  lag_pch_gdp_min + pchcaprat,
                cureform =  ~ charmander + psyduck + bulbasaur + 
                  icowsal + riveriss + mariss + 
                  lncaprat + igosum, 
                data = b, 
                var = T, nboot = 30, brglm = T); summary(comp1)
comp1 <- tvcure(Surv(clstart, clstop, midissyr) ~  lag_pch_gdp_min + pchcaprat,
                cureform =  ~ charmander + psyduck + bulbasaur + 
                  icowsal + riveriss + mariss + 
                  lncaprat + igosum, 
                data = b, 
                var = T, nboot = 30, brglm = T); summary(comp1)
comp1 <- tvcure(Surv(clstart, clstop, midissyr) ~  lag_pch_gdp_min + pchcaprat + strata(issue),
                cureform =  ~ charmander + psyduck + 
                  icowsal + riveriss + mariss + 
                  lncaprat + igosum, 
                data = b, 
                var = T, nboot = 30, brglm = T); summary(comp1)


###### This one
comp1 <- tvcure(Surv(clstart, clstop, midissyr) ~ charmander + psyduck + lag_pch_gdp_min + pchcaprat + strata(issue),
                cureform =  ~ charmander + psyduck + 
                  icowsal + riveriss + mariss + 
                  lncaprat + igosum, 
                data = b, 
                var = T, nboot = 30, brglm = T); summary(comp1)

