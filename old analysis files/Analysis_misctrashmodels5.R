setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis")
library(tvcure)
options(scipen = 999)
cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)



att <- tvcure(Surv(clstart, clstop, attanyp) ~ dee +
               icowsal + riveriss + mariss +
               recmidwt + recfatwt + recnowt + recyeswt +
               lag_pch_gdp_min,
             cureform =  ~ dee +
               demdy + autdy +
               lcaprat + ldefense + contdir + igosum, 
             data = icow_part_cyr, 
             var = T, nboot = 100, brglm = T); summary(att)
att_pred <- prediction3(att, "dee", c(-44, -29), type = "uncureprob", CI = T)
plot(att_pred)

ag <- tvcure(Surv(clstart, clstop, agree) ~ dee +
                 icowsal + riveriss + mariss +
                 recmidwt + recfatwt + recnowt + recyeswt +
                 lag_pch_gdp_min,
               cureform =  ~ dee +
                 icowsal + riveriss + mariss +
                 demdy + autdy +
                 lcaprat + ldefense + contdir + igosum, 
               data = icow_part_cyr, 
               var = T, nboot = 100, brglm = T); summary(ag)
agiss <- tvcure(Surv(clstart, clstop, agreeiss) ~ dee + fattymagoo + 
                 recmidwt + recfatwt + recnowt + recyeswt +
                 lag_pch_gdp_min,
               cureform =  ~ dee +
                 icowsal + riveriss + mariss +
                 demdy + autdy +
                 lcaprat + ldefense + contdir + igosum, 
               data = icow_part_cyr, 
               var = T, nboot = 100, brglm = T); summary(agiss)
agiss_pred <- prediction3(agiss, "dee", c(-46, -31), type = "uncureprob", CI = T)
plot(agiss_pred)

agiss2 <- tvcure(Surv(clstart, clstop, agreeiss) ~ frank + 
                  recmidwt + recfatwt + recnowt + recyeswt +
                  lag_pch_gdp_min,
                cureform =  ~ frank +
                  icowsal + riveriss + mariss +
                  demdy + autdy +
                  lcaprat + ldefense + contdir + igosum, 
                data = icow_part_cyr, 
                var = T, nboot = 100, brglm = T); summary(agiss2)
agiss2_pred <- prediction3(agiss2, "frank", c(-44, -29), type = "uncureprob", CI = T)
plot(agiss2_pred, xlab = "Probability of Agreement", ylab = "Maximum Economic Interdependence (ln(Trade/GDP))")

b <- read.dta13("./data/icow_final.dta")
b <- b %>% rename("agstart1" = "_t0", "agstop2" = "_t", "agfail3" = "_d")
ag_fail <- tvcure(Surv(agstart1, agstop2, agfail3) ~ recmidwt + recfatwt + recnowt + recyeswt +
                    lag_pch_gdp_min,
                  cureform =  ~ dee +
                    icowsal + riveriss + mariss +
                    demdy + autdy +
                    lcaprat + ldefense + contdir + igosum, 
                  data = b, 
                  var = T, nboot = 100, brglm = T); summary(ag_fail)
ag_fail <- coxph(Surv(agstart1, agstop2, agfail3) ~ dee +
                    icowsal + riveriss + mariss +
                    recmidwt + recfatwt + recnowt + recyeswt +
                    lag_pch_gdp_min, data = b)
                  