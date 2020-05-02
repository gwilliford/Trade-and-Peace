setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis")
library(tvcure)
options(scipen = 999)
if (!exists("icow_part_cyr")) {
  icow_part_cyr <- readRDS("./data/ICOWFinal.RDS")
} else icow <- icow_part_cyr

hazform <- as.formula(~ icowsal + riveriss + mariss +
  recmidwt + recfatwt + recnowt + recyeswt +
  lag_pch_gdpmax + lgovcrisesDy)

cureform <- as.formula(~ icowsal + riveriss + mariss +
                          demdy + autdy +
                          lcaprat + ldefense + contdir + igosum + 
                          lag_pch_gdpmax + lgovcrisesDy)

# varlist = list("Capability Change" = "capchange",
#                "Foreign Imposed Regime Change" = "archigosFIRC",
#                "Deaths" = "lndeaths",
#                "index")
##### attempt models---------------------------------------------
at1 <- tvcure(update.formula(hazform, Surv(at_start, at_stop, attanyp) ~ lagdee2 + .), 
              cureform = update.formula(cureform, ~ lagdee2 + .),
             data = icow, 
             var = T, nboot = 100, brglm = T); summary(at1)
at2 <- tvcure(update.formula(hazform, Surv(at_start, at_stop, attanyp) ~ lagmac2 + .),
              cureform =  update.formula(cureform, ~ lagmac2 + .),
              data = icow,
              var = T, nboot = 100, brglm = T, parallel = T); summary(at2)
at1_t <- tvtable.tvcure(at1, format = "long")
at2_t <- tvtable.tvcure(at2, format = "long")
at_table <- tvtable_combine(c("at1_t", "at2_t"), format = "long", footnote = "text")
x <- tvtable_xtable(at_table, caption = "Peaceful Settlement Attempts")
printer(x, file = "./tables/at_table.tex", tabular.enviroment = "tabularx", booktabs = T)


gtools::
##### agreement models-------------------------------------------
ag1 <- tvcure(update.formula(hazform, Surv(agtart, agtop, agterm) ~ lagdee2 + .),
             cureform =  ~ update.formula(hazform, Surv(midstart, midstop, midissyr) ~ lagdee2 + .),
             data = icow, 
             var = T, nboot = 100, brglm = T); summary(ap1)
ag2 <- tvcure(update.formula(cureform ~ lagmac2 + .),
             cureform = update.formula(cureform, ~ lagmac2 + .),
             data = icow, 
             var = T, nboot = 100, brglm = T); summary(ag2)

##### substantive agreement models -----------------------------------------------------
agiss1 <- tvcure(update.formula(hazform, Surv(agiss_start, agiss_stop, agissterm) ~ lagdee2 + .),
                cureform = update.formula(hazform, Surv(midstart, midstop, midissyr) ~ lagdee2 + .),
                data = icow, 
                var = T, nboot = 100, brglm = T); summary(agiss1)
agiss2 <- tvcure(update.formula(cureform, ~ lagmac2 + .),
                #lag_pch_gdp_min,
                cureform =  update.formula(cureform, Surv(midstart, midstop, midissyr) ~ lagmac2 + .),
                data = icow, 
                var = T, nboot = 100, brglm = T); summary(agiss2)

##### Claim termination models ----------------------------------------------------------------
clterm1 <- tvcure(update.formula(hazform, Surv(clstart, clstop, clterm) ~ lagdee2 + .),
                  cureform = update.formula(cureform, ~ lagdee2 + .),
                data = icow, 
                var = T, nboot = 100, brglm = T); summary(clterm1)
clterm2 <- tvcure(update.formula(cureform ~ lagmac2 + .),
                  cureform = update.formula(cureform, ~ lagmac2 + .),
                 data = icow, 
                 var = T, nboot = 100, brglm = T); summary(clterm2)


##### MID models ------------------------------------------
midterm1 <- tvcure(update.formula(hazform, Surv(clstart, clstop, clterm) ~ lagdee2 + .),
                 cureform = update.formula(cureform, ~ lagdee2 + .), 
                 data = icow, 
                 var = F, nboot = 100, brglm = T); summary(midterm1)
midterm2 <- tvcure(update.formula(cureform, Surv(midstart, midstop, midissyr) ~ lagmac2 + .),
                  cureform =  ~ update.formula(cureform, ~ lagmac2 + .),
                  data = icow,
                  var = T, nboot = 100, brglm = T); summary(midterm2)
