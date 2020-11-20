setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis")
library(tvcure)
options(scipen = 999)
readRDS(icow_part_ct, "./data/icow_part_ct.RDS")
if(!exists("icow_part_ct")) icow_part_ct <- load("./data/icow_part_ct.RDS")
b <- read.dta13("./data/icow_final.dta")
b <- b %>% rename("agstart1" = "_t0", "agstop2" = "_t", "agfail3" = "_d")

cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)



agfail <- tvcure(formula  = Surv(agstart1, agstop2, agfail3) ~ lamin + lcmin + 
                   lag_pch_gdp_min + 
                    lcaprat + ldefense,
                  cureform = ~ lamin + lcmin + 
                   icowsal + riveriss + mariss + 
                   samereg + 
                   lcaprat + ldefense + contdir, 
                  data = b,
                  brglm = T, var = T, nboot = 30);summary(agfail)


# Works well  - probablity equation
anyterm <- tvcure(formula  = Surv(clstart, clstop, clterm) ~ lag_depdymin_100 + lag_deptotmin_100 + 
                    icowsal + riveriss + mariss + 
                    lcaprat + ldefense + contdir, 
                  cureform = ~ lag_depdymin_100 + lag_deptotmin_100 + 
                    lag_pch_gdp_min + GovCrises1 + GovCrises2
                    icowsal + riveriss + mariss + 
                    samereg + 
                    ldefense + contdir,
                  data = icow_part_ct,
                  brglm = T, var = T, nboot = 30);summary(anyterm)

# Adding samereg wrecks results
anyterm <- tvcure(formula  = Surv(agstart, agstop, clterm) ~ lamin + lbmin +
                    icowsal + riveriss + mariss + 
                    lcaprat + ldefense + contdir, 
                  cureform = ~ lamin + lbmin + 
                    lag_pch_gdp_min + 
                    icowsal + riveriss + mariss + 
                    samereg + 
                    lcaprat + ldefense + contdir, 
                  data = icow_part_ct,
                  brglm = F, var = T, nboot = 30);summary(anyterm)

anyfail <- tvcure(formula  = Surv(agstart1, agstop2, midissyr3) ~ lag_depdymin + 
                    lcaprat + ldefense + contdir,
                  cureform = ~ lag_depdymin + 
                    samereg + 
                    lag_pch_gdp_min + 
                    icowsal + riveriss + mariss + 
                    lcaprat + ldefense + contdir, 
                  data = icow_part_ct,
                  brglm = F, var = T, nboot = 30);summary(anyfail)


# Compliance

