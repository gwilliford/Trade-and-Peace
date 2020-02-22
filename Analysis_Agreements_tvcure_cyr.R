setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis")
library(readr)
library(tvcure)
options(scipen = 999)

if(!exists("icow_part_cyr_ct")) icow_part_cyr_ct <- read_csv("./data/icow_part_cyr_ct.csv")
cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)

# Looking for positive coefficients on the probability eq
# Looking for positive coefficients on the hazard eq
##### Bargaining/Self-enforcing agreements
# Information
  # Capability ratio
  # Last Battle Fatalities
  # Recent MIDs
  # Recagree - signals willingness to compromise
# Commitment Problems
  # Recagree - builds trust - mitigates domestic opposition
# Opportunity Costs
  # Foregone trade
  # Trade
  # Total Trade Dependence
  # Alliance
# Domestic Variables
  # W / Regime Type

# m <- tvcure(Surv(y0, year, agreeiss) ~ lterugtdepmax + lag_ln_trade + 
#               + lag_ln_gdp_chal + lag_ln_gdp_tgt + 
#               lpol_chal * lpol_tgt + lcaprat + contdir,
#             cureform = ~ lag_ln_gdp_chal + lag_ln_gdp_tgt + lag_ln_gdpcap_chal + lag_ln_gdpcap_tgt + 
#               ldefense)
# 
# m <- tvcure(Surv(y0, year, agreeiss) ~ lterugtdepmax, cureform = ~ recmidwt + recnowt + recyeswt,
#             data = icow_part_cyr_ct, var = T, brglm = T, subset = icow_part_cyr_ct$lrivugtdepmax > 0)
# m <- tvcure(Surv(y0, year, agreeiss) ~ lterugtdepmax, cureform = ~ recfatwt + lcaprat +,
#             data = icow_part_cyr_ct, var = T, brglm = T, subset = icow_part_cyr_ct$lrivugtdepmax > 0, nboot = 30);summary(m)
# 
# 
# m <- tvcure(Surv(y0, year, agreeiss) ~ lterugtdepmax, cureform = ~ recfatwt + lcaprat + trival,
#             data = icow_part_cyr_ct, var = T, brglm = T, subset = icow_part_cyr_ct$lrivugtdepmax > 0, nboot = 30);summary(m)

cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
m <- tvcure(Surv(y0, year, agreeiss) ~ lterugtdepmax + lag_ln_depdymax + lag_ln_deptotmax + 
              lpol1 + lpol2 + demdy +  
              icowsal + 
              recmidwt + recfatwt + recnowt + recyeswt + 
              lcaprat + ldefense, 
            cureform = ~ lterugtdepmax + lag_ln_depdymax + lag_ln_deptotmax + 
              lag_ln_gdp_chal + lag_ln_gdp_tgt + 
              W_chal * W_tgt + GovCrises_chal + GovCrises_tgt + 
              icowsal + 
              trival + recmidwt + recfatwt + recnowt + recyeswt + 
              lcaprat + ldefense,
            data = icow_part_cyr_ct, subset = icow_part_cyr_ct$sub == 1 & icow_part_cyr_ct$year >= 1870,
            brglm = T, var = F, nboot = 30);summary(m)
m <- tvcure(Surv(y0, year, agreeiss) ~ lterugtdepmax + lag_ln_depdymax + lag_ln_deptotmax + 
              lpol1 + lpol2 + demdy +  
              icowsal + 
              recmidwt + recfatwt + recnowt + recyeswt + 
              lcaprat + ldefense, 
            cureform = ~ lterugtdepmax + lag_ln_depdymax + lag_ln_deptotmax + 
              lag_ln_gdp_chal + lag_ln_gdp_tgt + 
              W_chal * W_tgt + GovCrises_chal + GovCrises_tgt + 
              icowsal + 
              trival + recmidwt + recfatwt + recnowt + recyeswt + 
              lcaprat + ldefense,
            data = icow_part_cyr_ct, subset = icow_part_cyr_ct$sub == 1 & icow_part_cyr_ct$year >= 1870,
            brglm = T, var = F, nboot = 30);summary(m)

m <- tvcure(Surv(y0, year, agreeiss) ~ lterugtdepmax + lag_ln_depdymax + lag_ln_deptotmax + 
              polmin + polmax + demdy + autdy + 
              icowsal + 
              recmidwt + recfatwt + recnowt + recyeswt + 
              lcaprat + ldefense, 
            cureform = ~ lterugtdepmax + lag_ln_depdymax + lag_ln_deptotmax + 
              polmin + polmax + demdy + autdy + #GovCrises_1 + GovCrises_2 + 
              icowsal + 
              trival + recmidwt + recfatwt + recnowt + recyeswt + 
              lcaprat + ldefense,
            data = icow_part_cyr, subset = (icow_part_cyr$sub == 1 & icow_part_cyr$year >= 1870),
            brglm = T, var = T, nboot = 30);summary(m)

teratt <- tvcure(Surv(y0, year, agreeiss) ~ lterugtdepmax + lag_ln_depdymax + lag_ln_deptotmax + 
                   polmin + polmax + demdy + autdy + 
                   icowsal + 
                   recmidwt + recfatwt + recnowt + recyeswt + 
                   lcaprat + ldefense, 
                 cureform = ~ lterugtdepmax + lag_ln_depdymax + lag_ln_deptotmax + 
                   polmin + polmax + demdy + autdy + #GovCrises_1 + GovCrises_2 + 
                   icowsal + 
                   trival + recmidwt + recfatwt + recnowt + recyeswt + 
                   lcaprat + ldefense,
                 data = icow_part_cyr, subset = (icow_part_cyr$sub == 1 & icow_part_cyr$year >= 1870),
                 brglm = T, var = T, nboot = 30);summary(teratt)

cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
teragr <- tvcure(Surv(y0, year, agree) ~ lterugtdepmax + lag_ln_depdymax + lag_ln_deptotmax + 
              #polmin + polmax + demdy + autdy + 
              W1 * W2 + 
              icowsal + 
              recmidwt + recfatwt + recnowt + recyeswt + 
              lcaprat + ldefense, 
            cureform = ~ lterugtdepmax + lag_ln_depdymax + lag_ln_deptotmax + 
              #polmin + polmax + demdy + autdy + #GovCrises_1 + GovCrises_2 + 
              W1 * W2 + 
              icowsal + 
              trival + recmidwt + recfatwt + recnowt + recyeswt + 
              lcaprat + ldefense,
            data = icow_part_cyr, subset = (icow_part_cyr$sub == 1 & icow_part_cyr$year >= 1870),
            brglm = T, var = T, nboot = 30);summary(teragr)
rivatt <- tvcure(Surv(y0, year, agreeiss) ~ lrivugtdepmax + lag_ln_depdymax + lag_ln_deptotmax + 
                   polmin + polmax + demdy + autdy + 
                   icowsal + 
                   recmidwt + recfatwt + recnowt + recyeswt + 
                   lcaprat + ldefense, 
                 cureform = ~ lrivugtdepmax + lag_ln_depdymax + lag_ln_deptotmax + 
                   polmin + polmax + demdy + autdy + #GovCrises_1 + GovCrises_2 + 
                   icowsal + 
                   trival + recmidwt + recfatwt + recnowt + recyeswt + 
                   lcaprat + ldefense,
                 data = icow_part_cyr, subset = (icow_part_cyr$subr == 1 & icow_part_cyr$year >= 1900),
                 brglm = T, var = T, nboot = 30);summary(rivagr)
rivagr <- tvcure(Surv(y0, year, agreeiss) ~ lrivugtdepmax + lag_ln_depdymax + lag_ln_deptotmax + 
                   polmin + polmax + demdy + autdy + 
                   icowsal + 
                   recmidwt + recfatwt + recnowt + recyeswt + 
                   lcaprat + ldefense, 
                 cureform = ~ lrivugtdepmax + lag_ln_depdymax + lag_ln_deptotmax + 
                   polmin + polmax + demdy + autdy + #GovCrises_1 + GovCrises_2 + 
                   icowsal + 
                   trival + recmidwt + recfatwt + recnowt + recyeswt + 
                   lcaprat + ldefense,
                 data = icow_part_cyr, subset = (icow_part_cyr$subr == 1 & icow_part_cyr$year >= 1900),
                 brglm = T, var = T, nboot = 30);summary(rivagr)
maratt <- tvcure(Surv(y0, year, agreeiss) ~ lmarugtdepmax + lag_ln_depdymax + lag_ln_deptotmax + 
                   polmin + polmax + demdy + autdy + 
                   icowsal + 
                   recmidwt + recfatwt + recnowt + recyeswt + 
                   lcaprat + ldefense, 
                 cureform = ~ lmarugtdepmax + lag_ln_depdymax + lag_ln_deptotmax + 
                   polmin + polmax + demdy + autdy + #GovCrises_1 + GovCrises_2 + 
                   icowsal + 
                   trival + recmidwt + recfatwt + recnowt + recyeswt + 
                   lcaprat + ldefense,
                 data = icow_part_cyr, subset = (icow_part_cyr$sub == 1 & icow_part_cyr$year >= 1900),
                 brglm = T, var = T, nboot = 30);summary(maragr)
maragr <- tvcure(Surv(y0, year, agreeiss) ~ lmarugtdepmax + lag_ln_depdymax + lag_ln_deptotmax + 
                   polmin + polmax + demdy + autdy + 
                   icowsal + 
                   recmidwt + recfatwt + recnowt + recyeswt + 
                   lcaprat + ldefense, 
                 cureform = ~ lmarugtdepmax + lag_ln_depdymax + lag_ln_deptotmax + 
                   polmin + polmax + demdy + autdy + #GovCrises_1 + GovCrises_2 + 
                   icowsal + 
                   trival + recmidwt + recfatwt + recnowt + recyeswt + 
                   lcaprat + ldefense,
                 data = icow_part_cyr, subset = (icow_part_cyr$sub == 1 & icow_part_cyr$year >= 1900),
                 brglm = T, var = T, nboot = 30);summary(maragr)

# peq <- formula(
#   
# )
# heq <- formula(as.character(~ lag_ln_depdymax + lag_ln_deptotmax + # Trade vars
#                               icowsal + # Claim salience
#                               #filler + # domestic leader support
#                               polmin + polmax + demdy + autdy + # domestic institutions
#                               trival + recmidwt + recfatwt + recnowt + recyeswt + # Conflict and claim management vars
#                               lcaprat + ldefense # Additional controls
#                             )) 
# teragr <-  coxph(update.formula(heq, Surv(y0, year, agreeiss) ~ lterugtdepmax + .), data = icow_part_cyr)
# rivagr <-  coxph(update.formula(heq, Surv(y0, year, agreeiss) ~ lrivugtdepmax + .), data = icow_part_cyr)
# maragr <-  coxph(update.formula(heq, Surv(y0, year, agreeiss) ~ lmarugtdepmax + .), data = icow_part_cyr)
# 
# teragr <-  coxph(update.formula(heq, Surv(y0, year, attanyp) ~ lterugtdepmax + .), data = icow_part_cyr)
# rivagr <-  coxph(update.formula(heq, Surv(y0, year, attanyp) ~ lrivugtdepmax + .), data = icow_part_cyr)
# maragr <-  coxph(update.formula(heq, Surv(y0, year, attanyp) ~ lmarugtdepmax + .), data = icow_part_cyr)

testpred1a <- prediction2(m, "lterugtdepmax", c(-.35, .85), "spop", CI = T, nsims = 100); testpred1a
testpred1a <- prediction2(m, "lterugtdepmax", c(-.35, .85), "suncure", CI = T, nsims = 100); testpred1a
testpred1a <- prediction2(m, "lterugtdepmax", c(-.3, .7), "uncureprob", CI = T, nsims = 100); testpred1a






m <- tvcure(Surv(y0, year, agreeiss) ~ lrivugtdepmax + lag_ln_trade + 
              + lag_ln_gdp_chal + lag_ln_gdp_tgt + 
              lpol_chal * lpol_tgt + lcaprat + contdir,
            cureform = ~ lrivugtdepmax + lag_ln_trade + lag_ln_deptot_chal + lag_ln_deptot_tgt + 
              + lag_ln_gdp_chal + lag_ln_gdp_tgt + lag_ln_gdpcap_chal + lag_ln_gdpcap_tgt + 
              lpol_chal * lpol_tgt + lcaprat + ldefense,
            data = icow_part_cyr_ct, var = T, brglm = T, subset = icow_part_cyr_ct$lrivugtdepmax > 0)
lag_ln_trade + lag_ln_deptot_chal + lag_ln_deptot_tgt + 
  + lag_ln_gdp_chal + lag_ln_gdp_tgt + lag_ln_gdpcap_chal + lag_ln_gdpcap_tgt + 
  lpol_chal * lpol_tgt + lcaprat
icowsal
lag_ln_deptotmax +                  + 
recnowt + recyeswt + recmidwt

m2 <- tvcure(Surv(y0, year, agreeiss) ~ lag_ln_gdp_chal + lag_ln_gdp_tgt + 
              lpol_chal * lpol_tgt + lcaprat + contdir,
            cureform = ~ lrivugtdepmax + lag_ln_trade + lag_ln_deptot_chal + lag_ln_deptot_tgt + 
              + lag_ln_gdp_chal + lag_ln_gdp_tgt + lag_ln_gdpcap_chal + lag_ln_gdpcap_tgt + 
              lpol_chal * lpol_tgt,
            data = icow_part_cyr_ct, var = T, brglm = T, subset = icow_part_cyr_ct$lrivugtdepmax > 0)

m <- tvcure(Surv(y0, year, attanyp) ~ lterugtdepmax + lag_ln_trade + 
              lpol_chal * lpol_tgt + lcaprat + contdir,
            cureform = ~ lterugtdepmax + lag_ln_trade + 
              + lag_ln_gdp_chal + lag_ln_gdp_tgt + lag_ln_gdpcap_chal + lag_ln_gdpcap_tgt + 
              lpol_chal * lpol_tgt + lcaprat + ldefense, data = icow_part_cyr_ct, var = T, brglm = T)


att_dy <- brglm(attanyp ~ lterugtdepmax + lag_ln_trade + lag_ln_deptotmax + 
                  lag_gdp_chal + lag_gdp_tgt + lag_gdpcap_chal + lag_gdpcap_tgt + 
                  lpol_chal  * lpol_tgt +
                  icowsal + 
                  recnowt + recyeswt + recmidwt +
                  lcaprat + ldefense + contdir + 
