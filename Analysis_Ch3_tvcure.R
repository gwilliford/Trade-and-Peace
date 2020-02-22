heq <- ~ lag_ln_depdymax + lag_ln_deptotmax + # Trade variables
  #lag_ln_gdpcap1 + lag_ln_gdpcap2 + # GDP variables
  icowsal + # Issue Salience
  polmin + polmax + demdy + autdy + # Domestic institutions
  #GovCrises1 + GovCrises2 + # Leader Support
  trival + recmidwt + recfatwt + recnowt + recyeswt + # Previous Conflict Management
  lcaprat + ldefense
peq <- as.formula(" ~ lag_ln_depdymax + lag_ln_deptotmax + lag_ln_gdpcap1 + lag_ln_gdpcap2 +                     icowsal + 
                    polmin + polmax + demdy + autdy +                     GovCrises1 + GovCrises2 +                     trival + recmidwt + recfatwt + recnowt + recyeswt     ")

cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
#### TERRITORY
teratt   <- tvcure(formula  = update.formula(heq, Surv(start, stop, attanyp)  ~ lterugtdepmax + .),
                   cureform = update.formula(peq, ~ lterugtdepmax + .),
                   data = icow_part_cyr, subset = icow_part_cyr$terriss == 1,
                   brglm = T, var = T, nboot = 30);summary(teratt)
teragany <- tvcure(formula  = update.formula(heq, Surv(y0, year, agree)    ~ lterugtdepmax + .),
                   cureform = update.formula(peq, ~ lterugtdepmax + .),
                   data = icow_part_cyr, subset = icow_part_cyr$terriss == 1,
                   brglm = T, var = T, nboot = 30);summary(teragany)
teragiss <- tvcure(formula  = update.formula(heq, Surv(start, stop, agreeiss) ~ lterugtdepmax + .),
                   cureform = update.formula(peq, ~ lterugtdepmax + .),
                   data = icow_part_cyr, subset = icow_part_cyr$terriss == 1,
                   brglm = T, var = T, nboot = 30);summary(teragiss)
tercox <- coxph(formula  = Surv(start, stop, agreeiss) ~ lterugtdepmax,
                
                   data = icow_part_cyr, subset = icow_part_cyr$terriss == 1);summary(teragiss)

teragiss <- tvcure(formula  = Surv(start, stop, agreeiss) ~ lag_ln_deptotmax + recmidwt,
                   cureform = ~ lterugtdepmax + caprat + demdy,
                   data = icow_part_cyr, subset = icow_part_cyr$terriss == 1,
                   brglm = T, var = T, nboot = 30);summary(teragiss)

terugtmin <- min(icow_part_cyr$lterugtdepmax, na.rm = T)
terugtmax <- max(icow_part_cyr$lterugtdepmax, na.rm = T)
testpred1a <- prediction2(teragiss, "lterugtdepmax", c(terugtmin, terugtmax), 
                          "uncureprob", CI = T, nsims = 1000); testpred1a
testpred1a <- prediction2(teragiss, "lterugtdepmax", c(terugtmin, terugtmax), 
                          "spop", CI = T, nsims = 1000); testpred1a


#### RIVERS
rivatt   <- tvcure(formula  = update.formula(heq, Surv(y0, year, attanyp)  ~ lrivugtdepmax + .),
                   cureform = update.formula(peq, ~ lrivugtdepmax + .),
                   data = icow_part_cyr, subset = icow_part_cyr$riveriss == 1,
                   brglm = T, var = T, nboot = 30);summary(rivatt)
rivagany <- tvcure(formula  = update.formula(heq, Surv(y0, year, agree)    ~ lrivugtdepmax + .),
                   cureform = update.formula(peq, ~ lrivugtdepmax + .),
                   data = icow_part_cyr, subset = icow_part_cyr$riveriss == 1,
                   brglm = T, var = T, nboot = 30);summary(rivatt)
rivagiss <- tvcure(formula =  update.formula(heq, Surv(y0, year, agreeiss) ~ lrivugtdepmax + .),
                   cureform = update.formula(peq, ~ lrivugtdepmax + .),
                   data = icow_part_cyr, subset = icow_part_cyr$riveriss == 1,
                   brglm = T, var = T, nboot = 30);summary(rivatt)

#### MARITIME
maratt   <- tvcure(formula  = update.formula(Surv(y0, year, attanyp) ~ lmarugtdepmax + .), 
                   cureform = update.formula(peq, ~ lmarugtdepmax + .),
                   data = icow_part_cyr, subset = icow_part_cyr$mariss == 1,
                   brglm = T, var = T, nboot = 30);summary(maratt)
maragany <- tvcure(formula  = update.formula(Surv(y0, year, agree) ~ lmarugtdepmax + .),
                   cureform = update.formula(peq, ~ lmarugtdepmax + .),
                   data = icow_part_cyr, subset = icow_part_cyr$mariss == 1,
                   brglm = T, var = T, nboot = 30);summary(maragany)
maragiss <- tvcure(formula  = update.formula(Surv(y0, year, agreeiss) ~ lmarugtdepmax + .),
                   cureform = update.formula(peq, ~ lmarugtdepmax + .),
                   data = icow_part_cyr, subset = icow_part_cyr$mariss == 1,
                   brglm = T, var = T, nboot = 30);summary(maragiss)

cor(teragiss$X)
cor(teragiss$Z)