# Code conflict after agreements


Start with agreement - time until conflict


# Start var: Year in which agreement occurs
  # agree year = year if agreeiss == 1
    # = LOCF year
  # stop = year - agree year creates time since last agreement
  # start = stop - 1
  # fail = midissyr
# Sample
  # Drop if agree year = 0 or missing 

icow_part_cyr$agyear = ifelse(icow_part_cyr$agreeiss == 1, icow_part_cyr$year, NA)
icow_part_cyr = ungroup(icow_part_cyr %>% group_by(dyad) %>% mutate(
  yrlastag = LOCF(agyear)
))
icow_part_cyr$agstop = icow_part_cyr$year - icow_part_cyr$yrlastag + 1
icow_part_cyr$agstart = icow_part_cyr$agstop - 1

subtest <- icow_part_cyr[!is.na(icow_part_cyr$yrlastag), ]

coxph(Surv(agstart, agstop, midissyr) ~ icowsal + demdy + lterugtdepmax, data = subtest, subset = terriss == 1)

coxph(Surv(clstart, clstop, clterm) ~ icowsal + demdy + lterugtdepmax, data = subtest, subset = terriss == 1)

heq <- ~ lag_ln_depdymax + lag_ln_deptotmax + # Trade variables
  #lag_ln_gdpcap1 + lag_ln_gdpcap2 + # GDP variables
  icowsal + # Issue Salience
  # Previous Conflict Management
  lcaprat + ldefense
peq <- as.formula(" ~ lag_ln_depdymax + lag_ln_deptotmax + lag_ln_gdpcap1 + lag_ln_gdpcap2 +                     icowsal +                     trival + recmidwt + recfatwt + recnowt + recyeswt     ")


#### TERRITORY
cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
teragfail   <- tvcure(formula  = update.formula(heq, Surv(agstart, agstop, midissyr) ~ lterugtdepmax + .),
                   cureform = update.formula(peq, ~ lterugtdepmax + .),
                   data = icow_part_cyr, subset = icow_part_cyr$terriss == 1,
                   brglm = F, var = T, nboot = 30);summary(teragfail)   #### SOMething isn't happening with these bootstrap replications
rivagfrail   <- tvcure(formula  = update.formula(heq, Surv(agstart, agstop, midissyr) ~ lrivugtdepmax + .),
                   cureform = update.formula(peq, ~ lrivugtdepmax + .),
                   data = icow_part_cyr, subset = icow_part_cyr$riveriss == 1,
                   brglm = F, var = T, nboot = 30);summary(rivagfail)   #### SOMething isn't happening with these bootstrap replications
maragfail  <- tvcure(formula  = update.formula(heq, Surv(agstart, agstop, midissyr) ~ lcaprat + recmidwt),
                   cureform = update.formula(peq, ~ lag_ln_deptotmax + lmarugtdepmax),
                   data = icow_part_cyr, subset = icow_part_cyr$mariss == 1,
                   brglm = F, var = T, nboot = 30);summary(maragfail)   #### SOMething isn't happening with these bootstrap replications

marugtmin <- min(icow_part_cyr$lmarugtdepmax, na.rm = T)
marugtmax <- max(icow_part_cyr$lmarugtdepmax, na.rm = T)

testpred2c <- prediction2(teragfail, "recnowt", c(0, 10), 
                          "uncureprob", CI = T, nsims = 1000); testpred2c

maragfail  <- tvcure(formula  = update.formula(heq, Surv(agstart, agstop, midissyr) ~ lcaprat + recmidwt),
                     cureform = update.formula(peq, ~ demdy + lcaprat),
                     data = icow_part_cyr, subset = icow_part_cyr$mariss == 1,
                     brglm = T, var = T, nboot = 30, link = "probit");summary(maragfail)   #### SOMething isn't happening with these bootstrap replications

testpred3 <- prediction2(maragfail, "lcaprat", c(.5, 1), 
                          "uncureprob", CI = T, nsims = 1000); testpred3
