peq <- as.formula(~ lag_ln_depdymax + lag_ln_deptotmax +                  # Trade
                    icowsal +                                             # Salience
                    polity_chal + polity_tgt + samereg +                  # Institutions
                    #GovCrises1 + GovCrises2 +                           # Domestic Support
                    trival + recmidwt + recnowt + recyeswt +   # Historical interactions
                    lcaprat + ldefense + contdir)                         # Dyadic controls
heq <- as.formula(~ lag_ln_depdymax + lag_ln_deptotmax + # GDP variables
                    icowsal +                                             # Issue Salience
                    #polity1 + polity2 + samereg + # Domestic institutions
                    #GovCrises1 + GovCrises2 + # Leader Support
                    trival + recmidwt + recnowt + recyeswt# Previous Conflict Management
                    #lcaprat + ldefense)
)


cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
#### TERRITORY
teratt   <- tvcure(formula  = update.formula(heq, Surv(spstart, spstop, attanyp)   ~ .),
                   cureform = update.formula(peq, ~  .),
                   data = icow_part_ct, subset = icow_part_ct$terriss == 1,
                   brglm = T, var = T, nboot = 30);summary(teratt)
teragany <- tvcure(formula  = update.formula(heq, Surv(spstart, spstop, agree)     ~ .),
                   cureform = update.formula(peq, ~ .),
                   data = icow_part_ct, subset = icow_part_ct$terriss == 1,
                   brglm = T, var = T, nboot = 30);summary(teragany)
teragiss <- tvcure(formula  = update.formula(heq, Surv(spstart, spstop, agreeiss) ~ .),
                   cureform = update.formula(peq,                                  ~ .),
                   data = icow_part_ct, subset = icow_part_ct$terriss == 1,
                   brglm = T, var = T, nboot = 100);summary(teragiss)
tercldur <- tvcure(formula   = update.formula(heq, Surv(clstart, clstop, clterm)   ~ .),
                   cureform = update.formula(peq, ~ .),
                   data = icow_part_ct, subset = icow_part_ct$terriss == 1,
                   brglm = T, var = F, nboot = 30);summary(tercldur)
tercldur2 <- tvcure(formula   = update.formula(heq, Surv(clstart, clstop, clterm)  ~ lterugtdepmax + .),
                   cureform = update.formula(peq, ~ lterugtdepmax + .),
                   data = icow_part_ct, subset = icow_part_ct$terriss == 1,
                   brglm = T, var = T, nboot = 30);summary(tercldur)

teragfail <- tvcure(formula  = update.formula(heq, Surv(agstart, agstop, midissyr) ~ .),
                    cureform = update.formula(peq, ~ lterugtdepmax + .),
                    data = icow_part_ct, subset = icow_part_ct$terriss == 1,
                    brglm = T, var = F, nboot = 30);summary(teragfail)


anyagfail <- tvcure(formula  = update.formula(heq, Surv(agstart, agstop, midissyr) ~ .),
                    cureform = update.formula(peq, ~ lterugtdepmax + .),
                    data = icow_part_ct, subset = icow_part_ct$terriss == 1,
                    brglm = T, var = F, nboot = 30);summary(teragfail)


ter <- coxph(update.formula(peq, Surv(agstart, agstop, midissyr) ~ lterugtdepmax + .), 
             data = icow_part_ct, subset = icow_part_ct$terriss == 1); summary(ter)
ter <- glm(update.formula(peq, Surv(agstart, agstop, midissyr) ~ lterugtdepmax + .), 
             data = icow_part_ct, subset = icow_part_ct$terriss == 1, 
           family = binomial); summary(ter)

# terugtmin <- min(icow_part_ct$lterugtdepmax, na.rm = T)
# terugtmax <- max(icow_part_ct$lterugtdepmax, na.rm = T)
# testpred1a <- prediction2(teragiss, "lterugtdepmax", c(terugtmin, terugtmax), 
#                           "uncureprob", CI = T, nsims = 1000); testpred1a
# testpred1a <- prediction2(teragiss, "lterugtdepmax", c(terugtmin, terugtmax), 
#                           "spop", CI = T, nsims = 1000); testpred1a


#### RIVERS
cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
rivatt   <- tvcure(formula  = update.formula(heq, Surv(spstart, spstop, attanyp)  ~ lrivugtdepmax + .),
                   cureform = update.formula(peq,                          ~ lrivugtdepmax + .),
                   data = icow_part_ct, subset = icow_part_ct$riveriss == 1,
                   brglm = T, var = T, nboot = 30);summary(rivatt)
rivagany <- tvcure(formula  = update.formula(heq, Surv(spstart, spstop, agree)    ~ lrivugtdepmax + .),
                   cureform = update.formula(peq, ~ lrivugtdepmax + .),
                   data = icow_part_ct, subset = icow_part_ct$riveriss == 1,
                   brglm = T, var = T, nboot = 30);summary(rivagany)
rivagiss <- tvcure(formula =  update.formula(heq, Surv(spstart, spstop, agreeiss) ~ lrivugtdepmax + .),
                   cureform = update.formula(peq, ~ lrivugtdepmax + .),
                   data = icow_part_ct, subset = icow_part_ct$riveriss == 1,
                   brglm = T, var = T, nboot = 30);summary(rivagiss)

#### MARITIME
maratt   <- tvcure(formula  = update.formula(Surv(spstart, spstop, attanyp) ~ lmarugtdepmax + .), 
                   cureform = update.formula(peq, ~ lmarugtdepmax + .),
                   data = icow_part_ct, subset = icow_part_ct$mariss == 1,
                   brglm = T, var = T, nboot = 30);summary(maratt)
maragany <- tvcure(formula  = update.formula(heq, Surv(spstart, spstop, agree) ~ lmarugtdepmax + .),
                   cureform = update.formula(peq,                              ~ lmarugtdepmax + .),
                   data = icow_part_ct, subset = icow_part_ct$mariss == 1,
                   brglm = T, var = T, nboot = 30);summary(maragany)
maragiss <- tvcure(formula  = update.formula(heq, Surv(spstart, spstop, agreeiss) ~ lmarugtdepmax + .),
                   cureform = update.formula(peq,                                 ~ lmarugtdepmax + .),
                   data = icow_part_ct, subset = icow_part_ct$mariss == 1,
                   brglm = T, var = T, nboot = 30);summary(maragiss)

cor(teragiss$X)
cor(teragiss$Z)