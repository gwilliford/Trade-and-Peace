setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis")
library(tvcure)
options(scipen = 999)
# a <- tmerge(icow_part_ct, icow_part_ct, claimdy, clterm = event(agreeiss))

icow_part_ct$ttrademax = rowMaxs(cbind(icow_part_ct$agg_1001, icow_part_ct$agg_1002))
cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
anyterm <- tvcure(formula  = Surv(clstart, clstop, clterm) ~ lag_depdymin_100 + 
                    icowsal + riveriss + mariss + 
                    lcaprat + ldefense + contdir,
                  cureform = ~ lag_depdymin + 
                    icowsal + riveriss + mariss + 
                    samereg + 
                    lcaprat + ldefense + contdir + lnccdist, 
                  data = icow_part_ct,
                  brglm = T, var = T, nboot = 30);summary(anyterm)

anyterm <- coxph(Surv(clstart, clstop, clterm) ~ lag_depdymax_100 + lag_deptotmax_100 + 
  icowsal + riveriss + mariss + 
  samereg + 
  lcaprat + ldefense + contdir + lnccdist, data = icow_part_ct);summary(anyterm)
anyterm <- coxph(Surv(agstart, agstop, agreeiss) ~ lag_depdymax_100 + lag_deptotmax_100 + 
                   icowsal + riveriss + mariss + 
                   samereg + 
                   lcaprat + ldefense + contdir + lnccdist, data = icow_part_ct);summary(anyterm)

a <- formula(Surv(midstart, midstop, midissyr) ~ lag_depdymax_100 + lag_deptotmax_100 + 
                    icowsal + riveriss + mariss + 
                    samereg + 
                    lcaprat + ldefense + contdir + lnccdist)
b <- na.omit(get_all_vars(a, icow_part_ct, na.action = "na.omit"))

anyterm <- coxphf(Surv(midstart, midstop, midissyr) ~ lag_ln_depdy_chal + lag_ln_depdy_tgt + lag_deptotmax_100 + 
                   icowsal + riveriss + mariss + 
                   samereg + 
                   lcaprat + ldefense + contdir + lnccdist, data = icow_part_c);summary(anyterm)

# anyterm <- brglm::brglm(agreeiss ~ lag_ln_depdy100_chal + lag_ln_depdy100_tgt + lag_ln_deptot100__chal + lag_deptot100__tgt + lag_ln_gdpcap_chal + lag_ln_gdpcap_tgt + 
#                           icowsal + riveriss + mariss + 
#                           samereg + 
#                           lcaprat + ldefense + contdir + lnccdist + year + ysq + y3, data = icow_part_ct, family = binomial);summary(anyterm)
anyterm <- coxph(Surv(midstart, midstop, midissyr) ~ lag_ln_depdymax + lag_deptotmax_100 + lag_ln_gdpcap_max + 
                          icowsal + riveriss + mariss + 
                          samereg + 
                          recmidwt + recnowt + recyeswt + 
                          lcaprat + ldefense + contdir + lnccdist, 
                        data = icow_part_ct,
                 subset = icow_part_ct$year >= 1900 & icow_part_ct$sub == 1 & !is.na(icow_part_ct$midstart))
anyterm <- coxph(Surv(agstart, agstop, agreeiss) ~ lag_ln_depdymax + lag_deptotmax_100 + lag_ln_gdpcap_max + 
                   icowsal + riveriss + mariss + 
                   samereg + 
                   recmidwt + recnowt + recyeswt + 
                   lcaprat + ldefense + contdir + lnccdist, 
                 data = icow_part_ct,
                 subset = icow_part_ct$year >= 1900 & icow_part_ct$sub == 1); anyterm

icow_part_ct$midstop2 <- icow_part_ct$midstop^2 / 1000
icow_part_ct$midstop3 <- icow_part_ct$midstop^3 / 10000
anyterm <- glmer(midissyr ~ lanyugtdepmax + lag_ln_depdymax + lag_deptotmax_100 + lag_ln_gdpcap_max + 
                 icowsal + riveriss + mariss + 
                 samereg + 
                 recmidwt + recnowt + recyeswt + 
                 lcaprat + ldefense + contdir + 
                   midstop + midstop2 + midstop3 + 
                 (1 | dyad) + (1 | year),
               family = binomial,
               data = icow_part_ct, 
               subset = icow_part_ct$year >= 1900 & icow_part_ct$sub == 1 & !is.na(icow_part_ct$midstart), 
               control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                     optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))); summary(anyterm)

icow_part_ct$clstop2 <- icow_part_ct$clstop^2 / 1000
icow_part_ct$clstop3 <- icow_part_ct$clstop^3 / 10000
anyterm <- glmer(clterm ~ lanyugtdepmax + lag_ln_depdymax + lag_deptotmax_100 + lag_ln_gdpcap_max + 
                   icowsal + riveriss + mariss + 
                   samereg + 
                   recmidwt + recnowt + recyeswt + 
                   lcaprat + ldefense + contdir + clstop + clstop2 + clstop3 + 
                   (1 | dyad) + (1 | year),
                 family = binomial,
                 data = icow_part_ct, 
                 subset = icow_part_ct$year >= 1900 & icow_part_ct$sub == 1, 
                 control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                        optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))); summary(anyterm)

icow_part_ct$agstop2 <- icow_part_ct$agstop^2 / 1000
icow_part_ct$agstop3 <- icow_part_ct$agstop^3 / 10000

suba <- filter(icow_part_ct, year >= 1900 & sub == 1)
anyterm <- brm(agreeiss ~ lanyugtdepmax + lag_ln_depdymax + lag_deptotmax_100 + lag_ln_gdpcap_max + 
                   icowsal + riveriss + mariss + 
                   samereg + 
                   recmidwt + recnowt + recyeswt + 
                   lcaprat + ldefense + contdir + agstop + agstop2 + agstop3 + 
                   (1 | mm(ccode1, ccode2)) + (1 | year),
                 family = bernoulli,
                 data = suba, 
                 cores = 4); summary(anyterm)





anyterm <- brglm::brglm(agreeiss ~ lanyugtdepmax + 
                          icowsal + riveriss + mariss + 
                          samereg + 
                          lcaprat + ldefense + contdir + lnccdist + year + ysq + y3, data = icow_part_ct, family = binomial);summary(anyterm)




anyterm <- tvcure(formula  = Surv(clstart, clstop, clterm) ~ icowsal + riveriss + mariss +
                    trival + lcaprat, 
                  cureform = ~ lanyugtdepmax + 
                    icowsal + riveriss + mariss + 
                    samereg + 
                    lcaprat + ldefense + contdir, 
                  data = icow_part_ct,
                  brglm = T, var = T, nboot = 30);summary(anyterm)

anyag <- tvcure(formula  = Surv(agstart, agstop, agreeiss) ~ lag_ln_trade + lag_ln_depdymax + lag_ln_deptotmax + 
                    icowsal + riveriss + mariss +
                    trival + lcaprat, 
                  cureform = ~ lag_ln_trade + lag_ln_depdymax + lag_ln_deptotmax + 
                    icowsal + riveriss + mariss + 
                    polmin + polmax + samereg + 
                    trival + recmidwt + recnowt + recyeswt +
                    lcaprat + ldefense + contdir, 
                  data = icow_part_ct,
                  brglm = T, var = T, nboot = 30);summary(anyag)
anymid <- tvcure(formula  = Surv(midstart, midstop, midfail) ~ lanyugtdepmax + lag_ln_trade + lag_ln_depdymax + lag_ln_deptotmax + 
                  icowsal + riveriss + mariss +
                  trival + recmidwt + recnowt + recyeswt + 
                  lcaprat + ldefense + contdir, 
                cureform = ~ lag_ln_trade + lag_ln_depdymax + lag_ln_deptotmax + 
                  icowsal + riveriss + mariss + 
                  polmin + polmax + samereg + 
                  trival + recmidwt + recnowt + recyeswt +
                  lcaprat + ldefense + contdir, 
                data = icow_part_ct,
                brglm = T, var = T, nboot = 30);summary(anymid)


anymid <- tvcure(formula  = Surv(midstart, midstop, midfail) ~ lag_ln_depdy_chal + lag_ln_depdy_tgt + 
                   lag_ln_deptot_chal + lag_ln_deptot_tgt +
                   lag_ln_gdpcap_chal + lag_ln_gdpcap_tgt +
                   icowsal + riveriss + mariss +
                   trival + recmidwt + recnowt + recyeswt + 
                   lcaprat + ldefense + contdir, 
                 cureform = ~ lag_ln_depdy_chal + lag_ln_depdy_tgt + 
                   lag_ln_deptot_chal + lag_ln_deptot_tgt +
                   lag_ln_gdpcap_chal + lag_ln_gdpcap_tgt +
                   icowsal + riveriss + mariss + 
                   polmin + polmax + samereg + 
                   trival + recmidwt + recnowt + recyeswt +
                   lcaprat + ldefense + contdir, 
                 data = icow_part_ct,
                 brglm = T, var = T, nboot = 30);summary(anymid)

depdymin <- min(icow_part_ct$lag_ln_depdymax, na.rm = T)
depdymax <- max(icow_part_ct$lag_ln_depdymax, na.rm = T)
deptotmin <- min(icow_part_ct$lag_ln_deptotmax, na.rm = T)
deptotmax <- max(icow_part_ct$lag_ln_deptotmax, na.rm = T)

testpred1a <- prediction2(anyagr, "lag_ln_depdymax", c(depdymin, depdymax),
                          "uncureprob", CI = T, nsims = 1000); testpred1a
testpred1a <- prediction2(anyagr, "lag_ln_deptotmax", c(deptotmin, deptotmax),
                          "uncureprob", CI = T, nsims = 1000); testpred1a

anyagr <- tvcure(formula  = Surv(agstart, agstop, agreeiss) ~ lanyugtdepmax + icowsal + trival + lcaprat, 
                 cureform = ~ lanyugtdepmax + lag_ln_depdymax +
                   icowsal + 
                   politymin + politymax + samereg + 
                   trival + recmidwt + recnowt + recyeswt +
                   lcaprat + ldefense + contdir + 
                   riveriss + mariss,
                 data = icow_part_ct,
                 brglm = T, var = T, nboot = 30);summary(anyagr)
anymid <- tvcure(formula  = Surv(midstart, midstop, midfail) ~ lanyugtdepmax + lag_ln_depdymax + 
                   icowsal + riveriss + mariss + 
                   trival + lcaprat, 
                 cureform = ~ lanyugtdepmax + lag_ln_depdymax +
                   icowsal + 
                   polity_chal + polity_tgt + samereg + 
                   trival + recmidwt + recnowt + recyeswt + 
                   lcaprat + ldefense + contdir + 
                   riveriss + mariss,
                 data = icow_part_ct,
                 brglm = T, var = T, nboot = 30);summary(anyagr)
anymid <- tvcure(formula  = Surv(clstart, clstop, midfail) ~ lanyugtdepmax,
                 cureform = ~ lanyugtdepmax + lag_ln_depdymax * icowsal +
                   icowsal + riveriss + mariss + 
                   samereg + 
                   lcaprat + ldefense + contdir,
                 data = icow_part_ct,
                 brglm = T, var = T, nboot = 30);summary(anymid)
anymid <- tvcure(formula  = Surv(clstart, clstop, midfail) ~ lanyugtdepmax +
                   icowsal + riveriss + mariss + 
                   samereg +
                   lcaprat + ldefense + contdir,
                 cureform = ~ lanyugtdepmax  * icowsal  + lag_ln_depdymax_100 +
                   icowsal + riveriss + mariss + 
                   samereg + 
                   lcaprat + ldefense + contdir,
                 data = icow_part_ct,
                 subset = icow_part_ct$year >= 1900 & icow_part_cyr$sub == 1,
                 brglm = T, var = T, nboot = 30);summary(anymid)




anyagr <- tvcure(formula  = Surv(agstart, agstop, agreeiss) ~ lterugtdepmax + icowsal + trival + lcaprat, 
                 cureform = ~ lanyugtdepmax + lag_ln_depdymax +
                   icowsal + 
                   polity_chal + polity_tgt + samereg + 
                   trival + 
                   lcaprat + ldefense + contdir,
                 data = icow_part_ct, subset = dat$sub == 1,
                 brglm = T, var = T, nboot = 30);summary(anyagr)

teragfail <- tvcure(formula  = Surv(clstart, clstop, clterm) ~ lag_ln_trade + lag_ln_depdy_chal + lag_ln_depdy_tgt + lag_ln_depoth_chal + lag_ln_depoth_tgt + 
                      icowsal + trival + lcaprat, 
                    cureform = ~ lag_ln_trade + lag_ln_depdy_chal + lag_ln_depdy_tgt + lag_ln_depoth_chal + lag_ln_depoth_tgt +
                      icowsal + 
                      polity_chal + polity_tgt + samereg + 
                      trival + recmidwt + recnowt + recyeswt + 
                      lcaprat + ldefense + contdir,
                    data = icow_part_ct, subset = icow_part_ct$terriss == 1,
                    brglm = T, var = T, nboot = 200);summary(teragfail)
teragfail <- coxph(formula  = Surv(clstart, clstop, clterm) ~ lag_ln_trade + lag_ln_depdy_chal + lag_ln_depdy_tgt + lag_ln_depoth_chal + lag_ln_depoth_tgt +
                      icowsal + 
                      polity_chal + polity_tgt + samereg + 
                      trival + 
                      lcaprat + ldefense + contdir,
                    data = icow_part_ct, subset = icow_part_ct$terriss == 1);summary(teragfail)




-----------------------------------------------------------
  cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
anyterm <- tvcure(formula  = Surv(clstart, clstop, clterm) ~ lag_ln_depdy100__chal + lag_ln_depdy100__tgt + 
                    lag_ln_deptot_chal + lag_ln_deptot_tgt + 
                    icowsal + riveriss + mariss + 
                    samereg + lcaprat + ldefense + contdir,
                  cureform = ~ lag_ln_trade + 
                    lag_ln_depdy100__chal + lag_ln_depdy100__tgt + 
                    lag_ln_deptot100__chal + lag_ln_deptot100__tgt + 
                    icowsal + riveriss + mariss + 
                    samereg + 
                    lcaprat + ldefense + contdir + lnccdist, 
                  data = icow_part_ct,
                  brglm = T, var = T, nboot = 30);summary(anyterm)

anyterm <- tvcure(formula  = Surv(clstart, clstop, clterm) ~ icowsal + samereg,
                  cureform = ~ lag_ln_depdy_chal + lag_ln_depdy_tgt + lag_ln_deptot_chal + lag_ln_deptot_tgt + 
                    icowsal + riveriss + mariss + 
                    samereg + 
                    lcaprat + ldefense + contdir + lnccdist, 
                  data = icow_part_ct,
                  brglm = F, var = T, nboot = 30);summary(anyterm)

anyterm <- tvcure(formula  = Surv(agstart, agstop, agreeiss) ~ lag_ln_depdy_chal + lag_ln_depdy_tgt + 
                    lag_ln_deptot_chal + lag_ln_deptot_tgt + 
                    icowsal + samereg + lcaprat + ldefense + contdir,
                  cureform = ~ lag_ln_depdy_chal + lag_ln_depdy_tgt + 
                    lag_ln_deptot_chal + lag_ln_deptot_tgt + 
                    icowsal + riveriss + mariss + 
                    samereg + 
                    lcaprat + ldefense + contdir + lnccdist, 
                  data = icow_part_ct,
                  brglm = T, var = T, nboot = 30);summary(anyterm)

anyterm <- tvcure(formula  = Surv(midstart, midstop, midissyr) ~ lag_ln_depdy_chal + lag_ln_depdy_tgt + 
                    lag_ln_deptot_chal + lag_ln_deptot_tgt + 
                    icowsal + samereg + lcaprat + ldefense + contdir,
                  cureform = ~ lag_ln_depdy_chal + lag_ln_depdy_tgt + 
                    lag_ln_deptot_chal + lag_ln_deptot_tgt + 
                    icowsal + riveriss + mariss + 
                    samereg + 
                    lcaprat + ldefense + contdir + lnccdist, 
                  data = icow_part_ct,
                  brglm = T, var = T, nboot = 30);summary(anyterm)




anyterm <- tvcure(formula  = Surv(midstart, midstop, midissyr) ~ icowsal + samereg,
                  cureform = ~ lag_ln_depdymax + lag_ln_depdymin + lag_ln_deptotmax + lag_ln_deptotmin + 
                    icowsal + riveriss + mariss + 
                    samereg + 
                    lcaprat + ldefense + contdir + lnccdist, 
                  data = icow_part_ct,
                  brglm = F, var = T, nboot = 30);summary(anyterm)
anyterm <- tvcure(formula  = Surv(midstart, midstop, midissyr) ~ icowsal + samereg,
                  cureform = ~ lag_ln_depdy_chal + lag_ln_depdy_tgt + lag_ln_deptot_chal + lag_ln_deptot_tgt + 
                    icowsal + riveriss + mariss + 
                    samereg + 
                    lcaprat + ldefense + contdir + lnccdist, 
                  data = icow_part_ct,
                  brglm = F, var = T, nboot = 30);summary(anyterm)

