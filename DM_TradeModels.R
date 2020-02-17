setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis")
if(!exists("dat")) dat <- read_csv("./data/TradeInputs.csv")
# dsub <- dsub %>% filter(chal < 0, chal < 1)


termod <- lmer(ln_trade ~ ldyterrclaim + lag_ln_gdp1 + lag_ln_gdp2 + lag_ln_gdpcap1 + lag_ln_gdpcap2 + 
                 contdir + ldefense + lcaprat + lpol1 * lpol2 + year + y2 + (1 | dyad) + (1 | year),
               data = dat, 
               control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                     optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)));summary(termod)
# termod <- lmer(ln_trade ~ ldyterrclaim + lag_ln_gdp1 + lag_ln_gdp2 + lag_ln_gdpcap1 + lag_ln_gdpcap2 + 
#                  contdir + ldefense + lcaprat + lpol1 * lpol2 + year + y2 + (1 | dyad) + (1 | year),
#                data = dat, subset = dat$btclaim == 1,
#                control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
#                                      optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
rivmod <- lmer(ln_trade ~ lbrclaim + lag_ln_gdp1 + lag_ln_gdp2 + lag_ln_gdpcap1 + lag_ln_gdpcap2 +
                 contdir + ldefense + lcaprat + lpol1 * lpol2 + year + y2 + (1 | dyad) + (1 | year),
               data = dat, subset = dat$subr == 1,
               control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                     optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))); summary(rivmod)
marmod <- lmer(ln_trade ~ lbmclaim + lag_ln_gdp1 + lag_ln_gdp2 + lag_ln_gdpcap1 + lag_ln_gdpcap2 +
                 contdir + ldefense + lcaprat + lpol1 * lpol2 + year + y2 + (1 | dyad) + (1 | year),
               data = dat, subset = dat$sub == 1,
               control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                     optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))); summary(marmod)

# summary(tm1);r.squaredGLMM(tm1) #  .662/.93 lndymid


dsim <- dat
dsim$ldyterrclaim <- 0
dsim$lbtclaim <- 0
dsim$lbrclaim <- 0
dsim$lbmclaim <- 0
tsim <- predict(termod, dsim, allow.new.levels = T)
msim <- predict(marmod, dsim, allow.new.levels = T)
rsim <- predict(rivmod, dsim, allow.new.levels = T)
dat$tsim <- tsim
dat$msim <- msim
dat$rsim <- rsim

#dat$trsim1 <- predict(tm1, dsim1, allow.new.levels = T)
#dat$ugtpred <- trsim0 - trsim1
# dat$dyugt  <- dat$trsim0 - dat$ln_trade #doesn't look great
dat$terugt <- tsim
dat$rivugt <- rsim
dat$marugt <- msim

# Put in icow dm file
# dat$terugtdep1 <- dat$terugt / dat$ln_gdp1
# dat$terugtdep2 <- dat$terugt / dat$ln_gdp2
# dat$terugtdepmax <- rowMaxs(cbind(dat$terugtdep1, dat$terugtdep2))
# dat$rivugtdep1 <- dat$rivugt / dat$ln_gdp1
# dat$rivugtdep2 <- dat$rivugt / dat$ln_gdp2
# dat$rivugtdepmax <- rowMaxs(cbind(dat$rivugtdep1, dat$rivugtdep2))
# dat$marugtdep1 <- dat$marugt / dat$ln_gdp1
# dat$marugtdep2 <- dat$marugt / dat$ln_gdp2
# dat$marugtdepmax <- rowMaxs(cbind(dat$marugtdep1, dat$marugtdep2))
# 
# 
# dat <- dat %>% arrange(dyad, year) %>% mutate(
#   lterugt = lag(terugt),
#   lterugtdep1 = lag(terugtdep1),
#   lterugtdep2 = lag(terugtdep2),
#   lterugtdepmax = lag(terugtdepmax),
#   lrivugt = lag(rivugt),
#   lrivugtdep1 = lag(rivugtdep1),
#   lrivugtdep2 = lag(rivugtdep2),
#   lrivugtdepmax = lag(rivugtdepmax),
#   lmarugt = lag(marugt),
#   lmarugtdep1 = lag(marugtdep1),
#   lmarugtdep2 = lag(marugtdep2),
#   lmarugtdepmax = lag(marugtdepmax)
# )

save.image("./data/TradeOut.Rdata")
