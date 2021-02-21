setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis")
if(!exists("dat")) dat <- read_csv("./data/TradeInputs.csv")
# dsub <- dsub %>% filter(chal < 0, chal < 1)


# termod <- lmer(ln_trade_100 ~ ldyterrclaim + lag_ln_gdp1 + lag_ln_gdp2 + lag_ln_gdpcap1 + lag_ln_gdpcap2 + 
#                  contdir + ldefense + lcaprat + lpol1 * lpol2 + year + (1 | dyad) + (1 | year),
#                data = dat, 
#                control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
#                                      optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)));summary(termod)
# termod <- lmer(ln_trade_100 ~ ldyterrclaim + lag_ln_gdp1 + lag_ln_gdp2 +QA lag_ln_gdpcap1 + lag_ln_gdpcap2 + 
#                  contdir + ldefense + lcaprat + lpol1 * lpol2 + year + ysq + (1 | dyad) + (1 | year),
#                data = dat, subset = dat$sub == 1,
#                control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
#                                      optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)));summary(termod)
# 
# # termod <- lmer(ln_trade_100 ~ ldyterrclaim + lag_ln_gdp1 + lag_ln_gdp2 + lag_ln_gdpcap1 + lag_ln_gdpcap2 + 
# #                  contdir + ldefense + lcaprat + lpol1 * lpol2 + year + ysq + (1 | dyad) + (1 | year),
# #                data = dat, subset = dat$btclaim == 1,
# #                control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
# #                                      optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
# termod <- lmer(ln_trade_100 ~ ldyterrclaim + lag_ln_gdp_min + lag_ln_gdp_max + lag_ln_gdpcap_min + lag_ln_gdpcap_max +
#                  contdir + ldefense + lcaprat + polmin + polmax + samereg + year + ysq + (1 | dyad) + (1 | year),
#                data = dat,
#                control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
#                                      optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))); summary(termod)
# rivmod <- lmer(ln_trade_100 ~ lbrclaim + lag_ln_gdp_min + lag_ln_gdp_max + lag_ln_gdpcap_min + lag_ln_gdpcap_max +
#                  contdir + ldefense + lcaprat + samereg + year + ysq + (1 | dyad) + (1 | year),
#                data = dat, subset = dat$subr==1 & dat$year >= 1900,
#                control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
#                                      optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))); summary(rivmod)
# marmod <- lmer(ln_trade_100 ~ lbmclaim + lag_ln_gdp_min + lag_ln_gdp_max + lag_ln_gdpcap_min + lag_ln_gdpcap_max +
#                  contdir + ldefense + lcaprat + samereg + year + ysq + (1 | dyad) + (1 | year),
#                data = dat, subset = dat$sub & dat$year >= 1900,
#                control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
#                                      optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))); summary(marmod)
anymod <- lmer(ln_trade_100 ~ ltotclaim +
                 lag_ln_gdp_min + lag_ln_gdp_max + lag_ln_gdpcap_min + lag_ln_gdpcap_max +
                 lag_ln_depdymax + lag_ln_deptotmax + 
                 samereg + 
                 lnccdist + contdir + 
                 lcaprat + ldefense +
                 year + ysq +
                 (1 | dyad) + (1 | year),
               data = dat, subset = dat$sub & dat$year >= 1900,
               control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                     optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))); summary(anymod)
# #anymod <- lm(ln_trade_100 ~ lanyclaim + lag_ln_gdp_min + lag_ln_gdp_max + lag_ln_gdpcap_min + lag_ln_gdpcap_max +
#                  contdir + ldefense + lcaprat + polmin + polmax + samereg + year + ysq + (1 | dyad) + (1 | year),
#                data = dat, subset = dat$sub & dat$year >= 1900,
#                control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
#                                      optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))); summary(anymod)

# summary(tm1);r.squaredGLMM(tm1) #  .662/.93 lndymid


dsim <- dat
dsim$ldyterrclaim <- 0
dsim$lbtclaim <- 0
dsim$lbrclaim <- 0
dsim$lanyclaim <- 0
dsim$lbmclaim <- 0
dsim$lntclaim <- 0
dsim$lnrclaim <- 0
dsim$lnmclaim <- 0
dsim$ltotclaim <- 0

# tsim <- predict(termod, dsim, allow.new.levels = T)
# msim <- predict(marmod, dsim, allow.new.levels = T)
# rsim <- predict(rivmod, dsim, allow.new.levels = T)
asim <- predict(anymod, dsim, allow.new.levels = T)
# dat$tsim <- tsim
# dat$msim <- msim
# dat$rsim <- rsim
dat$asim <- asim


# dsim2 <- dsim
# dsim2$lntclaim <- max(dat$lntclaim)
# dsim2$lnrclaim <- max(dat$lnrclaim)
# dsim2$lnmclaim <- max(dat$lnmclaim)
# tsim2 <- predict(termod, dsim2, allow.new.levels = T)
# msim2 <- predict(marmod, dsim2, allow.new.levels = T)
# rsim2 <- predict(rivmod, dsim2, allow.new.levels = T)

#dat$trsim1 <- predict(tm1, dsim1, allow.new.levels = T)
#dat$ugtpred <- trsim0 - trsim1
# dat$dyugt  <- dat$trsim0 - dat$ln_trade_100 #doesn't look great

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

# save.image("./data/TradeOut.Rdata")


# library(dynpanel)
# dat2 <- na.omit(dat %>% select(ln_trade_100, ldyterrclaim, gdp1, gdp2, dyad, year))
# 
# dpd(ln_trade_100 ~ ldyterrclaim + gdp1 + gdp2, data = dat2, index = c("dyad", "year"), p = 1,  meth = c("gdp1"))
# PIB~INF+TIR
# data the dataframe
# index : id is the name of the identity groups and time is the time per group
# p scalar, autoregressive order for dependent variable
# library(OrthoPanels)
# a <- opm(lntrade ~ ldyterrclaim, data = dat, index = c("dyad", "year"))
# a <- lmer(lntrade ~ ldyterrclaim + lag_ln_gdp1 + lag_ln_gdp2 + lag_ln_gdpcap1 + lag_ln_gdpcap2 + 
#                  contdir + ldefense + lcaprat + lpol1 * lpol2 + year + ysq + (1 | dyad) + (1 | year), data = dat, 
#                control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
# library(OrthoPanels)
# b <- dat %>% dplyr::select(dyad, year, sub, ln_trade_100, anyclaim, lag_ln_gdp_min, lag_ln_gdp_max, lag_ln_gdpcap_min, lag_ln_gdpcap_max,  contdir, ldefense, lcaprat, polmin, polmax) %>% filter(sub == 1 & year > 1900)
#          
# a <- opm(ln_trade_100 ~ anyclaim + lag_ln_gdp_min + lag_ln_gdp_max + lag_ln_gdpcap_min + lag_ln_gdpcap_max +
#            contdir + ldefense + lcaprat + polmin + polmax,
#          data = b, subset = dat$year > 1900 & dat$sub == 1,
#          index = c("dyad", "year"), n.samp = 100)
