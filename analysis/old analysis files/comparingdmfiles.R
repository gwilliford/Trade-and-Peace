datold <- dat
tm1 <- lmer(lntrade ~ ldyterrclaim + laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + contdir + ldefense + lcaprat + lpol1 * lpol2 + year + I(year^2) + (1 | dyad) + (1 | year), data = datold,           control= lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb",starttests = FALSE, kkt = FALSE)));summary(tm1)

datnew <- dat
termod <- lmer(ln_trade ~ ldyterrclaim + lag_ln_gdp1 + lag_ln_gdp2 + lag_ln_gdpcap1 + lag_ln_gdpcap2 + 
                 contdir + ldefense + lcaprat + lpol1 * lpol2 + year + I(year^2) + (1 | dyad) + (1 | year),
               data = datnew, 
               control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                     optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)));summary(termod)


with(datold, summary(cbind(lntrade, ldyterrclaim, laglngdp1 , laglngdp2 , laglngdpcap1 , laglngdpcap2 , contdir , ldefense , lcaprat , lpol1 , lpol2 , year)))
with(datnew, summary(cbind(ln_trade, ldyterrclaim , lag_ln_gdp1 , lag_ln_gdp2 , lag_ln_gdpcap1 , lag_ln_gdpcap2 , 
                       contdir , ldefense , lcaprat , lpol1, lpol2 , year)))

summary(tm1);summary(termod)
