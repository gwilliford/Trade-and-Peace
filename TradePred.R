# 
a <- as.data.frame(na.omit(with(dat, cbind(dyad, year, lntrade, ldyterrclaim, laglngdp1, laglngdp2, laglngdpcap1, laglngdpcap2, contdir, ldefense, mid, lcaprat, polity21, polity22))))
a <- 


# Panel stationarity test
library(plm)
a <- pdata.frame(a, index = c("dyad","year"))
purtest(lntrade ~ 1, data = a, index = c("dyad", "year"), pmax = 4, test = "madwu")

# Prais-winston regression
library(prais)
pw <- prais_winsten(lntrade ~ ldyterrclaim + laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + contdir + ldefense + mid + lcaprat + polity21 * polity22, data = a, index = c("dyad", "year"))

#Neural Net
library(neuralnet)

nn = neuralnet(lntrade ~ ldyterrclaim + laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + contdir + ldefense + mid + lcaprat + polity21 + polity22, data = a)
b <- as.data.frame(a)
b$ldyterrclaim <- 0
predict(nn, newdata = b)




# OLS
# tm1 <- lm(trade ~ dyterrclaim + gdpcomb + (conttype == 1), data = dat); summary(tm1)
# tm1 <- lm(lntrade ~ dyterrclaim + llngdpcapt + (conttype == 1), data = dat); summary(tm1)
# tm1 <- lmer(trade ~ dyterrclaim + gdpcomb + (conttype == 1) + defense + mid + open + (1 | dyad), data = dat); summary(tm1)
#tm1 <- lmer(trade ~ dyterrclaim + gdpcomb + (conttype == 1) + (dyterrclaim | dyad), data = dat); summary(tm1)
# # with(dat, cor(cbind(lntrade, ldyterrclaim, gdp1, gdp2, lngdp1, lngdp2, pop1, pop2, lnpop1, lnpop2, contdir, ldefense, mid, lcaprat, polity21, polity22, demdy), use = "complete.obs"))
# library(MuMIn)
# tm1 <- lmer(lntrade ~ ldyterrclaim + gdp1 + gdp2 + pop1 + pop2 + contdir + ldefense + mid + lcaprat + demdy + (1 | dyad), data = dat); summary(tm1)
# r.squaredGLMM(tm1) # .36/.97
# 
# tm1 <- lm(lntrade ~ ldyterrclaim + lngdp1 + lngdp2 + lnpop1 + lnpop2 + contdir + ldefense + mid + lcaprat + polity21 * polity22 + laglntrade, data = dat)
# r.squaredLR(tm1)
# tm1 <- lmer(lntrade ~ ldyterrclaim + lngdp1 + lngdp2 + lnpop1 + lnpop2 + contdir + ldefense + mid + lcaprat + polity21 * polity22 + laglntrade + (1 | dyad), data = dat)
# r.squaredLR(tm1)

tm2 <- lmer(lntrade ~ dyterrclaim + lngdp1 + lngdp2 + lngdpcap1 + lngdpcap2 + contdir + ldefense + mid + lcaprat + polity21 * polity22 + (1 | dyad) + (1 | year), data = dat); summary(tm2)


# I estimate the parameters using feasible generalized least
# squares (FGLS), specifically Prais-Winsten regression, and the standard errors are
# adjusted for heteroskedasticity and contemporaneous correlations across panels
# (Panel Corrected Standard Errors), according to Beck and Katz’s (1995, 1996)
# recommendations.18
# 
# tm2 <- brm(lntrade | mi() ~ 
#              ldyterrclaim + laglngdp1 + laglngdp2 + lnpop1 + lnpop2 + contdir + mid +
#              polity21 * polity22,
#            data = dat, cores = 4); summary(tm1) #  .662/.93




# Validity Tests
# hist(predout$lntrade) # larger values - regularized
# hist(predout$pred1)
# var(predout$lntrade, na.rm = T) # larger variance
# var(predout$pred1, na.rm = T)
# median(predout$lntrade, na.rm = T) # medians pretty similar
# median(predout$pred1, na.rm = T)
# mean(predout$lntrade, na.rm = T) # Means pretty similar
# mean(predout$pred1, na.rm = T)
# t.test(mean(predout$lntrade), mean(predout$pred1)) # means are not statistically distinct
# t.test(predout$lntrade, predout$pred1)
# library(coin)
# a <- c(predout$lntrade, predout$pred1)
# b <- c(rep(0, length(a)/2), rep(1, length(a)/2))
# d <- as.data.frame(cbind(a, b))
# d <- na.omit(d)
# median_test(a ~ as.factor(b), d)

# Unrealized trade
# write_csv(dat, "TradeOut.csv")


# plot(dat$lntrade ~ dat$year)
# plot(predict(tm1))
# 
# plot(a$lntrade ~ c(1870:2014), col = "red")
# lines(y = predict(z), x = c(1870:2014))
# lines(y = predict(z2), x = c(1870:2014))
# lines(y = predict(z3), x = c(1870:2014))


# Explore
a <- dtrade %>% group_by(year) %>% summarize(
  tflow1 = sum(flow1, na.rm = T),
  tflow2 = sum(flow2, na.rm = T),
  ttrade = tflow1 + tflow2, 
  lntrade = log(ttrade)
)
z <- lm(lntrade ~ year + I(year^2), data = a)
z2 <- lm(lntrade ~ year + I(year^2) + I(year^3), data = a)
z3 <- lm(lntrade ~ year + I(year^2) + I(year^3) + I(year^4), data = a)
plot(a$lntrade ~ c(1870:2014), col = "red")
lines(y = predict(z), x = c(1870:2014))
lines(y = predict(z2), x = c(1870:2014))
lines(y = predict(z3), x = c(1870:2014))


# beta <- predict(alpha)
charlie <- data.frame(year = 1817:2014, year2 = (1817:2014)^2)
mac <- predict(alpha, charlie)
plot(y = mac, x = a$year)
# sum(dtrade1$sflow1 if ccode == 200)
# sum(dtrade2$sflow2 if ccode == 200)

