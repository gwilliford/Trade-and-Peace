# Estimating Unrealized Gains from Trade
setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis - Trade Models")
library(readr)
library(readxl)
library(haven)
library(dplyr)
library(matrixStats)
library(lme4)
library(countrycode)

### Import Monadic Data
# dgdp = read_dta("gdpcapfinal2017_01_23.dta")
# dgdp = mutate(dgdp, ccode = as.numeric(ccode), year = as.numeric(year), gdpcap = as.numeric(gdpcap), lgdpcap, as.numeric(lgdpcap))
dcap = read_dta("NMC_5_0.dta")
dpol = read_excel("p4v2016.xls")
dw   = read_dta("bdm2s2_nation_year_data_may2002.dta")
# dmad = see other file

mad <- read_excel("mpd2018.xlsx", sheet = "Full data", n_max = 19357)
mad$ccode <- as.numeric(with(mad, countrycode(countrycode, 'iso3c', 'cown')))
mad$gdp <- mad$rgdpnapc/mad$pop
mad <- filter(mad, !is.na(ccode))

# Merge Monadic Data
#dmon = select(gdp, ccode, year, gdpcap, lgdpcap)
dmon = full_join(mad, dcap)
dmon = full_join(dmon, select(dpol, ccode, year, polity2))
dmon = full_join(dmon, select(dw, ccode, year, W, S))
dmon = full_join(dmon, mad)

# Check for duplicates
# sum(duplicated(dmon[, c("ccode", "year")]))
# dmon[duplicated(dmon[, c("ccode", "year")]),]

dmon$lngdp <- log(dmon$gdp)
names(dmon$rgdpnapc) <- "gdpcap"
dmon <- rename(dmon, gdpcap = rgdpnapc)
dmon$lngdpcap <- log(dmon$gdpcap)
dmon$lnpop <- log(dmon$pop)

dmon <- dmon %>% arrange(ccode, year) %>% mutate(
  lgdpcap = lag(gdpcap),
  lpol = lag(polity2), 
  lpop = lag(pop),
  lgdp = lag(gdp), 
  lcinc = lag(cinc),
  lagW = lag(W),
  lagS = lag(S),
  laglngdp = lag(lngdp),
  laglngdpcap = lag(lngdpcap),
  laglnpop = lag(lnpop)
)

# Create separate versions of monadic data
dmon1 = dmon %>% select(-"version") %>% setNames(paste0(names(.), "1")) %>% rename(year = year1)
dmon2 = dmon %>% select(-"version") %>% setNames(paste0(names(.), "2")) %>% rename(year = year2)

########### DYADIC DATA ##########
### Create common identifiers for dyadic data
undirdyads <- function(df, ccode1, ccode2) {
  attach(df)
  ccodes = cbind(ccode1, ccode2)
  cmin   = sprintf("%03d", matrixStats::rowMins(ccodes))
  cmax   = sprintf("%03d", matrixStats::rowMaxs(ccodes))
  dyad = as.numeric(paste(cmin, cmax, sep = ""))
  detach(df)
  return(dyad)
}

### Trade Data
dtrade <- read_csv("Dyadic_COW_4.0.csv")
dtrade$dyad = undirdyads(dtrade, ccode1, ccode2)
dtrade$flow1 <- ifelse(dtrade$flow1 < 0, NA, dtrade$flow1)
dtrade$flow2 <- ifelse(dtrade$flow2 < 0, NA, dtrade$flow2)
dtrade$smoothtotrade <- ifelse(dtrade$smoothtotrade < 0, NA, dtrade$smoothtotrade)

### Distance data
ddist  <- read_csv("COW_Distance_NewGene_Export.csv")
ddist$dyad <- undirdyads(ddist, ccode1, ccode2)

### Contiguity Data
dcont  <- read_csv("COW_Contiguity_NewGeneExport.csv")
dcont$dyad <- undirdyads(dcont, ccode1, ccode2)
dcont <- dcont[dcont$ccode1 < dcont$ccode2, ]
# dcont$dup <- duplicated(dcont[, c("dyad", "year")])
# dsub <- dcont[dcont$dup == 1, ]
# duplicated(dcont[, c("dyad", "year")])
# dsub <- dcont[order("dyad", "year"), ]
# dsub <- arrange(dcont, dyad, year)

### ICOW global territory claim year data
icow_full_cyr  = read_csv("ICOWprovyr101.csv")
# Collapse to dyad year
icow_full_dyr = icow_full_cyr %>% group_by(dyad, year) %>% summarize(
  dyterrclaim = 1
)

### Alliance data
dally <- read_csv("alliance_v4.1_by_dyad_yearly.csv")
dally$dyad <- undirdyads(dally, ccode1, ccode2)

### Gibler MID data
dmid <- read_csv("gml-ndy-disputes-2.0.csv")
dmid$dyad <- undirdyads(dmid, ccode1, ccode2)
dmiddy <- dmid %>% group_by(dyad, year) %>% summarize(
  mid = 1
)

### Merge dyadic data
ddy <- select(dtrade, ccode1, ccode2, dyad, year, flow1, flow2, smoothflow1, smoothflow2, smoothtotrade)
ddy <- full_join(ddy, select(ddist, dyad, year, ccdistance, mindistance))
ddy <- full_join(ddy, select(dcont, dyad, year, conttype))
ddy <- full_join(ddy, icow_full_dyr)
ddy <- full_join(ddy, select(dally, dyad, year, defense, neutrality, nonaggression, entente))
ddy <- full_join(ddy, dmiddy)

######### Merge dyadic and monadic data ###########
dat <- left_join(ddy, dmon1)
dat <- left_join(dat, dmon2)

# Create trade variables
dat$trade = dat$flow1 + dat$flow2
dat$lntrade = ifelse(dat$trade == 0, 0, log(dat$trade))

dat <- dat %>% group_by(dyad, year) %>% mutate(
  # Trade variables
  sflow1 = sum(flow2), 
  sflow2 = sum(flow1),
  # tsym = tdepdy1/tdepdy2, # Values above 0 indicate that 1 is more dependent on trade than 2
  # tsym = ifelse(is.infinite(tsym), 0, tsym)
  # 
)
dat$tdeptot1 = dat$dat$sflow2/gdp1,
dat$tdeptot2 = sflow1/gdp2, 
dat$tdepdy1 = flow2/gdp1, #tdepdy1 is 1's dependence on 2 (exports/gdpcap)
dat$tdepdy2 = flow1/gdp2 # higher values indicate that 2 is more dependent on trade


# Total GDP variables
dat$gdpt = dat$gdp1 + dat$gdp2
dat$lngdpt = log(dat$gdpt)
dat$gdpcapt = dat$gdpcap1 + dat$gdpcap2
dat$lngdpcapt = log(dat$gdpcapt)

# Other variables
dat$lnccdist <- ifelse(dat$ccdistance == 0, 0, log(dat$ccdistance))
dat$conttype[is.na(dat$conttype)] <- 6
dat$contdir <- ifelse(dat$conttype == 1, 1, 0)
dat$dyterrclaim <- ifelse(!is.na(dat$dyterrclaim), 1, 0)
dat$lnccdist <- ifelse(dat$ccdistance == 0, 0, log(dat$ccdistance))
dat$defense <- ifelse(is.na(dat$defense), 0, dat$defense)
dat$mid <- ifelse(is.na(dat$mid), 0, 1)
dat$caprat <- rowMaxs(cbind(dat$cinc1, dat$cinc2)) / (dat$cinc1 + dat$cinc2)
dat$demdy <- ifelse(dat$polity21 > 5 & dat$polity22 > 5, 1, 0)

# Lags
dat <- dat %>% arrange(dyad, year) %>% mutate(
  lcaprat = lag(caprat),
  lgdpcap1a = lag(gdpcap1),
  lgdpcap2a = lag(gdpcap2),
  lgdpcapt = lag(gdpcapt),
  #laglngdpcap1 = lag(lngdpcap1),
  #laglngdpcap2 = lag(lngdpcap2),
  laglngdpcapt = lag(lngdpcapt),
  ltrade = lag(trade),
  laglntrade <- lag(lntrade),
  ltdeptot1 = lag(tdeptot1),
  ltdeptot2 = lag(tdeptot2),
  ltdepdy1 = lag(tdepdy1),
  ltdepdy2 = lag(tdepdy2),
  lmid = lag(mid),
  ldyterrclaim = lag(dyterrclaim),
  ldemdy = lag(demdy),
  ldefense = lag(defense)
)
dat$ldemdy2 <- ifelse(dat$lpol1 > 5 & dat$lpol2 > 5, 1, 0)
summary(dat$ldemdy)
summary(dat$ldemdy2)
dat$lgdpcapt2 <- dat$lgdpcap1 + dat$lgdpcap2
summary(dat$lgdpcapt)
summary(dat$lgdpcapt2)
summary(dat$lgdpcap1a)
summary(dat$lgdpcap1)

# dat$open <- dat$trade / dat$gdpcapt
  # Lagging ldyterrclaim, defense, and caprat leads to a ton of dropped observations

# OLS
# tm1 <- lm(trade ~ dyterrclaim + gdpcomb + (conttype == 1), data = dat); summary(tm1)
# tm1 <- lm(lntrade ~ dyterrclaim + llngdpcapt + (conttype == 1), data = dat); summary(tm1)
# tm1 <- lmer(trade ~ dyterrclaim + gdpcomb + (conttype == 1) + defense + mid + open + (1 | dyad), data = dat); summary(tm1)
#tm1 <- lmer(trade ~ dyterrclaim + gdpcomb + (conttype == 1) + (dyterrclaim | dyad), data = dat); summary(tm1)
# with(dat, cor(cbind(lntrade, ldyterrclaim, gdp1, gdp2, lngdp1, lngdp2, pop1, pop2, lnpop1, lnpop2, contdir, ldefense, mid, lcaprat, polity21, polity22, demdy), use = "complete.obs"))
library(MuMIn)
tm1 <- lmer(lntrade ~ ldyterrclaim + gdp1 + gdp2 + pop1 + pop2 + contdir + ldefense + mid + lcaprat + demdy + (1 | dyad), data = dat); summary(tm1)
r.squaredGLMM(tm1) # .36/.97

tm1 <- lmer(lntrade ~ ldyterrclaim + lngdp1 + lngdp2 + lnpop1 + lnpop2 + contdir + ldefense + mid + lcaprat + polity21 * polity22 + (1 | dyad), data = dat); summary(tm1) #  .662/.93
r.squaredGLMM(tm1)

# Diagnostics
with(dat, cor(cbind(lntrade, ldyterrclaim, lngdp1, lngdp2, lnpop1, lnpop2, contdir, ldefense, mid, lcaprat, polity21, polity22), use = "complete.obs"))
with(dat, summary(cbind(lntrade, ldyterrclaim, lngdp1, lngdp2, lnpop1, lnpop2, contdir, ldefense, mid, lcaprat, polity21, polity22), use = "complete.obs"))


# ldefense - but defense has a lot
# lcaprat - but caprat has a lot
# mid == 1?
r.squaredGLMM(tm1)



tm2 <- brm(lntrade | mi() ~ dyterrclaim + lngdp1 + lngdp2 + lnpop1 + lnpop2 + contdir + mid + polity21 * polity22, data = dat, cores = 4); summary(tm1) #  .662/.93
r.squaredGLMM(tm1)


bform <- bf(bmi | mi() ~ age * mi(chl)) +
  bf(chl | mi() ~ age) + set_rescor(FALSE)


# tm1 <- lmer(trade ~ dyterrclaim + llngdpcapt + (conttype == 1) + defense + mid + caprat + demdy + (1 | dyad), data = dat); summary(tm1) # 5billion dollars when not logged

dsim0 <- dat
dsim0$ldyterrclaim <- 0
#dsim1 <- dat
#dsim1$ldyterrclaim <- 1
dat$trsim0 <- predict(tm1, dsim0, allow.new.levels = T)
#dat$trsim1 <- predict(tm1, dsim1, allow.new.levels = T)
#dat$ugtpred <- trsim0 - trsim1
dat$ugt  <- dat$trsim0 - dat$lntrade #doesn't look great
dat$ugtdep1  <- dat$ugt/dat$gdpcap1 * 100000
dat$ugtdep2 <- dat$ugt/dat$gdpcap2 * 100000
dat$ugtdept <- dat$ugt/dat$gdpcapt * 100000
dat$ugtdivtr1 <- dat$ugt/dat$sflow2
dat$ugtdivtr2 <- dat$ugt/dat$sflow1

# sum(!is.na(dat[dat$ldyterrclaim == 1, "ugt"]))
# sum(!is.na(icow_cyr_part_out[icow_cyr_part_out$ldyterrclaim == 1, "ugt"]))

dat <- dat %>% arrange(dyad, year) %>% mutate(
  lugt = lag(ugt),
  lugtdep1 = lag(ugtdep1),
  lugtdep2 = lag(ugtdep2),
  lugtdept = lag(ugtdept),
  lugtdivtr1 <- lag(ugtdivtr1),
  lugtdivtr2 <- lag(ugtdivtr2)
)
# dat$ugddeppred <- dat$ugtpred/dat$gdpcomb  this doesn't work because trsim0 is .3 greater than trsim1 for all observations (duh, linear model)


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

### ICOW Partial Data
icow_part_cyr  <- read_dta("ICOWdyadyr.dta")
icow_part_cyr  <- filter(icow_part_cyr, terriss == 1)
icow_part_dyr <- icow_part_cyr %>% group_by(dyad, year) %>% summarize(
  npeace = sum(attemptsp),
  bpeace = max(attanyp),
  nmids = sum(nmidiss),
  bmids = max(midissyr),
  totsalmax = max(icowsal),
  chalsalmax = max(salchal),
  tgtsalmax = max(saltgt),
  recnowt = sum(recnowt),
  recyeswt = sum(recyeswt) 
)
#icow_part_cyr_out <- full_join(dat, icow_part_cyr)
icow_part_cyr <- left_join(icow_part_cyr, dat)
icow_part_dyr <- left_join(icow_part_dyr, dat)
# write_csv(icow_part_cyr_out, "icow_part_cyr.csv")
# write_csv(icow_part_dyr_out, "icow_part_dyr.csv")

########## ICOW Settlement Data
icow_set <- read_dta("ICOWsettle.dta")
icow_set <- icow_set %>% filter(midiss == 0 & terriss == 1) # drop mids and non-territorial claims
icow_set <- left_join(icowset, dat)

### Results of settlement attempt
# Agree - settlement attempt resulted in an agreement
# Agreeiss - settlement attempt result in a substantive agreement
# Effect 4 - what was the outcome of the settlement attempt (agree, ratify, comply, end)
  # NA: sett attempt hasn't ended by end of current data
  # 4: Agreement ended claim
  # 3: Both states complied, but it didn't end the claim
  # 2: Both states ratified, but at least one didn't comply
  # 1: Both reached an agreement, but at least one didn't ratify
  # 0: Attempt did not produce an agreement
# Effect 3 is the same as above, except that nonratification and noncompliance are merged
  # 3: Agreement ended claim
  # 2: Both complied with agreement, but claim didn't end
  # 1: Agreement reached, but at least one didn't ratify or comply
  # 0: Attempt did not produce an agreement
# Results of settlement attempt disaggregated by partial and total claim termination
# Did cm attempt lead to an agreement that ended all or part of the claim?
icow_set$ag_end_any  <- ifelse(icow_set$agreeiss == 1 & icow_set$claimend == 1:2, 1, 0)
# Did CM attempt lead to an agreement that ended part of the claim?
icow_set$ag_end_part <- ifelse(icow_set$agreeiss == 1 & icow_set$claimend == 1, 1, 0)
# Did cm attempt lead to an agreement that ended all of the claim?
icow_set$ag_end_full <- ifelse(icow_set$agreeiss == 1 & icow_set$claimend == 2, 1, 0)

# Alternate coding possibility - subset to agreements only and code outcomes

### Claim resolution variables
# What led to claim termination?
# *** This variable is avaible in the claim level data for both the full and partial datasets
# resolved: Type of claim resolution
#   -9 (missing values): Ongoing (the claim is not resolved at the current end of the data set)
#   1: Dropped by Challenger
#   2: Renounced by Challenger
#   3: (this value is no longer used)
#   4: Bilateral
#   5: Independence
#   6: Actor Leaves System
#   7: Military Conquest/Occupation
#   8: Dropped by Target
#   9: Renounced by Target
#   10: Plebiscite
#   11: Claim No Longer Relevant
#   12: Binding Third Party Decision
#   13: Non-binding Third Party Activity
#   14: Peace conference



#Neural Net
# library(neuralnet)
# nn = neuralnet(Placed~TKS+CSS,data=df, hidden=3,act.fct = "logistic",
#              linear.output = FALSE)
# library(neuralnet)
# datnarm <- na.omit(dat)
# nn = neuralnet(trade ~ gdpcomb + dyterrclaim + conttype, data = datnarm)

## 
# dmon$e <- dmon$gdpcap * dmon$tpop
# f <- dmon[dmon$year == 2010, "e"]
# f <- unlist(f)
# summary(f)
# median(f, na.rm = T) # median 111672727
# # Mean   :  467396326  dollars --- so an increase of 5 billion is pretty damn significant

