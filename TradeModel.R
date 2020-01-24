# Estimating Unrealized Gains from Trade
setwd("C:/Users/gwill/Dropbox/Research/Dissertation/TradeModels")
library(readr)
library(readxl)
library(haven)
library(dplyr)
library(matrixStats)
library(lme4)
library(countrycode)
library(MuMIn)

### Import Monadic Data
# dgdp = read_dta("gdpcapfinal2017_01_23.dta")
# dgdp = mutate(dgdp, ccode = as.numeric(ccode), year = as.numeric(year), gdpcap = as.numeric(gdpcap), lgdpcap, as.numeric(lgdpcap))
# dmad = see other file


dcap = read_dta("./data/NMC_5_0.dta")
dpol = read_excel("./data/p4v2016.xls")
dw   = read_dta("./data/bdm2s2_nation_year_data_may2002.dta")

# Merge Monadic Data
#dmon = select(gdp, ccode, year, gdpcap, lgdpcap)
dmon = full_join(madd, dcap)
dmon = full_join(dmon, select(dpol, ccode, year, polity2))
dmon = full_join(dmon, select(dw, ccode, year, W, S))
dmon = full_join(dmon, madd)

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
dtrade <- read_csv("./data/Dyadic_COW_4.0.csv")
dtrade$dyad = undirdyads(dtrade, ccode1, ccode2)
dtrade$flow1 <- ifelse(dtrade$flow1 < 0, NA, dtrade$flow1)
dtrade$flow2 <- ifelse(dtrade$flow2 < 0, NA, dtrade$flow2)
dtrade$smoothtotrade <- ifelse(dtrade$smoothtotrade < 0, NA, dtrade$smoothtotrade)

### Distance data
ddist  <- read_csv("./data/COW_Distance_NewGene_Export.csv")
ddist$dyad <- undirdyads(ddist, ccode1, ccode2)

### Contiguity Data
dcont  <- read_csv("./data/COW_Contiguity_NewGeneExport.csv")
dcont$dyad <- undirdyads(dcont, ccode1, ccode2)
dcont <- dcont[dcont$ccode1 < dcont$ccode2, ]
# dcont$dup <- duplicated(dcont[, c("dyad", "year")])
# dsub <- dcont[dcont$dup == 1, ]
# duplicated(dcont[, c("dyad", "year")])
# dsub <- dcont[order("dyad", "year"), ]
# dsub <- arrange(dcont, dyad, year)

### ICOW global territory claim year data
icow_full_cyr  = read_csv("./data/ICOWprovyr101.csv")
# Collapse to dyad year
icow_full_dyr = icow_full_cyr %>% group_by(dyad, year) %>% summarize(
  dyterrclaim = 1,
  mainland = min(tcoffshore),
  cyricowsal = max(icowsal)
)

### Alliance data
dally <- read_csv("./data/alliance_v4.1_by_dyad_yearly.csv")
dally$dyad <- undirdyads(dally, ccode1, ccode2)
dally <- dally %>% group_by(dyad, year) %>% summarize(
  defense = sum(defense)
)

### Gibler MID data
dmid <- read_csv("./data/gml-ndy-disputes-2.0.csv")
dmid$dyad <- undirdyads(dmid, ccode1, ccode2)
dmiddy <- dmid %>% group_by(dyad, year) %>% summarize(
  mid = 1
)

### Merge dyadic data
ddy <- select(dtrade, ccode1, ccode2, dyad, year, flow1, flow2, smoothflow1, smoothflow2, smoothtotrade)
ddy <- full_join(ddy, select(ddist, dyad, year, ccdistance, mindistance))
ddy <- full_join(ddy, select(dcont, dyad, year, conttype))
ddy <- full_join(ddy, icow_full_dyr)
ddy <- full_join(ddy, select(dally, dyad, year, defense))#neutrality, nonaggression, entente
ddy <- full_join(ddy, dmiddy)

######### Merge dyadic and monadic data ###########
dat <- left_join(ddy, dmon1)
dat <- left_join(dat, dmon2)

# Dyadic trade variables
dat$trade = dat$flow1 + dat$flow2
dat$lntrade = ifelse(dat$trade == 0, 0, log(dat$trade))

# Aggregate trade with all countries
dat <- ungroup(dat %>% group_by(dyad, year) %>% mutate(
  sflow1 = sum(flow2), 
  sflow2 = sum(flow1),
))

# Mean dyadic trade
dat <- ungroup(dat %>% group_by(dyad) %>% mutate(
  mflow1 = mean(flow1),
  mflow2 = mean(flow2),
  mtrade = mean(trade, na.rm = T),
  mlntrade = mean(lntrade, na.rm = T),
))
dat$trdev = dat$trade - dat$mtrade
dat$lntrdev = dat$lntrade - dat$mlntrade

# Trade Dependence
dat$tdeptot1 = dat$sflow2/dat$gdp1
dat$tdeptot2 = dat$sflow1/dat$gdp2
dat$tdeptotmax = rowMaxs(cbind(dat$tdeptot1, dat$tdeptot2))
dat$tdepdy1 = dat$flow2/dat$gdp1 #tdepdy1 is 1's dependence on 2 (exports/gdpcap)
dat$tdepdy2 = dat$flow1/dat$gdp2 # higher values indicate that 2 is more dependent on trade
dat$tdepdymax = rowMaxs(cbind(dat$tdepdy1, dat$tdepdy2))
dat$lntdeptot1 = ifelse(dat$tdeptot1 == 0, 0, log(dat$tdeptot1))
dat$lntdeptot2 = ifelse(dat$tdeptot2 == 0, 0, log(dat$tdeptot2))
dat$lntdeptotmax = ifelse(dat$tdeptotmax == 0, 0, log(dat$tdeptotmax))
dat$lntdepdy1 = ifelse(dat$tdepdy1 == 0, 0, log(dat$tdepdy1))
dat$lntdepdy2 = ifelse(dat$tdepdy2 == 0, 0, log(dat$tdepdy2))
dat$lntdepdymax = ifelse(dat$tdepdymax == 0, 0, log(dat$tdepdymax))

# Dyadic GDP variables
dat$gdpt = dat$gdp1 + dat$gdp2
dat$lngdpt = log(dat$gdpt)
dat$gdpcapt = dat$gdpcap1 + dat$gdpcap2
dat$lngdpcapt = log(dat$gdpcapt)

# Other dyadic variables
dat$lnccdist <- ifelse(dat$ccdistance == 0, 0, log(dat$ccdistance))
dat$conttype[is.na(dat$conttype)] <- 6
dat$contdir <- ifelse(dat$conttype == 1, 1, 0)
dat$dyterrclaim <- ifelse(!is.na(dat$dyterrclaim), 1, 0)
dat$cyricowsal <- ifelse(!is.na(dat$cyricowsal), dat$cyricowsal, 0)
dat$mainland <- ifelse(!is.na(dat$mainland), 1, 0)
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
  laglntrade = lag(lntrade),
  # lmtrade = lag(mtrade), -- not needed, don't need to lag an average
  # lmlntrade = lag(mlntrade),
  ltdeptot1 = lag(tdeptot1),
  ltdeptot2 = lag(tdeptot2),
  ltdepdy1 = lag(tdepdy1),
  ltdepdy2 = lag(tdepdy2),
  laglntdeptot1 = lag(lntdeptot1),
  laglntdeptot2 = lag(lntdeptot2),
  laglntdepdy1 = lag(lntdepdy1),
  laglntdepdy2 = lag(lntdepdy2),
  lmid = lag(mid),
  ldyterrclaim = lag(dyterrclaim),
  ldemdy = lag(demdy),
  #ldefense = lag(sdally)
  ldefense = lag(defense)
)
# dat$ldemdy2 <- ifelse(dat$lpol1 > 5 & dat$lpol2 > 5, 1, 0)
# summary(dat$ldemdy)
# summary(dat$ldemdy2)
# dat$lgdpcapt2 <- dat$lgdpcap1 + dat$lgdpcap2
# summary(dat$lgdpcapt)
# summary(dat$lgdpcapt2)
# summary(dat$lgdpcap1a)
# summary(dat$lgdpcap1)

# dat$open <- dat$trade / dat$gdpcapt
  # Lagging ldyterrclaim, defense, and caprat leads to a ton of dropped observations

tm1 <- lmer(lntrade ~ ldyterrclaim + laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + contdir + ldefense + mid + lcaprat + polity21 * polity22 + (1 | dyad), data = dat); summary(tm1);r.squaredGLMM(tm1) #  .662/.93


# Diagnostics
# with(dat, cor(cbind(lntrade, ldyterrclaim, lngdp1, lngdp2, lnpop1, lnpop2, contdir, ldefense, mid, lcaprat, polity21, polity22), use = "complete.obs"))
# with(dat, summary(cbind(lntrade, ldyterrclaim, lngdp1, lngdp2, lnpop1, lnpop2, contdir, ldefense, mid, lcaprat, polity21, polity22), use = "complete.obs"))
### SOrting error - fix dplyr by ungrouping grouped data - woof

# ldefense - but defense has a lot
# lcaprat - but caprat has a lot
# mid == 1?

dsim0 <- dat
dsim0$ldyterrclaim <- 0
#dsim1 <- dat
#dsim1$ldyterrclaim <- 1
dat$trsim0 <- predict(tm1, dsim0, allow.new.levels = T)
#dat$trsim1 <- predict(tm1, dsim1, allow.new.levels = T)
#dat$ugtpred <- trsim0 - trsim1
dat$ugt  <- dat$trsim0 - dat$lntrade #doesn't look great
dat$ugtdep1  <- dat$ugt/dat$gdp1 * 100000
dat$ugtdep2 <- dat$ugt/dat$gdp2 * 100000
dat$ugtdept <- dat$ugt/dat$gdpt * 100000
dat$ugtdivtr1 <- dat$ugt/dat$sflow2
dat$ugtdivtr2 <- dat$ugt/dat$sflow1

# sum(!is.na(dat[dat$ldyterrclaim == 1, "ugt"]))
# sum(!is.na(icow_cyr_part_out[icow_cyr_part_out$ldyterrclaim == 1, "ugt"]))

dat <- dat %>% arrange(dyad, year) %>% mutate(
  lugt = lag(ugt),
  lugtdep1 = lag(ugtdep1),
  lugtdep2 = lag(ugtdep2),
  lugtdept = lag(ugtdept),
  lugtdivtr1 = lag(ugtdivtr1),
  lugtdivtr2 = lag(ugtdivtr2)
)
# dat$ugddeppred <- dat$ugtpred/dat$gdpcomb  this doesn't work because trsim0 is .3 greater than trsim1 for all observations (duh, linear model)

### ICOW settlement data
icow_set <- read_dta("./data/ICOWsettle.dta")
icow_set <- icow_set %>% filter(midiss == 0 & terriss == 1) # drop mids and non-territorial claims

icow_set <- select(icow_set, -mid)
icow_set <- left_join(icow_set, dat)
icow_set$agreeiss <- ifelse(is.na(icow_set$agreeiss), 0, icow_set$agreeiss)

icow_set_col <- icow_set %>% group_by(dyad, year) %>% summarize (
  sagree = sum(agree),
  sagreeiss = sum(agreeiss)
)
icow_set_col$bagree <- ifelse(icow_set_col$sagree > 0, 1, 0)
icow_set_col$bagreeiss <- ifelse(icow_set_col$sagreeiss > 0, 1, 0)

### ICOW Partial Data
icow_part_cyr  <- read_dta("./data/ICOWdyadyr.dta")
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
  recyeswt = sum(recyeswt),
  recmidwt = sum(recmidwt)
)

#icow_part_cyr_out <- full_join(dat, icow_part_cyr)
icow_part_cyr <- left_join(icow_part_cyr, dat)
icow_part_cyr <- left_join(icow_part_cyr, icow_set_col)
icow_part_dyr <- left_join(icow_part_dyr, dat)
icow_part_dyr <- left_join(icow_part_dyr, icow_set_col)
# write_csv(icow_part_cyr_out, "icow_part_cyr.csv")r
# write_csv(icow_part_dyr_out, "icow_part_dyr.csv")

########## ICOW Settlement Data




### Results of settlement attempt
# # Agree - settlement attempt resulted in an agreement - only missings are ongoing at end of data
# summary(icow_set[is.na(icow_set$agree), "year"])
# 
# # Agreeiss - settlement attempt result in a substantive agreement - recode NAs which could mean no agreement
# summary(icow_set[is.na(icow_set$agreeiss), "year"])
# icow_set$agreeiss <- ifelse(is.na(icow_set$agreeiss), 0, icow_set$agreeiss)
  ### Consider recoding if ongoing at end

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


## 
# dmon$e <- dmon$gdpcap * dmon$tpop
# f <- dmon[dmon$year == 2010, "e"]
# f <- unlist(f)
# summary(f)
# median(f, na.rm = T) # median 111672727
# # Mean   :  467396326  dollars --- so an increase of 5 billion is pretty damn significant

#Neural Net
# library(neuralnet)
# nn = neuralnet(Placed~TKS+CSS,data=df, hidden=3,act.fct = "logistic",
#              linear.output = FALSE)
# library(neuralnet)
# datnarm <- na.omit(dat)
# nn = neuralnet(trade ~ gdpcomb + dyterrclaim + conttype, data = datnarm)


# summary(icow_part_dyr$trsim0)
# summary(icow_part_dyr$lntrade)
# summary(icow_part_dyr$ugt) 
# # exp (-4.768) = 0.0085 millions = 8500 foregone
# # exp 8.106 = 3,314 millions foregone
# exp(.202) # mean - 1.224 million foregone
# exp(.076) # median - 1.079 million foregone
# 
# with(icow_part_dyr, summary(gdp1 + gdp2))
