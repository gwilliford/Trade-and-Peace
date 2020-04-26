# Estimating Unrealized Gains from Trade
setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis")
library(readr)
library(readxl)
library(haven)
library(readstata13)
library(dplyr)
library(matrixStats)
library(lme4)
library(MuMIn)
library(optimx)
library(DescTools)


### Import Monadic Data
madd = read_csv("./data/madd.csv")
dcap = read_dta("./data/NMC_5_0.dta")
dpol = read_excel("./data/p4v2016.xls")
dw   = read_dta("./data/bdm2s2_nation_year_data_may2002.dta")
dw   = select(dw, ccode, year, W, S, GovCrises, strikes)
chisols <- read_dta('./data/CHISOLSstyr4_0.dta')
chisols <- dplyr::select(chisols, ccode, year, totalldrtrans, leadertrans, solschange, solschdum, solschange30, solsch30dum, solsminchange, solsminchdum)

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

bifdi <- read_dta("./data/FDI_ICOW_LEEMITCHELL11.dta")

### Dyadic trade data
dtrade <- read_csv("./data/Dyadic_COW_4.0.csv")
dtrade$dyad = undirdyads(dtrade, ccode1, ccode2)
dtrade$flow1 <- ifelse(dtrade$flow1 < 0, NA, dtrade$flow1)
dtrade$flow2 <- ifelse(dtrade$flow2 < 0, NA, dtrade$flow2)
dtrade$smoothtotrade <- ifelse(dtrade$smoothtotrade < 0, NA, dtrade$smoothtotrade)# * 1000000
dtrade$flow1_100 <- dtrade$flow1 * 1000000
dtrade$flow2_100 <- dtrade$flow2 * 1000000
dtrade$smoothtotrade_100 <- dtrade$smoothtotrade * 1000000

### Monadic trade
mtrade <- read_csv('./data/National_COW_4.0.csv')
mtrade$imports <- mtrade$imports
mtrade$exports <- mtrade$exports
mtrade$imports_100 <- mtrade$imports * 1000000
mtrade$exports_100 <- mtrade$exports * 1000000
mtrade$agg <- mtrade$imports + mtrade$exports
mtrade$agg_100 <- mtrade$imports_100 + mtrade$exports_100

### ICOW partial data
icow_part_cyr <- read_dta("./data/ICOWdyadyr.dta")
icow_part_dyr <- ungroup(icow_part_cyr %>% group_by(dyad, year) %>% summarize(
  ntclaim = sum(terriss, na.rm = T),
  btclaim = max(terriss),
  nmclaim = sum(mariss, na.rm = T),
  bmclaim = max(mariss),
  nrclaim = sum(riveriss, na.rm = T),
  brclaim = max(riveriss), 
  totclaim = ntclaim + nmclaim + nrclaim,
  anyclaim = 1
))


### ICOW dyad year data
icow_full_dyr = icow_full_cyr %>% group_by(dyad, year) %>% summarize(
  dyterrclaim = 1,
  mainland = max(tcoffshore),
  claimyrsal = max(icowsal),
  claimyrtan = max(saltan),
  claimyrint = max(salint),
  claimyrintc = max(salintc),
  claimyrintt = max(salintt)
)

### Leader support data
ead <- read_dta("./data/EAD+2.0+Annual+0101019.dta")

### Merge Monadic Data
dmon = full_join(madd, dcap)
dmon = full_join(dmon, select(dpol, ccode, year, polity2))
dmon = full_join(dmon, filter(dw, !is.na(ccode)))
dmon = full_join(dmon, chisols)
dmon = full_join(dmon, select(mtrade, ccode, year, imports, exports, agg, imports_100, exports_100, agg_100))
dmon = full_join(dmon, ead)

# Check for duplicates
# sum(duplicated(dmon[, c("ccode", "year")]))
# dmon[duplicated(dmon[, c("ccode", "year")]),]

#dmon$lnpop <- log(dmon$pop)

### Monadic Lags
dmon <- dmon %>% arrange(ccode, year) %>% mutate(
  # Trade vars
    ln_agg = ifelse(is.na(agg), 0, log(agg)),
    ln_agg_100 = ifelse(is.na(agg_100), 0, log(agg_100)),
    lag_ln_agg = lag(ln_agg),
    lag_ln_agg_100 = lag(ln_agg_100),
  # GDP vars
    lag_gdp = lag(gdp), 
    lag_gdpcap = lag(gdpcap),
    ln_gdp = ifelse(gdp == 0, 0, log(gdp)),
    lag_ln_gdp = lag(ln_gdp),
    ln_gdpcap = ifelse(gdpcap == 0, 0, log(gdpcap)),
    lag_ln_gdpcap = lag(ln_gdpcap),
    pch_gdp    = (gdp - lag_gdp)/lag_gdp * 100,
    pch_gdpcap = (gdpcap - lag_gdpcap)/lag_gdpcap * 100,
    pch_ln_gdp    = (ln_gdp - lag_ln_gdp)/lag_ln_gdp * 100,
    pch_ln_gdpcap = (ln_gdpcap - lag_ln_gdpcap)/lag_ln_gdpcap * 100,
  # Control vars
    lcinc = lag(cinc) * 10,
    lnmidcyr  = lag(nmidcyr),
    lbmidcyr = lag(bmidcyr),
    lbterrclaim = lag(bterrclaim),
    lnterrclaim = lag(nterrclaim),
    lag_pop = lag(pop),
    ln_pop = log(pop),
    lag_ln_pop = lag(ln_pop),
  # Institutional vars
    lpol = lag(polity2), 
    lagW = lag(W),
    lagS = lag(S),
    dy2 = year * year / 1000
)

# Create separate versions of monadic data
dmon1 = dmon %>% select(-"version") %>% setNames(paste0(names(.), "1")) %>% rename(year = year1)
dmon2 = dmon %>% select(-"version") %>% setNames(paste0(names(.), "2")) %>% rename(year = year2)

########### DYADIC DATA ##########

### Import igo data
igo <- read_dta("./data/igocount.dta")

### Distance and contiguity data
ddist  <- read_csv("./data/COW_Distance_NewGene_Export.csv")
dcont  <- read_csv("./data/COW_Contiguity_NewGeneExport.csv")
dcont <- dcont[dcont$ccode1 < dcont$ccode2, ]

### Alliance data
dally <- read_csv("./data/alliance_v4.1_by_dyad_yearly.csv")
dally$dyad <- undirdyads(dally, ccode1, ccode2)
dally <- dally %>% group_by(dyad, year) %>% summarize(
  defense = sum(defense, na.rm = T)
)

### Thompson Rivalry Data
triv <- read_dta("./data/ThompsonDyadYear.dta")
# triv$trival <- ifelse(is.na(triv$trival), -1, 1)

### Merge dyadic data
ddy <- select(dtrade, ccode1, ccode2, dyad, year, flow1, flow2, smoothflow1, smoothflow2, smoothtotrade, flow1_100, flow2_100, smoothtotrade_100)
ddy <- left_join(ddy, select(ddist, ccode1, ccode2, year, ccdistance, mindistance))
ddy <- left_join(ddy, select(dcont, ccode1, ccode2, year, conttype))
ddy <- left_join(ddy, icow_full_dyr)
ddy <- left_join(ddy, icow_part_dyr)
ddy <- left_join(ddy, select(dally, dyad, year, defense))#neutrality, nonaggression, entente
ddy <- left_join(ddy, dmiddy)
ddy <- left_join(ddy, triv)
ddy <- left_join(ddy, igo)
# Full joins are unnecessary - merging to trade data - lags of this are going to mean that I lose all of the first observations - so losing lags for that first var doesn't matterW%$

######### Merge dyadic and monadic data ###########
dat <- left_join(ddy, dmon1)
dat <- left_join(dat, dmon2)
dat <- rename(dat, 'polity1' = 'polity21', 'polity2' = 'polity22')

# Dyadic trade variables
dat$trade = dat$smoothtotrade #XXX
dat$trade_100 = dat$smoothtotrade_100 #XXX
#dat$trade = dat$flow1 + dat$flow2
dat$ln_trade = ifelse(dat$trade == 0, 0, log(dat$trade))
dat$ln_trade_100 = ifelse(dat$trade_100 == 0, 0, log(dat$trade_100))
# Mean dyadic trade
dat <- ungroup(dat %>% group_by(dyad) %>% mutate(
  mflow1 = mean(flow1),
  mflow2 = mean(flow2),
  mtrade = mean(trade, na.rm = T),
  mtrade_100 = mean(trade_100),
  mlntrade = mean(ln_trade, na.rm = T),
))
dat$aggtrademin = rowMins(cbind(dat$agg1, dat$agg2))

# Deviations from mean dyadic trade
dat$trdev = dat$trade - dat$mtrade
dat$lntrdev = dat$ln_trade - dat$mlntrade

# Dependence on global trade
dat$deptot1 = (dat$exports1 + dat$imports1) / (dat$gdp1)
dat$deptot2 = (dat$exports2 + dat$imports2) /(dat$gdp2)
dat$deptot100_1 = (dat$agg_1001) / (dat$gdp1)
dat$deptot100_2 = (dat$agg_1002) /(dat$gdp2)
dat$deptotmin = rowMins(cbind(dat$deptot1, dat$deptot2))
dat$deptotmax = rowMaxs(cbind(dat$deptot1, dat$deptot2))
dat$deptotmin_100 = rowMins(cbind(dat$deptot100_1, dat$deptot100_2))
dat$deptotmax_100 = rowMaxs(cbind(dat$deptot100_1, dat$deptot100_2))
dat$depoth1 = (dat$exports1 + dat$imports1 - dat$trade) / (dat$gdp1)
dat$depoth2 = (dat$exports2 + dat$imports2 - dat$trade) /(dat$gdp2)
dat$depoth100_1 = (dat$agg_1001 - dat$trade_100) / (dat$gdp1)
dat$depoth100_2 = (dat$agg_1002 - dat$trade_100) /(dat$gdp2)
dat$depothmin = rowMins(cbind(dat$depoth1, dat$depoth2))
dat$depothmax = rowMaxs(cbind(dat$depoth1, dat$depoth2))
dat$depothmin_100 = rowMins(cbind(dat$depoth100_1, dat$depoth100_2))
dat$depothmax_100 = rowMaxs(cbind(dat$depoth100_1, dat$depoth100_2))
dat$ln_deptot1 = ifelse(dat$deptot1 == 0, 0, log(dat$deptot1))
dat$ln_deptot2 = ifelse(dat$deptot2 == 0, 0, log(dat$deptot2))
dat$ln_deptotmin = ifelse(dat$deptotmin == 0, 0, log(dat$deptotmin))
dat$ln_deptotmax = ifelse(dat$deptotmax == 0, 0, log(dat$deptotmax))
dat$ln_deptot100_1 = ifelse(dat$deptot100_1 == 0, 0, log(dat$deptot100_1))
dat$ln_deptot100_2 = ifelse(dat$deptot100_2 == 0, 0, log(dat$deptot100_2))
dat$ln_depothmin = ifelse(dat$depothmin == 0, 0, log(dat$depothmin))
dat$ln_deptotmin_100 = ifelse(dat$deptotmin_100 == 0, 0, log(dat$deptotmin_100))
dat$ln_deptotmax_100 = ifelse(dat$deptotmax_100 == 0, 0, log(dat$deptotmax_100))
dat$ln_depoth1 = ifelse(dat$depoth1 == 0, 0, log(dat$depoth1))
dat$ln_depoth2 = ifelse(dat$depoth2 == 0, 0, log(dat$depoth2))
dat$ln_depothmax = ifelse(dat$depothmax == 0, 0, log(dat$depothmax))
dat$ln_depoth100_1 = ifelse(dat$depoth100_1 == 0, 0, log(dat$depoth100_1))
dat$ln_depoth100_2 = ifelse(dat$depoth100_2 == 0, 0, log(dat$depoth100_2))
dat$ln_depothmin_100 = ifelse(dat$depothmin_100 == 0, 0, log(dat$depothmin_100))
dat$ln_depothmax_100 = ifelse(dat$depothmax_100 == 0, 0, log(dat$depothmax_100))
dat$b1 = dat$ln_agg1 / dat$ln_gdp1
dat$b2 = dat$ln_agg2 / dat$ln_gdp2
dat$bmin = rowMins(cbind(dat$b1, dat$b2))
dat$bmax = rowMaxs(cbind(dat$b1, dat$b2))
dat$c1 = (dat$ln_agg1 - dat$ln_trade)/ dat$ln_gdp1
dat$c2 = (dat$ln_agg2 - dat$ln_trade)/ dat$ln_gdp2
dat$cmin = rowMins(cbind(dat$c1, dat$c2))
dat$cmax = rowMaxs(cbind(dat$c1, dat$c2))

# Dependence on dyadic trade
dat$depdy1 = dat$trade/dat$gdp1 #depdy1 is 1's dependence on 2 (exports/gdpcap)
dat$depdy2 = dat$trade/dat$gdp2  # higher values indicate that 2 is more dependent on trade
dat$depdymax = rowMaxs(cbind(dat$depdy1, dat$depdy2))
dat$depdymin = rowMins(cbind(dat$depdy1, dat$depdy2))
dat$depdy100_1 = dat$trade_100/dat$gdp1 #depdy1 is 1's dependence on 2 (exports/gdpcap)
dat$depdy100_2 = dat$trade_100/dat$gdp2  # higher values indicate that 2 is more dependent on trade
dat$depdymin_100 = rowMins(cbind(dat$depdy100_1, dat$depdy100_2))
dat$depdymax_100 = rowMaxs(cbind(dat$depdy100_1, dat$depdy100_2))
dat$ln_depdy1 = ifelse(dat$depdy1 == 0, 0, log(dat$depdy1))
dat$ln_depdy2 = ifelse(dat$depdy2 == 0, 0, log(dat$depdy2))
dat$ln_depdymax = ifelse(dat$depdymax == 0, 0, log(dat$depdymax))
dat$ln_depdymin = ifelse(dat$depdymin == 0, 0, log(dat$depdymin))
dat$ln_depdy100_1 = ifelse(dat$depdy100_1 == 0, 0, log(dat$depdy100_1))
dat$ln_depdy100_2 = ifelse(dat$depdy100_2 == 0, 0, log(dat$depdy100_2))
dat$ln_depdymin_100 = ifelse(dat$depdymin_100 == 0, 0, log(dat$depdymin_100))
dat$ln_depdymax_100 = ifelse(dat$depdymax_100 == 0, 0, log(dat$depdymax_100))
dat$a1 <- dat$ln_trade_100 - dat$ln_gdp1
dat$a2 <- dat$ln_trade_100 - dat$ln_gdp2
dat$amin <- rowMins(cbind(dat$a1, dat$a2))
dat$amax <- rowMaxs(cbind(dat$a1, dat$a2))

# The log of a quotient is the difference of the logs.
# 
# loga (x/y) = loga x - loga y

summary(dat$ln_trade_100 - dat$ln_gdp1)
summary(log(dat$ln_trade_100/dat$ln_gdp1))

# Dyadic GDP variables
dat$gdpt = dat$gdp1 + dat$gdp2
dat$lngdpt = log(dat$gdpt)
dat$gdpcapt = dat$gdpcap1 + dat$gdpcap2
dat$lngdpcapt = log(dat$gdpcapt)
dat$gdp_min = rowMins(cbind(dat$gdp1, dat$gdp2))
dat$gdp_max = rowMaxs(cbind(dat$gdp1, dat$gdp2))
dat$ln_gdp_min = rowMins(cbind(dat$ln_gdp1, dat$ln_gdp2))
dat$ln_gdp_max = rowMaxs(cbind(dat$ln_gdp1, dat$ln_gdp2))
dat$ln_gdpcap_min = rowMins(cbind(dat$ln_gdpcap1, dat$ln_gdpcap2))
dat$ln_gdpcap_max = rowMaxs(cbind(dat$ln_gdpcap1, dat$ln_gdpcap2))
dat$pch_gdp_min = rowMins(cbind(dat$pch_gdp1, dat$pch_gdp2))


# Other dyadic variables
dat$lnccdist <- ifelse(dat$ccdistance == 0, 0, log(dat$ccdistance))
dat$conttype[is.na(dat$conttype)] <- 6
dat$contdir <- ifelse(dat$conttype == 1, 1, 0)
dat$dyterrclaim <- ifelse(is.na(dat$dyterrclaim), 0, 1)
dat$btclaim <- ifelse(is.na(dat$btclaim), 0, 1)
dat$bmclaim <- ifelse(is.na(dat$bmclaim), 0, 1)
dat$brclaim <- ifelse(is.na(dat$brclaim), 0, 1)
dat$anyclaim <- ifelse(is.na(dat$anyclaim), 0, 1)
dat$fatality <- ifelse(is.na(dat$fatality), 0, dat$fatality)
dat$trival <- ifelse(is.na(dat$trival), 0, 1)

#dat$cyricowsal <- ifelse(!is.na(dat$cyrsal), dat$cyrsal, 0)
dat$mainland <- ifelse(!is.na(dat$mainland), 1, 0)
dat$defense <- ifelse(is.na(dat$defense), 0, dat$defense)
dat$bdymid <- ifelse(is.na(dat$bdymid), 0, 1)
dat$ndymid <- ifelse(is.na(dat$ndymid), 0, 1)
dat$caprat <- rowMaxs(cbind(dat$cinc1, dat$cinc2)) / (dat$cinc1 + dat$cinc2)
dat$ln_caprat <- ifelse(dat$caprat == 0, 0, log(dat$caprat))
dat$polmin <- rowMins(cbind(dat$polity1, dat$polity2))
dat$polmax <- rowMaxs(cbind(dat$polity1, dat$polity2))
dat$demdy <- ifelse(dat$polity1 > 5 & dat$polity2 > 5, 1, 0)
dat$autdy <- ifelse(dat$polity1 < -5 & dat$polity2 < -5, 1, 0)
dat$samereg <- ifelse(dat$demdy == 1 | dat$autdy == 1, 1, 0)
dat$ysq = (dat$year^2) / 1000
dat$y3 = (dat$year^3) / 1000
dat$Wmin = rowMins(cbind(dat$W1, dat$W2))
dat$Wmax = rowMaxs(cbind(dat$W1, dat$W2))
# dat$GovCrisesMin = rowMins(cbind(dat$GovCrises1, dat$GovCrises2)) # don't make sense
# dat$GovCrisesMax = rowMaxs(cbind(dat$GovCrises1, dat$GovCrises2))
dat$GovCrisesDy = ifelse(dat$GovCrises1 > 0 | dat$GovCrises2 > 0, 1, 0)

# Lags
dat <- dat %>% arrange(dyad, year) %>% mutate(
  lcaprat = lag(caprat),
  lag_ln_caprat = lag(ln_caprat),
  lgdpcapt = lag(gdpcapt),
  lag_ln_gdpcapt = lag(lngdpcapt),
  lag_ln_gdp_min = lag(ln_gdp_min),
  lag_ln_gdp_max = lag(ln_gdp_max),
  lag_ln_gdpcap_min = lag(ln_gdpcap_min),
  lag_ln_gdpcap_max = lag(ln_gdpcap_max),
  lag_trade = lag(trade),
  lag_ln_trade = lag(ln_trade),
  lag_deptot1 = lag(deptot1),
  lag_deptot2 = lag(deptot2),
  lag_deptotmin = lag(deptotmin),
  lag_deptotmax = lag(deptotmax),
  lag_depdy1 = lag(depdy1),
  lag_depdy2 = lag(depdy2),
  lag_depdymin = lag(depdymin),
  lag_depdymax = lag(depdymax),
  lag_ln_deptot1 = lag(ln_deptot1),
  lag_ln_deptot2 = lag(ln_deptot2),
  lag_ln_deptotmax = lag(ln_deptotmax),
  lag_ln_deptotmin = lag(ln_deptotmin),
  lag_ln_depoth1 = lag(ln_depoth1),
  lag_ln_depoth2 = lag(ln_depoth2),
  lag_ln_depothmin = lag(ln_depothmin),
  lag_ln_depothmax = lag(ln_depothmax),
  lag_ln_depdy1 = lag(ln_depdy1),
  lag_ln_depdy2 = lag(ln_depdy2),
  lag_ln_depdymin = lag(ln_depdymin),
  lag_ln_depdymax = lag(ln_depdymax),
  lag_trade_100 = lag(trade_100),
  lag_ln_trade_100 = lag(ln_trade_100),
  lag_deptot100_1 = lag(deptot100_1),
  lag_deptot100_2 = lag(deptot100_2),
  lag_deptotmin_100 = lag(deptotmin_100),
  lag_deptotmax_100 = lag(deptotmax_100),
  lag_depdy100_1 = lag(depdy100_1),
  lag_depdy100_2 = lag(depdy100_2),
  lag_depdymax_100 = lag(depdymax_100),
  lag_depdymin_100 = lag(depdymin_100),
  lag_depothmin = lag(depothmin),
  lag_depothmin_100 = lag(depothmin_100),
  lag_ln_deptot100_1 = lag(ln_deptot100_1),
  lag_ln_deptot100_2= lag(ln_deptot100_2),
  lag_ln_deptotmin_100 = lag(ln_deptotmin_100),
  lag_ln_deptotmax_100 = lag(ln_deptotmax_100),
  lag_ln_depoth100_1 = lag(ln_depoth100_1),
  lag_ln_depoth100_2 = lag(ln_depoth100_2),
  lag_ln_depothmin_100 = lag(ln_depothmin_100),
  lag_ln_depothmax_100 = lag(ln_depothmax_100),
  lag_ln_depdy100_1 = lag(ln_depdy100_1),
  lag_ln_depdy100_2 = lag(ln_depdy100_2),
  lag_ln_depdymin_100 = lag(ln_depdymin_100),
  lag_ln_depdymax_100 = lag(ln_depdymax_100),
  lag_pch_ln_gdp1 = lag(pch_ln_gdp1),
  lag_pch_ln_gdp2 = lag(pch_ln_gdp2),
  lag_pch_ln_gdpcap1 = lag(pch_ln_gdpcap1),
  lag_pch_ln_gdpcap2 = lag(pch_ln_gdpcap2),
  lbdymid = lag(bdymid),
  lndymid = lag(ndymid),
  ldyterrclaim = lag(dyterrclaim),
  lbtclaim = lag(btclaim),
  lbmclaim = lag(bmclaim),
  lbrclaim = lag(brclaim),
  lntclaim = lag(ntclaim),
  lnmclaim = lag(nmclaim),
  lnrclaim = lag(nrclaim),
  lanyclaim = lag(anyclaim),
  ltotclaim = lag(totclaim),
  ldemdy = lag(demdy),
  ldefense = lag(defense), 
  lfatality = lag(fatality),
  lGovCrises1 = lag(GovCrises1),
  lGovCrises2 = lag(GovCrises2),
  # lGovCrisesMin = lag(GovCrisesMin),
  # lGovCrisesMax = lag(GovCrisesMax),
  lGovCrisesDy = lag(GovCrisesDy),
  lag_pch_gdp_min = lag(pch_gdp_min),
  lamin = lag(amin),
  lamax = lag(amax),
  lbmin = lag(bmin),
  lbmax = lag(bmax),
  lcmax = lag(cmax),
  lcmin = lag(cmin),
  pchcaprat = lag(caprat) / caprat,
  lpchcaprat = lag(pchcaprat)
)
#> with(dat, cor(cbind(ldyterrclaim, lag_ln_gdp1, lag_ln_gdp2, lag_ln_gdpcap1, lag_ln_gdpcap2, contdir, ldefense, lcaprat, lpol1, lpol2, year, y2), use = "complete.obs"))
# 
# write_csv(dat, "./data/TradeInputs.csv")
# write_rds(dat, "./data/TradeInputs.RDS")
# region codes
# ceuro <- icow_part_cyr[icow_part_cyr$region == 2, "chal"]
# teuro <- icow_part_cyr[icow_part_cyr$region == 2, "tgt"]
# aeuro <- unlist(c(ceuro, teuro))
# sort(unique(aeuro))
dat$sub  <- ifelse(dat$ccode1 %in% 1:330 | dat$ccode2 %in% 1:330, 1, 0)
dat$subr <- ifelse(dat$ccode1 %in% c(1:330, 600:699) | dat$ccode2 %in% c(1:330, 600:699), 1, 0)

dsub <- na.omit(filter(dat, sub == 1 & year >= 1900) %>% select(ln_trade, anyclaim, lag_ln_gdp_min, lag_ln_gdp_max, lag_ln_gdpcap_min, lag_ln_gdpcap_max, contdir, ldefense, lcaprat, polmin, polmax, dyad, year))
# a <- opm(ln_trade_100 ~ anyclaim + lag_ln_gdp_min + lag_ln_gdp_max + lag_ln_gdpcap_min + lag_ln_gdpcap_max +
#            contdir + ldefense + lcaprat + polmin + polmax,
#          data = b, subset = dat$year > 1900 & dat$sub == 1,
#          index = c("dyad", "year"), n.samp = 100)

# saveRDS(dsub, "./data/opminputs.RDS")
# # Visualize trade
# library(lattice)
# dat2 <- dat[dat$dyad == c(2400, 710800, 501560)]
# dat2 <- dat %>% group_by(year) %>% summarize(
#   mt = mean(trade, na.rm = T)
# )
# xyplot(mt~year, data=dat2, type='l', xlab="Year", ylab='Number of Enforcement Actions', main="Clean Air Act Enforcement Actions by State")
# 
# xyplot(trade~year, data=dat, type='l', groups=dyad, xlab="Year", ylab='Number of Enforcement Actions', main="Clean Air Act Enforcement Actions by State")
# 

datout <- select(dat, dyad, year, lag_pch_gdp_min, lGovCrisesDy, polmin, polmax, autdy, demdy, samereg, Wmin, Wmax, lag_depdymin, lag_depdymin_100, lag_deptotmin, lag_deptotmin_100, lag_depothmin, lag_depothmin_100, lag_ln_depdymin_100, lamin, lamax, lbmin, lbmax, lcmin, lcmax, lcaprat, lpchcaprat, ldefense, contdir, trival, igosum)
