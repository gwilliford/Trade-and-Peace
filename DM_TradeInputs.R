# Estimating Unrealized Gains from Trade
setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis")
library(readr)
library(readxl)
library(haven)
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

### Dyadic trade data
dtrade <- read_csv("./data/Dyadic_COW_4.0.csv")
dtrade$dyad = undirdyads(dtrade, ccode1, ccode2)
dtrade$flow1 <- ifelse(dtrade$flow1 < 0, NA, dtrade$flow1)
dtrade$flow2 <- ifelse(dtrade$flow2 < 0, NA, dtrade$flow2)
dtrade$smoothtotrade <- ifelse(dtrade$smoothtotrade < 0, NA, dtrade$smoothtotrade)
dtrade$flow1 <- dtrade$flow1 * 1000000
dtrade$flow2 <- dtrade$flow2 * 1000000
dtrade$smoothtotrade <- dtrade$smoothtotrade * 1000000
### Total global trade for each country-year
# Barbieri in millions of dollars
# Maddison in dollars
# Get sum of imports, exports, and both when country i is ccode1
# dtrade1 <- dtrade %>% group_by(ccode1, year) %>% summarize(
#   imp1 = sum(flow1, na.rm = T), # sum of exports
#   exp1 = sum(flow2, na.rm = T), # sum of imports
#   agg1 = imp1 + exp1 # total trade
# ) %>% rename(ccode = ccode1)
# # Get sum of imports and exports when country i is ccode2
# dtrade2 <- dtrade %>% group_by(ccode2, year) %>% summarize(
#   imp2 = sum(flow2, na.rm = T),
#   exp2 = sum(flow1, na.rm = T),
#   agg2 = imp2 + exp2
# ) %>% rename(ccode = ccode2)
# # Merge dtrade1 and dtrade2 by ccode and year and create aggregate variables
# dtradea <- full_join(dtrade1, dtrade2) %>% mutate (
#   imptot = imp1 + imp2,
#   exptot = exp1 + exp2,
#   aggtot = agg1 + agg2
# ) %>% select(ccode, year, imptot, exptot, aggtot)
# dtradea$lnagg <- ifelse(dtradea$aggtot == 0, 0, log(dtradea$aggtot))
mtrade <- read_csv('./data/National_COW_4.0.csv')
mtrade$imports <- mtrade$imports * 1000000
mtrade$exports <- mtrade$exports * 1000000
mtrade$agg <- mtrade$imports + mtrade$exports

### ICOW global territory claim year data
icow_full_cyr = read_csv("./data/ICOWprovyr101.csv")
icow_full_cyr$ccode1 = rowMins(cbind(icow_full_cyr$chal, icow_full_cyr$tgt))
icow_full_cyr$ccode2 = rowMaxs(cbind(icow_full_cyr$chal, icow_full_cyr$tgt))
icow_full_cyr$tclaim = 1
icow_full_cyr$salint1 = with(icow_full_cyr, ifelse(chal == ccode1, max(salintc), max(salintt)))
icow_full_cyr$salint2 = with(icow_full_cyr, ifelse(chal == ccode2, max(salintc), max(salintt)))

### ICOS partial data
icow_part_cyr <- read_dta("./data/ICOWdyadyr.dta")
icow_part_dyr <- icow_part_cyr %>% group_by(dyad, year) %>% summarize(
  ntclaim = sum(terriss, na.rm = T),
  btclaim = max(terriss),
  nmclaim = sum(mariss, na.rm = T),
  bmclaim = max(mariss),
  nrclaim = sum(riveriss, na.rm = T),
  brclaim = max(riveriss)
)

### ICOW country year summary
icow_country1 = icow_full_cyr %>% group_by(ccode1, year) %>% summarize(
  nterrclaim1 = sum(tclaim),
  bterrclaim1 = 1,
  # mainland1 = max(tcoffshore),
  # salmax1 = max(icowsal),
  # saltanmax1 = max(saltan),
  # salintmax1 = max(salint1)
) %>% rename(ccode = ccode1)

icow_country2 = icow_full_cyr %>% group_by(ccode2, year) %>% summarize(
  nterrclaim2 = sum(tclaim),
  bterrclaim2 = 1,
  # mainland2 = max(tcoffshore),
  # salmax2 = max(icowsal),
  # saltanmax2 = max(saltan),
  # salintmax2 = max(salint2)
) %>% rename(ccode = ccode2)

icow_countrya <- full_join(icow_country1, icow_country2) %>% mutate (
  nterrclaim = nterrclaim1 + nterrclaim2,
  bterrclaim = bterrclaim1 + bterrclaim2,
  # mainland = mainland1 + mainland2,
  # salmax = salmax1 + salmax2,
  # saltanmax = saltanmax1 + saltanmax2,
  # salintmax = salintmax1 + salintmax2
)

### Gibler MID data
dmid <- read_csv("./data/gml-ndy-disputes-2.0.csv")
dmid$dyad <- undirdyads(dmid, ccode1, ccode2)
dmid$fatality <- LOCF(dmid$fatality)
dmid$fatality <- ifelse(dmid$fatality == -9, 0, dmid$fatality)
dmid$dmidyr = 1
dmiddy <- dmid %>% group_by(dyad, year) %>% summarize(
  ndymid = sum(dmidyr, na.rm = T),
  bdymid = 1, 
  fatality = max(fatality)
)

cmid1 <- dmid %>% group_by(ccode1, year) %>% summarize(
  nmidcyr1 = sum(dmidyr, na.rm = T),
  bmidcyr1 = 1
) %>% rename(ccode = ccode1) %>% select(ccode, year, nmidcyr1, bmidcyr1)

cmid2 <- dmid %>% group_by(ccode2, year) %>% summarize(
  nmidcyr2 = sum(dmidyr, na.rm = T),
  bmidcyr2 = 1
) %>% rename(ccode = ccode2) %>% select(ccode, year, nmidcyr2, bmidcyr2)

cmida <- full_join(cmid1, cmid2) %>% mutate(
  nmidcyr = nmidcyr1 + nmidcyr2,
  bmidcyr = bmidcyr1 + bmidcyr2
) %>% select(ccode, year, nmidcyr, bmidcyr)

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

### Merge Monadic Data
dmon = full_join(madd, dcap)
dmon = full_join(dmon, select(dpol, ccode, year, polity2))
dmon = full_join(dmon, select(dw, ccode, year, W, S))
dmon = full_join(dmon, chisols)
#dmon = full_join(dmon, dtradea)
# dmon = full_join(dmon, mtrade)
dmon = full_join(dmon, select(mtrade, ccode, year, imports, exports, agg))
dmon = full_join(dmon, cmida)
dmon = full_join(dmon, icow_countrya)

dmon$nmidcyr <- ifelse(is.na(dmon$nmidcyr), 0, dmon$nmidcyr)
dmon$bmidcyr <- ifelse(is.na(dmon$bmidcyr), 0, dmon$bmidcyr)
dmon$nterrclaim <- ifelse(is.na(dmon$nterrclaim), 0, dmon$nterrclaim)
dmon$bterrclaim <- ifelse(is.na(dmon$bterrclaim), 0, dmon$bterrclaim)
# dmon$mainland <- ifelse(is.na(dmon$mainland), 0, dmon$mainland)
# dmon$salmax <- ifelse(is.na(dmon$salmax), 0, dmon$salmax)
# dmon$saltanmax <- ifelse(is.na(dmon$saltanmax), 0, dmon$saltanmax)
# dmon$salintmax <- ifelse(is.na(dmon$salintmax), 0, )

# Check for duplicates
# sum(duplicated(dmon[, c("ccode", "year")]))
# dmon[duplicated(dmon[, c("ccode", "year")]),]

dmon <- mutate(dmon,
)

#dmon$lnpop <- log(dmon$pop)

### Monadic Lags
dmon <- dmon %>% arrange(ccode, year) %>% mutate(
  # Trade vars
    ln_agg = ifelse(is.na(agg), 0, log(agg)),
    lag_ln_agg = lag(ln_agg),
  # GDP vars
    lag_gdp = lag(gdp), 
    lag_gdpcap = lag(gdpcap),
    ln_gdp = ifelse(gdp == 0, 0, log(gdp)),
    lag_ln_gdp = lag(ln_gdp),
    ln_gdpcap = ifelse(gdpcap == 0, 0, log(gdpcap)),
    lag_ln_gdpcap = lag(ln_gdpcap),
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

### Predict total monadic trade using monadic variables
# monmod <- lmer(ln_agg ~ lbterrclaim + lag_ln_gdp + lag_ln_gdpcap + lbmidcyr + lcinc +
#                  lpol + year + dy2,
#                data = dmon); summary(monmod)#;r.squaredGLMM(monmod) #  .662/.93
# bmonsim <- dmon
# monsim$lnterrclaim <- 0
# dmon$monsim <- predict(monmod, monsim, allow.new.levels = T)
# dmon$ugdep1  <- dmon$ugt/dmon$gdp1 * 1000000
# dmon$ugdep2 <- dmon$ugt/dmon$gdp2 * 1000000
# dmon$ugdept <- dmon$ugt/dmon$gdpt * 1000000
# dmon$ugtdivtr1 <- dmon$ugt/dmon$sflow2
# dmon$ugtdivtr2 <- dmon$ugt/dmon$sflow1


# Create separate versions of monadic data
dmon1 = dmon %>% select(-"version") %>% setNames(paste0(names(.), "1")) %>% rename(year = year1)
dmon2 = dmon %>% select(-"version") %>% setNames(paste0(names(.), "2")) %>% rename(year = year2)

########### DYADIC DATA ##########

### Distance and contiguity data
ddist  <- read_csv("./data/COW_Distance_NewGene_Export.csv")
ddist$dyad <- undirdyads(ddist, ccode1, ccode2)
dcont  <- read_csv("./data/COW_Contiguity_NewGeneExport.csv")
dcont$dyad <- undirdyads(dcont, ccode1, ccode2)
dcont <- dcont[dcont$ccode1 < dcont$ccode2, ]
# dcont$dup <- duplicated(dcont[, c("dyad", "year")])
# dsub <- dcont[dcont$dup == 1, ]
# duplicated(dcont[, c("dyad", "year")])
# dsub <- dcont[order("dyad", "year"), ]
# dsub <- arrange(dcont, dyad, year)


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
ddy <- select(dtrade, ccode1, ccode2, dyad, year, flow1, flow2, smoothflow1, smoothflow2, smoothtotrade)
ddy <- left_join(ddy, select(ddist, dyad, year, ccdistance, mindistance))
ddy <- left_join(ddy, select(dcont, dyad, year, conttype))
ddy <- left_join(ddy, icow_full_dyr)
ddy <- left_join(ddy, select(dally, dyad, year, defense))#neutrality, nonaggression, entente
ddy <- left_join(ddy, dmiddy)
ddy <- left_join(ddy, icow_part_dyr)
ddy <- left_join(ddy, triv)
# Full joins are unnecessary - merging to trade data - lags of this are going to mean that I lose all of the first observations - so losing lags for that first var doesn't matter

######### Merge dyadic and monadic data ###########
dat <- left_join(ddy, dmon1)
dat <- left_join(dat, dmon2)

# Dyadic trade variables
dat$trade = dat$flow1 + dat$flow2
dat$ln_trade = ifelse(dat$trade == 0, 0, log(dat$trade))

# Mean dyadic trade
dat <- ungroup(dat %>% group_by(dyad) %>% mutate(
  mflow1 = mean(flow1),
  mflow2 = mean(flow2),
  mtrade = mean(trade, na.rm = T),
  mlntrade = mean(ln_trade, na.rm = T),
))

# Deviations from mean dyadic trade
dat$trdev = dat$trade - dat$mtrade
dat$lntrdev = dat$ln_trade - dat$mlntrade

# Dependence on global trade
# dat$deptot1 = dat$exptot1/dat$gdp1 * 1000000
# dat$deptot2 = dat$exptot2/dat$gdp2 * 1000000
dat$deptot1 = (dat$exports1 + dat$imports1 - dat$trade) / dat$gdp1
dat$deptot2 = (dat$exports2 + dat$imports2 - dat$trade) /dat$gdp2
dat$deptotmax = rowMaxs(cbind(dat$deptot1, dat$deptot2))
dat$ln_deptot1 = ifelse(dat$deptot1 == 0, 0, log(dat$deptot1))
dat$ln_deptot2 = ifelse(dat$deptot2 == 0, 0, log(dat$deptot2))
dat$ln_deptotmax = ifelse(dat$deptotmax == 0, 0, log(dat$deptotmax))

# Dependence on dyadic trade
dat$depdy1 = dat$trade/dat$gdp1 #depdy1 is 1's dependence on 2 (exports/gdpcap)
dat$depdy2 = dat$trade/dat$gdp2  # higher values indicate that 2 is more dependent on trade
dat$depdymax = rowMaxs(cbind(dat$depdy1, dat$depdy2))
dat$ln_depdy1 = ifelse(dat$depdy1 == 0, 0, log(dat$depdy1))
dat$ln_depdy2 = ifelse(dat$depdy2 == 0, 0, log(dat$depdy2))
dat$ln_depdymax = ifelse(dat$depdymax == 0, 0, log(dat$depdymax))

# Dyadic GDP variables
dat$gdpt = dat$gdp1 + dat$gdp2
dat$lngdpt = log(dat$gdpt)
dat$gdpcapt = dat$gdpcap1 + dat$gdpcap2
dat$lngdpcapt = log(dat$gdpcapt)

# Other dyadic variables
dat$lnccdist <- ifelse(dat$ccdistance == 0, 0, log(dat$ccdistance))
dat$conttype[is.na(dat$conttype)] <- 6
dat$contdir <- ifelse(dat$conttype == 1, 1, 0)
dat$dyterrclaim <- ifelse(is.na(dat$dyterrclaim), 0, 1)
dat$btclaim <- ifelse(is.na(dat$btclaim), 0, 1)
dat$bmclaim <- ifelse(is.na(dat$bmclaim), 0, 1)
dat$brclaim <- ifelse(is.na(dat$brclaim), 0, 1)
dat$fatality <- ifelse(is.na(dat$fatality), 0, dat$fatality)
dat$trival <- ifelse(is.na(dat$trival), 0, 1)

#dat$cyricowsal <- ifelse(!is.na(dat$cyrsal), dat$cyrsal, 0)
dat$mainland <- ifelse(!is.na(dat$mainland), 1, 0)
dat$defense <- ifelse(is.na(dat$defense), 0, dat$defense)
dat$bdymid <- ifelse(is.na(dat$bdymid), 0, 1)
dat$ndymid <- ifelse(is.na(dat$ndymid), 0, 1)
dat$caprat <- rowMaxs(cbind(dat$cinc1, dat$cinc2)) / (dat$cinc1 + dat$cinc2)
dat$demdy <- ifelse(dat$polity21 > 5 & dat$polity22 > 5, 1, 0)
dat$y2 = (dat$year^2) / 1000
dat$y3 = (dat$year^3) / 1000

# Lags
dat <- dat %>% arrange(dyad, year) %>% mutate(
  lcaprat = lag(caprat),
  lgdpcapt = lag(gdpcapt),
  lag_ln_gdpcapt = lag(lngdpcapt),
  lag_trade = lag(trade),
  lag_ln_trade = lag(ln_trade),
  lag_deptot1 = lag(deptot1),
  lag_deptot2 = lag(deptot2),
  lag_deptotmax = lag(deptotmax),
  lag_depdy1 = lag(depdy1),
  lag_depdy2 = lag(depdy2),
  lag_depdymax = lag(depdymax),
  lag_ln_deptot1 = lag(ln_deptot1),
  lag_ln_deptot2 = lag(ln_deptot2),
  lag_ln_deptotmax = lag(ln_deptotmax),
  lag_ln_depdy1 = lag(ln_depdy1),
  lag_ln_depdy2 = lag(ln_depdy2),
  lag_ln_depdymax = lag(ln_depdymax),
  lbdymid = lag(bdymid),
  lndymid = lag(ndymid),
  ldyterrclaim = lag(dyterrclaim),
  lbmclaim = lag(bmclaim),
  lbrclaim = lag(brclaim),
  ldemdy = lag(demdy),
  ldefense = lag(defense), 
  lfatality = lag(fatality)
)
#> with(dat, cor(cbind(ldyterrclaim, lag_ln_gdp1, lag_ln_gdp2, lag_ln_gdpcap1, lag_ln_gdpcap2, contdir, ldefense, lcaprat, lpol1, lpol2, year, y2), use = "complete.obs"))

# write_csv(dat, "./data/TradeInputs.csv")
