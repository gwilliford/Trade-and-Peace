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
dpol = select(dpol, ccode, year, polity2)
dw   = read_dta("./data/bdm2s2_nation_year_data_may2002.dta") %>%
  select(ccode, year, W, S, GovCrises, strikes)
chisols <- read_dta('./data/CHISOLSstyr4_0.dta')
select(dpol, ccode, year, polity2)
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

### Monadic trade
mtrade <- read_csv('./data/National_COW_4.0.csv')
mtrade$importsmil <- mtrade$imports * 1000000
mtrade$exportsmil <- mtrade$exports * 1000000
mtrade$aggtrade <- mtrade$imports + mtrade$exports
mtrade$aggtrademil <- mtrade$importsmil + mtrade$exportsmil
mtrade <- mtrade %>%
  select(ccode, year, imports, exports, importsmil, exportsmil, aggtrade, aggtrademil)

### Leader support data
ead <- read_dta("./data/EAD+2.0+Annual+0101019.dta")

### Merge Monadic Data
dmon = full_join(madd, dcap)
dmon = full_join(dmon, dpol)
dmon = full_join(dmon, chisols)
dmon = full_join(dmon, dw)
dmon = full_join(dmon, ead)
dmon = full_join(dmon, mtrade)

### Monadic Lags
dmon <- dmon %>%
  arrange(ccode, year) %>%
  mutate(
    
    # Trade vars
    ln_aggtrade = ifelse(is.na(aggtrade), 0, log(aggtrade)),
    ln_aggtrademil = ifelse(is.na(aggtrademil), 0, log(aggtrademil)),
    lag_ln_aggtrade = lag(ln_aggtrade),
    lag_ln_aggtrademil = lag(ln_aggtrademil),
    
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
    lag_pop = lag(pop),
    ln_pop = log(pop),
    lag_ln_pop = lag(ln_pop),
    
    # Institutional vars
    lpol = lag(polity2), 
    lagW = lag(W),
    lagS = lag(S),
  )

# Check for duplicates
sum(duplicated(dmon[, c("ccode", "year")]))
dmon[duplicated(dmon[, c("ccode", "year")]),]

# Create separate versions of monadic data
dmon1 = dmon %>% select(-"version") %>% setNames(paste0(names(.), "1")) %>% rename(year = year1)
dmon2 = dmon %>% select(-"version") %>% setNames(paste0(names(.), "2")) %>% rename(year = year2)

########### DYADIC DATA ##########
ddist  <- read_csv("./data/COW_Distance_NewGene_Export.csv") %>%
  select(ccode1, ccode2, year, ccdistance, mindistance)
dcont  <- read_csv("./data/COW_Contiguity_NewGeneExport.csv") %>%
  select(ccode1, ccode2, year, conttype)
dcont <- dcont[dcont$ccode1 < dcont$ccode2, ]
triv <- read_dta("./data/ThompsonDyadYear.dta")
dally <- read_csv("./data/alliance_v4.1_by_dyad_yearly.csv")
dally$dyad <- undirdyads(dally, ccode1, ccode2)
dally <- dally %>%
  group_by(dyad, year) %>%
  summarize(
    defense = sum(defense, na.rm = T)
  ) %>% 
  select(dyad, year, defense)#neutrality, nonaggression, entente

### Gibler MID data
dmid <- read_csv("./data/gml-ndy-disputes-2.0.csv")
dmid$dyad <- undirdyads(dmid, ccode1, ccode2)
dmid$dmidyr = 1
dmid$fatality <- replace(dmid$fatality, dmid$fatality == -9, NA)
dmiddy <- dmid %>%
  group_by(dyad, year) %>%
  summarize(
    ndymid = sum(dmidyr, na.rm = T),
    bdymid = 1, 
    fatality = max(fatality)
  )
# cmid1 <- dmid %>% 
#   group_by(ccode1, year) %>%
#   summarize(
#     nmidcyr1 = sum(dmidyr, na.rm = T),
#     bmidcyr1 = 1
#   ) %>%
#   select("ccode1" = "ccode", year, nmidcyr1, bmidcyr1)
# cmid2 <- dmid %>%
#   group_by(ccode2, year) %>%
#   summarize(
#     nmidcyr2 = sum(dmidyr, na.rm = T),
#     bmidcyr2 = 1
#   ) %>% 
#   select("ccode2" = "ccode", year, nmidcyr2, bmidcyr2)
# cmida <- full_join(cmid1, cmid2) %>%
#   mutate(
#     nmidcyr = nmidcyr1 + nmidcyr2,
#     bmidcyr = bmidcyr1 + bmidcyr2
#   ) %>%
#   select(ccode, year, nmidcyr, bmidcyr)
# 

dtrade <- read_csv("./data/Dyadic_COW_4.0.csv")
dtrade$dyad = undirdyads(dtrade, ccode1, ccode2)
dtrade$flow1 <- ifelse(dtrade$flow1 < 0, NA, dtrade$flow1)
dtrade$flow2 <- ifelse(dtrade$flow2 < 0, NA, dtrade$flow2)
dtrade$smoothtotrade <- ifelse(dtrade$smoothtotrade < 0, NA, dtrade$smoothtotrade)# * 1000000
dtrade$flow1mil <- dtrade$flow1 * 1000000
dtrade$flow2mil <- dtrade$flow2 * 1000000
dtrade$smoothtotrademil <- dtrade$smoothtotrade * 1000000
dtrade <- dtrade %>% select(ccode1, ccode2, dyad, year, flow1, flow2,
                            smoothflow1, smoothflow2, 
                            "trade" = "smoothtotrade",
                            flow1mil, flow2mil, "trademil" = "smoothtotrademil")

### Merge dyadic data
ddy <- left_join(dtrade, ddist) 
ddy <- left_join(ddy, dcont)
# ddy <- left_join(ddy, icow_full_dyr)
# ddy <- left_join(ddy, icow_part_dyr)
ddy <- left_join(ddy, dally)
ddy <- left_join(ddy, dmiddy)
ddy <- left_join(ddy, triv)
ddy <- left_join(ddy, igo)
# Full joins are unnecessary - merging to trade data - lags of this are going to mean that I lose all of the first observations - so losing lags for that first var doesn't matterW%$

######### Merge dyadic and monadic data ###########
dat <- left_join(ddy, dmon1)
dat <- left_join(dat, dmon2)
dat <- rename(dat, 'polity1' = 'polity21', 'polity2' = 'polity22')

# Dyadic trade variables
dat$ln_trade = ifelse(dat$trade == 0, 0, log(dat$trade))
dat$ln_trademil = ifelse(dat$trademil == 0, 0, log(dat$trademil))
dat <- ungroup(dat %>% group_by(dyad) %>% mutate(
  mflow1 = mean(flow1),
  mflow2 = mean(flow2),
  mtrade = mean(trade, na.rm = T),
  mtrademil = mean(trademil),
  mln_trade = mean(ln_trade, na.rm = T),
  mln_trademil = mean(ln_trademil, na.rm = T)
))
dat$aggtrademin = rowMins(cbind(dat$aggtrade1, dat$aggtrade2))
dat$aggtradenax = rowMaxs(cbind(dat$aggtrade1, dat$aggtrade2))

# Deviations from mean dyadic trade
dat$trdev = dat$trade - dat$mtrade
dat$ln_trdev = dat$ln_trade - dat$mln_trade

dat$trdevmil = dat$trademil - dat$mtrademil
dat$ln_trdevmil = dat$ln_trademil - dat$mln_trademil

# Monadic dependence on global trade
dat$deptot1 = dat$aggtrade1 / dat$gdp1
dat$deptot2 = dat$aggtrade2 / dat$gdp2
dat$deptotmin = rowMins(cbind(dat$deptot1, dat$deptot2))
dat$deptotmax = rowMaxs(cbind(dat$deptot1, dat$deptot2))

dat$deptotmil1 = dat$aggtrademil1 / dat$gdp1
dat$deptotmil2 = dat$aggtrademil2 /dat$gdp2
dat$deptotmilmin = rowMins(cbind(dat$deptotmil1, dat$deptotmil1))
dat$deptotmilmax = rowMaxs(cbind(dat$deptotmil1, dat$deptotmil2))

dat$ln_deptot1 = ifelse(dat$deptot1 == 0, 0, log(dat$deptot1))
dat$ln_deptot2 = ifelse(dat$deptot2 == 0, 0, log(dat$deptot2))
dat$ln_deptotmin = ifelse(dat$deptotmin == 0, 0, log(dat$deptotmin))
dat$ln_deptotmax = ifelse(dat$deptotmax == 0, 0, log(dat$deptotmax))

dat$ln_deptotmil1 = ifelse(dat$deptotmil1 == 0, 0, log(dat$deptotmil1))
dat$ln_deptotmil2 = ifelse(dat$deptotmil2 == 0, 0, log(dat$deptotmil2))
dat$ln_deptotmilmin = ifelse(dat$deptotmilmin == 0, 0, log(dat$deptotmilmin))
dat$ln_deptotmilmax = ifelse(dat$deptotmilmax == 0, 0, log(dat$deptotmilmax))

# Monadic dependence on trade with other countries besides other dyad member
dat$depoth1 = dat$aggtrade1 - dat$trade / dat$gdp1
dat$depoth2 = dat$aggtrade2 / dat$gdp2
dat$depothmin = rowMins(cbind(dat$depoth1, dat$depoth2))
dat$depothmax = rowMaxs(cbind(dat$depoth1, dat$depoth2))

dat$depothmil1 = dat$aggtrademil1 - dat$trademil / dat$gdp1
dat$depothmil2 = dat$aggtrademil2 - dat$trademil / dat$gdp2
dat$depothmilmin = rowMins(cbind(dat$depothmil1, dat$depothmil2))
dat$depothmilmax = rowMaxs(cbind(dat$depothmil1, dat$depothmil2))

dat$ln_depoth1 = ifelse(dat$depoth1 == 0, 0, log(dat$depoth1))
dat$ln_depoth2 = ifelse(dat$depoth2 == 0, 0, log(dat$depoth2))
dat$ln_depothmin = ifelse(dat$depothmin == 0, 0, log(dat$depothmin))
dat$ln_depothmax = ifelse(dat$depothmax == 0, 0, log(dat$depothmax))

dat$ln_depothmil1 = ifelse(dat$depothmil1 == 0, 0, log(dat$depothmil1))
dat$ln_depothmil2 = ifelse(dat$depothmil2 == 0, 0, log(dat$depothmil2))
dat$ln_depothmilmin = ifelse(dat$depothmil1 == 0, 0, log(dat$depothmil2))
dat$ln_depothmilmax = ifelse(dat$depothmil1 == 0, 0, log(dat$depothmil2))

# dat$b1 = dat$ln_agg1 / dat$ln_gdp1
# dat$b2 = dat$ln_agg2 / dat$ln_gdp2
# dat$bmin = rowMins(cbind(dat$b1, dat$b2))
# dat$bmax = rowMaxs(cbind(dat$b1, dat$b2))
# dat$c1 = (dat$ln_agg1 - dat$ln_trade)/ dat$ln_gdp1
# dat$c2 = (dat$ln_agg2 - dat$ln_trade)/ dat$ln_gdp2
# dat$cmin = rowMins(cbind(dat$c1, dat$c2))
# dat$cmax = rowMaxs(cbind(dat$c1, dat$c2))

# Dependence on dyadic trade
dat$depdy1 = dat$trade/dat$gdp1 #depdy1 is 1's dependence on 2 (exports/gdpcap)
dat$depdy2 = dat$trade/dat$gdp2  # higher values indicate that 2 is more dependent on trade
dat$depdymax = rowMaxs(cbind(dat$depdy1, dat$depdy2))
dat$depdymin = rowMins(cbind(dat$depdy1, dat$depdy2))

dat$depdymil1 = dat$trademil/dat$gdp1 #depdy1 is 1's dependence on 2 (exports/gdpcap)
dat$depdymil2 = dat$trademil/dat$gdp2  # higher values indicate that 2 is more dependent on trade
dat$depdymilmin = rowMins(cbind(dat$depdymil1, dat$depdymil2))
dat$depdymilmax = rowMaxs(cbind(dat$depdymil1, dat$depdymil2))

dat$ln_depdy1 = ifelse(dat$depdy1 == 0, 0, log(dat$depdy1))
dat$ln_depdy2 = ifelse(dat$depdy2 == 0, 0, log(dat$depdy2))
dat$ln_depdymax = ifelse(dat$depdymax == 0, 0, log(dat$depdymax))
dat$ln_depdymin = ifelse(dat$depdymin == 0, 0, log(dat$depdymin))

dat$ln_depdymil1 = ifelse(dat$depdymil1 == 0, 0, log(dat$depdymil1))
dat$ln_depdymil2 = ifelse(dat$depdymil2 == 0, 0, log(dat$depdymil2))
dat$ln_depdymilmin = ifelse(dat$depdymilmin == 0, 0, log(dat$depdymilmin))
dat$ln_depdymilmax = ifelse(dat$depdymilmax == 0, 0, log(dat$depdymilmax))

# dat$a1 <- dat$ln_trademil - dat$ln_gdp1
# dat$a2 <- dat$ln_trademil - dat$ln_gdp2
# dat$amin <- rowMins(cbind(dat$a1, dat$a2))
# dat$amax <- rowMaxs(cbind(dat$a1, dat$a2))

# The log of a quotient is the difference of the logs.
# 
# loga (x/y) = loga x - loga y

# summary(dat$ln_trademil - dat$ln_gdp1)
# summary(log(dat$ln_trademil/dat$ln_gdp1))

# Dyadic GDP variables
dat$gdpmin = rowMins(cbind(dat$gdp1, dat$gdp2))
dat$gdpmax = rowMaxs(cbind(dat$gdp1, dat$gdp2))
dat$gdpt = dat$gdp1 + dat$gdp2

dat$ln_gdpmin = rowMins(cbind(dat$ln_gdp1, dat$ln_gdp2))
dat$ln_gdpmax = rowMaxs(cbind(dat$ln_gdp1, dat$ln_gdp2))
dat$ln_gdpt = log(dat$gdpt)

dat$ln_gdpcapmin = rowMins(cbind(dat$ln_gdpcap1, dat$ln_gdpcap2))
dat$ln_gdpcapmax = rowMaxs(cbind(dat$ln_gdpcap1, dat$ln_gdpcap2))
dat$gdpcapt = dat$gdpcap1 + dat$gdpcap2

dat$ln_gdpcapmin = rowMins(cbind(dat$ln_gdpcap1, dat$ln_gdpcap2))
dat$ln_gdpcapmax = rowMaxs(cbind(dat$ln_gdpcap1, dat$ln_gdpcap2))
dat$ln_gdpcapt = log(dat$gdpcapt)

# percent change gdp dyadic
# dat$pch_gdpmin = rowMins(cbind(dat$pch_gdp1, dat$pch_gdp2))

# Other dyadic variables
dat$lnccdist <- ifelse(dat$ccdistance == 0, 0, log(dat$ccdistance))
dat$conttype[is.na(dat$conttype)] <- 6
dat$contdir <- ifelse(dat$conttype == 1, 1, 0)
dat$ndymid <- ifelse(is.na(dat$ndymid), 0, dat$ndymid)
dat$bdymid <- ifelse(is.na(dat$bdymid), 0, dat$bdymid)
dat$fatality <- ifelse(is.na(dat$fatality), 0, dat$fatality)
dat$trival <- ifelse(is.na(dat$trival), 0, 1)
dat$caprat <- rowMaxs(cbind(dat$cinc1, dat$cinc2)) / (dat$cinc1 + dat$cinc2)
dat$ln_caprat <- ifelse(dat$caprat == 0, 0, log(dat$caprat))
dat$polmin <- rowMins(cbind(dat$polity1, dat$polity2))
dat$polmax <- rowMaxs(cbind(dat$polity1, dat$polity2))
dat$demdy <- ifelse(dat$polity1 > 5 & dat$polity2 > 5, 1, 0)
dat$autdy <- ifelse(dat$polity1 < -5 & dat$polity2 < -5, 1, 0)
dat$samereg <- ifelse(dat$demdy == 1 | dat$autdy == 1, 1, 0)
dat$ysquare = (dat$year^2) / 1000
dat$ycubed  = (dat$year^3) / 1000
dat$Wmin = rowMins(cbind(dat$W1, dat$W2))
dat$Wmax = rowMaxs(cbind(dat$W1, dat$W2))
dat$govcrisesdy = ifelse(dat$GovCrises1 > 0 | dat$GovCrises2 > 0, 1, 0)

# Lags
datlag <- dat %>% arrange(dyad, year) %>% mutate(
  lcaprat = lag(caprat),
  lag_ln_caprat = lag(ln_caprat),
  pchcaprat = lag(caprat) / caprat,
  lag_pchcaprat = lag(pchcaprat),
  
  lgdpcapt = lag(gdpcapt),
  lag_ln_gdpcapt = lag(ln_gdpcapt),
  lag_ln_gdpmin = lag(ln_gdpmin),
  lag_ln_gdpmax = lag(ln_gdpmax),
  
  lag_ln_gdpcapmin = lag(ln_gdpcapmin),
  lag_ln_gdpcapmax = lag(ln_gdpcapmax),
  
  lag_trade = lag(trade),
  lag_ln_trade = lag(ln_trade),
  lag_trademil = lag(trademil),
  lag_ln_trademil = lag(ln_trademil),
  
  lag_deptot1 = lag(deptot1),
  lag_deptot2 = lag(deptot2),
  lag_deptotmin = lag(deptotmin),
  lag_deptotmax = lag(deptotmax),
  
  lag_ln_deptot1 = lag(ln_deptot1),
  lag_ln_deptot2 = lag(ln_deptot2),
  lag_ln_deptotmax = lag(ln_deptotmax),
  lag_ln_deptotmin = lag(ln_deptotmin),
  
  lag_deptotmil1 = lag(deptotmil1),
  lag_deptotmil2 = lag(deptotmil2),
  lag_deptotmilmin = lag(deptotmilmin),
  lag_deptotmilmax = lag(deptotmilmax),
  
  lag_ln_deptotmil1 = lag(ln_deptotmil1),
  lag_ln_deptotmil2= lag(ln_deptotmil2),
  lag_ln_deptotmilmin = lag(ln_deptotmilmin),
  lag_ln_deptotmilmax = lag(ln_deptotmilmax),
  
  lag_ln_depoth1 = lag(ln_depoth1),
  lag_ln_depoth2 = lag(ln_depoth2),
  lag_depothmin = lag(depothmin),
  lag_depothmilmin = lag(depothmilmin),
  
  lag_ln_depothmin = lag(ln_depothmin),
  lag_ln_depothmax = lag(ln_depothmax),
  
  lag_ln_depothmil1 = lag(ln_depothmil1),
  lag_ln_depothmil2 = lag(ln_depothmil2),
  lag_ln_depothmilmin = lag(ln_depothmilmin),
  lag_ln_depothmilmax = lag(ln_depothmilmax),
  
  lag_depdy1 = lag(depdy1),
  lag_depdy2 = lag(depdy2),
  lag_depdymin = lag(depdymin),
  lag_depdymax = lag(depdymax),
  
  lag_ln_depdy1 = lag(ln_depdy1),
  lag_ln_depdy2 = lag(ln_depdy2),
  lag_ln_depdymin = lag(ln_depdymin),
  lag_ln_depdymax = lag(ln_depdymax),
  
  lag_depdymil1 = lag(depdymil1),
  lag_depdymil2 = lag(depdymil2),
  lag_depdymilmax = lag(depdymilmax),
  lag_depdymilmin = lag(depdymilmin),
  
  lag_ln_depdymil1 = lag(ln_depdymil1),
  lag_ln_depdymil2 = lag(ln_depdymil2),
  lag_ln_depdymilmin = lag(ln_depdymilmin),
  lag_ln_depdymilmax = lag(ln_depdymilmax),
  
  lag_pch_ln_gdp1 = lag(pch_ln_gdp1),
  lag_pch_ln_gdp2 = lag(pch_ln_gdp2),
  lag_pch_ln_gdpcap1 = lag(pch_ln_gdpcap1),
  lag_pch_ln_gdpcap2 = lag(pch_ln_gdpcap2),
  # lag_pch_gdpmin = lag(pch_gdpmin),
  
  lbdymid = lag(bdymid),
  lndymid = lag(ndymid),
  lfatality = lag(fatality),
  
  ldemdy = lag(demdy),
  ldefense = lag(defense), 
  
  lgovcrises1 = lag(GovCrises1),
  lgovcrises2 = lag(GovCrises2),
  lgovcrisesDy = lag(govcrisesdy)
)

write_rds(dat, "./data/TradeInputs.RDS")

#> with(dat, cor(cbind(ldyterrclaim, lag_ln_gdp1, lag_ln_gdp2, lag_ln_gdpcap1, lag_ln_gdpcap2, contdir, ldefense, lcaprat, lpol1, lpol2, year, y2), use = "complete.obs"))
# 
# write_csv(dat, "./data/TradeInputs.csv")
# write_rds(dat, "./data/TradeInputs.RDS")
# region codes
# ceuro <- icow_part_cyr[icow_part_cyr$region == 2, "chal"]
# teuro <- icow_part_cyr[icow_part_cyr$region == 2, "tgt"]
# aeuro <- unlist(c(ceuro, teuro))
# sort(unique(aeuro))
# dat$sub  <- ifelse(dat$ccode1 %in% 1:330 | dat$ccode2 %in% 1:330, 1, 0)
# dat$subr <- ifelse(dat$ccode1 %in% c(1:330, 600:699) | dat$ccode2 %in% c(1:330, 600:699), 1, 0)

# dsub <- na.omit(filter(dat, sub == 1 & year >= 1900) %>% select(ln_trade, anyclaim, lag_ln_gdpmin, lag_ln_gdpmax, lag_ln_gdpcapmin, lag_ln_gdpcapmax, contdir, ldefense, lcaprat, polmin, polmax, dyad, year))
# a <- opm(ln_trademil ~ anyclaim + lag_ln_gdpmin + lag_ln_gdpmax + lag_ln_gdpcapmin + lag_ln_gdpcapmax +
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

# datout <- select(dat, dyad, year, lag_pch_gdpmin, lGovCrisesDy, polmin, polmax, autdy, demdy, samereg, Wmin, Wmax, lag_depdymin, lag_depdymin100, lag_deptotmin, lag_deptotmin100, lag_depothmin, lag_depothmin100, lag_ln_depdymin100, lamin, lamax, lbmin, lbmax, lcmin, lcmax, lcaprat, lpchcaprat, ldefense, contdir, trival, igosum)
