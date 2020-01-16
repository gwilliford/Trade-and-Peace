# Estimating Unrealized Gains from Trade
setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis - Trade Models")
library(readr)
library(readxl)
library(haven)
library(dplyr)
library(matrixStats)
library(lme4)

# Import Monadic Data
dgdp   = read_dta("gdpcapfinal2017_01_23.dta")
dgdp   = mutate(dgdp, ccode = as.numeric(ccode), year = as.numeric(year), gdpcap = as.numeric(gdpcap), lgdpcap, as.numeric(lgdpcap))
dcap   = read_dta("NMC_5_0.dta")
dpol   = read_excel("p4v2016.xls")

# Merge Monadic Data
dmon = select(dgdp, ccode, year, gdpcap, lgdpcap)
dmon = left_join(dmon, dcap)
dmon = left_join(dmon, select(dpol, ccode, year, polity, polity2))

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

### ICOW data (global territory data)
dicow  = read_csv("ICOWprovyr101.csv")
dicowcol = dicow %>% group_by(dyad, year) %>% summarize(
  dyterrclaim = 1  
)

### Alliance data
dally <- read_csv("alliance_v4.1_by_dyad_yearly.csv")
dally$dyad <- undirdyads(dally, ccode1, ccode2)

### MID data
dmid <- read_csv("gml-ndy-disputes-2.0.csv")
dmid$dyad <- undirdyads(dmid, ccode1, ccode2)
dmiddy <- dmid %>% group_by(dyad, year) %>% summarize(
  mid = 1
)

### Merge dyadic data
ddy <- select(dtrade, ccode1, ccode2, dyad, year, flow1, flow2, smoothflow1, smoothflow2, smoothtotrade)
ddy <- full_join(ddy, select(ddist, dyad, year, ccdistance, mindistance))
ddy <- full_join(ddy, select(dcont, dyad, year, conttype))
ddy <- full_join(ddy, dicowcol)
ddy <- full_join(ddy, select(dally, dyad, year, defense, neutrality, nonaggression, entente))
ddy <- full_join(ddy, dmiddy)

######### Merge dyadic and monadic data ###########
dat <- left_join(ddy, dmon1)
dat <- left_join(dat, dmon2)

# Create additional variables
dat$trade <- dat$flow1 + dat$flow2
dat$dyterrclaim <- ifelse(!is.na(dat$dyterrclaim), 1, 0)
dat$conttype[is.na(dat$conttype)] <- 6
dat$gdpcomb <- dat$gdpcap1 + dat$gdpcap2
dat$tgdp <- dat$gdpcap1 + dat$gdpcap2
dat$lngdpcomb <- log(dat$gdpcomb)
dat$lntrade <- ifelse(dat$trade == 0, 0, log(dat$trade))
dat$lnccdist <- ifelse(dat$ccdistance == 0, 0, log(dat$ccdistance))
dat$defense <- ifelse(is.na(dat$defense), 0, dat$defense)
dat$mid <- ifelse(is.na(dat$mid), 0, 1)
dat$caprat <- rowMaxs(cbind(dat$cinc1, dat$cinc2)) / (dat$cinc1 + dat$cinc2)
dat$demdy <- ifelse(dat$polity21 > 5 & dat$polity22 > 5, 1, 0)

# dat$open <- dat$trade / dat$tgdp

# Import PTA/RTA data
# Import GDP data
# Other controls - war/mid
# Trade may be driven by - UN votes - alliances - joint democracy - general trade ooppennes - mitliary disputes - ln cap distance - ln combined population - ln combined gdp - shared border
  
## Recode missings
## Standardize econ variables
# gdp 
# trade

# OLS
# tm1 <- lm(trade ~ dyterrclaim + gdpcomb + (conttype == 1), data = dat); summary(tm1)
# tm1 <- lm(lntrade ~ dyterrclaim + lngdpcomb + (conttype == 1), data = dat); summary(tm1)
# tm1 <- lmer(trade ~ dyterrclaim + gdpcomb + (conttype == 1) + defense + mid + open + (1 | dyad), data = dat); summary(tm1)
#tm1 <- lmer(trade ~ dyterrclaim + gdpcomb + (conttype == 1) + (dyterrclaim | dyad), data = dat); summary(tm1)
tm1 <- lmer(lntrade ~ dyterrclaim + lngdpcomb + (conttype == 1) + defense + mid + caprat + demdy + (1 | dyad), data = dat); summary(tm1)


dsub1 <- dat[dat$dyterrclaim == 1, ]
dsub0 <- dsub1
dsub0$dyterrclaim <- 0
#predict.merMod
pred1 <- predict(tm1, dsub1, allow.new.levels = T)
pred0 <- predict(tm1, dsub0, allow.new.levels = T)
# why does this require new levels?


predout <- with(dsub1, as.data.frame(cbind(dyad, year, lntrade, pred1, pred0)))

# Validity Tests
hist(predout$lntrade) # larger values - regularized
hist(predout$pred1)
var(predout$lntrade, na.rm = T) # larger variance
var(predout$pred1, na.rm = T)
median(predout$lntrade, na.rm = T) # medians pretty similar
median(predout$pred1, na.rm = T)
mean(predout$lntrade, na.rm = T) # Means pretty similar
mean(predout$pred1, na.rm = T)
t.test(mean(predout$lntrade), mean(predout$pred1)) # means are not statistically distinct
t.test(predout$lntrade, predout$pred1)
library(coin)
a <- c(predout$lntrade, predout$pred1)
b <- c(rep(0, length(a)/2), rep(1, length(a)/2))
d <- as.data.frame(cbind(a, b))
d <- na.omit(d)
median_test(a ~ as.factor(b), d)

# Unrealized trade
predout$ugtpred <- predout$pred0 - predout$pred1
predout$ugtobs <- predout$pred0 - predout$lntrade

write.csv("")

# Trade data is in US millions of current dollars (2014?)
# Maddison GDP Data is in 2011 dollars

#Neural Net
# library(neuralnet)
# nn = neuralnet(Placed~TKS+CSS,data=df, hidden=3,act.fct = "logistic",
#              linear.output = FALSE)
# library(neuralnet)
# datnarm <- na.omit(dat)
# nn = neuralnet(trade ~ gdpcomb + dyterrclaim + conttype, data = datnarm)



