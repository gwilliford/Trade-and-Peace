setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis")
library(readr)
library(haven)
library(dplyr)

library(tibble)
library(DescTools)

if(!exists("dat")) dat <- load("./data/TradeOut.Rdata")

dat$terugt <- dat$tsim - dat$ln_trade
dat$marugt <- dat$msim - dat$ln_trade
dat$rivugt <- dat$rsim - dat$ln_trade
dat$terugtdep1 <- dat$terugt / dat$ln_gdp1
dat$terugtdep2 <- dat$terugt / dat$ln_gdp2
dat$terugtdepmax <- rowMaxs(cbind(dat$terugtdep1, dat$terugtdep2))
dat$rivugtdep1 <- dat$rivugt / dat$ln_gdp1
dat$rivugtdep2 <- dat$rivugt / dat$ln_gdp2
dat$rivugtdepmax <- rowMaxs(cbind(dat$rivugtdep1, dat$rivugtdep2))
dat$marugtdep1 <- dat$marugt/ dat$ln_gdp1
dat$marugtdep2 <- dat$marugt / dat$ln_gdp2
dat$marugtdepmax <- rowMaxs(cbind(dat$marugtdep1, dat$marugtdep2))

dat <- dat %>% arrange(dyad, year) %>% mutate(
  lterugt = lag(terugt),
  lterugtdep1 = lag(terugtdep1),
  lterugtdep2 = lag(terugtdep2),
  lterugtdepmax = lag(terugtdepmax),
  lrivugt = lag(rivugt),
  lrivugtdep1 = lag(rivugtdep1),
  lrivugtdep2 = lag(rivugtdep2),
  lrivugtdepmax = lag(rivugtdepmax),
  lmarugt = lag(marugt),
  lmarugtdep1 = lag(marugtdep1),
  lmarugtdep2 = lag(marugtdep2),
  lmarugtdepmax = lag(marugtdepmax)
)

# ##### Predicted trade values
#   dat$terugt <- dat$tsim - dat$ln_trade
#   dat$marugt <- dat$msim - dat$ln_trade
#   dat$rivugt <- dat$rsim - dat$ln_trade
#   
#   # sum(!is.na(dat[dat$ldyterrclaim == 1, "ugt"]))
#   # sum(!is.na(icow_cyr_part_out[icow_cyr_part_out$ldyterrclaim == 1, "ugt"]))
#   
#   dat$terugtdep1 <- dat$terugt / dat$ln_gdp1
#   dat$terugtdep2 <- dat$terugt / dat$ln_gdp2
#   dat$terugtdepmax <- rowMaxs(cbind(dat$terugtdep1, dat$terugtdep2))
#   dat$rivugtdep1 <- dat$terugt / dat$ln_gdp1
#   dat$rivugtdep2 <- dat$terugt / dat$ln_gdp2
#   dat$rivugtdepmax <- rowMaxs(cbind(dat$rivugtdep1, dat$rivugtdep2))
#   dat$marugtdep1 <- dat$terugt / dat$ln_gdp1
#   dat$marugtdep2 <- dat$terugt / dat$ln_gdp2
#   dat$marugtdepmax <- rowMaxs(cbind(dat$marugtdep1, dat$marugtdep2))

##### ICOW settlement data
icow_set <- read_dta("./data/ICOWsettle.dta")
icow_set <- icow_set %>% filter(midiss == 0) # drop mids and non-territorial claims

icow_set <- dplyr::select(icow_set, -mid)
icow_set$agreeiss <- ifelse(is.na(icow_set$agreeiss), 0, icow_set$agreeiss)

##### Collapse icow settlement data to dyad year format
icow_set_dyr <- ungroup(icow_set %>% group_by(dyad, year) %>% summarize (
  sagree = sum(agree),
  sagreeiss = sum(agreeiss)
))

# Was an agreement reached?
icow_set_dyr$bagree <- ifelse(icow_set_dyr$sagree > 0, 1, 0)
icow_set_dyr$bagreeiss <- ifelse(icow_set_dyr$sagreeiss > 0, 1, 0)

# Was an agreement that ended the claim reached?
icow_set$ag_end_any  <- ifelse(icow_set$agreeiss == 1 & (icow_set$claimend == 1:2), 1, 0)
icow_set$ag_end_part <- ifelse(icow_set$agreeiss == 1 & icow_set$claimend == 1, 1, 0)
icow_set$ag_end_full <- ifelse(icow_set$agreeiss == 1 & icow_set$claimend == 2, 1, 0)

##### ICOW aggregate claim data 
icow_claimdy <- read_dta("./data/ICOWclaimdy.dta")
icow_claimdy <- icow_claimdy # %>% filter(terriss == 1)
icow_claimdy$reschdrop = ifelse(icow_claimdy$resolved %in% c(1, 2), 1, 0)
icow_claimdy$resmil = ifelse(icow_claimdy$resolved == 7, 1, 0)
icow_claimdy$restgtdrop = ifelse(icow_claimdy$resolved %in% c(8, 9), 1, 0)
icow_claimdy$respleb = ifelse(icow_claimdy$resolved == 10, 1, 0)
icow_claimdy$respset = ifelse(icow_claimdy$resolved %in% c(4, 12, 13, 14), 1, 0)
icow_claimdy$resother = ifelse(icow_claimdy$resolved %in% c(5, 6, 11), 1, 0) # independence, actor leaves system, disp territory no longer exists


##### ICOW Partial Claim-Year Data
icow_part_cyr <- read_dta("./data/ICOWdyadyr.dta")
icow_part_cyr <- filter(icow_part_cyr, chal != 2220 & tgt != 2200)
icow_part_cyr <- ungroup(icow_part_cyr %>% group_by(dyad) %>% mutate(
  cltermyr = max(year)
))
icow_part_cyr$clstart <- icow_part_cyr$year - icow_part_cyr$year + 1
icow_part_cyr$clstop <- icow_part_cyr$clstart + 1
icow_part_cyr$clterm <- ifelse(icow_part_cyr$year == icow_part_cyr$cltermyr, 1, 0)

# 
# coxph(Surv(clstart, clstop, clterm) ~ icowsal + demdy + lterugtdepmax, data = icow_part_cyr, subset = terriss == 1)
# coxph(Surv(y0, year, clterm) ~ icowsal + demdy + lterugtdepmax, data = icow_part_cyr, subset = terriss == 1)
# 
# 
# Code resolution type variable
# icow_part_cyr <- left_join(icow_part_cyr, dplyr::select(icow_claimdy, claimdy, reschdrop, resmil, restgtdrop, respleb, respset, resother))
# icow_part_cyr$chdrop = ifelse(icow_part_cyr$reschdrop == 1 & icow_part_cyr$year == icow_part_cyr$fyear, 1, 0)
# icow_part_cyr$milres = ifelse(icow_part_cyr$resmil == 1 & icow_part_cyr$year == icow_part_cyr$fyear, 1, 0)
# icow_part_cyr$tgtdrop = ifelse(icow_part_cyr$restgtdrop == 1 & icow_part_cyr$year == icow_part_cyr$fyear, 1, 0)
# icow_part_cyr$pleb = ifelse(icow_part_cyr$respleb == 1 & icow_part_cyr$year == icow_part_cyr$fyear, 1, 0)
# icow_part_cyr$pset = ifelse(icow_part_cyr$respset == 1 & icow_part_cyr$year == icow_part_cyr$fyear, 1, 0)
# icow_part_cyr$other = ifelse(icow_part_cyr$resother == 1 & icow_part_cyr$year == icow_part_cyr$fyear, 1, 0)

# ICOW compliance coding
# Claimdy is already separate 
# icow_part_cyr <- left_join(icow_part_cyr, icow_set, by = c("claimdy", "year"), suffix = c("", ".y"))
# icow_part_cyr$agreeiss <- ifelse(is.na(icow_part_cyr$agreeiss), 0, icow_part_cyr$agreeiss)
# # icow_part_cyr <- 
# #   cumagree = cumsum(agreeiss), 
# #   cummid = cumsum(midissyr),
# #   # agch = as.numeric(agreeiss == 1 & cumagree != lag(cumagree)),
# #   # agch = ifelse(is.na(agch), 0, agch)
# #   # aggyr = ifelse(aggiss == 1, )
# #   clbegyear = min(year), 
# #   tyr = year - clbegyear,
# #   agbegyr = as.numeric(agreeiss == 1),
# #   cat = cumagree + 1,
# #   one = 1
# #   #ifelse(agreeiss == 1, )
# # )
# icow_part_cyr <- ungroup(icow_part_cyr %>% arrange(claimdy, year) %>% group_by(claimdy) %>% mutate(
#   cumagree = cumsum(agreeiss),
#   cummid = cumsum(midissyr),
#   cat = cumagree + 1
# ))
# icow_part_cyr$clagnum = as.numeric(paste(icow_part_cyr$claimdy, icow_part_cyr$cat, sep = ""))
# # icow_part_cyr_clagnum = icow_part_cyr %>% group
# # icow_part_cyr = ungroup(icow_part_cyr %>% group_by(claimdy, cat) %>% mutate(
# #   agyr = cumsum(one)
# # ))
# icow_part_cyr <- icow_part_cyr %>% group_by(clagnum) %>% mutate(
#     clagcummid = cumsum(midissyr),
#     maxclagyr = max(year)
# )
# 
# icow_part_cyr$agfailmid <- ifelse(icow_part_cyr$clagcummid > 0 & icow_part_cyr$cumagree > 1 & icow_part_cyr$midissyr, 1, 0)
# 
# # icow_part_clagsum <- icow_part_cyr %>% group_by(clagnum) %>% summarize(
# # )
# # icow_part_cyr = left_join(icow_part_cyr, select(icow_part_clag, clagnum, year, icow_part_clag))
# icow_part_cyr$clagnummid = as.numeric(with(icow_part_clag, paste(clagnum, clagcummid, sep = "")))
# icow_part_cyr_mid = icow_part_cyr %>% group_by(clagnummid) %>% summarize(
#   begmid = min(year)
# )
# icow_part_cyr = left_join(icow_part_cyr, icow_part_cyr_mid)
# icow_part_cyr$claglastmid <- icow_part_cyr$year - icow_part_cyr$begmid
# View(icow_part_cyr[, c("claimdy", "clagnum", "clagnummid", "clagmidyr", "year", "begmid", "agreeiss", "midissyr", "cumagree", "cummid")])
# 
# icow_part_midyr = icow_part_cyr %>% group_by(clagnummid) %>% summarize(
#   maxmidyr = max(year),
#   # maxagree = max(cumagree),
#   # midmin = min(cummid)
#   # maxcummid = max(cummid),
#   # maxyr = ifelse(maxcummid == 0, )
# )
# icow_part_cyr <- left_join(icow_part_cyr, icow_part_midyr)
# icow_part_cyr$agfailmid = ifelse(icow_part_cyr$year == icow_part_cyr$maxyr & icow_part_cyr$cummid)
# maxclagyr 
# # icow_part_cyr$agfailmid <- with(icow_part_cyr, ifelsemaxagree > cumagree
# # icow_part_cyr <- left_join(icow_part_cyr, icow_part_clag)
# icow_part_cyr$claglastyr = icow_part_cyr$year - icow_part_cyr$clagbegyr
# icow_part_cyr <- icow_part_cyr %>% group_by(clagnum) %>% mutae(
# )


### ICOW Partial Dyad-Year Data
# icow_part_dyr <- icow_part_cyr %>% group_by(dyad, year) %>% summarize(
#   npeace = sum(attemptsp),
#   bpeace = max(attanyp),
#   nmids = sum(nmidiss),
#   bmids = max(midissyr),
#   totsalmax = max(icowsal),
#   chalsalmax = max(salchal),
#   tgtsalmax = max(saltgt),
#   recnowt = sum(recnowt),
#   recyeswt = sum(recyeswt),
#   recmidwt = sum(recmidwt)
# )
# icow_part_dyr$y0 <- icow_part_dyr$year - 1

#icow_part_cyr_out <- full_join(dat, icow_part_cyr)
icow_part_cyr <- left_join(icow_part_cyr, icow_set_dyr) # need to resolve duplication here - probably just drop variables on other sidef

icow_part_cyr$agree <- ifelse(icow_part_cyr$sagree > 0, 1, 0)
icow_part_cyr$agree <- ifelse(is.na(icow_part_cyr$agree), 0, icow_part_cyr$agree)
icow_part_cyr$agreeiss <- ifelse(icow_part_cyr$sagreeiss > 0, 1, 0)
icow_part_cyr$agreeiss <- ifelse(is.na(icow_part_cyr$agreeiss), 0, icow_part_cyr$agreeiss)

icow_part_cyr$agyear = ifelse(icow_part_cyr$agreeiss == 1, icow_part_cyr$year, NA)
icow_part_cyr = ungroup(icow_part_cyr %>% group_by(claimdy) %>% mutate(
  clyrmin = min(year),
  yrlastag = LOCF(agyear),
  agiss2 = if_else(year == clyrmin & agreeiss == 0, 1, agreeiss),
))
icow_part_cyr$agstop = icow_part_cyr$year - icow_part_cyr$yrlastag + 1
icow_part_cyr$agstart = icow_part_cyr$agstop - 1
icow_part_cyr$agyr2 <- ifelse(icow_part_cyr$agiss2 == 1, icow_part_cyr$year, NA)
icow_part_cyr = ungroup(icow_part_cyr %>% group_by(claimdy) %>% mutate(
  yrlastag2 = LOCF(agyr2)
))
icow_part_cyr$stop <- icow_part_cyr$year - icow_part_cyr$yrlastag2 + 1
icow_part_cyr$start <- icow_part_cyr$stop - 1
# icow_part_cyr$stop <- ifelse(icow_part_cyr$start == 0, 0, icow_part_cyr$stop)

# View(icow_part_cyr[, c("claimdy", "year", "agyear", "clyrmin", "yrlastag", "agiss2", "agyr2", "yrlastag2", "start", "stop")])

icow_part_cyr <- left_join(icow_part_cyr, select(dat, -start))

# icow_part_cyr$agreeiss <- with(icow_part_cyr, ifelse(terriss == 1 & year >= 1870 & sub == 1 & is.na(agreeiss), 0, agreeiss))
# icow_part_cyr$agreeiss <- with(icow_part_cyr, ifelse(riveriss == 1 & year >= 1900 & subr == 1 & is.na(agreeiss), 0, agreeiss))
# icow_part_cyr$agreeiss <- with(icow_part_cyr, ifelse(mariss == 1 & year >= 1900 & sub == 1 & is.na(agreeiss), 0, agreeiss))

#   icow_part_cyr %>% filter() %>% mutate(
#   
# )
# icow_part_cyr$agree <- ifelse(icow_part_cyr$agreeiss == 0 | is.na(icow_part_cyr$agreeiss), 0, icow_part_cyr$agreeiss)
# icow_part_cyr$agreeiss <- ifelse(icow_part_cyr$agreeiss == 0 | is.na(icow_part_cyr$agreeiss), 0, icow_part_cyr$agreeiss)
# icow_part_cyr$ag_end_any <- ifelse(icow_part_cyr$ag_end_any == 0 | is.na(icow_part_cyr$ag_end_any), 0, icow_part_cyr$ag_end_any)
# icow_part_cyr$ag_end_part <- ifelse(icow_part_cyr$ag_end_part == 0 | is.na(icow_part_cyr$ag_end_part), 0, icow_part_cyr$ag_end_part)
# icow_part_cyr$ag_end_full <- ifelse(icow_part_cyr$ag_end_full == 0 | is.na(icow_part_cyr$ag_end_full), 0, icow_part_cyr$ag_end_full)
# icow_part_cyr <- ungroup(icow_part_cyr[!is.na(icow_part_cyr$ccode1) & !is.na(icow_part_cyr$ccode2), ])
# write_csv(icow_part_cyr, "icow_part_cyr.csv")
# icow_part_dyr <- left_join(icow_part_dyr, dat)
# icow_part_dyr <- left_join(icow_part_dyr, icow_set_dyr)
# icow_part_dyr <- left_join(icow_part_dyr, icow_claimdy)
# icow_set <- left_join(icow_set, dat)
# # write_csv(icow_part_cyr, "icow_part_cyr.csv")
# write_csv(icow_part_dyr, "icow_part_dyr.csv")

#icow_part_cyr <- ungroup(icow_part_cyr)
# icow_part_cyr_tempt <- dplyr::rename_at(icow_part_cyr, vars(ends_with("1")), setNames(icow_part_cyr, paste0(names(), "_chal")))
# icow_part_cyr %>% setNames(dplyr::select_at(icow_part_cyr, vars(ends_with("1"))) =  paste0(names(.), "_chal"))
# dplyr::select_at(icow_part_cyr, vars(ends_with("1"))) %>% paste0(names(.), "_chal")
# icow_part_cyr_ct <- chaltgt(icow_part_cyr)
# 
# chaltgt <- function(dat) {
#   require(dplyr)
#   attach(dat)
#   if (ccode1 == chal | is.na(ccode1) | is.na(chal)) {
#     dat <- setNames(dat, paste0(names(dplyr::select(dat, ends_with("1"))), "_chal"))
#     dat <- setNames(dat, paste0(names(dplyr::select(dat, ends_with("2"))), "_tgt"))
#   } else if (ccode2 == chal | is.na(ccode2) | is.na(chal)) {
#     dat <- setNames(dat, paste0(names(dplyr::select(dat, ends_with("2"))), "_chal"))
#     dat <- setNames(dat, paste0(names(dplyr::select(dat, ends_with("1"))), "_tgt"))
#   }
#   detach(dat)
# }
# a <- chaltgt(dat)
#if (icow_part_cyr$ccode1 == icow_part_cyr$chal) {

# Create challenger/target dataset
  # sub1 contains vars for chal when chal == ccode1,
  # sub2 contains vars for target when target == ccode1
  # sub3 contains vars for chal when chal == ccode2
  # sub4 contains vars for tgt when tgt == ccode2
#   # Combine sub1 and sub4 to get variables for chal(ccode1)-tgt(ccode2) pair
#   # Combine sub2 and sub3 to get variables for chal(ccode2)-tgt(ccode1) pair
# vl1 <- colnames(dplyr::select(icow_part_cyr, ends_with("1")))
#   vchal <- gsub('.{0,1}$', '', vl1)
#   vchal <- paste0(vchal, '_chal')
#   vl1 <- c(vl1, "rownum", "year")
# vl2 <- colnames(dplyr::select(icow_part_cyr, ends_with("2"), -y2))
#   vtgt <- gsub('.{0,1}$', '', vl2)
#   vtgt <- paste0(vtgt, '_tgt')
#   vl2 <- c(vl2, "rownum", "year")
#   
#   dim(icow_part_cyr[icow_part_cyr$chal == icow_part_cyr$ccode1, ])
#   dim(icow_part_cyr[icow_part_cyr$chal == icow_part_cyr$ccode1, ])
#   sub1n <- icow_part_cyr %>% filter(ccode1 == chal)
#   sub2n <- icow_part_cyr %>% filter(ccode1 == tgt)
#   sub3n <- icow_part_cyr %>% filter(ccode1 != chal & ccode1 != tgt)
#     select(ends_with("2"), -y2) %>%
#     # sub1n <- icow_part_cyr %>% filter(ccode1 == chal) %>%
#     # select(ends_with("2"), -y2)
# sub1 <- icow_part_cyr %>%
#   rownames_to_column("rownum") %>%
#   filter(ccode1 == chal | is.na(ccode1) | is.na(chal)) %>%
#   select_if(names(.) %in% c(vl1, "chal")) # var1 assigend to chal
# sub2 <- icow_part_cyr %>%
#   rownames_to_column('rownum') %>%
#   filter(ccode1 == tgt  | is.na(ccode1) | is.na(tgt))  %>%
#   select_if(names(.) %in% c(vl1, "tgt")) # var1 assigend to tgt
# sub3 <- icow_part_cyr %>%
#   rownames_to_column("rownum") %>%
#   filter(ccode2 == chal) %>%
#   select_if(names(.) %in% c(vl2, "chal")) #var2 assigned to chal
# sub4 <- icow_part_cyr %>%
#   rownames_to_column('rownum') %>%
#   filter(ccode2 == tgt)  %>%
#   select_if(names(.) %in% c(vl2, "tgt")) # Var2 assigned to target
# colnames(sub1) <- c("rownum", "chal", "year", vchal)
# colnames(sub2) <- c("rownum", "tgt", "year", vtgt)
# colnames(sub3) <- c("rownum", "chal", "year", vchal)
# colnames(sub4) <- c("rownum", "tgt", "year", vtgt)

# subc <- icow_part_cyr[icow_part_cyr$chal == icow_part_cyr$ccode1, ]
# subt <- icow_part_cyr[icow_part_cyr$chal == icow_part_cyr$ccode2, ]
# suba <- icow_part_cyr[(icow_part_cyr$chal == icow_part_cyr$ccode1) & (icow_part_cyr$chal == icow_part_cyr$ccode2),]
# 
# subc2 <- icow_part_cyr[icow_part_cyr$chal < icow_part_cyr$tgt, ]
# subt2 <- icow_part_cyr[icow_part_cyr$chal > icow_part_cyr$tgt, ]
# 
# subc <- icow_part_cyr %>% filter(chal < tgt) %>% 

#### For some reason R is matching chal to tgt
  # There are NAs in ccode 1 and ccode2 where chal/tgt have matches # but very few that don't have a dyad
subc <- cbind(dplyr::select(sub1, -ccode_chal), dplyr::select(sub4, -ccode_tgt, -year, -rownum))
subt <- cbind(dplyr::select(sub2, -ccode_tgt), dplyr::select(sub3, -ccode_chal, -year, -rownum))
subf <- arrange(rbind(subc, subt), as.numeric(rownum))
icow_part_cyr_ct <- cbind(icow_part_cyr, subf)
# icow_part_cyr_ct <- left_join(icow_part_cyr, subf)
# 
# # write_csv(icow_part_cyr_ct, "./data/icow_part_cyr_ct.csv")
# 
#   ### Results of settlement attempt
#   # # Agree - settlement attempt resulted in an agreement - only missings are ongoing at end of data
#   # summary(icow_set[is.na(icow_set$agree), "year"])
#   # 
#   # # Agreeiss - settlement attempt result in a substantive agreement - recode NAs which could mean no agreement
#   # summary(icow_set[is.na(icow_set$agreeiss), "year"])
#   # icow_set$agreeiss <- ifelse(is.na(icow_set$agreeiss), 0, icow_set$agreeiss)
#   ### Consider recoding if ongoing at end
#   
#   # Effect 4 - what was the outcome of the settlement attempt (agree, ratify, comply, end)
#   # NA: sett attempt hasn't ended by end of current data
#   # 4: Agreement ended claim
#   # 3: Both states complied, but it didn't end the claim
#   # 2: Both states ratified, but at least one didn't comply
#   # 1: Both reached an agreement, but at least one didn't ratify
#   # 0: Attempt did not produce an agreement
#   # Effect 3 is the same as above, except that nonratification and noncompliance are merged
#   # 3: Agreement ended claim
#   # 2: Both complied with agreement, but claim didn't end
#   # 1: Agreement reached, but at least one didn't ratify or comply
#   # 0: Attempt did not produce an agreement
#   # Results of settlement attempt disaggregated by partial and total claim termination
# 
#   # Alternate coding possibility - subset to agreements only and code outcomes
#   
#   ### Claim resolution variables
#   # What led to claim termination?
#   # *** This variable is avaible in the claim level data for both the full and partial datasets
#   # resolved: Type of claim resolution
#   #   -9 (missing values): Ongoing (the claim is not resolved at the current end of the data set)
#   #   1: Dropped by Challenger
#   #   2: Renounced by Challenger
#   #   3: (this value is no longer used)
#   #   4: Bilateral
#   #   5: Independence
#   #   6: Actor Leaves System
#   #   7: Military Conquest/Occupation
#   #   8: Dropped by Target
#   #   9: Renounced by Target
#   #   10: Plebiscite
#   #   11: Claim No Longer Relevant
#   #   12: Binding Third Party Decision
#   #   13: Non-binding Third Party Activity
#   #   14: Peace conference
#   
#   
#   ## 
#   # dmon$e <- dmon$gdpcap * dmon$tpop
#   # f <- dmon[dmon$year == 2010, "e"]
#   # f <- unlist(f)
#   # summary(f)
#   # median(f, na.rm = T) # median 111672727
#   # # Mean   :  467396326  dollars --- so an increase of 5 billion is pretty damn significant
#   
#   #Neural Net
#   # library(neuralnet)
#   # nn = neuralnet(Placed~TKS+CSS,data=df, hidden=3,act.fct = "logistic",
#   #              linear.output = FALSE)
#   # library(neuralnet)
#   # datnarm <- na.omit(dat)
#   # nn = neuralnet(trade ~ gdpcomb + dyterrclaim + conttype, data = datnarm)
#   
#   
#   # summary(icow_part_dyr$trsim0)
#   # summary(icow_part_dyr$lntrade)
#   # summary(icow_part_dyr$ugt) 
#   # # exp (-4.768) = 0.0085 millions = 8500 foregone
#   # # exp 8.106 = 3,314 millions foregone
#   # exp(.202) # mean - 1.224 million foregone
#   # exp(.076) # median - 1.079 million foregone
#   # 
#   # with(icow_part_dyr, summary(gdp1 + gdp2))
#   