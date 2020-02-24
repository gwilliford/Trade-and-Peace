setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis")
library(readr)
library(haven)
library(dplyr)
library(stringr)
library(tibble)
library(DescTools)

if(!exists("dat")) dat <- load("./data/TradeOut.Rdata")

dat$tsim <- tsim
dat$msim <- msim
dat$rsim <- rsim

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
icow_part_cyr <- left_join(icow_part_cyr, icow_set_dyr)
icow_part_cyr$agree <- ifelse(icow_part_cyr$sagree > 0, 1, 0)
icow_part_cyr$agree <- ifelse(is.na(icow_part_cyr$agree), 0, icow_part_cyr$agree)
icow_part_cyr$agreeiss <- ifelse(icow_part_cyr$sagreeiss > 0, 1, 0)
icow_part_cyr$agreeiss <- ifelse(is.na(icow_part_cyr$agreeiss), 0, icow_part_cyr$agreeiss)
icow_part_cyr$agyear = ifelse(icow_part_cyr$agreeiss == 1, icow_part_cyr$year, NA)
icow_part_cyr = ungroup(icow_part_cyr %>% group_by(claimdy) %>% mutate(
  clyrmin = min(year),
  yrlastag = LOCF(agyear),
  agissb = if_else(year == clyrmin & agreeiss == 0, 1, agreeiss),
))
icow_part_cyr$agstop = icow_part_cyr$year - icow_part_cyr$yrlastag + 1
icow_part_cyr$agstart = icow_part_cyr$agstop - 1
icow_part_cyr$agyrb <- ifelse(icow_part_cyr$agissb == 1, icow_part_cyr$year, NA)
icow_part_cyr = ungroup(icow_part_cyr %>% group_by(claimdy) %>% mutate(
  yrlastagb = LOCF(agyrb)
))
icow_part_cyr$spstop <- icow_part_cyr$year - icow_part_cyr$yrlastagb + 1
icow_part_cyr$spstart <- icow_part_cyr$spstop - 1
icow_part_cyr <- left_join(icow_part_cyr, select(dat, -start)) %>% rownames_to_column('rownum')
icow_part_cyr$rownum <- as.numeric(icow_part_cyr$rownum)
icow_part_cyr <- filter(icow_part_cyr, !is.na(ccode1))

# View(icow_part_cyr[, c("claimdy", "year", "agyear", "clyrmin", "yrlastag", "agiss2", "agyr2", "yrlastag2", "start", "stop")])


subc1 <- icow_part_cyr %>% filter(chal == ccode1 & !is.na(ccode1)) %>% 
  select(ends_with("1"), claimdy, chal, tgt, ccode1, ccode2, year, rownum) %>% 
  rename_at(vars(ends_with("1")), funs(str_replace(., "1", "_chal"))) 
subc2 <- icow_part_cyr %>% filter(chal == ccode2 & !is.na(ccode2)) %>% 
  select(ends_with("2"), claimdy, chal, tgt, ccode1, ccode2, year, rownum) %>% 
  rename_at(vars(ends_with("2")), funs(str_replace(., "2", "_chal"))) 
subt1 <- icow_part_cyr %>% filter(chal == ccode1 & !is.na(ccode1)) %>% 
  select(ends_with("2"), claimdy, chal, tgt, ccode1, ccode2, year, rownum) %>% 
  rename_at(vars(ends_with("2")), funs(str_replace(., "2", "_tgt"))) 
subt2 <- icow_part_cyr %>% filter(chal == ccode2 & !is.na(ccode2)) %>% 
  select(ends_with("2"), claimdy, chal, tgt, ccode1, ccode2, year, rownum) %>% 
  rename_at(vars(ends_with("1")), funs(str_replace(., "1", "_tgt"))) 
subcf <- cbind(select(subc1, -ccode_chal, -chal), select(subt1, -ccode_tgt, -year, -rownum, -claimdy, -tgt))
subtf <- cbind(select(subc2, -ccode_chal, -chal), select(subt2, -ccode_tgt, -year, -rownum, -claimdy, -tgt))

# 
# icow_part_ct2 <- arrange(subtf, as.numeric(rownum))
# 
# subf <- rbind(subcf, subtf)
# icow_part_cyr_ct <- cbind(icow_part_cyr, subf)



# Create challenger/target dataset
# sub1 contains vars for chal when chal == ccode1,
# sub2 contains vars for target when target == ccode1
# sub3 contains vars for chal when chal == ccode2
# sub4 contains vars for tgt when tgt == ccode2
  # Combine sub1 and sub4 to get variables for chal(ccode1)-tgt(ccode2) pair
  # Combine sub2 and sub3 to get variables for chal(ccode2)-tgt(ccode1) pair
vl1 <- colnames(dplyr::select(icow_part_cyr, ends_with("1")))
  vchal <- gsub('.{0,1}$', '', vl1)
  vchal <- paste0(vchal, '_chal')
  vl1 <- c(vl1, "rownum", "year")
vl2 <- colnames(dplyr::select(icow_part_cyr, ends_with("2")))
  vtgt <- gsub('.{0,1}$', '', vl2)
  vtgt <- paste0(vtgt, '_tgt')
  vl2 <- c(vl2, "rownum", "year")

  dim(icow_part_cyr[icow_part_cyr$chal == icow_part_cyr$ccode1, ])
  dim(icow_part_cyr[icow_part_cyr$chal == icow_part_cyr$ccode1, ])
  sub1n <- icow_part_cyr %>% filter(ccode1 == chal)
  sub2n <- icow_part_cyr %>% filter(ccode1 == tgt)
  # sub3n <- icow_part_cyr %>% filter(ccode1 != chal & ccode1 != tgt)
  #   select(ends_with("2"), -y2) %>%
  #   # sub1n <- icow_part_cyr %>% filter(ccode1 == chal) %>%
  #   # select(ends_with("2"), -y2)
sub1 <- icow_part_cyr %>%
  #rownames_to_column("rownum") %>%
  filter(ccode1 == chal & !is.na(ccode1)) %>%
  select_if(names(.) %in% c(vl1, "chal")) # var1 assigend to chal
sub2 <- icow_part_cyr %>%
  #rownames_to_column('rownum') %>%
  filter(ccode1 == tgt  & !is.na(ccode1))  %>%
  select_if(names(.) %in% c(vl1, "tgt")) # var1 assigend to tgt
sub3 <- icow_part_cyr %>%
  #rownames_to_column("rownum") %>%
  filter(ccode2 == chal & !is.na(ccode2)) %>%
  select_if(names(.) %in% c(vl2, "chal")) #var2 assigned to chal
sub4 <- icow_part_cyr %>%
  #rownames_to_column('rownum') %>%
  filter(ccode2 == tgt & !is.na(ccode2))  %>%
  select_if(names(.) %in% c(vl2, "tgt")) # Var2 assigned to target
colnames(sub1) <- c("rownum", "chal", "year", vchal)
colnames(sub2) <- c("rownum", "tgt", "year", vtgt)
colnames(sub3) <- c("rownum", "chal", "year", vchal)
colnames(sub4) <- c("rownum", "tgt", "year", vtgt)
# subc <- icow_part_cyr[icow_part_cyr$chal == icow_part_cyr$ccode1, ]
# subt <- icow_part_cyr[icow_part_cyr$chal == icow_part_cyr$ccode2, ]
# suba <- icow_part_cyr[(icow_part_cyr$chal == icow_part_cyr$ccode1) & (icow_part_cyr$chal == icow_part_cyr$ccode2),]

subc <- cbind(dplyr::select(sub1, -ccode_chal), dplyr::select(sub4, -ccode_tgt, -year, -rownum))
subt <- cbind(dplyr::select(sub2, -ccode_tgt), dplyr::select(sub3, -ccode_chal, -year, -rownum))
subf <- arrange(rbind(subc, subt), as.numeric(rownum)) %>% select(-chal, -tgt)
# icow_part_ct <- arrange(subcf, as.numeric(rownum))
# icow_part_ct <- rbind(subcf, arrange(subtf, as.numeric(rownum)))
icow_part_ct <- left_join(icow_part_cyr, subf, by = c("rownum", "year"))
View(icow_part_ct %>% select(dyad, year, ccode1, ccode2, chal, tgt, ln_gdp1, ln_gdp_chal, ln_gdp2, ln_gdp_tgt))



rbind(subcf, subtf)
%>%
  rename_at(vars(ends_with("2")), funs(str_replace(., "2", "_tgt"))) %>% 

%>%
  rename_all(funs(str_replace(., "1", "chal")))
  
  
 )) %>% 
  rename_at(vars(ends_with("2")), funs(str_replace(., "2", "_tgt"))) %>%
  select(ends_with("_chal"), ends_with("_tgt"), claimdy, dyad, year)
subt <- icow_part_cyr %>% filter(chal == ccode2 & !is.na(ccode2)) %>%
  rename_at(vars(ends_with("1")), funs(str_replace(., "1", "_tgt"))) %>% 
  rename_at(vars(ends_with("2")), funs(str_replace(., "2", "_chal")))  %>%
  select(ends_with("_chal"), ends_with("_tgt"), claimdy, dyad, year)
icow_part_ct <- arrange(subc, .)
icow_part_cyr <- full_join(icow_part_ct, icow_part_cyr, by = c("claimdy", "year"))


# # suba <- icow_part_cyr[(icow_part_cyr$chal == icow_part_cyr$ccode1) & (icow_part_cyr$chal == icow_part_cyr$ccode2),]
# # 
# # subc2 <- icow_part_cyr[icow_part_cyr$chal < icow_part_cyr$tgt, ]
# # subt2 <- icow_part_cyr[icow_part_cyr$chal > icow_part_cyr$tgt, ]
# # 
# # subc <- icow_part_cyr %>% filter(chal < tgt) %>% 
# 
# #### For some reason R is matching chal to tgt
#   # There are NAs in ccode 1 and ccode2 where chal/tgt have matches # but very few that don't have a dyad
# subc <- cbind(dplyr::select(sub1, -ccode_chal), dplyr::select(sub4, -ccode_tgt, -year, -rownum))
# subt <- cbind(dplyr::select(sub2, -ccode_tgt), dplyr::select(sub3, -ccode_chal, -year, -rownum))
# subf <- arrange(rbind(subc, subt), as.numeric(rownum))
# 
# sub1 <- icow_part_cyr %>% filter(ccode1 != chal & ccode2 != tgt)
# 
#   rename(ends_with("2")))
# colnames(dplyr::select(icow_part_cyr, ends_with("2"), )
# #   vtgt <- gsub('.{0,1}$', '', vl2)
# subt2 <- icow_part_cyr[icow_part_cyr$chal > icow_part_cyr$tgt, ]
# 
# # icow_part_cyr_ct <- rbind(subc2, subt2)
# # # icow_part_cyr_ct <- left_join(icow_part_cyr, subf)
# # # 
# # # write_csv(icow_part_cyr_ct, "./data/icow_part_cyr_ct.csv")
# # 
# #   ### Results of settlement attempt
# #   # # Agree - settlement attempt resulted in an agreement - only missings are ongoing at end of data
# #   # summary(icow_set[is.na(icow_set$agree), "year"])
# #   # 
# #   # # Agreeiss - settlement attempt result in a substantive agreement - recode NAs which could mean no agreement
# #   # summary(icow_set[is.na(icow_set$agreeiss), "year"])
# #   # icow_set$agreeiss <- ifelse(is.na(icow_set$agreeiss), 0, icow_set$agreeiss)
# #   ### Consider recoding if ongoing at end
# #   
# #   # Effect 4 - what was the outcome of the settlement attempt (agree, ratify, comply, end)
# #   # NA: sett attempt hasn't ended by end of current data
# #   # 4: Agreement ended claim
# #   # 3: Both states complied, but it didn't end the claim
# #   # 2: Both states ratified, but at least one didn't comply
# #   # 1: Both reached an agreement, but at least one didn't ratify
# #   # 0: Attempt did not produce an agreement
# #   # Effect 3 is the same as above, except that nonratification and noncompliance are merged
# #   # 3: Agreement ended claim
# #   # 2: Both complied with agreement, but claim didn't end
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