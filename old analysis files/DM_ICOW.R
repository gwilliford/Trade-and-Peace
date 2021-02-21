setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis")
library(readr)
library(haven)
library(dplyr)
library(stringr)
library(tibble)
library(DescTools)

if(!exists("datlag")) datlag <- readRDS("./data/TradeInputs.RDS")

##### ICOW settlement data
icow_set <- read_dta("./data/ICOWsettle.dta") %>%
  # filter(midiss == 0) %>% 
  select(-mid)

# Collapse icow settlement data to dyad year format
icow_set_cldy <- icow_set %>%
  group_by(claimdy, yearend) %>%
  summarize(
    # sicowmid = sum(icowmid, na.rm = T),
    sagree = sum(agree, na.rm = T),
    sagreeiss = sum(agreeiss, na.rm = T),
    sconc = sum(concany, na.rm = T),
    # bicowmid = if_else(sicowmid > 0, 1, 0),
    bagree = if_else(sagree > 0, 1, 0),
    bagreeiss = if_else(sagreeiss > 0, 1, 0),
    bconc = if_else(sconc > 0, 1, 0),
    sratfail = sum(ratfail, na.rm = T),
    bratfail = if_else(sratfail > 0, 1, 0),
    scomply = sum(comply2, na.rm = T),
    bcomply = if_else(scomply > 0, 1, 0),
    sclaimend = sum(clmendatt, na.rm = T),
    bclaimend = if_else(sclaimend > 0, 1, 0),
    sclaimendall = sum(clmendall, na.rm = T),
    bclaimendall = if_else(sclaimendall > 0, 1, 0)
  ) %>%
  ungroup(icow_set) %>%
  rename("year" = "yearend")

##### ICOW aggregate claim data 
icow_claimdy <- read_dta("./data/ICOWclaimdy.dta")
icow_claimdy$reschdrop = ifelse(icow_claimdy$resolved %in% c(1, 2), 1, 0)
icow_claimdy$resmil = ifelse(icow_claimdy$resolved == 7, 1, 0)
icow_claimdy$restgtdrop = ifelse(icow_claimdy$resolved %in% c(8, 9), 1, 0)
icow_claimdy$respleb = ifelse(icow_claimdy$resolved == 10, 1, 0)
icow_claimdy$respset = ifelse(icow_claimdy$resolved %in% c(4, 12, 13, 14), 1, 0)
icow_claimdy$resother = ifelse(icow_claimdy$resolved %in% c(5, 6, 11), 1, 0) # independence, actor leaves system, disp territory no longer exists
icow_claimdy$pterm <- with(icow_claimdy, ifelse(resolved == 1 | resolved ==2 | resolved == 3 | resolved == 4 | resolved == 8 | resolved == 9, 1, 0))
icow_claimdy$pterm2 <- with(icow_claimdy, ifelse(resolved %in% c(3, 4), 1, 0))

icow_claimdy <- icow_claimdy %>% select(claimdy, reschdrop, resmil, restgtdrop, resmil, restgtdrop, respleb, respset, resother, pterm, pterm2)
# 1: Dropped by Challenger
# 2: Renounced by Challenger
# 3: Third Party
# 4: Bilateral
# 5: Independence
# 6: Actor Leaves System
# 7: Military Occupation
# 8: Dropped by Target
# 9: Renounced by Target
# 10: Plebiscite
# 11: Claim No Longer Relevant

##### Add settlement variables to ICOW Partial Claim-Year Data
icow_part_cyr <- read_dta("./data/ICOWdyadyr.dta") %>%
  filter(chal != 2220 & tgt != 2200)
icow_part_cyr <- left_join(icow_part_cyr, icow_claimdy)

# Merge settlement attempt variables into claim-year data
icow_part_cyr <- left_join(icow_part_cyr, icow_set_cldy)
icow_part_cyr$one <- 1
icow_part_cyr$agree <- ifelse(icow_part_cyr$sagree > 0, 1, 0)
icow_part_cyr$agree <- ifelse(is.na(icow_part_cyr$agree), 0, icow_part_cyr$agree)
icow_part_cyr$agreeiss <- ifelse(icow_part_cyr$sagreeiss > 0, 1, 0)
icow_part_cyr$agreeiss <- ifelse(is.na(icow_part_cyr$agreeiss), 0, icow_part_cyr$agreeiss)
icow_part_cyr$conc <- ifelse(icow_part_cyr$sconc > 0, 1, 0)
icow_part_cyr$conc <- ifelse(is.na(icow_part_cyr$conc), 0, icow_part_cyr$conc)
icow_part_cyr$midyear = ifelse(icow_part_cyr$midissyr == 1, icow_part_cyr$year, NA)
icow_part_cyr$ratyear = ifelse(icow_part_cyr$sratfail > 0, 1, 0)
icow_part_cyr$ratyear <- ifelse(is.na(icow_part_cyr$ratyear), 0, icow_part_cyr$ratyear)
icow_part_cyr$compyear = ifelse(icow_part_cyr$scomply > 0, 1, 0)
icow_part_cyr$compyear <- ifelse(is.na(icow_part_cyr$compyear), 0, icow_part_cyr$compyear)
icow_part_cyr$claimend = ifelse(icow_part_cyr$sclaimend > 0, 1, 0)
icow_part_cyr$claimend <- ifelse(is.na(icow_part_cyr$claimend), 0, icow_part_cyr$claimend)
icow_part_cyr$claimendall = ifelse(icow_part_cyr$sclaimendall > 0, 1, 0)
icow_part_cyr$claimendall <- ifelse(is.na(icow_part_cyr$claimendall), 0, icow_part_cyr$claimendall)

# # Time until claim termination variables
icow_part_cyr <- icow_part_cyr %>%
  arrange(claimdy, year) %>%
  group_by(claimdy) %>%
  mutate(
    cltermyr  = max(year),
    clterm    = if_else(cltermyr == year, 1, 0),
    clstop    = cumsum(one),
    clstart   = clstop - 1,
    
    # Spell variables for cumagr, cumagriss, cummid
    cumatt    = cumsum(attanyp),
    cumagr    = cumsum(agree),
    cumagriss = cumsum(agreeiss),
    cummid    = cumsum(midissyr),
    
    cumatt    = if_else(agree == 1, cumatt - 1, cumatt),
    cumagr    = if_else(agree == 1, cumagr - 1, cumagr),
    cumagriss = if_else(agreeiss == 1, cumagriss - 1, cumagriss),
    cummid    = if_else(midissyr == 1, cummid - 1, cummid),
    
    lastmid   = LOCF(midyear)
  ) %>% 
  ungroup(icow_part_cyr)

# Time until attempt variables
icow_part_cyr <- icow_part_cyr %>%
  group_by(claimdy, cumatt) %>%
  mutate(
    at_termyr = max(year),
    at_term = if_else(at_termyr == year, 1, 0),
    at_stop = cumsum(one),
    at_start = at_stop - 1,
  ) %>% 
  ungroup(icow_part_cyr)
  
# Time until agreement variables
icow_part_cyr <- icow_part_cyr %>%
  group_by(claimdy, cumagr) %>%
  mutate(
    agtermyr = max(year),
    agterm = if_else(agtermyr == year, 1, 0),
    agstop = cumsum(one),
    agstart = agstop - 1,
  ) %>% 
  ungroup(icow_part_cyr)

# Time until agreeiss variables
icow_part_cyr <- icow_part_cyr %>%
  group_by(claimdy, cumagriss) %>%
  mutate(
    agisstermyr = max(year),
    agissterm = if_else(agisstermyr == year, 1, 0),
    agiss_stop = cumsum(one),
    agiss_start = agiss_stop - 1,
  ) %>% 
  ungroup(icow_part_cyr)

icow_part_cyr <- icow_part_cyr %>%
  group_by(claimdy, cummid) %>%
  mutate(
    midtermyr = max(year),
    midterm = if_else(midtermyr == year, 1, 0),
    midstop = cumsum(one),
    midstart = midstop - 1,
  ) %>% 
  ungroup(icow_part_cyr)
icow_part_cyr <- left_join(icow_part_cyr, datlag)

icow_part_cyr$pterm <- replace(icow_part_cyr$pterm, icow_part_cyr$pterm == 1 & icow_part_cyr$clterm != 1, 0)
icow_part_cyr$ldefense <- replace(icow_part_cyr$ldefense, is.na(icow_part_cyr$ldefense), 0)
icow_part_cyr$igosum <- replace(icow_part_cyr$igosum, is.na(icow_part_cyr$igosum), 0)
icow_part_cyr$lcw <- replace(icow_part_cyr$lcw, is.na(icow_part_cyr$lcw), 0)
icow_part_cyr$c <- icow_part_cyr$clstop
icow_part_cyr$c2 <- (icow_part_cyr$c)^2 / 1000
icow_part_cyr$c3 <- (icow_part_cyr$c)^3 / 1000

icow <- icow_part_cyr
write_rds(icow, "./data/ICOWFinal.RDS")

# icow_part_cyr$tdepmin <- icow_part_cyr$dee
# icow_part_cyr$tdepmax <- icow_part_cyr$frank
# icow_part_cyr$tdepavg <- (icow_part_cyr$tdepmin + icow_part_cyr$tdepmax) / 2
# icow_part_cyr$tdepdif <- icow_part_cyr$tdepmax - icow_part_cyr$tdepmin
# 
# icow_part_cyr$lncaprat <- log(icow_part_cyr$lcaprat)
# icow_part_cyr$igosum <- ifelse(is.na(icow_part_cyr$igosum), 0, icow_part_cyr$igosum)
# icow_part_cyr$c <- icow_part_cyr$clstop
# icow_part_cyr$c2 <- icow_part_cyr$clstop^2 / 1000
# icow_part_cyr$c3 <- icow_part_cyr$clstop^3 / 10000




# # View(icow_part_cyr[, c("claimdy", "year", 
#                        "agree", "cumagr", "agstart", "agstop", "agtermyr", "agterm",
#                        "midissyr", "cummid", "midstart", "midstop", "midterm", "midtermyr",
#                        "agreeiss", "cumagriss", "agiss_start", "agiss_stop", "agisstermyr", "agissterm")])
# View(select(icow_part_cyr, claimdy, cumagr, year, clstart, clstop, clterm, agreeiss, clagrspell)) %>% filter(claimdy == 7801)

# icow_part_cyr$charlie <- ifelse(icow_part_cyr$depdymin_100 == 0, 0, log(icow_part_cyr$depdymin_100))
# icow_part_cyr$mac <- icow_part_cyr$ln_trade_100 - icow_part_cyr$ln_gdp_min
# icow_part_cyr$dennis <- icow_part_cyr$gdp_min * 1000000
# icow_part_cyr$cricket <- icow_part_cyr$gdp_max * 1000000
# icow_part_cyr$dee <- icow_part_cyr$ln_trade - log(icow_part_cyr$dennis)
# icow_part_cyr$frank <- icow_part_cyr$ln_trade - log(icow_part_cyr$cricket)
# icow_part_cyr$mcpoyle <- icow_part_cyr$dee / icow_part_cyr$frank
# icow_part_cyr$ponderosa <- icow_part_cyr$dee - icow_part_cyr$frank
# icow_part_cyr$waitress <- (icow_part_cyr$trade - icow_part_cyr$dennis) / (icow_part_cyr$trade - icow_part_cyr$cricket)
# icow_part_cyr$fattymagoo <- log(icow_part_cyr$waitress)

# icow_part_cyr = icow_part_cyr %>% mutate(
#   pikachu = trade_100/(gdp1) * 100,
#   squirtle = trade_100/(gdp2) * 100,
#   bulbasaur = pikachu * squirtle, 
#   charmander = rowMins(cbind(pikachu, squirtle)),
#   psyduck = rowMaxs(cbind(pikachu, squirtle)), 
#   staryu = if_else(ccode1 == chal, pikachu, squirtle),
#   starmie = if_else(ccode1 == tgt, pikachu, squirtle)
# )
# icow_part_cyr$pikachu2 <- ifelse(icow_part_cyr$pikachu > 1, 1, icow_part_cyr$pikachu)
# summary(icow_part_cyr$bulbasaur) * 100


# # Time from claimstart to MID variables
# icow_part_cyr$cummid = ifelse(icow_part_cyr$midissyr == 1, icow_part_cyr$cummid - 1, icow_part_cyr$cummid)
# icow_part_cyr = ungroup(icow_part_cyr %>% 
#                           group_by(claimdy, cummid) %>%
#                           mutate(
#                             midstop = cumsum(one),
#                             midstart = midstop - 1
#                           )
# )
# icow_part_cyr$cummid = ifelse(icow_part_cyr$cummid == 0, NA, icow_part_cyr$cummid)
# icow_part_cyr$midstart = ifelse(icow_part_cyr$cummid == 0, NA, icow_part_cyr$midstart)
# icow_part_cyr$midstop = ifelse(icow_part_cyr$cummid == 0, NA, icow_part_cyr$midstop)
# icow_part_cyr$midfail = ifelse(icow_part_cyr$cummid == 0, NA, icow_part_cyr$midissyr)
# 
# 
# test <- select(icow_part_cyr, claimdy, year, clstart, clstop, cltermyr, clterm, cumagr, agreeiss, agstart, agstop, midissyr, midyear, lastmid, cummid, midstart, midstop, midfail) %>% filter(claimdy == 7801);View(test)
#                            # yrlastag = LOCF(agyear),

# # subc1 <- icow_part_cyr %>% filter(chal == ccode1 & !is.na(ccode1)) %>% 
# #   select(ends_with("1"), claimdy, chal, tgt, ccode1, ccode2, year, rownum) %>% 
# #   rename_at(vars(ends_with("1")), funs(str_replace(., "1", "_chal"))) 
# # subc2 <- icow_part_cyr %>% filter(chal == ccode2 & !is.na(ccode2)) %>% 
# #   select(ends_with("2"), claimdy, chal, tgt, ccode1, ccode2, year, rownum) %>% 
# #   rename_at(vars(ends_with("2")), funs(str_replace(., "2", "_chal"))) 
# # subt1 <- icow_part_cyr %>% filter(chal == ccode1 & !is.na(ccode1)) %>% 
# #   select(ends_with("2"), claimdy, chal, tgt, ccode1, ccode2, year, rownum) %>% 
# #   rename_at(vars(ends_with("2")), funs(str_replace(., "2", "_tgt"))) 
# # subt2 <- icow_part_cyr %>% filter(chal == ccode2 & !is.na(ccode2)) %>% 
# #   select(ends_with("2"), claimdy, chal, tgt, ccode1, ccode2, year, rownum) %>% 
# #   rename_at(vars(ends_with("1")), funs(str_replace(., "1", "_tgt"))) 
# # subcf <- cbind(select(subc1, -ccode_chal, -chal), select(subt1, -ccode_tgt, -year, -rownum, -claimdy, -tgt))
# # subtf <- cbind(select(subc2, -ccode_chal, -chal), select(subt2, -ccode_tgt, -year, -rownum, -claimdy, -tgt))
# 
# # 
# # icow_part_ct2 <- arrange(subtf, as.numeric(rownum))
# # 
# # subf <- rbind(subcf, subtf)
# # icow_part_cyr_ct <- cbind(icow_part_cyr, subf)
# 
# 
# 
# # Create challenger/target dataset
# # sub1 contains vars for chal when chal == ccode1,
# # sub2 contains vars for target when target == ccode1
# # sub3 contains vars for chal when chal == ccode2
# # sub4 contains vars for tgt when tgt == ccode2
#   # Combine sub1 and sub4 to get variables for chal(ccode1)-tgt(ccode2) pair
#   # Combine sub2 and sub3 to get variables for chal(ccode2)-tgt(ccode1) pair
# vl1 <- colnames(dplyr::select(icow_part_cyr, ends_with("1")))
#   vchal <- gsub('.{0,1}$', '', vl1)
#   vchal <- paste0(vchal, '_chal')
#   vl1 <- c(vl1, "rownum", "year")
# vl2 <- colnames(dplyr::select(icow_part_cyr, ends_with("2")))
#   vtgt <- gsub('.{0,1}$', '', vl2)
#   vtgt <- paste0(vtgt, '_tgt')
#   vl2 <- c(vl2, "rownum", "year")
# 
#   # dim(icow_part_cyr[icow_part_cyr$chal == icow_part_cyr$ccode1, ])
#   # dim(icow_part_cyr[icow_part_cyr$chal == icow_part_cyr$ccode1, ])
#   # sub1n <- icow_part_cyr %>% filter(ccode1 == chal)
#   # sub2n <- icow_part_cyr %>% filter(ccode1 == tgt)
#   # sub3n <- icow_part_cyr %>% filter(ccode1 != chal & ccode1 != tgt)
#   #   select(ends_with("2"), -y2) %>%
#   #   # sub1n <- icow_part_cyr %>% filter(ccode1 == chal) %>%
#   #   # select(ends_with("2"), -y2)
# sub1 <- icow_part_cyr %>%
#   #rownames_to_column("rownum") %>%
#   filter(ccode1 == chal & !is.na(ccode1)) %>%
#   select_if(names(.) %in% c(vl1, "chal")) # var1 assigend to chal
# sub2 <- icow_part_cyr %>%
#   #rownames_to_column('rownum') %>%
#   filter(ccode1 == tgt  & !is.na(ccode1))  %>%
#   select_if(names(.) %in% c(vl1, "tgt")) # var1 assigend to tgt
# sub3 <- icow_part_cyr %>%
#   #rownames_to_column("rownum") %>%
#   filter(ccode2 == chal & !is.na(ccode2)) %>%
#   select_if(names(.) %in% c(vl2, "chal")) #var2 assigned to chal
# sub4 <- icow_part_cyr %>%
#   #rownames_to_column('rownum') %>%
#   filter(ccode2 == tgt & !is.na(ccode2))  %>%
#   select_if(names(.) %in% c(vl2, "tgt")) # Var2 assigned to target
# colnames(sub1) <- c("rownum", "chal", "year", vchal)
# colnames(sub2) <- c("rownum", "tgt", "year", vtgt)
# colnames(sub3) <- c("rownum", "chal", "year", vchal)
# colnames(sub4) <- c("rownum", "tgt", "year", vtgt)
# # subc <- icow_part_cyr[icow_part_cyr$chal == icow_part_cyr$ccode1, ]
# # subt <- icow_part_cyr[icow_part_cyr$chal == icow_part_cyr$ccode2, ]
# # suba <- icow_part_cyr[(icow_part_cyr$chal == icow_part_cyr$ccode1) & (icow_part_cyr$chal == icow_part_cyr$ccode2),]
# 
# subc <- cbind(dplyr::select(sub1, -ccode_chal), dplyr::select(sub4, -ccode_tgt, -year, -rownum))
# subt <- cbind(dplyr::select(sub2, -ccode_tgt), dplyr::select(sub3, -ccode_chal, -year, -rownum))
# subf <- arrange(rbind(subc, subt), as.numeric(rownum)) %>% select(-chal, -tgt)
# # icow_part_ct <- arrange(subcf, as.numeric(rownum))
# # icow_part_ct <- rbind(subcf, arrange(subtf, as.numeric(rownum)))a
# icow_part_ct <- left_join(icow_part_cyr, subf, by = c("rownum", "year"))
# # Check
# # View(icow_part_ct %>% select(dyad, year, ccode1, ccode2, chal, tgt, ln_gdp1, ln_gdp_chal, ln_gdp2, ln_gdp_tgt))


# 
# rbind(subcf, subtf)
# %>%
#   rename_at(vars(ends_with("2")), funs(str_replace(., "2", "_tgt"))) %>% 
# 
# %>%
#   rename_all(funs(str_replace(., "1", "chal")))
#   
#   
#  )) %>% 
#   rename_at(vars(ends_with("2")), funs(str_replace(., "2", "_tgt"))) %>%
#   select(ends_with("_chal"), ends_with("_tgt"), claimdy, dyad, year)
# subt <- icow_part_cyr %>% filter(chal == ccode2 & !is.na(ccode2)) %>%
#   rename_at(vars(ends_with("1")), funs(str_replace(., "1", "_tgt"))) %>% 
#   rename_at(vars(ends_with("2")), funs(str_replace(., "2", "_chal")))  %>%
#   select(ends_with("_chal"), ends_with("_tgt"), claimdy, dyad, year)
# icow_part_ct <- arrange(subc, .)
# icow_part_cyr <- full_join(icow_part_ct, icow_part_cyr, by = c("claimdy", "year"))


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
#   # dmon$e <- dmon$gdpcap * dmon$tpop
#   # f <- dmon[dmon$year == 2010, "e"]
#   # f <- unlist(f)
#   # summary(f)
#   # median(f, na.rm = T) # median 111672727
#   # # Mean   :  467396326  dollars --- so an increase of 5 billion is pretty damn significant
#   
#   

# saveRDS(icow_part_ct, "./data/icow_part_ct.RDS")
# a <- icow_part_cyr #%>% select(claimdy, year, clstart, clstop, agreeiss, midissyr)
# 
# stata("my_do_file.do", 
#       stata.path = "/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp", # yours probably differs: use the chooseStataBin() command on windows or linux machines; on Macs, right click on the Stata app, select "Show Package Contents", then see what's in the Contents/MacOS/ directory
#       stata.version = 13)  # again, specify what _you_ have
# options("RStata.StataPath" = "/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp")
# options("RStata.StataVersion" = 13)
