chaltgt <- function(dat) {
  require(dplyr)
  attach(dat)
  if (ccode1 == chal) {
    dat <- setNames(dat, paste0(names(dplyr::select(dat, ends_with("1"))), "_chal"))
    dat <- setNames(dat, paste0(names(dplyr::select(dat, ends_with("2"))), "_tgt"))
  } else if (ccode2 == chal) {
    dat <- setNames(dat, paste0(names(dplyr::select(dat, ends_with("2"))), "_chal"))
    dat <- setNames(dat, paste0(names(dplyr::select(dat, ends_with("1"))), "_tgt"))
  }
  detach(dat)
}

### ICOW settlement data
icow_set <- read_dta("ICOWsettle.dta")
icow_set <- icow_set %>% filter(midiss == 0 & terriss == 1) # drop mids and non-territorial claims

icow_set <- dplyr::select(icow_set, -mid)
icow_set$agreeiss <- ifelse(is.na(icow_set$agreeiss), 0, icow_set$agreeiss)

### Collapse icow settlement data to dyad year format
icow_set_col <- icow_set %>% group_by(dyad, year) %>% summarize (
  sagree = sum(agree),
  sagreeiss = sum(agreeiss)
)

# Was an agreement reached?
icow_set_col$bagree <- ifelse(icow_set_col$sagree > 0, 1, 0)
icow_set_col$bagreeiss <- ifelse(icow_set_col$sagreeiss > 0, 1, 0)

# Was an agreement that ended the claim reached?
icow_set$ag_end_any  <- ifelse(icow_set$agreeiss == 1 & icow_set$claimend == 1:2, 1, 0)
icow_set$ag_end_part <- ifelse(icow_set$agreeiss == 1 & icow_set$claimend == 1, 1, 0)
icow_set$ag_end_full <- ifelse(icow_set$agreeiss == 1 & icow_set$claimend == 2, 1, 0)

### ICOW aggregate claim data
icow_claimdy <- read_dta("ICOWclaimdy.dta")
icow_claimdy <- icow_claimdy %>% filter(terriss == 1)
icow_claimdy$reschdrop = ifelse(icow_claimdy$resolved %in% c(1, 2), 1, 0)
icow_claimdy$resmil = ifelse(icow_claimdy$resolved == 7, 1, 0)
icow_claimdy$restgtdrop = ifelse(icow_claimdy$resolved %in% c(8, 9), 1, 0)
icow_claimdy$respleb = ifelse(icow_claimdy$resolved == 10, 1, 0)
icow_claimdy$respset = ifelse(icow_claimdy$resolved %in% c(4, 12, 13, 14), 1, 0)
icow_claimdy$resother = ifelse(icow_claimdy$resolved %in% c(5, 6, 11), 1, 0) # independence, actor leaves system, disp territory no longer exists

### ICOW Partial Claim-Year Data
icow_part_cyr  <- read_dta("ICOWdyadyr.dta")
icow_part_cyr  <- filter(icow_part_cyr, terriss == 1)
icow_part_cyr$y0 <- icow_part_cyr$year - 1
icow_part_cyr <- icow_part_cyr %>% group_by(dyad) %>% mutate(
  fyear = max(year)
)
icow_part_cyr <- left_join(icow_part_cyr, dplyr::select(icow_claimdy, dyad, reschdrop, resmil, restgtdrop, respleb, respset, resother))
icow_part_cyr$chdrop = ifelse(icow_part_cyr$reschdrop == 1 & icow_part_cyr$year == icow_part_cyr$fyear, 1, 0)
icow_part_cyr$milres = ifelse(icow_part_cyr$resmil == 1 & icow_part_cyr$year == icow_part_cyr$fyear, 1, 0)
icow_part_cyr$tgtdrop = ifelse(icow_part_cyr$restgtdrop == 1 & icow_part_cyr$year == icow_part_cyr$fyear, 1, 0)
icow_part_cyr$pleb = ifelse(icow_part_cyr$respleb == 1 & icow_part_cyr$year == icow_part_cyr$fyear, 1, 0)
icow_part_cyr$pset = ifelse(icow_part_cyr$respset == 1 & icow_part_cyr$year == icow_part_cyr$fyear, 1, 0)
icow_part_cyr$other = ifelse(icow_part_cyr$resother == 1 & icow_part_cyr$year == icow_part_cyr$fyear, 1, 0)

### ICOW Partial Dyad-Year Data
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
icow_part_dyr$y0 <- icow_part_dyr$year - 1

#icow_part_cyr_out <- full_join(dat, icow_part_cyr)
icow_part_cyr <- left_join(icow_part_cyr, dplyr::select(icow_set, dyad, year, agree, agreeiss, ag_end_any, ag_end_part, ag_end_full)) # need to resolve duplication here - probably just drop variables on other sidef
icow_part_cyr$agree <- ifelse(icow_part_cyr$agreeiss == 0 | is.na(icow_part_cyr$agreeiss), 0, icow_part_cyr$agreeiss)
icow_part_cyr$agreeiss <- ifelse(icow_part_cyr$agreeiss == 0 | is.na(icow_part_cyr$agreeiss), 0, icow_part_cyr$agreeiss)
icow_part_cyr$ag_end_any <- ifelse(icow_part_cyr$ag_end_any == 0 | is.na(icow_part_cyr$ag_end_any), 0, icow_part_cyr$ag_end_any)
icow_part_cyr$ag_end_part <- ifelse(icow_part_cyr$ag_end_part == 0 | is.na(icow_part_cyr$ag_end_part), 0, icow_part_cyr$ag_end_part)
icow_part_cyr$ag_end_full <- ifelse(icow_part_cyr$ag_end_full == 0 | is.na(icow_part_cyr$ag_end_full), 0, icow_part_cyr$ag_end_full)

icow_part_cyr <- left_join(icow_part_cyr, dat)
icow_part_dyr <- left_join(icow_part_dyr, dat)
icow_part_dyr <- left_join(icow_part_dyr, icow_set_col)
icow_part_dyr <- left_join(icow_part_dyr, icow_claimdy)
icow_set <- left_join(dplyr::select(icow_set, -mid), dat)
# write_csv(icow_part_cyr, "icow_part_cyr.csv")
# write_csv(icow_part_dyr, "icow_part_dyr.csv")

icow_part_cyr <- ungroup(icow_part_cyr)
icow_part_cyr_tempt <- dplyr::rename_at(icow_part_cyr, vars(ends_with("1")), setNames(icow_part_cyr, paste0(names(), "_chal")))
icow_part_cyr %>% setNames(dplyr::select_at(icow_part_cyr, vars(ends_with("1"))) =  paste0(names(.), "_chal"))
dplyr::select_at(icow_part_cyr, vars(ends_with("1"))) %>% paste0(names(.), "_chal")
# icow_part_cyr_ct <- chaltgt(icow_part_cyr)
# 
# chaltgt <- function(dat) {
#   require(dplyr)
#   attach(dat)
#   if (ccode1 == chal) {
#     dat <- setNames(dat, paste0(names(dplyr::select(dat, ends_with("1"))), "_chal"))
#     dat <- setNames(dat, paste0(names(dplyr::select(dat, ends_with("2"))), "_tgt"))
#   } else if (ccode2 == chal) {
#     dat <- setNames(dat, paste0(names(dplyr::select(dat, ends_with("2"))), "_chal"))
#     dat <- setNames(dat, paste0(names(dplyr::select(dat, ends_with("1"))), "_tgt"))
#   }
#   detach(dat)
# }
