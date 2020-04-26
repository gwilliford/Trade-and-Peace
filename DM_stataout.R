icow_part_cyr <- read_dta("./data/ICOWdyadyr.dta")
icow_part_cyr <- filter(icow_part_cyr, chal != 2220 & tgt != 2200)
icow_part_cyr <- left_join(icow_part_cyr, icow_set_cldy)
icow_part_cyr$one <- 1
icow_part_cyr$agree <- ifelse(icow_part_cyr$sagree > 0, 1, 0)
icow_part_cyr$agree <- ifelse(is.na(icow_part_cyr$agree), 0, icow_part_cyr$agree)
icow_part_cyr$agreeiss <- ifelse(icow_part_cyr$sagreeiss > 0, 1, 0)
icow_part_cyr$agreeiss <- ifelse(is.na(icow_part_cyr$agreeiss), 0, icow_part_cyr$agreeiss)
icow_part_cyr$midyear = ifelse(icow_part_cyr$midissyr == 1, icow_part_cyr$year, NA)
icow_part_cyr <- ungroup(icow_part_cyr %>%
                           arrange(claimdy, year) %>%
                           group_by(claimdy) %>%
                           mutate(
                             cltermyr = max(year),
                             clterm = if_else(cltermyr == year, 1, 0),
                             cumagr = cumsum(agreeiss), 
                             cummid = cumsum(midissyr),
                             cumagr = if_else(agreeiss == 1, cumagr + 1, cumagr)
                             cumagr = if_else()
                             clstop = cumsum(one),
                             clstart = clstop - 1,
                             ifelse(year == cltermyr, 1, 0), 
                             lastmid = LOCF(midyear)
                           ))
icow_part_cyr = ungroup(icow_part_cyr %>% 
                          group_by(claimdy, cumagr) %>%
                          mutate(
                            #clyrmin = min(year),
                            agstart = cumsum(1),
                            agstop = agstart + 1
                          )
)




event time = midissyr

start indicator = agreeiss
event = midissyr



surv()

# start = agreement signed
# event = mid occurs
# time since last mid


# agreeiss = start indicator
# start variable = agyearc 
# midstopyr = stop indicator
# Stop variable = midissyr

a <- eha::toBinary(icow_part_cyr, surv = c("agyear", "midyear", "midissyr"))



b <- read.dta13("./data/icow_final.dta")
b <- b %>% rename("agstart1" = "_t0", "agstop2" = "_t", "agfail3" = "_d")
coxph(Surv(agstart1, agstop2, agfail3) ~ terriss, data = b)


rea
