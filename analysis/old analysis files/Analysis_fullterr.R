icow_full_cyr = read_csv("./data/ICOWprovyr101.csv")
icow_full_cyr$ccode1 = rowMins(cbind(icow_full_cyr$chal, icow_full_cyr$tgt))
icow_full_cyr$ccode2 = rowMaxs(cbind(icow_full_cyr$chal, icow_full_cyr$tgt))
icow_full_cyr$tclaim = 1
icow_full_cyr$salint1 = with(icow_full_cyr, ifelse(chal == ccode1, max(salintc), max(salintt)))
icow_full_cyr$salint2 = with(icow_full_cyr, ifelse(chal == ccode2, max(salintc), max(salintt)))

icow_full_cyr = left_join(icow_full_cyr, dat)
icow_full_cyr$one = 1
icow_full_cyr <- ungroup(icow_full_cyr %>%
                           arrange(claimdy, year) %>%
                           group_by(claimdy) %>%
                           mutate(
                             cltermyr = max(year),
                             clterm = if_else(cltermyr == year, 1, 0),
                             cummid = cumsum(midissyr),
                             cummid = ifelse(midissyr == 1, cummid - 1, cummid),
                             clstop = cumsum(one),
                             clstart = clstop - 1,
                             ifelse(year == cltermyr, 1, 0)
                           ))
# Time from agreement until MIDs variables
# icow_part_cyr$midyear = ifelse(icow_part_cyr$midissyr == 1, icow_part_cyr$year, NA)
icow_full_cyr = ungroup(icow_full_cyr %>% 
                          group_by(claimdy, cummid) %>%
                          mutate(
                            midstop = cumsum(one),
                            midstart = midstop - 1
                          )
)
# icow_part_cyr$cummid = ifelse(icow_part_cyr$cummid == 0, NA, icow_part_cyr$cummid)
# icow_part_cyr$midstart = ifelse(icow_part_cyr$cummid == 0, NA, icow_part_cyr$midstart)
# icow_part_cyr$midstop = ifelse(icow_part_cyr$cummid == 0, NA, icow_part_cyr$midstop)
# icow_part_cyr$midfail = ifelse(icow_part_cyr$cummid == 0, NA, icow_part_cyr$midissyr)

anyterm <- tvcure(formula  = Surv(clstart, clstop, clterm) ~ lag_ln_depdy1 + lag_ln_depdy2 + 
                    lag_ln_deptot1 + lag_ln_deptot2 + 
                    icowsal + 
                    samereg + lcaprat + ldefense + contdir,
                  cureform = ~ lag_ln_depdy1 + lag_ln_depdy2 + 
                    lag_ln_deptot1 + lag_ln_deptot2 + 
                    icowsal + 
                    samereg + 
                    lcaprat + ldefense + contdir + lnccdist, 
                  data = icow_full_cyr,
                  brglm = T, var = T, nboot = 1000);summary(anyterm)

anyterm2 <- tvcure(formula  = Surv(clstart, clstop, clterm) ~ lag_ln_depdy1 + lag_ln_depdy2 + 
                    icowsal  +
                    samereg + lcaprat + ldefense + contdir,
                  cureform = ~ lag_ln_depdy1 + lag_ln_depdy2 + 
                    icowsal + 
                    samereg + 
                    lcaprat + ldefense + contdir + lnccdist, 
                  data = icow_full_cyr,
                  brglm = T, var = T, nboot = 100);summary(anyterm2)

anyterm3 <- tvcure(formula  = Surv(clstart, clstop, clterm) ~ lag_ln_deptotmax + lag_ln_deptotmin + 
                    icowsal + 
                    samereg + lcaprat + ldefense + contdir,
                  cureform = ~ lag_ln_deptotmax + lag_ln_deptotmin + 
                    icowsal + 
                    samereg + 
                    lcaprat + ldefense + contdir + lnccdist, 
                  data = icow_full_cyr,
                  brglm = T, var = T, nboot = 100);summary(anyterm3)
 
anyterm4 <- tvcure(formula  = Surv(midstart, midstop, midissyr) ~ lag_ln_deptotmax + lag_ln_deptotmin + 
                     icowsal + 
                     samereg + lcaprat + ldefense + contdir,
                   cureform = ~ lag_ln_deptotmax + lag_ln_deptotmin + 
                     icowsal + 
                     samereg + 
                     lcaprat + ldefense + contdir + lnccdist, 
                   data = icow_full_cyr,
                   brglm = T, var = T, nboot = 100);summary(anyterm4)


anyterm3 <- tvcure(formula  = Surv(clstart, clstop, clterm) ~ lag_ln_depdymax + lag_ln_deptotmax +
                     icowsal + 
                     samereg + lcaprat + ldefense + contdir,
                   cureform = ~ lag_ln_depdymax + lag_ln_deptotmax +
                     icowsal + 
                     samereg + 
                     lcaprat + ldefense + contdir + lnccdist, 
                   data = icow_full_cyr,
                   brglm = T, var = T, nboot = 100);summary(anyterm3)

anyterm5 <- tvcure(formula  = Surv(midstart, midstop, midissyr) ~ lag_ln_depdymax + lag_ln_deptotmax +
                     icowsal + 
                     samereg + lcaprat + ldefense + contdir,
                   cureform = ~ lag_ln_depdymax + lag_ln_deptotmax +
                     icowsal + 
                     samereg + 
                     lcaprat + ldefense + contdir + lnccdist, 
                   data = icow_full_cyr,
                   brglm = T, var = T, nboot = 100);summary(anyterm5)
