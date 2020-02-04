library(readr)
library(brms)
library(brglm)

if(!exists("dat")) dat <- read_csv("./data/icow_part_cyr_ct.csv")


a <- icow_full_cyr %>% filter((chal == 651 & tgt == 666) | (chal == 666 & tgt == 651))
summary(a$year)


### For some reason chal/tgt data has different results when using 1 and 2 and chal and tgt, and 1 and 2 differ from original file
# Chal/tgt is actually dropping less observations
# Dyadic: Unrealized gains from trade

att_dy <- glm(attanyp ~ lugt + lntrade + laglntdepdy1 + laglntdepdy2 + laglntdeptot1 + laglntdeptot2 + lugt * saltgt +
                lag_1 + lag_2 + lag_cap1 + lag_cap2 + 
                polity21 * polity22 +
                icowsal + 
                recnowt + recyeswt + recmidwt +
                lcaprat + ldefense + contdir,
              data = icow_part_cyr_ct,
              family = binomial); summary(att_dy)

att_dy <- brglm::brglm(attanyp ~ lrivugt + lntrade + laglntdepdy_chal + laglntdepdy_tgt + laglntdeptot_chal + laglntdeptot_tgt +
                lag_ln_gdp_chal + lag_ln_gdp_chal + lag_ln_gdpcap_tgt + lag_ln_gdpcap_tgt + 
                lpol_chal * lpol_tgt +
                icowsal + 
                recnowt + recyeswt + recmidwt +
                lcaprat + ldefense + contdir,
              data = icow_part_cyr_ct,
              family = binomial); summary(att_dy)

att_dy <- brglm::brglm(attanyp ~ ltotugt + laglntrade + laglntdepdy_chal + laglntdepdy_tgt + laglntdeptot_chal + laglntdeptot_tgt +
                         lag_ln_gdp_chal + lag_ln_gdp_tgt + lag_ln_gdpcap_chal + lag_ln_gdpcap_tgt + 
                         lpol_chal * lpol_tgt +
                         icowsal + 
                         recnowt + recyeswt + recmidwt +
                         lcaprat + ldefense + contdir,
                       data = icow_part_cyr_ct,
                       family = binomial); summary(att_dy)
att_dy <- MASS::glm.nb(attanyp ~ ltotugt + laglntrade + laglntdepdy_chal + laglntdepdy_tgt + laglntdeptot_chal + laglntdeptot_tgt +
                         lag_ln_gdp_chal + lag_ln_gdp_tgt + lag_ln_gdpcap_chal + lag_ln_gdpcap_tgt + 
                         lpol_chal * lpol_tgt +
                         icowsal + 
                         recnowt + recyeswt + recmidwt +
                         lcaprat + ldefense + contdir,
                       data = icow_part_cyr_ct); summary(att_dy)


att_dy <- glm(attanyp ~ lugt + lntrade + laglntdepdy1 + laglntdepdy2 + laglntdeptot1 + laglntdeptot2 + lugt * salintc +
                   lag_ln_gdp1 + lag_ln_gdp2 + lag_ln_gdpcap1 + lag_ln_gdpcap2 + 
                   polity21 * polity22 +
                   icowsal + 
                   recnowt + recyeswt + recmidwt +
                   lcaprat + ldefense + contdir,
                 data = icow_part_cyr,
                 family = binomial); summary(att_dy)

att_dy_b <- brm(attanyp ~ lugt + lntrade + laglntdepdy1 + laglntdepdy2 + laglntdeptot1 + laglntdeptot2 + 
                         lag_ln_gdp1 + lag_ln_gdp2 + lag_ln_gdpcap1 + lag_ln_gdpcap2 + 
                         polity21 * polity22 +
                         icowsal + 
                         recnowt + recyeswt + recmidwt +
                         lcaprat + ldefense + contdir + 
                        (1 | mm(ccode1, ccode2)),
                       data = icow_part_cyr, cores = 4,
                      family = "bernoulli")

# Unrealized gains from trade dependence
# att_dy <- brglm::brglm(attanyp ~ lugt + lntrade + laglntdepdy1 + laglntdepdy2 + laglntdeptot1 + laglntdeptot2 + 
#                          lag_1 + lag_2 + lag_cap1 + lag_cap2 + 
#                          polity21 * polity22 +
#                          icowsal + 
#                          recnowt + recyeswt + recmidwt +
#                          lcaprat + ldefense + contdir,
#                        data = icow_part_cyr,
#                        family = binomial); summary(att_dy)

# Dyadic: Actual Trade
att_dy <- glm(attanyp ~ lntrade + laglntdepdy1 + laglntdepdy2 + laglntdeptot1 + laglntdeptot2 + 
                lag_1 + lag_2 + lag_cap1 + lag_cap2 + 
                lpol1 * lpol2 +
                icowsal + 
                recnowt + recyeswt + recmidwt +
                lcaprat + ldefense + contdir,
              data = icow_part_cyr,
              family = binomial); summary(att_dy)

# Dyadic: Trade dependence
att_dy <- glm(attanyp ~ lntrade + laglntdepdy1 + laglntdepdy2 + 
                lag_1 + lag_2 + lag_cap1 + lag_cap2 + 
                polity21 * polity22 +
                icowsal + 
                recnowt + recyeswt + recmidwt +
                lcaprat + ldefense + contdir,
              data = icow_part_cyr,
              family = binomial); summary(att_dy)


# Systemic: Trade dependence
att_dy <- glm(attanyp ~ lntrade + laglntdeptot1 + laglntdeptot2 + 
                lag_1 + lag_2 + lag_cap1 + lag_cap2 + 
                polity21 * polity22 +
                icowsal + 
                recnowt + recyeswt + recmidwt +
                lcaprat + ldefense + contdir,
              data = icow_part_cyr,
              family = binomial); summary(att_dy)
