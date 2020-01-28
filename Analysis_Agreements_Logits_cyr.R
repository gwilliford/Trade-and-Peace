library(brms)
library(brglm)

# Dyadic: Unrealized gains from trade
att_dy <- glm(attanyp ~ lugt + lntrade + laglntdepdy1 + laglntdepdy2 + laglntdeptot1 + laglntdeptot2 + 
                   laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + 
                   polity21 * polity22 +
                   icowsal + 
                   recnowt + recyeswt + recmidwt +
                   lcaprat + ldefense + contdir,
                 data = icow_part_cyr,
                 family = binomial); summary(att_dy)

att_dy_b <- brm(attanyp ~ lugt + lntrade + laglntdepdy1 + laglntdepdy2 + laglntdeptot1 + laglntdeptot2 + 
                         laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + 
                         polity21 * polity22 +
                         icowsal + 
                         recnowt + recyeswt + recmidwt +
                         lcaprat + ldefense + contdir + 
                        (1 | mm(ccode1, ccode2)),
                       data = icow_part_cyr, cores = 4,
                      family = "bernoulli")

# Unrealized gains from trade dependence
# att_dy <- brglm::brglm(attanyp ~ lugt + lntrade + laglntdepdy1 + laglntdepdy2 + laglntdeptot1 + laglntdeptot2 + 
#                          laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + 
#                          polity21 * polity22 +
#                          icowsal + 
#                          recnowt + recyeswt + recmidwt +
#                          lcaprat + ldefense + contdir,
#                        data = icow_part_cyr,
#                        family = binomial); summary(att_dy)

# Dyadic: Actual Trade
att_dy <- glm(attanyp ~ lntrade + laglntdepdy1 + laglntdepdy2 + laglntdeptot1 + laglntdeptot2 + 
                laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + 
                lpol1 * lpol2 +
                icowsal + 
                recnowt + recyeswt + recmidwt +
                lcaprat + ldefense + contdir,
              data = icow_part_cyr,
              family = binomial); summary(att_dy)

# Dyadic: Trade dependence
att_dy <- glm(attanyp ~ lntrade + laglntdepdy1 + laglntdepdy2 + 
                laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + 
                polity21 * polity22 +
                icowsal + 
                recnowt + recyeswt + recmidwt +
                lcaprat + ldefense + contdir,
              data = icow_part_cyr,
              family = binomial); summary(att_dy)


# Systemic: Trade dependence
att_dy <- glm(attanyp ~ lntrade + laglntdeptot1 + laglntdeptot2 + 
                laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + 
                polity21 * polity22 +
                icowsal + 
                recnowt + recyeswt + recmidwt +
                lcaprat + ldefense + contdir,
              data = icow_part_cyr,
              family = binomial); summary(att_dy)
