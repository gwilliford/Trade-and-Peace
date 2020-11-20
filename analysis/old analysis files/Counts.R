# Settlement attempts count

library(MASS)
library(dplyr)

att_dy <- glm.nb(attemptsp ~ laglntdepdy1 + laglntdepdy2 + lntdeptot1 + lntdeptot2 + 
                   laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + 
                   polity21 * polity22 +
                   icowsal + 
                   recnowt + recyeswt + recmidwt +
                   lcaprat + ldefense + contdir,
                 data = icow_part_cyr,
                 maxit = 100000); summary(att_dy)

att_dy_ct <- glm.nb(attemptsp ~ lugt + totugt_chal + totugt_tgt + laglntdeptot_chal + laglntdeptot_tgt +
                      W_chal * W_tgt + polity2_chal * polity2_tgt +
                      icowsal + recnowt + recyeswt + recmidwt + 
                      lcaprat + ldefense + contdir, data = icow_part_cyr_ct, maxit = 100000); summary(att_dy_ct)

icow_part_cyr_ct$max <- rowMaxs(cbind(icow_part_cyr_ct$laglntdepdy_chal, icow_part_cyr_ct$laglntdepdy_tgt))
icow_part_cyr_ct$max2 <- rowMaxs(cbind(icow_part_cyr_ct$laglntdeptot_chal, icow_part_cyr_ct$laglntdeptot_tgt))
att_dy_ct <- glm(attanyp ~ lugt + laglntrade + max + max2 + 
                      W_chal * W_tgt +
                      icowsal + recnowt + recyeswt + recmidwt + 
                      lcaprat + ldefense + contdir, data = icow_part_cyr_ct, maxit = 100000, family = binomial(lin = "logit")); summary(att_dy_ct)


# att_dy <- glm.nb(attemptsp ~ lugtdep1 + lugtdep2 +
#                    lngdpt + laglngdpcapt + 
#                    W1 * W2 + polity21 * polity22 +
#                    icowsal + 
#                    recnowt + recyeswt + recmidwt +
#                    lcaprat + ldefense + contdir,
#                  data = icow_part_cyr); summary(att_dy)
