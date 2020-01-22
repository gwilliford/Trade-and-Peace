# Settlement attempts count

library(MASS)

att_dy <- glm.nb(attemptsp ~ lugt + lntdepdy1 + lntdepdy2 +
                   lngdpt + laglngdpcapt + 
                   W1 * W2 + polity21 * polity22 +
                   icowsal + 
                   recnowt + recyeswt + recmidwt +
                   lcaprat + ldefense + contdir,
                 data = icow_part_cyr); summary(att_dy)


att_dy <- glm.nb(attemptsp ~ lugtdep1 + lugtdep2 +
                   lngdpt + laglngdpcapt + 
                   W1 * W2 + polity21 * polity22 +
                   icowsal + 
                   recnowt + recyeswt + recmidwt +
                   lcaprat + ldefense + contdir,
                 data = icow_part_cyr); summary(att_dy)
