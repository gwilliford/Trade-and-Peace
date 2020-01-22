att13 <- brglm(attanyp ~ ugt + laglntrade + laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + icowsal + lcaprat + W1 + W2 + polity21 * polity22 + recnowt + recyeswt, data = icow_part_cyr); summary(att13)
att14 <- brglm(bpeace ~ exp(ugt) + lntrade + lngdpcap1 + lngdpcap2 + lnpop1 + lnpop2 + totsalmax + lcaprat + W1 + W2 + polity21 * polity22 + recnowt + recyeswt, family = binomial(link = "logit"), data = icow_part_dyr); summary(att14)

att13 <- glm(attanyp ~ ugt * icowsal + laglntrade + laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + icowsal + lcaprat + W1 + W2 + polity21 * polity22 + recnowt + recyeswt, data = icow_part_cyr); summary(att13)
att14 <- glm(bpeace ~ ugt * totsalmax + lntrade + lngdpcap1 + lngdpcap2 + lnpop1 + lnpop2 + totsalmax + lcaprat + W1 + W2 + polity21 * polity22 + recnowt + recyeswt, family = binomial(link = "logit"), data = icow_part_dyr); summary(att14)


att16 <- glm(bpeace ~ ugt + lntrade + lngdpcap1 + lngdpcap2 + lnpop1 + lnpop2 + totsalmax + lcaprat + W1 + W2 + polity21 * polity22 + recnowt + recyeswt, family = binomial(link = "logit"), data = icow_part_dyr); summary(att16)


att17 <- glm(bagree ~ lugtdep1 + lugtdep2 + laglntrade + laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + icowsal + lcaprat + W1 + W2 + polity21 * polity22 + recnowt + recyeswt, data = icow_part_cyr); summary(att17)
att18 <- glm(bagreeiss ~ lugtdep1 + lugtdep2 + laglntrade + ltdeptot1 + ltdeptot2 + laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + totsalmax + lcaprat + W1 + W2 + polity21 * polity22 + recnowt + recyeswt, family = binomial(link = "logit"), data = icow_part_dyr); summary(att18)


att_cy <- glm(attanyp ~ laglntrade + mlntrade + ltdepdy1 + ltdepdy2 + ltdeptot1 + ltdeptot2 + mlntrade + lngdpt + laglngdpcapt + icowsal + lcaprat + ldefense + W1 + W2 + contdir + 
                  polity21 * polity22 + recnowt + recyeswt + recmidwt, 
                family = binomial(link = "logit"),
                data = icow_part_cyr); summary(att_cy)

att_dy <- glm(bpeace ~ laglntrade + mlntrade + ltdepdy1 + ltdepdy2 + ltdeptot1 + ltdeptot2 + mlntrade + lngdpt + laglngdpcapt + totsalmax + lcaprat + ldefense + W1 + W2 + contdir + 
                polity21 * polity22 + recnowt + recyeswt + recmidwt, 
              family = binomial(link = "logit"),
              data = icow_part_dyr); summary(att_dy)

att_cy <- glm(attemptsp ~ laglntrade + mlntrade + lntdepdy1 + lntdepdy2 + ltdeptot1 + ltdeptot2 + mlntrade + lngdpt + laglngdpcapt + icowsal + lcaprat + ldefense + W1 + W2 + contdir + 
                polity21 * polity22 + recnowt + recyeswt + recmidwt, 
              family = "poisson",
              data = icow_part_cyr); summary(att_cy)
att_cy <- glm(attemptsp ~ laglntrade + lntrdev + mlntrade + lntdepdy1 + lntdepdy2 + mlntrade +
                lngdpt + laglngdpcapt +
                icowsal +
                lcaprat + ldefense +
                W1 + W2 + contdir + 
                polity21 * polity22 + recnowt + recyeswt + recmidwt, 
              family = "poisson",
              data = icow_part_cyr); summary(att_cy)
library(ggpubr)

att_dy <- glm(npeace ~ laglntrade + lntrdev + mlntrade + lntdepdy1 + lntdepdy2 + mlntrade +
                lngdpt + laglngdpcapt + 
                W1 + W2 + polity21 * polity22 +
                totsalmax + 
                recnowt + recyeswt + recmidwt, 
                lcaprat + ldefense + contdir,
              family = "poisson",
              data = icow_part_dyr); summary(att_dy)

att_dy <- glm(npeace ~ laglntrade + lntdepdymax + 
                lngdpt + laglngdpcapt +
                W1 + W2 + polity21 * polity22 +
                totsalmax +
                recnowt + recyeswt + recmidwt +
              lcaprat + ldefense + contdir,
              family = "poisson",
              data = icow_part_dyr); summary(att_dy)
att_dy <- glm(npeace ~ lntdepdymax + 
                lngdpt + laglngdpcapt +
                W1 + W2 + polity21 * polity22 +
                totsalmax +
                recnowt + recyeswt + recmidwt +
                lcaprat + ldefense + contdir,
              family = "poisson",
              data = icow_part_dyr); summary(att_dy)
att_dy <- glm(npeace ~ mlntrade + ltdeptot1 + ltdeptot2 +
                lngdpt + laglngdpcapt +
                W1 + W2 + polity21 * polity22 +
                totsalmax +
                recnowt + recyeswt + recmidwt,
              lcaprat + ldefense + contdir,
              family = "poisson",
              data = icow_part_dyr); summary(att_dy)
att_dy <- glm(npeace ~ lntrdev +
                lngdpt + laglngdpcapt +
                W1 + W2 + polity21 * polity22 +
                totsalmax +
                recnowt + recyeswt + recmidwt,
              lcaprat + ldefense + contdir,
              family = "poisson",
              data = icow_part_dyr); summary(att_dy)
att_dy <- glm(npeace ~ lntdepdy1 + lntdepdy2 +
                lngdpt + laglngdpcapt +
                W1 + W2 + polity21 * polity22 +
                totsalmax +
                recnowt + recyeswt + recmidwt,
              lcaprat + ldefense + contdir,
              family = "poisson",
              data = icow_part_dyr); summary(att_dy)
att_dy <- glm(npeace ~ lntdepdy1 * lntdepdy2 +
                lngdpt + laglngdpcapt +
                W1 + W2 + polity21 * polity22 +
                totsalmax +
                recnowt + recyeswt + recmidwt,
              lcaprat + ldefense + contdir,
              family = "poisson",
              data = icow_part_dyr); summary(att_dy)


# att_cy <- glmer(attanyp ~ lntrade + laglntrade + laglngdp1 + laglngdp2 + laglngdpcap1 + 
#                  laglngdpcap2 + icowsal + lcaprat + ldefense + W1 + W2 + 
#                  polity21 * polity22 + recnowt + recyeswt + (1 | dyad) + (1 | year), 
#                family = binomial(link = "logit"),
#                data = icow_part_cyr)
# 
# att_dy <- brm(bpeace ~ mtrade + laglngdp1 + laglngdp2 + laglngdpcap1 + 
#                   laglngdpcap2 + totsalmax + lcaprat + ldefense + W1 + W2 + 
#                   polity21 * polity22 + recnowt + recyeswt + (1 | dyad) + (1 | year),
#                 family = bernoulli,
#                 cores = 4,
#                 data = icow_part_dyr); summary(att_dy)
