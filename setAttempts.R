setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis - Trade Models")
library(readr)
library(haven)
library(dplyr)
library(brms)
library(brglm)

# Why does contdir variable have NAs?

##### CM attempts - Claim Year
att1 <- brglm::brglm(attanyp ~ lugt + laglngdpcapt + icowsal + lcaprat + ldemdy + contdir + recnowt + recyeswt, data = icow_part_cyr_out, family = binomial(link = "logit")); summary(att1)
att2 <- glm(attanyp ~ ugtdepobs + lngdpcomb + totsalmax + lcaprat + demdy + (conttype == 1) + recnowt + recyeswt, data = icow_part_cyr_out, family = binomial(link = "logit")); summary(att2)
att3 <- glm(attemptsp ~ ugtobs + lngdpcomb + totsalmax + lcaprat + demdy + (conttype == 1) + recnowt + recyeswt, data = icow_part_cyr_out, family = poisson); summary(att3)
att4 <- glm(attemptsp ~ ugtdepobs + lngdpcomb + totsalmax + lcaprat + demdy + (conttype == 1) + recnowt + recyeswt, data = icow_part_cyr_out, family = poisson); summary(att4)

# BRGLM
att5 <- brm(attanyp ~ ugtobs + lngdpcomb + totsalmax + lcaprat + demdy + conttype + recnowt + recyeswt + (1 | mm(ccode1, ccode2)), data = icow_part_cyr_out, family = bernoulli(link = "logit"), cores = 4); summary(att5)


att6 <- brglm(attanyp ~ lugtdep1 + lugtdep2 +  ltdepdy1 + ltdepdy2 + ltdeptot1 + ltdeptot2 + laglngdpcap1 + laglngdpcap2 + icowsal + lcaprat + polity21 * polity22 + recnowt + recyeswt, data = icow_part_cyr_out, family = binomial(link = "logit"))
att6 <- brglm(bpeace ~ ugtdivtr1 + ugtdivtr2 + ltdepdy1 + ltdeptot1 + ltdeptot2 + laglngdpcap1 + laglngdpcap2 + totsalmax + lcaprat + W1 + W2 + polity21 * polity22 + recnowt + recyeswt, data = icow_part_dyr_out, family = binomial(link = "logit"));summary(att6)

att7 <- brm(bpeace ~ ugtdivtr1 + ugtdivtr2 + ltdepdy1 + ltdeptot1 + ltdeptot2 + laglngdpcap1 + laglngdpcap2 + totsalmax + lcaprat + W1 + W2 + polity21 * polity22 + recnowt + recyeswt, data = icow_part_dyr_out, family = "bernoulli");summary(att7)

att8 <- brglm(bpeace ~ ugt + gdp1 + gdp2 + pop1 + pop2 + totsalmax + lcaprat + W1 + W2 + polity21 * polity22 + recnowt + recyeswt, data = icow_part_dyr_out);summary(att8)

att9 <- brglm(attanyp ~ ugt + gdp1 + gdp2 + pop1 + pop2 + icowsal + lcaprat + W1 + W2 + polity21 * polity22 + recnowt + recyeswt, data = icow_part_cyr_out);summary(att9)
att10 <- brm(attanyp ~ ugt + gdp1 + gdp2 + pop1 + pop2 + icowsal + lcaprat + W1 + W2 + polity21 * polity22 + recnowt + recyeswt, 
            data = icow_part_cyr_out,
            family = "bernoulli", 
            cores = 4);summary(att10)

att11 <- brglm(attanyp ~ ugt + trade + gdp1 + gdp2 + ltdeptot1 + ltdeptot2 + ltdepdy1 + ltdepdy2 + pop1 + pop2 + icowsal + lcaprat + W1 + W2 + polity21 * polity22 + recnowt + recyeswt, data = icow_part_cyr_out)
att12 <- brglm::brglm(bpeace ~ ugt + trade + gdp1 + gdp2 + ltdeptot1 + ltdeptot2 + ltdepdy1 + ltdepdy2 + pop1 + pop2 + totsalmax + lcaprat + W1 + W2 + polity21 * polity22 + recnowt + recyeswt, family = binomial(link = "logit"), data = icow_part_dyr_out); summary(att12)

att11 <- brglm(attanyp ~ ugt + lntrade + lngdp1 + lngdp2 + ltdeptot1 + ltdeptot2 + ltdepdy1 + ltdepdy2 + lnpop1 + lnpop2 + icowsal + lcaprat + W1 + W2 + polity21 * polity22 + recnowt + recyeswt, data = icow_part_cyr_out); summary(att11)
att12 <- brglm::brglm(bpeace ~ ugt + lntrade + lngdp1 + lngdp2 + ltdeptot1 + ltdeptot2 + ltdepdy1 + ltdepdy2 + lnpop1 + lnpop2 + totsalmax + lcaprat + W1 + W2 + polity21 * polity22 + recnowt + recyeswt, family = binomial(link = "logit"), data = icow_part_dyr_out); summary(att12)

att11 <- brglm(attanyp ~ ugtdep1 + ugtdep2 + lntrade + lngdp1 + lngdp2 + ltdeptot1 + ltdeptot2 + ltdepdy1 + ltdepdy2 + lnpop1 + lnpop2 + icowsal + lcaprat + W1 + W2 + polity21 * polity22 + recnowt + recyeswt, data = icow_part_cyr_out); summary(att11)
att12 <- brglm::brglm(bpeace ~ ugtdep1 + ugtdep2 + lntrade + lngdp1 + lngdp2 + ltdeptot1 + ltdeptot2 + ltdepdy1 + ltdepdy2 + lnpop1 + lnpop2 + totsalmax + lcaprat + W1 + W2 + polity21 * polity22 + recnowt + recyeswt, family = binomial(link = "logit"), data = icow_part_dyr_out); summary(att12)

att11 <- brglm(attanyp ~ ugt + lntrade + lngdp1 + lngdp2 + lnpop1 + lnpop2 + icowsal + lcaprat + W1 + W2 + polity21 * polity22 + recnowt + recyeswt, data = icow_part_cyr_out); summary(att11)
att12 <- brglm(bpeace ~ ugt + lntrade + lngdp1 + lngdp2 + lnpop1 + lnpop2 + totsalmax + lcaprat + W1 + W2 + polity21 * polity22 + recnowt + recyeswt, family = binomial(link = "logit"), data = icow_part_dyr_out); summary(att12)




View(icow_part_cyr[!is.na(icow_part_cyr$trsim0), ])

summary(icow_part_cyr$trade)
summary(icow_part_cyr$lntrade)
summary(icow_part_cyr$trsim0)
tm1 <- lmer(lntrade ~ ldyterrclaim + lngdp1 + lngdp2 + lnpop1 + lnpop2 + contdir + ldefense + mid + lcaprat + polity21 * polity22 + (1 | dyad), data = dat); summary(tm1) #  .662/.93

dim(icow_part_cyr)
with(icow_part_dyr, summary(cbind(bpeace, ugt, trade, lntrade, lngdp1, lngdp2, lnpop1, lnpop2, totsalmax, cinc1, cinc2, caprat, lcaprat, defense, ldefense, mid, W1, W2, polity21, polity22, recnowt, recyeswt, trsim0)))


ldefense
lcaprat
mid == 1?
  
att11 <- brglm(attanyp ~ ugt + lntrade + lngdp1 + lngdp2 + lnpop1 + lnpop2 + icowsal + caprat + W1 + W2 + polity21 * polity22 + recnowt + recyeswt, data = icow_part_cyr); summary(att11)
att12 <- brglm(bpeace ~ ugt + lntrade + lngdp1 + lngdp2 + lnpop1 + lnpop2 + totsalmax + caprat + W1 + W2 + polity21 * polity22 + recnowt + recyeswt, family = binomial(link = "logit"), data = icow_part_dyr); summary(att12)


att13 <- brm(bpeace ~ ugt + lntrade + lngdp1 + lngdp2 + lnpop1 + lnpop2 + totsalmax + lcaprat + W1 + W2 + polity21 * polity22 + recnowt + recyeswt + (1 | dyad + mm(ccode1, ccode2)),
             family = "bernoulli", 
             
             prior = set_prior(prior = "lasso(df = 1, scale = 1)",
                               class = "b"),
             data = icow_part_dyr_out,
             iter = 10000, 
             cores = 4); summary(att13)
### Add random effects

# with(icow_part_dyr_out, cor(cbind(ltdeptot1, ltdeptot2), use = "complete.obs"))
# with(icow_part_dyr_out, cor(cbind(ltdepdy1, ltdepdy2), use = "complete.obs"))
# Recnowt Number of recent failed peaceful attempts over this claim, weighted by recency.
# This weighting assigns an event in the year before the current observation a value of 1.0,
# with the weight of earlier events decreasing by 10% per year (thus an event ten years before the
#                                                               current observation has a value of 0.1 and earlier events do not contribute to this score).

# Recyeswt Number of recent successful peaceful settlement attempts over this claim,
# weighted by recency.
# This weighting assigns an event in the year before the current observation a value of 1.0,
# with the weight of earlier events decreasing by 10% per year (thus an event ten years before the
#                                                               current observation has a value of 0.1 and earlier events do not contribute to this score).
# cor(dat$bpeace, dat$ugtdepobs, use = "complete.obs")
# summary(dat[dat$bpeace == 0, 'ugtdepobs'])
# summary(dat[dat$bpeace == 1, 'ugtdepobs'])
# ### dependence in past cm attempts

##### CM attempts - dyad year
att1 <- glm(bpeace ~ ugtobs + lngdpcomb + totsalmax + lcaprat + demdy + (conttype == 1) + recnowt + recyeswt, data = icow_part_dyr_out, family = binomial(link = "logit")); summary(att1)
att2 <- glm(bpeace ~ ugtdepobs + lngdpcomb + totsalmax + lcaprat + demdy + (conttype == 1) + recnowt + recyeswt, data = icow_part_dyr_out, family = binomial(link = "logit")); summary(att2)
att3 <- glm(npeace ~ ugtobs + totsalmax + lngdpcomb + lcaprat + demdy + (conttype == 1) + recnowt + recyeswt, data = icow_part_dyr_out, family = poisson); summary(att3)
att4 <- glm(npeace ~ ugtobs + totsalmax + lngdpcomb + lcaprat + demdy + (conttype == 1) + recnowt + recyeswt, data = icow_part_dyr_out, family = poisson); summary(att4)

            