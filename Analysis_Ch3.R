setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis")
library(readstata13)
library(dplyr)
library(tvcure)
library(brglm)
library(ggplot2)
library(ggeffects)
options(scipen = 999)
# library(MASS) not loaded to avoid conflicts with dplyr
b <- read.dta13("./data/icow_final.dta")
b <- b %>% rename("agstart1" = "_t0", "agstop2" = "_t", "agfail3" = "_d")
b$lncaprat <- log(b$lcaprat)
b$post45 <- ifelse(b$year > 1945, 1, 0)
b$igosum <- ifelse(is.na(b$igosum), 0, b$igosum)
cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)

att <- glmer(attanyp ~ lamin + 
               icowsal + riveriss + mariss +
               samereg + 
               lag_pch_gdp_min + 
               recmidwt + recnowt + recyeswt + 
               lcaprat + ldefense + contdir + 
               (1 | dyad) + (1 | year), 
             data = icow_part_ct);summary(att)

att <- MASS::glm.nb(attanyp ~ lamin + lamax + 
               icowsal + riveriss + mariss +
               samereg + 
               lag_pch_gdp_min + GovCrisesMax + 
               recmidwt + recnowt + recyeswt + 
               lcaprat + ldefense + contdir + trival, 
             data = icow_part_ct);summary(att)

agr_tv <- tvcure(Surv(clstart, clstop, agreeiss) ~ lamin + 
                      icowsal + 
                      samereg +
                      recmidwt + recnowt + recyeswt +
                      ldefense + contdir,
                 cureform = ~  lamin + icowsal + I(icowsal^2) + 
                   lcaprat + I(lcaprat^2) + demdy + autdy + contdir,
                    data = b,
                 brglm = T,
                 nboot = 30);summary(agr_tv)

# good
mid_tv <- tvcure(Surv(clstart, clstop, midissyr) ~ lamin + 
                   icowsal + 
                   samereg +
                   recmidwt + recnowt + recyeswt +
                   ldefense + contdir,
                 cureform = ~  lamin + icowsal + 
                   demdy + autdy +
                   lcaprat + ldefense + contdir,
                 data = b,
                 brglm = T,
                 nboot = 30);summary(mid_tv)


mid_tv <- tvcure(Surv(clstart, clstop, midissyr) ~ lamin + 
                   icowsal + 
                   recmidwt + recnowt + recyeswt +
                   ldefense + contdir,
                 cureform = ~  lamin + 
                   icowsal + terriss + mariss + 
                   demdy + autdy + lncaprat + 
                   ldefense + contdir,
                 data = b,
                 brglm = T,
                 nboot = 30);summary(mid_tv)
# good
agr <- tvcure(Surv(agstart1, agstop2, agfail3) ~ lamin + 
                   icowsal + 
                   recmidwt + recnowt + recyeswt +
                   ldefense + contdir,
                 cureform = ~  icowsal + terriss + mariss + 
                   demdy + autdy + lncaprat + 
                   ldefense + contdir,
                 data = b,
                 brglm = T,
                 nboot = 30);summary(mid_tv)

agr <- tvcure(Surv(agstart1, agstop2, agfail3) ~ lamin + icowsal + 
                recmidwt + recnowt + recyeswt +
                ldefense + contdir,
              cureform = ~  lamin + icowsal + terriss + mariss + 
                demdy + autdy + lncaprat + 
                ldefense + contdir + 
                recmidwt,
              data = b,
              brglm = T,
              nboot = 30);summary(agr)
a <- prediction2(agr, "lamin", c(-.2, .43), type = "uncureprob", CI = T, nsims = 10000)
plot(a)

agr.brm <- brm(agreeiss ~ lamin +
                 icowsal + riveriss + mariss +
                 lag_pch_gdp_min + 
                 recmidwt + recnowt + recyeswt + 
                 lcaprat + ldefense + contdir + 
                 (1|dyad) + (1|year),
               data = b);summary(agr.brm)
agr.glmer <- glmer(agreeiss ~ lamin +
                     icowsal + riveriss + mariss +
                     samereg + 
                     lag_pch_gdp_min + 
                     recmidwt + recnowt + recyeswt + 
                     lcaprat + ldefense + contdir  + 
                     (1|dyad) + (1|year),
                   data = b);summary(agr.glmer)

agr.glmer <- glmer(agreeiss ~ lamin +
                     icowsal + riveriss + mariss +
                     samereg + 
                     lag_pch_gdp_min + 
                     recmidwt + recnowt + recyeswt + 
                     lcaprat + ldefense + contdir  + 
                     (1|dyad) + (1|year),
                   data = b);summary(agr.glmer)
agr.br <- brglm(agreeiss ~ lamin + 
                  icowsal +
                  samereg + 
                  lag_pch_gdp_min + 
                  recmidwt + recnowt + recyeswt + 
                  lcaprat + ldefense + contdir,
                data = b);summary(agr.br)
agr.probit <- brglm(agreeiss ~ lamin + lcmin + 
                  icowsal +
                  samereg + 
                  lag_pch_gdp_min + 
                  recmidwt + recnowt + recyeswt + 
                  lcaprat + ldefense + contdir,
                data = b, family = binomial(link = "probit"));summary(agr.br)

agr.br <- brglm(agreeiss ~ lamin + 
                         icowsal + riveriss + mariss +
                         samereg + 
                         lag_pch_gdp_min + 
                         recmidwt + recnowt + recyeswt + 
                         lcaprat + ldefense + contdir,
                       data = b);summary(agr.br)

agr.nb <- MASS::glm.nb(sagreeiss ~ lamin + lcmin + lbmin + 
                         icowsal + riveriss + mariss +
                         samereg + 
                         lag_pch_gdp_min + 
                         recmidwt + recnowt + recyeswt + 
                         lcaprat + ldefense + contdir,
                       data = b);summary(agr.nb)

att_pr <- ggpredict(att, ci.lvl = 0.90, terms = c("lamin [-0.22, 0.43]"))
att_pl <- plot(att_pr) + geom_vline(xintercept = c(0, 0.37)) +
  labs(title = (""),
       x = ("Trade/GDP (Minimum)"),
       y = ("Probability of Peaceful Settlement Attempt")); plot(att_pl)
agr_pr <- ggpredict(agr, ci.lvl = 0.90, terms = c("lamin [-0.22, 0.43]"))
agr_pl <- plot(agr_pr) + geom_vline(xintercept = c(0, 0.37)) +
  labs(title = (""), #Effect of Trade Dependence on Substantive Agreements
       x = ("Trade/GDP (Minimum)"),
       y = ("Probability of Substantive Agreement")); plot(agr_pl)
# figures - caption below the figure
# tables - caption above the tapble
xtable(agr, 
       caption = "Negative Binomial Regression of ")
