setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis")
library(tvcure)
library(brms)
library(survminer)
library(xtable)
options(scipen = 999)

# Data
if (!exists("icow_part_cyr")) icow <- readRDS("./data/ICOWFinal.RDS")

fullform <- as.formula( ~ lagdee2 + icowsal + riveriss + mariss +
                          recmidwt + recnowt + recyeswt + bdymid +
                          lag_pch_gdpmax + lgovcrisesdy + lcw +
                          demdy + autdy +
                          lcaprat + ldefense + contdir + igosum +
                          c + c2 + c3 + 
                          (1 | claimdy) + (1 | year))

set.seed(39458230)
rat <- brm(update.formula(fullform, ratyear ~ . - (1 | claimdy) - (1 | year)),
           family = "bernoulli",
           prior = set_prior(prior = "student_t(1, 0, 2.5)", class = "b"),
           cores = 4,
           data = icow); rat 
comp <- brm(update.formula(fullform, compyear ~ . - (1 | claimdy) - (1 | year)),
           family = "bernoulli",
           prior = set_prior(prior = "student_t(1, 0, 2.5)", class = "b"),
           cores = 4,
           data = icow); comp
end <- brm(update.formula(fullform, claimend ~ . - (1 | claimdy) - (1 | year)),
           family = "bernoulli",
           prior = set_prior(prior = "student_t(1, 0, 2.5)", class = "b"),
           cores = 4,
           data = icow); end
endall <- brm(update.formula(fullform, claimendall ~ . - (1 | claimdy) - (1 | year)),
           family = "bernoulli",
           prior = set_prior(prior = "student_t(1, 0, 2.5)", class = "b"),
           cores = 4,
           data = icow); endall


icow_set2 <- read_dta("./data/ICOWsettle.dta") %>%
  # filter(midiss == 0) %>% 
  select(-mid)
icow_set2 <- left_join(icow_set2, datlag)

r2 <- brm(ratfail ~ lagdee2 + riveriss + mariss +
                               bdymid +
                               lag_pch_gdpmax + lgovcrisesdy + lcw +
                               demdy + autdy +
                               lcaprat + ldefense + contdir + igosum +
                               (1 | claimdy) + (1 | year),
              family = "bernoulli",
              prior = set_prior(prior = "student_t(1, 0, 2.5)", class = "b"),
              cores = 4,
              data = icow_set2); r2


#### percent
# quantile(r2$fit@sim$samples[[1]]$b_lagdee2, 0.10)
# sum(r2$fit@sim$samples[[1]]$b_lagdee2 > 0) / 2000
fullform <- as.formula( ~ lagdee2 + icowsal + riveriss + mariss +
                          recmidwt + recnowt + recyeswt + bdymid +
                          lag_pch_gdpmax + lgovcrisesdy + lcw +
                          demdy + autdy +
                          lcaprat + ldefense + contdir + igosum +
                          c + c2 + c3 + 
                          (1 | claimdy) + (1 | year))

# p <- model.frame(fullform, icow)
# p <- apply(p, 2, median, na.rm = T)
# p <- rbind(p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p)
plot(marginal_effects(comp, "lagdee2"), rug = T, plot = F)
