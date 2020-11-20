setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis/data/IBAD")
ibad = read.csv("ibadsett.csv")
ibad = ibad %>% select(dyad, intendyear)
ibad$sample = 1

ibad2 = read.csv("ibaddyr.csv")
ibad2 = ibad2 %>% select("dyad", "year","maxicowsal", "maxsaltan", "maxsalint", "terrchange")
dat = left_join(ibad, datlag)
dat$sett = ifelse(dat$year == dat$intendyear, 1, 0)
dats = dat[dat$sample == 1, ]
dats = dats %>% group_by(dyad) %>% mutate(lagsett = lag(sett))
dats = left_join(dats, ibad2)
dats$leadertransany = ifelse(dats$leadertrans1 == 1 | dats$leadertrans2 == 1, 1, 0)
dats$lcw = ifelse(is.na(dats$lcw), 0, 1)

fullform <- as.formula( ~ lagdee2 + icowsal + riveriss + mariss +
                          recmidwt + recnowt + recyeswt + bdymid +
                          lag_pch_gdpmax + lgovcrisesdy + lcw +
                          demdy + autdy +
                          lcaprat + ldefense + contdir + igosum +
                          c + c2 + c3 + 
                          (1 | claimdy) + (1 | year))

m1 = brglm(leadertransany ~ lagsett*lagdee2, data = dats, subset = sample == 1);summary(m1)
m2 = brglm(leadertransany ~ lagsett*lagdee2 + maxicowsal + conttype + lbdymid + demdy + lag_pch_gdpmax + lgovcrisesdy + lcw + lcaprat + ldefense + igosum + autdy, data = dats, subset = sample == 1);summary(m2)
m2 = brglm(leadertransany ~ lagsett*lagdee2 + maxicowsal * maxicowsal + conttype + log(caprat) + ldefense + igosum + autdy + lag_pch_gdpmax + lcw + trival, data = dats, subset = sample == 1);summary(m2)

m2 = brm(leadertransany ~ lagsett*lagdee2 + maxicowsal * maxicowsal + conttype + log(caprat) + ldefense + igosum + autdy + lag_pch_gdpmax + lcw + trival, data = dats, family = "bernoulli", cores = 4);summary(m2)

dats$solschany = ifelse(dats$solschdum1 == 1 | dats$solschdum2 == 1, 1, 0)

m3 = glm(solschany ~ lagsett*lagdee2 + maxicowsal * maxicowsal + conttype + log(caprat) + ldefense + igosum + autdy + lag_pch_gdpmax + lcw + trival, data = dats, subset = sample == 1);summary(m3)

s# m2 = brm(leadertransany ~ lagsett*lagdee2 + maxicowsal + conttype + lbdymid + demdy
#         + lcaprat + ldefense + igosum +  autdy + (1 | dyad) + (1 | year),
#         data = dats, 
#         family = "bernoulli",
#         cores = 4);summary(m2)



ddd = dats[dats$sample == 1, ]
d1 = d1 %>% group_by(ccode1, year) %>% summarize(
  maxdep = max(lagdee2, na.rm = t), 
  mindep = max(lagdee2, na.rm = t)
)