agr <- tvcure(Surv(agstart1, agstop2, agfail3) ~ lamin + 
                icowsal + 
                recmidwt + recnowt + recyeswt +
                ldefense + contdir,
              cureform = ~  icowsal + terriss + mariss + 
                samereg + lncaprat + 
                ldefense + contdir,
              data = b,
              brglm = T,
              nboot = 30);summary(agr)



Conditions under which a state cannot identify a mutually acceptable agreement:
- caprat
- alliances
- demdy
- autdy
- contdir
- rivalry

Conditions which facillitate agreements (information-exchange):
- capability change
- recent mids and recent agreements
- issues
- salience

agr <- tvcure(Surv(agstart1, agstop2, agfail3) ~ lag_depdymin_100 +
                icowsal + terriss + mariss + 
                recmidwt + recnowt + recyeswt,
              cureform = ~ lag_depdymin_100 +
                icowsal + 
                demdy + autdy + lncaprat + 
                ldefense + contdir + igosum,
              data = b,
              brglm = T,
              nboot = 30);summary(agr)



ggplot(data = b, mapping = aes(lamin, agreeiss)) + geom_point(shape = 3)
  
)
obsdv = b$agreeiss
pp = b$lamin
fill = "red"

d = b$lag_ln_depdymin_100
a = ifelse(b$agreeiss == 1, d, NA)
ggplot(data = b) + geom_density(aes(d)) + geom_rug(aes(a))



sepplot <- function(obsdv, pp, filename, fill = "red") {
  df <- data.frame(obsdv = obsdv, pp = pp, rank =  rank(pp))
  df2 <- df[df$obsdv == 1, ]
  minx <- min(df2$pp)
  maxx <- max(df2$pp)
  ggsave(
    filename = filename,
    ggplot(aes(x = pp, y = obsdv), data = df) +
      geom_rect(aes(fill = obsdv, xmin = minx, xmax = maxx, ymin = 0,
                    ymax = 1, group = pp), fill = fill, data = df) +
      xlim (0, maxx) #+ geom_line(aes(x = rank, y = pp)) + 
      labs(x = "Predicted Probability Rank", y = "Predicted Probability") + 
      theme(plot.margin = unit(c(0,0,0,0), "cm"), 
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank()), 
    width = 6.5,
    height = 1.5,
    dpi = 1200
  )
}
sepplot(dat.undersamp$midonset, pp.caprat.us[, 2], "NDUinsampSepPlot.png")

sepplot(b$lamin, b$agreeiss, "a.png")

# expand the bargaining range
# expand the bargaining range

agr <- tvcure(Surv(clstart, clstop, agreeiss) ~ lag_depdymin_100 +
                icowsal + terriss + mariss + 
                recmidwt + recnowt + recyeswt + recfatwt,
              cureform = ~       lag_depdymin_100+          icowsal + 
                demdy + autdy + lncaprat + 
                ldefense + contdir + igosum,
              data = b,
              brglm = T,
              nboot = 30);summary(agr)
z <- b$lag_depdymin_100 * 100
icow_part_cyr$sagreeiss <- ifelse(is.na(icow_part_cyr$sagreeiss), 0, icow_part_cyr$sagreeiss)
icow_part_cyr$charlie <- ifelse(icow_part_cyr$depdymin_100 == 0, 0, log(icow_part_cyr$depdymin_100))
b$sagreeiss <- ifelse(is.na(b$sagreeiss), 0, b$sagr)
g <- ifelse(b$lamin > )
agr <- MASS::glm.nb(sagreeiss ~ charlie + icowsal + 
                      riveriss + mariss +
                      recmidwt + recfatwt + recnowt + recyeswt + 
                      demdy + autdy + lcaprat + 
                      ldefense + contdir + clstop,
                    data = icow_part_cyr);summary(agr)
agr <- brglm::brglm(agreeiss ~ lag_ln_depdymin_100 + icowsal +  clstop +
                      icowsal + riveriss + mariss + 
                      recmidwt + recfatwt + recnowt + recyeswt + 
                      demdy + autdy + lcaprat +
                      ldefense + contdir,
                    family = binomial,
                    data = icow_part_cyr);summary(agr)
agr <- glm(agreeiss ~ charlie + icowsal +  clstop +
                      icowsal + riveriss + mariss + 
                      recmidwt + recnowt + recyeswt + 
                      demdy + autdy + lcaprat +
                      ldefense + contdir,
                    family = binomial,
                    data = icow_part_cyr);summary(agr)
agr <- glmer(agreeiss ~ charlie + icowsal +  clstop +
             icowsal + riveriss + mariss + 
             recmidwt + recfatwt + recnowt + recyeswt + 
             demdy + autdy + lcaprat +
             ldefense + contdir + (1 | claimdy) + (1 | year),
           family = binomial,
           data = icow_part_cyr,
           control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                  optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)));summary(agr)
agr <- brm(agreeiss ~ charlie + icowsal +  clstop +
             icowsal + riveriss + mariss + 
             recmidwt + recfatwt + recnowt + recyeswt + 
             demdy + autdy + lcaprat +
             ldefense + contdir + (1 | year) + 
             (1 | mm(ccode1, ccode2)),
           family = bernoulli,
           prior = set_prior(prior = "student_t(1, 0, 2.5)", # (degrees of freedom, mean, standard deviation)
                             class = "b"),
           data = icow_part_cyr,
           cores = 4); summary(agr)


plot(ggpredict(agr, c("charlie [-18, 0]")))

plot(ggpredict(agr, c("icowsal [2, 10]", "lamin [-.23, .4]")))

plot(ggpredict(agr, c("lamin [-.23, .4]", "icowsal [2, 10]")))
agr <- glmer(agreeiss ~ lamin * icowsal + clstop +
               icowsal + riveriss + mariss + 
               recmidwt + recfatwt + recnowt + recyeswt + 
               demdy + autdy + lncaprat +
               ldefense + contdir + 
               (1|claimdy) + (1|year),
             family = binomial,
             control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                    optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)),
             data = b);summary(agr)

alpha <- dat$depdymin_100
beta <- ifelse(alpha > 1, 1, alpha)
hist(beta)
charlie <- log(dat$depdymin_100)
icow_part_cyr$charlie <- ifelse(icow_part_cyr$depdymin_100 == 0, 0, log(icow_part_cyr$depdymin_100))
icow_part_cyr$mac <- icow_part_cyr$ln_trade_100 - icow_part_cyr$ln_gdp_min
icow_part_cyr$dennis <- icow_part_cyr$gdp_min * 1000000
icow_part_cyr$cricket <- icow_part_cyr$gdp_max * 1000000
icow_part_cyr$dee <- icow_part_cyr$ln_trade - log(icow_part_cyr$dennis)
icow_part_cyr$frank <- icow_part_cyr$ln_trade - log(icow_part_cyr$cricket)

icow_part_cyr = icow_part_cyr %>% mutate(
  pikachu = trade_100/(gdp1) * 100,
  squirtle = trade_100/(gdp2) * 100,
  bulbasaur = pikachu * squirtle, 
  charmander = rowMins(cbind(pikachu, squirtle)),
  psyduck = rowMaxs(cbind(pikachu, squirtle)), 
  staryu = if_else(ccode1 == chal, pikachu, squirtle),
  starmie = if_else(ccode1 == tgt, pikachu, squirtle)
)
summary(icow_part_cyr$bulbasaur) * 100

icow_part_cyr$lncaprat <- log(icow_part_cyr$lcaprat)
icow_part_cyr$igosum <- ifelse(is.na(icow_part_cyr$igosum), 0, icow_part_cyr$igosum)
icow_part_cyr$c <- icow_part_cyr$clstop
icow_part_cyr$c2 <- icow_part_cyr$clstop^2 / 1000
icow_part_cyr$c3 <- icow_part_cyr$clstop^3 / 10000


agr1 <- brglm(agreeiss ~ dee,
              family = binomial,
              data = icow_part_cyr); summary(agr1)

agr2 <- glm(agreeiss ~ dee + 
              icowsal + riveriss + mariss,
            family = binomial,
            data = icow_part_cyr); summary(agr2)

agr3 <- glm(agreeiss ~ dee + 
              icowsal + riveriss + mariss + 
              clstop + recmidwt + recfatwt + recnowt + recyeswt,
            family = binomial,
            data = icow_part_cyr); summary(agr3)

agr4 <- glm(agreeiss ~ dee + 
              icowsal + riveriss + mariss + 
              clstop + recmidwt + recfatwt + recnowt + recyeswt + 
              lcaprat + ldefense + contdir + igosum,
            family = binomial,
            data = icow_part_cyr); summary(agr4)

agr4 <- glm(agreeiss ~ dee + 
              icowsal + riveriss + mariss + 
              clstop + recmidwt + recfatwt + recnowt + recyeswt + 
              demdy + autdy,
            family = binomial,
            data = icow_part_cyr); summary(agr4)

agr5 <- glm(agreeiss ~ dee + 
              icowsal + riveriss + mariss + 
              c + recmidwt + recfatwt + recnowt + recyeswt + 
              lcaprat + ldefense + contdir + igosum + 
              demdy + autdy + polmin + 
              lag_pch_gdp_min,
            family = binomial,
            data = icow_part_cyr); summary(agr5)

agr6 <- glm(agreeiss ~ dee + fra
            icowsal + riveriss + mariss + 
              c + c2 + c3 +  recmidwt + recfatwt + recnowt + recyeswt + 
              demdy + autdy +
              lag_pch_gdp_min + 
              lcaprat + ldefense + contdir + igosum,
            data = icow_part_cyr); summary(agr6)
plot(ggpredict(agr6, "dee"))
quantile(icow_part_cyr$dee, 0.05, na.rm = T)
quantile(icow_part_cyr$dee, 0.95, na.rm = T)
plot(ggpredict(agr6, "dee [-37, -30]"))
agr6b <- cluster.wild.glm(agr6, icow_part_cyr, ~ claim)
agr6 <- MASS::glm.nb(sagreeiss ~ dee + 
                       icowsal + riveriss + mariss + 
                       c + c2 + c3 +  recmidwt + recfatwt + recnowt + recyeswt + 
                       demdy + autdy +
                       lag_pch_gdp_min + 
                       lcaprat + ldefense + contdir + igosum,
                     data = icow_part_cyr); summary(agr6)

agr6 <- glmer(agreeiss ~ dee + 
                icowsal + riveriss + mariss + 
                c + recmidwt + recfatwt + recnowt + recyeswt + 
                demdy + autdy +
                lag_pch_gdp_min + 
                lcaprat + ldefense + contdir + igosum + (1 | dyad) + (1 | year),
              data = icow_part_cyr, 
              family = binomial,
              control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                     optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))); summary(agr6)

icow_part_cyr$waitress <- icow_part_cyr$dee
agr7b <- glmer(agree ~ dee:frank + 
                 icowsal + riveriss + mariss + 
                 clstop + recmidwt + recfatwt + recnowt + recyeswt + 
                 demdy + autdy +
                 lag_pch_gdp_min + 
                 lcaprat + ldefense + contdir + igosum + 
                 (1 | dyad) + (1 | year),
               data = icow_part_cyr,
               family = binomial,
               control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                      optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))); summary(agr7b)
summary(icow_part_cyr$dee)
summary(icow_part_cyr$frank)
plot(ggpredict(agr7b, terms = c("dee [-40, -30, -20, 10, 0, 20, 30]", "frank [-45, 31]")))
agr7b <- glmer(agree ~ dee * frank + 
                 icowsal + riveriss + mariss + 
                 clstop + recmidwt + recfatwt + recnowt + recyeswt + 
                 demdy + autdy +
                 lag_pch_gdp_min + 
                 lcaprat + ldefense + contdir + igosum + 
                 (1 | dyad) + (1 | year),
               data = icow_part_cyr,
               family = binomial,
               control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                      optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))); summary(agr7)

agr7 <- glmer(agreeiss ~ dee + frank + 
                icowsal + riveriss + mariss + 
                clstop + recmidwt + recfatwt + recnowt + recyeswt + 
                demdy + autdy +
                lag_pch_gdp_min + 
                lcaprat + ldefense + contdir + igosum + 
                (1 | dyad) + (1 | year),
              data = icow_part_cyr,
              family = binomial,
              control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                     optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))); summary(agr7)

agr7 <- glmer(agreeiss ~ dee + 
                icowsal + riveriss + mariss + 
                c + c2 + c3 + recmidwt + recfatwt + recnowt + recyeswt + 
                demdy + autdy +
                lag_pch_gdp_min + 
                lcaprat + ldefense + contdir + igosum + 
                (1 | dyad) + (1 | year),
              data = icow_part_cyr,
              family = binomial,
              control = glmerControl(optimizer = "nloptwrap"));summary(agr7)
# 
agr8 <- brm(agreeiss ~ dee +
              icowsal + riveriss + mariss +
              c + c2 + c3 + recmidwt + recfatwt + recnowt + recyeswt +
              demdy + autdy +
              lag_pch_gdp_min +
              lcaprat + ldefense + contdir + igosum +
              (1 | dyad) + (1 | year),
            family = bernoulli,
            prior = set_prior(prior = "lasso()", # (degrees of freedom, mean, standard deviation)
                              class = "b"),
            #set_prior(prior = "student_t(1, 0, 2.5)", class = "sd")),
            data = icow_part_cyr,
            cores = 4); summary(agr8)


agr8 <- coxph(Surv(clstart, clstop, agreeiss) ~ dee +
                icowsal + riveriss + mariss +
                recmidwt + recfatwt + recnowt + recyeswt +
                demdy + autdy +
                lag_pch_gdp_min +
                lcaprat + ldefense + contdir + igosum, cluster = claimdy, data = icow_part_cyr); summary(agr8)


cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
agr8 <- tvcure(Surv(clstart, clstop, agree) ~ dee +
                 icowsal + riveriss + mariss +
                 recmidwt + recfatwt + recnowt + recyeswt +
                 demdy + autdy +
                 lag_pch_gdp_min +
                 lcaprat + ldefense + contdir + igosum, 
               cureform =  ~ dee +
                 icowsal + riveriss + mariss +
                 recmidwt + recfatwt + recnowt + recyeswt +
                 demdy + autdy +
                 lag_pch_gdp_min +
                 lcaprat + ldefense + contdir + igosum, 
               data = icow_part_cyr, 
               var = T, nboot = 30, brglm = T); summary(agr8)

a <- prediction3(agr8, "dee", c(-44, -29), "uncureprob", CI = T, nsims = 1000, internals = T)
a$splot
-# # agr8 <- brm(agreeiss ~ dee + 
  # #               icowsal + riveriss + mariss + 
  # #               clstop + recmidwt + recfatwt + recnowt + recyeswt + 
  # #               demdy + autdy +
  # #               lag_pch_gdp_min + 
  # #               lcaprat + ldefense + contdir + igosum + 
  # #               (dee | dyad) + (1 | year),
  # #             family = bernoulli,
  # #             prior = c(set_prior(prior = "student_t(1, 0, 2.5)", # (degrees of freedom, mean, standard deviation)
  # #                                 class = "b"),
  # #                       set_prior(prior = "student_t(1, 0, 2.5)", class = "sd")),
  # #             data = icow_part_cyr,
# #             cores = 4); summary(agr8)
agr9 <- glm(agreeiss ~ staryu * starmie + 
              icowsal + riveriss + mariss + 
              c + recmidwt + recfatwt + recnowt + recyeswt + 
              demdy + autdy +
              lag_pch_gdp_min + 
              lcaprat + ldefense + contdir + igosum,
            data = icow_part_cyr); summary(agr9)
agr6 <- glm(agreeiss ~ dee + 
              icowsal + riveriss + mariss + 
              c + recmidwt + recfatwt + recnowt + recyeswt + 
              demdy + autdy +
              lag_pch_gdp_min + 
              lcaprat + ldefense + contdir + igosum,
            data = icow_part_cyr); summary(agr6)

ag10 <- tvcure(Surv(clstart, clstop, agreeiss) + icowsal + riveriss + mariss, 
               cureform = ~ dee, var = F, data = icow_part_cyr)
