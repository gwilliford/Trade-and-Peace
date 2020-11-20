


sjp.glmer(a, type = "eff", )
plot(ggpredict(a, "lagdee2"), ci = T)
m <- margins(a, variables = "lagdee2", at = c(-27, 0))

plot(marginal_effects(a, icow, variables = "lagdee2", allow.new.levels = T))

predict(a)
library(lme4)
a <- glmer(attanyp ~ lagdee2 + icowsal + riveriss + mariss +
            recmidwt + recnowt + recyeswt + lbdymid +
            lag_pch_gdpmax + lgovcrisesdy +  lcw +
            demdy + autdy +
            lcaprat + ldefense + contdir + igosum + 
            c + c2 + c3 + 
            (1 | claimdy) + (1 | year),
          data = icow, family = binomial(link = "logit"))
atcoxd <- icow[, colnames(icow) %in% colnames(a@frame)]
atcoxd <- apply(atcoxd, 2, median, na.rm= T)
atcoxd <- rbind(atcoxd, atcoxd)
atcoxd[, "lagdee2"] <- c(-12, -1)
atcoxd <- as.data.frame(atcoxd)

r <- predict(a, atcoxd, allow.new.levels = F, type = "response")


me <- marginal_effects(at, "lagdee2", allow.new.levels = T)


  









marginal_effects(at, "lagdee2", titles = "abcd")

posterior_predict()
plot(posterior_predict(at, atcoxd, resp = "lagdee2"))

ggpredict(a)

plot(predict(a, as.data.frame(atcoxd)))
library(margins)
b <- margins(a, variables = "lagdee2", allow.new.levels = T, type = "response")
d <- marginal_effects(a, icow, "lagdee2", allow.new.levels = T)

fit <- survfit(atcox1, as.data.frame(atcoxd))
plot(fit, conf.int = T, col = 1:2)

posterior_predict

at <- brm(attanyp ~ lagdee2 + icowsal + riveriss + mariss +
           recmidwt + recnowt + recyeswt + lbdymid +
           lag_pch_gdpmax + lgovcrisesdy +  lcw +
           demdy + autdy +
           lcaprat + ldefense + contdir + igosum + 
           c + c2 + c3 + 
           (1 | claimdy) + (1 | year),
         family = "bernoulli",
         prior = set_prior(prior = "student_t(1, 0, 2.5)", # (degrees of freedom, mean, standard deviation)
                           class = "b"),
         chains = 1,
         iter = 10,
         data = icow)
at_plot <- plot(marginal_effects(at, "lagdee2"))
at_plot$lagdee2$
ag <- brm(agterm ~ lagdee2 + icowsal + riveriss + mariss +
            recmidwt + recnowt + recyeswt + lbdymid +
            lag_pch_gdpmax + lgovcrisesdy +  lcw +
            demdy + autdy +
            lcaprat + ldefense + contdir + igosum + 
            c + c2 + c3 + 
            (1 | claimdy) + (1 | year),
          family = "bernoulli",
          prior = set_prior(prior = "student_t(1, 0, 2.5)", # (degrees of freedom, mean, standard deviation)
                            class = "b"),
          cores = 4,
          data = icow)
ag2 <- brm(agissterm ~ lagdee2 + icowsal + riveriss + mariss +
            recmidwt + recnowt + recyeswt + lbdymid +
            lag_pch_gdpmax + lgovcrisesdy +  lcw +
            demdy + autdy +
            lcaprat + ldefense + contdir + igosum + 
            c + c2 + c3 + 
            (1 | claimdy) + (1 | year),
          family = "bernoulli",
          prior = set_prior(prior = "student_t(1, 0, 2.5)", # (degrees of freedom, mean, standard deviation)
                            class = "b"),
          cores = 4,
          data = icow)
as.table(summary(ag))
a <- summary(ag)

mdevtools::source_url("https://raw.githubusercontent.com/jkarreth/JKmisc/master/mcmctab.R")
mcmctab(ag)



a <- plot(marginal_effects(ag, "lagdee2"), xlab = "a")
a <- plot(marginal_effects(ag2, "lagdee2"), lot = FALSE)[[1]] + 
  scale_color_gradient(low = "#132B43", high = "#56B1F7") +
  scale_fill_grey()


b <- plot(marginal_effects(ag2, "lagdee2", spaghetti  = T))
a + theme_dark(a)

a$lagdee2$theme <- ggtheme()
,
          theme = ggtheem)

theme_set(theme_classic())


a$lagdee2$data$lagdee2
a$lagdee2$mapping
p



##############################################################
a <- glm(agissterm ~ lagdee2 + icowsal + riveriss + mariss +
           recmidwt + recnowt + recyeswt + bdymid +
           lag_pch_gdpmax + lgovcrisesdy +  + lcw +
           demdy + autdy +
           lcaprat + ldefense + contdir + igosum + 
           c + c2 + c3,
         data = icow)
a <- brm(agissterm ~ lagdee2 + icowsal + riveriss + mariss +
           recmidwt + recnowt + recyeswt + bdymid +
           lag_pch_gdpmax + lgovcrisesdy +  + lcw +
           demdy + autdy +
           lcaprat + ldefense + contdir + igosum + 
           c + c2 + c3 + 
           (1 | claimdy) + (1 | year),
         family = "bernoulli",
         cores = 4,
         data = icow)
b <- brm(agissterm ~ lagdee2 + icowsal + riveriss + mariss +
           recmidwt + recnowt + recyeswt + bdymid +
           lag_pch_gdpmax + lgovcrisesdy +  + lcw +
           demdy + autdy +
           lcaprat + ldefense + contdir + igosum + 
           c + c2 + c3 + 
           (1 | mm(ccode1, ccode2)) + (1 | year),
         family = "bernoulli",
         cores = 4,
         data = icow)
c <- brm(agissterm ~ lagdee2 + icowsal + riveriss + mariss +
           recmidwt + recnowt + recyeswt + bdymid +
           lag_pch_gdpmax + lgovcrisesdy +  + lcw +
           demdy + autdy +
           lcaprat + ldefense + contdir + igosum + 
           c + c2 + c3 + 
           (1 | mm(ccode1, ccode2)) + (1 | year),
         family = "bernoulli",
         prior = set_prior(prior = "student_t(1, 0, 2.5)", # (degrees of freedom, mean, standard deviation)
                           class = "b"),
         cores = 4,
         data = icow)
d <- glm(agissterm ~ lagdee + icowsal + riveriss + mariss +
           recmidwt + recnowt + recyeswt + bdymid +
           lag_pch_gdpmax + lgovcrisesdy +  + lcw +
           demdy + autdy +
           lcaprat + ldefense + contdir + igosum + 
           c + c2 + c3,
         data = icow)
e <- brm(agissterm ~ lagdee + icowsal + riveriss + mariss +
           recmidwt + recnowt + recyeswt + bdymid +
           lag_pch_gdpmax + lgovcrisesdy +  + lcw +
           demdy + autdy +
           lcaprat + ldefense + contdir + igosum + 
           c + c2 + c3 + 
           (1 | mm(ccode1, ccode2)) + (1 | year),
         family = "bernoulli",
         prior = set_prior(prior = "student_t(1, 0, 2.5)", # (degrees of freedom, mean, standard deviation)
                           class = "b"),
         cores = 4,
         data = icow)

f <- glm(agissterm ~ lag_depdymilmin + icowsal + riveriss + mariss +
           recmidwt + recnowt + recyeswt + lbdymid +
           lag_pch_gdpmax + lgovcrisesdy +  lcw +
           demdy + autdy +
           lcaprat + ldefense + contdir + igosum + 
           c + c2 + c3,
         data = icow)


varlist1 = list("Minimum Economic Dependence" = "lagdee2",
                "Maximum Economic Dependence" = "lagmac2",
                "Claim Salience" = "icowsal",
                "River Claim" = "riveriss",
                "Maritime Claim" = "mariss",
                "Percent Change in GDP" = "lag_pch_gdpmax",
                "Government Crisis in Either State" = "lgovcrisesdy",
                "Any MID Initiated this Year" = "midissyr",
                "Recent MIDs" = "recmidwt",
                "Recent Fatal MIDs" = "recfatwt",
                "Recent Failed CM Attempts" = "recnowt",
                "Recent Successful CM Attempts" = "recyeswt",
                "Capability Ratio" = "lcaprat",
                "Joint Democracy" = "demdy",
                "Joint Autocracy" = "autdy",
                "Defensive Alliance" = "ldefense",
                "Contiguity" = "contdir",
                "Shared IGO Membership" = "igosum")

deemin <- round(with(icow, mean(lagdee2, na.rm = 2) - 2 * sd(lagdee, na.rm = T)), 0)
deemax <- round(with(icow, mean(lagdee2, na.rm = 2) + 2 * sd(lagdee, na.rm = T)), 0)
macmin <- round(with(icow, mean(lagmac2, na.rm = 2) - 2 * sd(lagmac2, na.rm = T)), 0)
macmax <- round(with(icow, mean(lagmac2, na.rm = 2) + 2 * sd(lagmac2, na.rm = T)), 0)

atcox <- coxph(update.formula(fullform, Surv(at_start, at_stop, attanyp) ~ lagdee + .),
               data = icow); summary(atcox)
atcoxd <- icow[, colnames(icow) %in% attr(atcox$terms, "term.labels")]
atcoxd <- apply(atcoxd, 2, median, na.rm= T)
atcoxd <- rbind(atcoxd, atcoxd)
atcoxd[, "lagdee2"] <- c(-27, -1)
fit <- survfit(atcox1, as.data.frame(atcoxd))
plot(fit, conf.int = T, col = 1:2)

agcox <- coxph(update.formula(fullform, Surv(agstart, agstop, agterm) ~ lagdee2 + .),
               data = icow); summary(agcox)
agcoxd <- icow[, colnames(icow) %in% attr(agcox$terms, "term.labels")]
agcoxd <- apply(agcoxd, 2, median, na.rm= T)
agcoxd <- rbind(agcoxd, agcoxd)
agcoxd[, "lagdee2"] <- c(-27, -1)
fit <- survfit(agcox, as.data.frame(agcoxd))
plot(fit, conf.int = T, col = 1:2)

agisscox <- coxph(update.formula(fullform, Surv(agiss_start, agiss_stop, agissterm) ~ lagdee2 + . + cluster(claimdy)),
               data = icow); summary(agisscox)
agisscoxd <- icow[, colnames(icow) %in% attr(agisscox$terms, "term.labels")]
agissd <- apply(agisscoxd, 2, median, na.rm= T)
agissd <- rbind(agissd, agissd)
agissd[, "lagdee2"] <- c(-27, -1)
# a <- survfit(agisscox, as.data.frame(agissd))
# library(survminer)
# ggsurvplot(survfit(agisscox, data = agissd), data = agissd)
# plot(fit, conf.int = T, col = 1:2)

agisscox <- coxph(Surv(agiss_start, agiss_stop, agissterm) ~ lagdee2 + 
                    icowsal + riveriss + mariss +
                    recmidwt + recnowt + recyeswt + cluster(claimdy),
                  data = icow); summary(agisscox)

