setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis")
library(tvcure)
library(ggpubr)
options(scipen = 999)
if (!exists("icow_part_cyr")) {
  icow_part_cyr <- readRDS("./data/ICOWFinal.RDS")
} else icow <- icow_part_cyr


hazform <- as.formula( ~ icowsal + riveriss + mariss +
                         recmidwt + recfatwt + recnowt + recyeswt +
                         lag_pch_gdpmax + lgovcrisesdy + midissyr)

cureform <- as.formula( ~ icowsal + riveriss + mariss +
                          demdy + autdy +
                          lcaprat + ldefense + contdir + igosum + 
                          lag_pch_gdpmax + lgovcrisesdy)

varlist1 = list("Minimum Economic Dependence" = "lagdee2",
               "Claim Salience" = "icowsal",
               "Number of Obs." = "nfail")
varlist2 = list("Maximum Economic Dependence" = "lagmac2",
               "Claim Salience" = "icowsal",
               "Number of Obs." = "nfail")

deemin <- round(with(icow, mean(lagdee2, na.rm = 2) - 2 * sd(lagdee2, na.rm = T)), 0)
deemax <- round(with(icow, mean(lagdee2, na.rm = 2) + 2 * sd(lagdee2, na.rm = T)), 0)
macmin <- round(with(icow, mean(lagmac2, na.rm = 2) - 2 * sd(lagmac2, na.rm = T)), 0)
macmax <- round(with(icow, mean(lagmac2, na.rm = 2) + 2 * sd(lagmac2, na.rm = T)), 0)

##### attempt models---------------------------------------------
at1 <- tvcure(update.formula(hazform, Surv(at_start, at_stop, attanyp) ~ latdee2 + .), 
              cureform = update.formula(cureform, ~ latdee2 + .),
             data = icow, 
             var = T, nboot = 100, brglm = T); summary(at1)
at2 <- tvcure(update.formula(hazform, Surv(at_start, at_stop, attanyp) ~ latmac2 + .),
              cureform =  update.formula(cureform, ~ latmac2 + .),
              data = icow,
              var = T, nboot = 100, brglm = T, parallel = T); summary(at2)

# Predictions and plots
at1pred <- prediction3(at1, "lagdee2", c(seq(deemin, deemax, 1)), "uncureprob")
at2pred <- prediction3(at2, "lagmac2", c(seq(macmin, macmax, 1)), "uncureprob")

at1plot <- plot(at1pred)
at1plot <- at1plot + xlab("Minimum Economic Dependence"); at1plot
at2plot <- plot(at2pred)
at2plot <- at2plot + xlab("Maximum Economic Dependence"); at2plot

at1preds <- prediction3(at1, "lagdee2", c(deemin, deemax), "spop")
at2preds <- prediction3(at2, "lagmac2", c(macmin, macmax), "spop")

at1plots <- plot(at1preds)
at1plots <- at1plots + xlab("Minimum Economic Dependence"); at1plot
at2plots <- plot(at2preds)
at2plots <- at2plots + xlab("Maximum Economic Dependence"); at2plot

ggarrange(at1plot, at1plots, at2plot, at2plots)

at1_t <- tvtable.tvcure(at1, format = "long", varlist = varlist1)
at2_t <- tvtable.tvcure(at2, format = "long", varlist = varlist2)
at_table <- tvtable_combine(c("at1_t", "at2_t"), format = "long", footnote = "text", varlist = varlist)
x <- tvtable_xtable(at_table, caption = "Peaceful Settlement Attempts")
printer(x, file = "./tables/at_table.tex", tabular.enviroment = "tabularx", booktabs = T)

##### agreement models-------------------------------------------

ag1 <- tvcure(update.formula(hazform, Surv(agstart, agstop, agterm) ~ lagdee2 + .),
              cureform = update.formula(cureform, ~ lagdee2 + .),
              data = icow, 
              var = T, nboot = 100, brglm = T); summary(ag1)
ag2 <- tvcure(update.formula(hazform, Surv(agstart, agstop, agterm) ~ lagmac2 + .),
             cureform = update.formula(cureform, ~ lagmac2 + .),
             data = icow, 
             var = T, nboot = 100, brglm = T); summary(ag2)

# Predictions and plots
ag1pred <- prediction3(ag1, "lagdee2", c(seq(deemin, deemax, 1)), "uncureprob")
ag2pred <- prediction3(ag2, "lagmac2", c(seq(macmin, macmax, 1)), "uncureprob")

ag1plot <- plot(ag1pred)
ag1plot <- ag1plot + xlab("Minimum Economic Dependence"); ag1plot
ag2plot <- plot(ag2pred)
ag2plot <- ag2plot + xlab("Maximum Economic Dependence"); ag2plot
ggarrange(ag1plot, ag2plot)
ag1preds <- prediction3(ag1, "lagdee2", c(deemin, deemax), "spop")
ag2preds <- prediction3(ag2, "lagmac2", c(macmin, macmax), "spop")
ag1plots <- plot(ag1preds)
ag1plots <- ag1plots + xlab("Minimum Economic Dependence"); ag1plot
ag2plots <- plot(ag2preds)
ag2plots <- ag2plots + xlab("Maximum Economic Dependence"); ag2plot
ggarrange(ag1plots, ag2plots)
ggarrange(ag1plot, ag1plots)
ggarrange(ag1plot, ag1plots, ag2plot, ag2plots)

##### Claim termination models ----------------------------------------------------------------
clterm1 <- tvcure(update.formula(hazform, Surv(clstart, clstop, clterm) ~ lagdee2 + .),
                  cureform = update.formula(cureform, ~ lagdee2 + .),
                data = icow, 
                var = T, nboot = 100, brglm = T); summary(clterm1)
clterm2 <- tvcure(update.formula(hazform, Surv(clstart, clstop, clterm) ~ lagmac2 + .),
                  cureform = update.formula(cureform, ~ lagmac2 + .),
                 data = icow, 
                 var = T, nboot = 100, brglm = T); summary(clterm2)


##### MID models ------------------------------------------
midterm1 <- tvcure(update.formula(hazform, Surv(clstart, clstop, clterm) ~ lagdee2 + .),
                 cureform = update.formula(cureform, ~ lagdee2 + .), 
                 data = icow, 
                 var = F, nboot = 100, brglm = T); summary(midterm1)
midterm2 <- tvcure(update.formula(hazform, Surv(midstart, midstop, midissyr) ~ lagmac2 + .),
                  cureform =  ~ update.formula(cureform, ~ lagmac2 + .),
                  data = icow,
                  var = T, nboot = 100, brglm = T); summary(midterm2)
