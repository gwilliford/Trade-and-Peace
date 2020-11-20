setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis")
library(tvcure)
library(dplyr)
library(ggpubr)
library(survminer)
options(scipen = 999)
if (!exists("icow_part_cyr")) icow <- readRDS("./data/ICOWFinal.RDS")

hazform <- as.formula( ~ icowsal + riveriss + mariss +
                         recmidwt + recnowt + recyeswt + bdymid)

cureform <- as.formula( ~ icowsal + riveriss + mariss +
                          demdy + autdy +
                          lcaprat + ldefense + contdir + igosum)

fullform <- as.formula( ~ icowsal + riveriss + mariss +
                          recmidwt + recfatwt + recnowt + recyeswt +
                          lag_pch_gdpmax + lgovcrisesdy + midissyr +  
                          demdy + autdy +
                          lcaprat + ldefense + contdir + igosum)


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
                
deemin <- min(icow$lagdee2, na.rm = T)
deemax <- max(icow$lagdee2, na.rm = T)

deemin <- round(with(icow, mean(lagdee2, na.rm = 2) - 2 * sd(lagdee2, na.rm = T)), 0)
deemax <- round(with(icow, mean(lagdee2, na.rm = 2) + 2 * sd(lagdee2, na.rm = T)), 0)
macmin <- round(with(icow, mean(lagmac2, na.rm = 2) - 2 * sd(lagmac2, na.rm = T)), 0)
macmax <- round(with(icow, mean(lagmac2, na.rm = 2) + 2 * sd(lagmac2, na.rm = T)), 0)

##### Descriptives ------------------------------------------------------------------
desc <- icow %>%
  group_by(claimdy) %>%
  summarize(
    natt = if_else(sum(attanyp, na.rm = T) > 0, 1, 0),
    nag = if_else(sum(agree, na.rm = T) > 0, 1, 0)
  )
table(desc$natt)
table(desc$nag)
desc2 <- icow %>%
  group_by(claimdy, year) %>%
  summarize(
    natt = if_else(sum(attanyp, na.rm = T) > 0, 1, 0),
    nag = if_else(sum(agree, na.rm = T) > 0, 1, 0)
  )
table(desc2$natt)
table(desc2$nag)

### Descriptives
attime <- icow %>%
  filter(attanyp == 1)
summary(attime$at_stop)
plot(hist(attime$at_stop))
agtime <- icow %>%
  filter(agterm == 1)
summary(agtime$agstop)
plot(hist(agtime$agstop))
agisstime <- icow %>%
  filter(agissterm == 1)
summary(agisstime$agiss_stop)
plot(hist(agisstime$agiss_stop))


fit = survfit(Surv(at_start, at_stop, attanyp) ~ 1, data = icow)
fit = survfit(Surv(agstart, agstop, agterm) ~ 1, data = icow)
fit = survfit(Surv(agiss_start, agiss_stop, agissterm) ~ 1, data = icow)
fit = survfit(Surv(clstart, clstop, attanyp) ~ 1, data = icow)
fit = survfit(Surv(clstart, clstop, agterm) ~ 1, data = icow)
fit = survfit(Surv(clstart, clstop, agissterm) ~ 1, data = icow)
fit = survfit(Surv(clstart, clstop, clterm) ~ 1, data = icow)

ggsurvplot(fit, data = icow, surv.median.line = "v", risk.table = T, break.x.by = 20)
-a + geom_vline(xintercept = 21)

a <- seq(0, 160, 20)
a <- cut(icow$at_stop, breaks = 20)
icow$atperiod <- ifelse(as.logical(icow$at_stop <= 30), 1, NULL)
x <- icow %>% mutate(
  atperiod = ifelse(atstop <= 30, 30, NULL))#,
  aaareplace(atperiod, atstop > 31 & at_stop < 60, 60
          ))

icow$atint <- cut(icow$at_stop, breaks = seq(0, 160, 20))
icow$agint <- cut(icow$agstop, breaks = seq(0, 160, 20))
icow$agissint <- cut(icow$agiss_stop, breaks = seq(0, 160, 20))

a <- table(icow$atint, icow$attanyp)
b <- table(icow$agint, icow$agterm)
c <- table(icow$agissint, icow$agissterm)
d <- cbind(a, b, c)

##### attempt models---------------------------------------- -----
fullform2 <- as.formula( ~ icowsal + riveriss + mariss +
                          recmidwt + recnowt + recyeswt +
                          lag_pch_gdpmax + lgovcrisesdy + bdymid +  
                          demdy + autdy +
                          lcaprat + ldefense + contdir + igosum)

cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
agcox1 <- coxph(update.formula(fullform2, Surv(agstart, agstop, agterm) ~ lagdee2 + .), data = icow)
agisscox1 <- coxph(update.formula(fullform2, Surv(agiss_start, agiss_stop, agissterm) ~ lagdee2 + .), data = icow)
fit <- survfit(atcox1, icow)
ggsurvplot(fit, data = icow)
fullform <- model.matrix
d <- icow[, colnames(icow) %in% attr(atcox1$terms, "term.labels")]
d <- apply(d, 2, median, na.rm= T)
d <- rbind(d, d)
d[1, 16] <- -27
d[2, 16] <- deemax
fit <- survfit(atcox1, as.data.frame(d))
plot(fit, conf.int = T, col = 1:2)

at1 <- tvcure(update.formula(hazform, Surv(clstart, clstop, attanyp) ~ lagdee2 + .), 
              cureform = update.formula(cureform,  ~ updown  + .),
             data = icow, 
             var = T, nboot = 100, brglm = F); summary(at1)
at2 <- tvcure(update.formula(hazform, Surv(at_start, at_stop, attanyp) ~ .),
              cureform =  update.formula(cureform, ~ lagmac2 + .),
              data = icow,
              var = T, nboot = 100, brglm = T, parallel = T); summary(at2)







at_table <- tvtable(at1, at2, varlist = varlist1)
at_x     <- tvtable_xtable(at_table)
a <- printer(at_x, file = "C:/Users/gwill/Dropbox/Research/Dissertation/Chapter 3 - CM Attempts and Settlement/tab/tabattempts.tex")
# Predictions 
at1pred  <-  prediction3(at1, "mariss", c(0), "basesurv")

at1pred  <- prediction3(at1, "lagdee2", c(seq(deemin, deemax, 1)), "uncureprob")
at2pred  <- prediction3(at2, "lagmac2", c(seq(macmin, macmax, 1)), "uncureprob")
at1preds <- prediction3(at1, "lagdee2", c(deemin, deemax), "spop")
at1preds <- prediction3(at1, "lagdee2", c(deemin, deemax), "suncure")




at2preds <- prediction3(at2, "lagmac2", c(macmin, macmax), "spop")

# Probabilty plots
at1plot <- plot(at1pred)
at1plot <- at1plot + xlab("Minimum Economic Dependence") + ylab("Potential for Agreement")
at2plot <- plot(at2pred)
at2plot <- at2plot + xlab("Maximum Economic Dependence") + ylab("Potential for Agreement")

# get_legend<-function(myggplot){
#   tmp <- ggplot_gtable(ggplot_build(myggplot))
#   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#   legend <- tmp$grobs[[leg]]
#   return(legend)
# }
# get_legend<-function(myggplot){
#   tmp <- ggplot_gtable(ggplot_build(myggplot))
#   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#   legend <- tmp$grobs[[leg]]
#   return(legend)
# }

# Survival plots
at1plots <- plot(at1preds)
at1plots <- at1plots + 
  scale_fill_discrete(name = "Minimum Economic \nDependence", labels = c("Low", "High")) +
  scale_color_discrete(name = "Minimum Economic \nDependence", labels = c("Low", "High")) +
  scale_linetype_discrete(name = "Minimum Economic \nDependence", labels = c("Low", "High")) + 
  theme(legend.title.align = 0.5) 
legend1 <- ggpubr::get_legend(at1plots)
at1plots <- at1plots + theme(legend.position = "none")
at1plotf <- ggarrange(at1plot, at1plots, legend1, ncol = 3, widths = c(1, 1, 0.4))

# at1plotf <- grid.arrange(at1plot, at1plots, legend1, ncol = 2, heights = c(1, 0.1), layout_matrix = rbind(c(1,2), c(1,3)))
# b <- grid.arrange(at2plot, at2plots, legend2, ncol = 2, heights = c(1, 0.1), layout_matrix = rbind(c(1,2), c(3,3)))

# 

at2plots <- plot(at2preds)
at2plots <- at2plots + 
  scale_fill_discrete(name = "Maximum Economic \nDependence", labels = c("Low", "High")) +
  scale_color_discrete(name = "Maximum Economic \nDependence", labels = c("Low", "High")) +
  scale_linetype_discrete(name = "Maximum Economic \nDependence", labels = c("Low", "High")) + 
  theme(legend.title.align = 0.5) 
legend2 <- ggpubr::get_legend(at2plots)
at2plots <- at2plots + theme(legend.position = "none")
at2plotf <- ggarrange(at2plot, at2plots, legend2, ncol = 3, widths = c(1, 1, 0.4))

# # Combined plot
# # ggarrange(at1plot, at1plots, at2plot, at2plots, heights = c(1, 5, 1, 1))
# # grid.arrange(at1plot, at1plots, legend1, ncol = 2, heights = c(1, 0.1), layout_matrix = rbind(c(1,2), c(3,3)))
# a <- grid.arrange(at1plot, at1plots, legend1, ncol = 2, heights = c(1, 0.1), layout_matrix = rbind(c(1,2), c(1,3)))
# b <- grid.arrange(at2plot, at2plots, legend2, ncol = 2, heights = c(1, 0.1), layout_matrix = rbind(c(1,2), c(3,3)))
# grid.arrange(a, b)
grid.arrange(at1plot, at1plots, legend1,
             at2plot, at2plots, legend2,
             ncol = 3, nrow = 2, widths = c(1, 1, 0.5))
             # nrow = 4, heights = c(1, 0.1, 1, .1),
             

# 
# at1_t <- tvtable.tvcure(at1, format = "long", varlist = varlist1)
# at2_t <- tvtable.tvcure(at2, format = "long", varlist = varlist2)
# at_table <- tvtable_combine(c("at1_t", "at2_t"), format = "long", footnote = "text", varlist = varlist)
# x <- tvtable_xtable(at_table, caption = "Peaceful Settlement Attempts")
# printer(x, file = "./tables/at_table.tex", tabular.enviroment = "tabularx", booktabs = T)
# gridExtra::grid.arrange(at1plot, at2plot, at1plots, at2plots, layout_matrix = c(2, 2))

##### agreement models-------------------------------------------
cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
ag1 <- tvcure(update.formula(hazform, Surv(agstart, agstop, agterm) ~ lagdee2 + .),
              cureform = update.formula(cureform, ~ .),
              data = icow, 
              var = T, nboot = 100, brglm = T); summary(ag1)
ag2 <- tvcure(update.formula(hazform, Surv(agstart, agstop, agterm) ~ lagmac2 + .),
             cureform = update.formula(cureform, ~ .),
             data = icow, 
             var = T, nboot = 100, brglm = T); summary(ag2)


ag_table <- tvtable(ag1, ag2, varlist = varlist1)
ag_x     <- tvtable_xtable(ag_table)
a <- printer(ag_x, file = "C:/Users/gwill/Dropbox/Research/Dissertation/Chapter 3 - CM Attempts and Settlement/tab/tabagree.tex")

# Predictions and plots
ag1pred <- prediction3(ag1, "lagdee2", c(seq(deemin, deemax, 1)), "uncureprob")
ag2pred <- prediction3(ag2, "lagmac2", c(seq(macmin, macmax, 1)), "uncureprob")
ag1plot <- plot(ag1pred)
ag1plot <- ag1plot + xlab("Minimum Economic Dependence"); ag1plot
ag2plot <- plot(ag2pred)
ag2plot <- ag2plot + xlab("Maximum Economic Dependence"); ag2plot
ggarrange(ag1plot, ag2plot)

ag1preds <- prediction3(ag1, "lagdee2", c(deemin, deemax), "spop", internals = T)
ag2preds <- prediction3(ag2, "lagmac2", c(macmin, macmax), "spop")
ag1plots <- plot(ag1preds)
ag1plots <- ag1plots + xlab("Minimum Economic Dependence"); ag1plot
ag2plots <- plot(ag2preds)
ag2plots <- ag2plots + xlab("Maximum Economic Dependence"); ag2plot

ggarrange(ag1plots, ag2plots)
ggarrange(ag1plot, ag1plots)
ggarrange(ag1plot, ag1plots, ag2plot, ag2plots)

##### Claim termination models ----------------------------------------------------------------
cox <- coxph(update.formula(fullform, Surv(clstart, clstop, clterm) ~ lagdee2 + .),
              data = icow)
cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
clterm1 <- tvcure(update.formula(hazform, Surv(clstart, clstop, clterm) 
                                 ~ lagdee2 + .),
                  cureform = update.formula(cureform, ~ lagdee2 + .),
                  data = icow, 
                  var = T, nboot = 100, brglm = T); summary(clterm1)

clterm1 <- tvcure(update.formula(hazform, Surv(clstart, clstop, clterm) ~ lagdee2 + .),
                  cureform = update.formula(cureform, ~ lagdee2 + .),
                data = icow, 
                var = T, nboot = 100, brglm = T); summary(clterm1)
clterm2 <- tvcure(update.formula(hazform, Surv(clstart, clstop, clterm) ~ lagmac2 + .),
                  cureform = update.formula(cureform, ~ lagmac2 + .),
                 data = icow, 
                 var = T, nboot = 100, brglm = T); summary(clterm2)


hazform <- as.formula( ~ icowsal + riveriss + mariss +
                         recmidwt + recfatwt + recnowt + recyeswt + midissyr)

cureform <- as.formula( ~ icowsal + riveriss + mariss +
                          demdy + autdy +
                          lcaprat + ldefense + contdir + igosum)


cox <- coxph(update.formula(fullform, Surv(agstart, agstop, clterm) ~ lagdee2 + .),
             data = icow)
clterm1 <- tvcure(update.formula(hazform, Surv(agstart, agstop, clterm) ~ lagdee2 + .),
                  cureform = update.formula(cureform, ~ .),
                  data = icow, 
                  var = T, nboot = 100, brglm = T); summary(clterm1)
clterm2 <- tvcure(update.formula(hazform, Surv(agiss_start, agiss_stop, clterm) ~ lagdee2 + .),
                  cureform = update.formula(cureform, ~ .),
                  data = icow, 
                  var = T, nboot = 100, brglm = T); summary(clterm2)


##### MID models ------------------------------------------
cox <- coxph(update.formula(fullform, Surv(midstart, midstop, midterm) ~ lagdee2 + .),
             data = icow)

midterm1 <- tvcure(update.formula(hazform, Surv(midstart, midstop, midterm) ~ lagdee2 + .),
                 cureform = update.formula(cureform, ~ lagdee2 + .), 
                 data = icow, 
                 var = T, nboot = 100, brglm = T); summary(midterm1)
midterm2 <- tvcure(update.formula(hazform, Surv(midstart, midstop, midissyr) ~ lagmac2 + .),
                  cureform =  ~ update.formula(cureform, ~ lagmac2 + .),
                  data = icow,
                  var = T, nboot = 100, brglm = T); summary(midterm2)


######### Peaceful resolution
hazform <- as.formula( ~ icowsal + riveriss + mariss +
                         recmidwt + recnowt + recyeswt +
                         lag_pch_gdpmax + lgovcrisesdy + bdymid)

cureform <- as.formula( ~ icowsal + 
                          demdy + autdy +
                          lcaprat + ldefense + contdir + igosum)

cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
clterm1 <- tvcure(update.formula(hazform, Surv(clstart, clstop, pterm2) ~ lagdee2 + .),
                  cureform = update.formula(cureform, ~ lagdee2 + .),
                  data = icow,
                  var = T, nboot = 100, brglm = T); summary(clterm1)
clterm2 <- tvcure(update.formula(hazform, Surv(agstart, agstop, pterm2) ~ lagdee2 + .),
                  cureform = update.formula(cureform, ~ lagdee2 + .),
                  data = icow, subset = icow$cumagr > 0,
                  var = T, nboot = 100, brglm = T); summary(clterm2)
clterm3 <- tvcure(update.formula(hazform, Surv(agiss_start, agiss_stop, pterm2) ~ lagdee2 + .), 
                  cureform = update.formula(cureform, ~ lagdee2 + .),
                  data = icow, subset = icow$cumagriss > 0,
                  var = T, nboot = 100, brglm = T); summary(clterm3)
icow$updown <- ifelse(icow$lagdee2 > mean(icow$lagdee2, na.rm = T), 1, 0)

# KEEP
# clterm1 <- tvcure(update.formula(hazform, Surv(clstart, clstop, pterm2) ~ lagdee2 + .),
#                   cureform = update.formula(cureform, ~ lagdee2 + .),
#                   data = icow,
#                   var = T, nboot = 100, brglm = T); summary(clterm1)
# clterm2 <- tvcure(update.formula(hazform, Surv(agstart, agstop, pterm2) ~ lagdee2 + .),
#                   cureform = update.formula(cureform, ~ lagdee2 + .),
#                   data = icow, subset = icow$cumagr > 0,
#                   var = T, nboot = 100, brglm = T); summary(clterm2)
# clterm3 <- tvcure(update.formula(hazform, Surv(agiss_start, agiss_stop, pterm2) ~ lagdee2 + .), 
#                   cureform = update.formula(cureform, ~ lagdee2 + .),
#                   data = icow, subset = icow$cumagriss > 0,
#                   var = T, nboot = 100, brglm = T); summary(clterm3)
# a <- prediction3(clterm1, "lagdee2", c(deemin, deemax), "spop"); a
# b <- prediction3(clterm2, "lagdee2", c(deemin, deemax), "spop"); b
# d <- prediction3(clterm3, "lagdee2", c(deemin, deemax), "spop"); d
# a <- prediction3(clterm1, "lagdee2", c(deemin, deemax), "uncureprob"); a
# b <- prediction3(clterm2, "lagdee2", c(deemin, deemax), "uncureprob"); b
# d <- prediction3(clterm3, "lagdee2", c(deemin, deemax), "uncureprob"); d
cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
clterm4 <- tvcure(update.formula(hazform, Surv(clstart, clstop, pterm2) ~ lagdee2 + .),
                  cureform = update.formula(cureform, ~ updown + .),
                  data = icow,
                  var = T, nboot = 100, brglm = T); summary(clterm4)
clterm5 <- tvcure(update.formula(hazform, Surv(agstart, agstop, pterm2) ~ lagdee2 + .),
                  cureform = update.formula(cureform, ~ updown + .),
                  data = icow, subset = icow$cumagr > 0,
                  var = T, nboot = 100, brglm = T); summary(clterm5)
clterm6 <- tvcure(update.formula(hazform, Surv(agiss_start, agiss_stop, pterm2) ~ lagdee2 + .), 
                  cureform = update.formula(cureform, ~ updown + .),
                  data = icow, subset = icow$cumagriss > 0,
                  var = T, nboot = 100, brglm = T); summary(clterm6)
f <- prediction3(clterm4, "updown", c(0, 1), "spop"); f
e <- prediction3(clterm5, "updown", c(0, 1), "spop"); e
g <- prediction3(clterm6, "updown", c(0, 1), "spop"); g
f <- prediction3(clterm4, "lagdee2", c(deemin, deemax), "uncureprob"); f
e <- prediction3(clterm5, "lagdee2", c(deemin, deemax), "uncureprob"); e
g <- prediction3(clterm6, "lagdee2", c(deemin, deemax), "uncureprob"); g
f <- prediction3(clterm4, "lagdee2", c(-27, deemax), "suncure"); f
e <- prediction3(clterm5, "lagdee2", c(-27, deemax), "suncure"); e
g <- prediction3(clterm6, "lagdee2", c(-27, deemax), "suncure"); g



at1 <- tvcure(update.formula(hazform, Surv(clstart, clstop, attanyp) ~ lagdee2 + .), 
              cureform = update.formula(cureform, ~ updown + .),
              data = icow, 
              var = T, nboot = 100, brglm = F); summary(at1)
p1 <- prediction3(at1, "lagdee2", c(-27, deemax), "suncure", internals = T); p1
ag1 <- tvcure(update.formula(hazform, Surv(clstart, clstop, agterm) ~ lagdee2 + .),
              cureform = update.formula(cureform, ~ .),
              data = icow, 
              var = T, nboot = 100, brglm = T); summary(ag1)
p2 <- prediction3(ag1, "lagdee2", c(-27, deemax), "spop", internals = T); p2

