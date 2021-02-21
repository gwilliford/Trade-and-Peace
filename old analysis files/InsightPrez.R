setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis")
library(dplyr)
library(ggplot2)
library(tvcure)
options(scipen = 999)

# Data wrangling
mcy <- icow_part_cyr %>% group_by(c) %>% summarize(
  mmid = mean(nmidiss, na.rm = T),
  matt = mean(attemptsp, na.rm = T), 
  magg = mean(agree, na.rm = T), 
  miss = mean(agreeiss, na.rm = T),
  scl = sum(one), 
  rat = mmid/matt,
  dif = mean(mmid - matt, na.rm = T)
)
summary(mcy)
icow_part_cyr$ddum = ifelse(icow_part_cyr$ldefense > 1, 1, 0)

##### Data viz
# Number of Claims
ggplot(aes(y = scl, x = c), data = mcy) + geom_point() + geom_smooth(method = "loess") + 
  xlab("Length of Dispute (in years)") + ylab("Number of Claims") + 
  theme(panel.grid.major = element_line(colour = "grey"))

# Number of agreements
ggplot(aes(y = magg, x = c), data = mcy) + geom_point() + geom_smooth(method = "loess") + 
  xlab("Length of Dispute (in years)") + ylab("Number of Agreements") + 
  theme(panel.grid.major = element_line(colour = "grey"))

ggplot(aes(y = magg, x = c), data = mcy[mcy$c < 100, ]) + geom_point() + geom_smooth(method = "loess") + 
  xlab("Length of Dispute (in years)") + ylab("Number of Agreements") + 
  theme(panel.grid.major = element_line(colour = "grey"))

# Substantive agreements
ggplot(aes(y = miss, x = c), data = mcy) + geom_point() + geom_smooth(method = "loess") + 
  xlab("Length of Dispute (in years)") + ylab("Mean of Substantive Agreements") + 
  theme(panel.grid.major = element_line(colour = "grey"))

ggplot(aes(y = miss, x = c), data = mcy[mcy$c < 100, ]) + geom_point() + geom_smooth(method = "loess") + 
  xlab("Length of Dispute (in years)") + ylab("Mean of Substantive Agreements") + 
  theme(panel.grid.major = element_line(colour = "grey"))

# Peaceful settlement attempts vs. military disputes
ggplot(aes(y = matt, x = c), data = mcy) + geom_point(shape = 1, size = 2, color = "blue") +
  geom_smooth(method = "loess") + 
  geom_point(aes(y = mmid, x = c), color = "red", shape = 1, size = 2) + 
  geom_smooth(aes(y = mmid, x = c), method = "loess", color = "red") + 
  xlab("Length of Dispute (in years)") + ylab("Number of Peaceful Settlement Attempts") + 
  theme(panel.grid.major = element_line(colour = "grey"))


##### Analysis

# Peaceful Settlement Attempts
cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
att <- tvcure(Surv(clstart, clstop, attanyp) ~ tdepmin +
                icowsal + riveriss + mariss +
                demdy + autdy + 
                recmidwt + recfatwt + recnowt + recyeswt +
                lag_pch_gdp_min,
              cureform =  ~ tdepmin + aggtrademin + 
                demdy + autdy +
                lcaprat + ldefense + contdir + igosum, 
              data = icow_part_cyr, 
              var = T, nboot = 100, brglm = T); summary(att)
att_pred <- prediction3(att, "tdepmin", c(-44, -29), type = "uncureprob", CI = T)
plot(att_pred)

# Agreements
ag <- tvcure(Surv(clstart, clstop, agree) ~ tdepmin +
               icowsal + I(icowsal^2) + riveriss + mariss +
               recmidwt + recfatwt + recnowt + recyeswt +
               lag_pch_gdp_min,
             cureform =  ~ tdepmin +
               icowsal + I(icowsal^2) + riveriss + mariss +
               demdy + autdy +
               lcaprat + ddum + contdir + igosum, 
             data = icow_part_cyr, 
             var = T, nboot = 100, brglm = T); summary(ag)

# Substantive agreements
agiss <- tvcure(Surv(clstart, clstop, agreeiss) ~ tdepmin + aggtrademin + 
                  recmidwt + recfatwt + recnowt + recyeswt +
                  lag_pch_gdp_min,
                cureform =  ~ tdepmin + aggtrademin +
                  icowsal + I(icowsal^2) + riveriss + mariss +
                  samereg +
                  lcaprat + ldefense + contdir + igosum, 
                data = icow_part_cyr, 
                var = T, nboot = 100, brglm = T); summary(agiss)
agiss <- tvcure(Surv(clstart, clstop, agreeiss) ~ tdepavg + aggtrademin + 
                  recmidwt + recfatwt + recnowt + recyeswt +
                  lag_pch_gdp_min,
                cureform =  ~ tdepavg + aggtrademin +
                  icowsal + I(icowsal^2) + riveriss + mariss +
                  samereg +
                  lcaprat + ldefense + contdir + igosum, 
                data = icow_part_cyr, 
                var = T, nboot = 100, brglm = T); summary(agiss)


agiss <- tvcure(Surv(clstart, clstop, clterm) ~ tdepmin + aggtrademin + 
                  recmidwt + recfatwt + recnowt + recyeswt +
                  lag_pch_gdp_min,
                cureform =  ~ tdepmin  +
                  icowsal + I(icowsal^2) + riveriss + mariss +
                  samereg +
                  lcaprat + ldefense + contdir + igosum, 
                data = icow_part_cyr, 
                var = T, nboot = 100, brglm = T); summary(agiss)

agiss <- tvcure(Surv(clstart, clstop, clterm) ~ tdepmin + aggtrademin + 
                  recmidwt + recfatwt + recnowt + recyeswt + 
                  lag_pch_gdp_min,
                cureform =  ~ tdepmin  +
                  icowsal + I(icowsal^2) + riveriss + mariss +
                  midissyr + 
                  samereg +
                  lcaprat + ldefense + contdir + igosum, 
                data = icow_part_cyr, 
                var = T, nboot = 100, brglm = T); summary(agiss)

agiss_pred <- prediction3(agiss, "tdepmin", c(-46, -31), type = "uncureprob", CI = T)
plot(agiss_pred)
agiss2_pred <- prediction3(agiss2, "tdepmax", c(-44, -29), type = "uncureprob", CI = T)
plot(agiss2_pred, xlab = "Probability of Agreement", ylab = "Maximum Economic Interdependence (ln(Trade/GDP))")

# att_pred_dem <- prediction3(att, "tdepmin", c(-46, -31), type = "uncureprob", CI = T)
# att_suncure_dem <- prediction3(att, "demdy", c(0, 1), type = "suncure", CI = T)
# plot(att_suncure_dem)
# att_spop_dem <- prediction3(att, "demdy", c(0, 1), type = "spop", CI = T)
# plot(att_spop_dem)

# agiss2_dem = prediction3(agiss2, "tdepmax", c(-44, -29), type = "uncureprob", CI = T)
# agiss_pred2 <- prediction3(agiss, "tdepmin", c(-46, -31), type = "suncure", CI = T)
# plot(agiss_pred2)
# agiss_pred3 <- prediction3(agiss, "tdepmin", c(-46, -31), type = "spop", CI = T)
# plot(agiss_pred3)



