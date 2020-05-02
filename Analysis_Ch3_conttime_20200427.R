setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis")
library(tvcure)
options(scipen = 999)
cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)

a <- read_dta("./data/ICOWdyadyr.dta")
a <- filter(a, year < 1871, )

##### Attempt models -------------------------------------------------------------
at1 <- tvcure(Surv(clstart, clstop, at_term) ~ lagdee +
               icowsal + riveriss + mariss +
               recmidwt + recfatwt + recnowt + recyeswt,
             #lag_pch_gdp_min,
             cureform =  ~ lagdee +
               icowsal + riveriss + mariss +
               demdy + autdy +
               lcaprat + ldefense + contdir + igosum, 
             data = icow_part_cyr, 
             var = T, nboot = 1000, brglm = T); summary(at)
at2 <- tvcure(Surv(clstart, clstop, at_term) ~ lagmac +
                icowsal + riveriss + mariss +
                recmidwt + recfatwt + recnowt + recyeswt,
              #lag_pch_gdp_min,
              cureform =  ~ lagmac +
                icowsal + riveriss + mariss +
                demdy + autdy +
                lcaprat + ldefense + contdir + igosum, 
              data = icow_part_cyr, 
              var = T, nboot = 100, brglm = T); summary(at2)
at3 <- tvcure(Surv(clstart, clstop, at_term) ~ lagfrank +
                icowsal + riveriss + mariss +
                recmidwt + recfatwt + recnowt + recyeswt,
              #lag_pch_gdp_min,
              cureform =  ~ lagfrank +
                icowsal + riveriss + mariss +
                demdy + autdy +
                lcaprat + ldefense + contdir + igosum, 
              data = icow_part_cyr, 
              var = T, nboot = 100, brglm = T); summary(at3)
at4 <- tvcure(Surv(clstart, clstop, at_term) ~ lagfattymagoo +
                icowsal + riveriss + mariss +
                recmidwt + recfatwt + recnowt + recyeswt,
              #lag_pch_gdp_min,
              cureform =  ~ lagfattymagoo +
                icowsal + riveriss + mariss +
                demdy + autdy +
                lcaprat + ldefense + contdir + igosum, 
              data = icow_part_cyr, 
              var = T, nboot = 100, brglm = T); summary(at4)

##### agreement models-------------------------------------------
ag1 <- tvcure(Surv(clstart, clstop, agterm) ~ lagdee +
               icowsal + riveriss + mariss +
               recmidwt + recfatwt + recnowt + recyeswt,
             #lag_pch_gdp_min,
             cureform =  ~ lagdee +
               icowsal + riveriss + mariss +
               demdy + autdy +
               lcaprat + ldefense + contdir + igosum, 
             data = icow_part_cyr, 
             var = T, nboot = 100, brglm = T); summary(ag)
ag2 <- tvcure(Surv(clstart, clstop, agterm) ~ lagmac +
                icowsal + riveriss + mariss +
                recmidwt + recfatwt + recnowt + recyeswt,
              #lag_pch_gdp_min,
              cureform =  ~ lagmac +
                icowsal + riveriss + mariss +
                demdy + autdy +
                lcaprat + ldefense + contdir + igosum, 
              data = icow_part_cyr, 
              var = T, nboot = 100, brglm = T); summary(ag2)
ag3 <- tvcure(Surv(clstart, clstop, agterm) ~ lagfrank +
                icowsal + riveriss + mariss +
                recmidwt + recfatwt + recnowt + recyeswt,
              #lag_pch_gdp_min,
              cureform =  ~ lagfrank +
                icowsal + riveriss + mariss +
                demdy + autdy +
                lcaprat + ldefense + contdir + igosum, 
              data = icow_part_cyr, 
              var = T, nboot = 100, brglm = T); summary(ag3)
ag4 <- tvcure(Surv(clstart, clstop, agterm) ~ lagfattymagoo +
                icowsal + riveriss + mariss +
                recmidwt + recfatwt + recnowt + recyeswt,
              #lag_pch_gdp_min,
              cureform =  ~ lagfattymagoo +
                icowsal + riveriss + mariss +
                demdy + autdy +
                lcaprat + ldefense + contdir + igosum, 
              data = icow_part_cyr, 
              var = T, nboot = 100, brglm = T); summary(ag4)

##### substantive agreement models -----------------------------------------------------
agiss1 <- tvcure(Surv(clstart, clstop, agissterm) ~ lagdee + 
                  recmidwt + recfatwt + recnowt + recyeswt,
                #lag_pch_gdp_min,
                cureform =  ~ lagdee +
                  icowsal + riveriss + mariss +
                  demdy + autdy +
                  lcaprat + ldefense + contdir + igosum, 
                data = icow_part_cyr, 
                var = T, nboot = 100, brglm = T); summary(agiss)
agiss2 <- tvcure(Surv(clstart, clstop, agissterm) ~ lagmac + 
                   recmidwt + recfatwt + recnowt + recyeswt,
                 #lag_pch_gdp_min,
                 cureform =  ~ lagmac +
                   icowsal + riveriss + mariss +
                   demdy + autdy +
                   lcaprat + ldefense + contdir + igosum, 
                 data = icow_part_cyr, 
                 var = T, nboot = 100, brglm = T); summary(agiss2)
agiss3 <- tvcure(Surv(clstart, clstop, agissterm) ~ lagfrank + 
                   recmidwt + recfatwt + recnowt + recyeswt,
                 #lag_pch_gdp_min,
                 cureform =  ~ lagfrank +
                   icowsal + riveriss + mariss +
                   demdy + autdy +
                   lcaprat + ldefense + contdir + igosum, 
                 data = icow_part_cyr, 
                 var = T, nboot = 100, brglm = T); summary(agiss3)
agiss4 <- tvcure(Surv(clstart, clstop, agissterm) ~ lagfattymagoo + 
                   recmidwt + recfatwt + recnowt + recyeswt,
                 #lag_pch_gdp_min,
                 cureform =  ~ lagfattymagoo +
                   icowsal + riveriss + mariss +
                   demdy + autdy +
                   lcaprat + ldefense + contdir + igosum, 
                 data = icow_part_cyr, 
                 var = T, nboot = 100, brglm = T); summary(agiss4)

##### Claim termination models ----------------------------------------------------------------
clterm1 <- tvcure(Surv(clstart, clstop, clterm) ~ lagdee + 
                   recmidwt + recfatwt + recnowt + recyeswt,
                 #lag_pch_gdp_min,
                 cureform =  ~ lagdee +
                   icowsal + riveriss + mariss +
                   demdy + autdy +
                   lcaprat + ldefense + contdir + igosum, 
                 data = icow_part_cyr, 
                 var = T, nboot = 100, brglm = T); summary(clterm)

clterm2 <- tvcure(Surv(clstart, clstop, clterm) ~ lagmac + 
                    recmidwt + recfatwt + recnowt + recyeswt,
                  #lag_pch_gdp_min,
                  cureform =  ~ lagmac +
                    icowsal + riveriss + mariss +
                    demdy + autdy +
                    lcaprat + ldefense + contdir + igosum, 
                  data = icow_part_cyr, 
                  var = T, nboot = 100, brglm = T); summary(clterm2)
clterm3 <- tvcure(Surv(clstart, clstop, clterm) ~ lagfrank + 
                    recmidwt + recfatwt + recnowt + recyeswt,
                  #lag_pch_gdp_min,
                  cureform =  ~ lagfrank +
                    icowsal + riveriss + mariss +
                    demdy + autdy +
                    lcaprat + ldefense + contdir + igosum, 
                  data = icow_part_cyr, 
                  var = T, nboot = 100, brglm = T); summary(clterm3)
clterm4 <- tvcure(Surv(clstart, clstop, clterm) ~ lagfattymagoo + 
                    recmidwt + recfatwt + recnowt + recyeswt,
                  #lag_pch_gdp_min,
                  cureform =  ~ lagfattymagoo +
                    icowsal + riveriss + mariss +
                    demdy + autdy +
                    lcaprat + ldefense + contdir + igosum, 
                  data = icow_part_cyr, 
                  var = T, nboot = 100, brglm = T); summary(clterm4)

##### MID models ------------------------------------------
midterm1 <- tvcure(Surv(clstart, clstop, midissyr) ~ lagdee + 
                    recmidwt + recfatwt + recnowt + recyeswt,
                  #lag_pch_gdp_min,
                  cureform =  ~ lagdee +
                    icowsal + riveriss + mariss +
                    demdy + autdy +
                    lcaprat + ldefense + contdir + igosum, 
                  data = icow_part_cyr, 
                  var = T, nboot = 100, brglm = T); summary(midterm)

midterm2 <- tvcure(Surv(clstart, clstop, midissyr) ~ lagmac + 
                     recmidwt + recfatwt + recnowt + recyeswt,
                   #lag_pch_gdp_min,
                   cureform =  ~ lagmac +
                     icowsal + riveriss + mariss +
                     demdy + autdy +
                     lcaprat + ldefense + contdir + igosum, 
                   data = icow_part_cyr, 
                   var = T, nboot = 100, brglm = T); summary(midterm2)
midterm3 <- tvcure(Surv(clstart, clstop, midissyr) ~ lagfrank + 
                     recmidwt + recfatwt + recnowt + recyeswt,
                   #lag_pch_gdp_min,
                   cureform =  ~ lagfrank +
                     icowsal + riveriss + mariss +
                     demdy + autdy +
                     lcaprat + ldefense + contdir + igosum, 
                   data = icow_part_cyr, 
                   var = T, nboot = 100, brglm = T); summary(midterm3)
midterm4 <- tvcure(Surv(clstart, clstop, midissyr) ~ lagfattymagoo + 
                     recmidwt + recfatwt + recnowt + recyeswt,
                   #lag_pch_gdp_min,
                   cureform =  ~ lagfattymagoo +
                     icowsal + riveriss + mariss +
                     demdy + autdy +
                     lcaprat + ldefense + contdir + igosum, 
                   data = icow_part_cyr, 
                   var = T, nboot = 100, brglm = T); summary(midterm4)
