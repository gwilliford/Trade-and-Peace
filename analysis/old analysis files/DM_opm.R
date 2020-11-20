library(OrthoPanels)

dat <- readRDS("opminputs.RDS")

opm_mod <- opm(ln_trade ~ anyclaim + lag_ln_gdp_min + lag_ln_gdp_max + lag_ln_gdpcap_min + lag_ln_gdpcap_max +
           contdir + ldefense + lcaprat + polmin + polmax,
         data = dat,
         index = c("dyad", "year"), n.samp = 100)

saveRDS(opm_mod, "opm.RDS")