# Claim termination
library(survival)
library(brms)

# Claim termination models
a <- coxph(Surv(y0, year, chdrop) ~ ltotugt + laglntrade, dat = icow_part_cyr);  summary(a)
a <- coxph(Surv(y0, year, tgtdrop) ~ laglntrade, dat = icow_part_cyr); summary(a)
a <- coxph(Surv(y0, year, pset) ~ lugt + lag_ln_gdp1 + lag_ln_gdp2 + lag_ln_gdpcap1 + lag_ln_gdpcap2 +
             ldefense + contdir + lcaprat + lpol1 * lpol2, dat = icow_part_cyr); summary(a)


sub <- na.omit(select(icow_part_cyr_ct, y0, year, chdrop, tgtdrop, pset, ltotugt, lterugt, lmarugt, lrivugt, starts_with("lag_ln_gdp"), ldefense, contdir, lcaprat, lpol_chal, lpol_tgt))
a <- coxphf::coxphf(Surv(y0, year, chdrop) ~ ltotugt + lag_ln_gdp_chal + lag_ln_gdp_tgt + lag_ln_gdpcap1 + lag_ln_gdpcap2 + ldefense + contdir + lcaprat + lpol_chal * lpol_tgt, dat = sub); summary(a)
a <- coxph(Surv(y0, year, tgtdrop) ~ ltotugt + lag_ln_gdp_chal + lag_ln_gdp_tgt + lag_ln_gdpcap1 + lag_ln_gdpcap2 + ldefense + contdir + lcaprat + lpol_chal * lpol_tgt + (1|dyad),
           dat = icow_part_cyr_ct); summary(a)
a <- coxph(Surv(y0, year, pset) ~ lrivugt + lag_ln_gdp_chal + lag_ln_gdp_tgt + lag_ln_gdpcap1 + lag_ln_gdpcap2 + ldefense + contdir + lcaprat + lpol_chal * lpol_tgt + (1|dyad),
           dat = icow_part_cyr_ct); summary(a)

a <- brm(pset ~ lrivugt + lag_ln_gdp_chal + lag_ln_gdp_tgt + lag_ln_gdpcap1 + lag_ln_gdpcap2 + ldefense + contdir + lcaprat + lpol_chal + lpol_tgt + year + y2 + y3 + (1|dyad),
           dat = icow_part_cyr_ct,
          family = "bernoulli",
          cores = 4); summary(a)


# a <- brm(chdrop ~ ltotugt + lag_lngdp1 + lag_ln_gdp2 + lag_ln_gdpcap1 + lag_ln_gdpcap2 + ldefense + contdir + lcaprat + polity21 * polity22 + year + I(year^2) + I(year^3) + (1|dyad),
#          family = "bernoulli",
#          dat = icow_part_cyr,
#         #          family = "bernoulli",



# Time to agreement models
a <- coxph(Surv(y0, year, chdrop) ~ laglntrade, dat = icow_part_cyr);  summary(a)
a <- coxph(Surv(y0, year, agree) ~ lugt + mlntrade + lag_ln_gdp1 + lag_ln_gdp2 + lag_ln_gdpcap1 + lag_ln_gdpcap2 + ldefense + contdir + lcaprat + polity21 * polity22, dat = icow_part_cyr);  summary(a)

a <- coxph(Surv(y0, year, agree) ~ laglntrade + mlntrade + lag_ln_gdp1 + lag_ln_gdp2 + lag_ln_gdpcap1 + lag_ln_gdpcap2 + ldefense + contdir + lcaprat + polity21 * polity22, dat = icow_part_cyr);  summary(a)
a <- coxph(Surv(y0, year, agreeiss) ~ laglntrade + mlntrade + lag_ln_gdp1 + lag_ln_gdp2 + lag_ln_gdpcap1 + lag_ln_gdpcap2 + ldefense + contdir + lcaprat + polity21 * polity22, dat = icow_part_cyr);  summary(a)
a <- coxph(Surv(y0, year, ag_end_any) ~ laglntrade + mlntrade + lag_ln_gdp1 + lag_ln_gdp2 + lag_ln_gdpcap1 + lag_ln_gdpcap2 + ldefense + contdir + lcaprat + polity21 * polity22, dat = icow_part_cyr);  summary(a)
a <- coxph(Surv(y0, year, ag_end_part) ~ laglntrade + mlntrade + lag_ln_gdp1 + lag_ln_gdp2 + lag_ln_gdpcap1 + lag_ln_gdpcap2 + ldefense + contdir + lcaprat + polity21 * polity22, dat = icow_part_cyr);  summary(a)
a <- coxph(Surv(y0, year, ag_end_full) ~ laglntrade + mlntrade + lag_ln_gdp1 + lag_ln_gdp2 + lag_ln_gdpcap1 + lag_ln_gdpcap2 + ldefense + contdir + lcaprat + polity21 * polity22 + frailty(dyad), dat = icow_part_cyr);  summary(a)
