# Claim termination
library(survival)

# Claim termination models
a <- coxph(Surv(y0, year, chdrop) ~ laglntrade, dat = icow_part_cyr);  summary(a)
a <- coxph(Surv(y0, year, tgtdrop) ~ laglntrade, dat = icow_part_cyr); summary(a)
a <- coxph(Surv(y0, year, pset) ~ lugt + laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + ldefense + contdir + lcaprat + polity21 * polity22, dat = icow_part_cyr); summary(a)

a <- brm(chdrop ~ lugt + laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + ldefense + contdir + lcaprat + polity21 * polity22 + year + I(year^2) + I(year^3) + (1|dyad),
         family = "bernoulli",
         dat = icow_part_cyr,
         cores = 4); summary(a)


# Time to agreement models
a <- coxph(Surv(y0, year, chdrop) ~ laglntrade, dat = icow_part_cyr);  summary(a)
a <- coxph(Surv(y0, year, agree) ~ lugt + mlntrade + laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + ldefense + contdir + lcaprat + polity21 * polity22, dat = icow_part_cyr);  summary(a)

a <- coxph(Surv(y0, year, agree) ~ laglntrade + mlntrade + laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + ldefense + contdir + lcaprat + polity21 * polity22, dat = icow_part_cyr);  summary(a)
a <- coxph(Surv(y0, year, agreeiss) ~ laglntrade + mlntrade + laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + ldefense + contdir + lcaprat + polity21 * polity22, dat = icow_part_cyr);  summary(a)
a <- coxph(Surv(y0, year, ag_end_any) ~ laglntrade + mlntrade + laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + ldefense + contdir + lcaprat + polity21 * polity22, dat = icow_part_cyr);  summary(a)
a <- coxph(Surv(y0, year, ag_end_part) ~ laglntrade + mlntrade + laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + ldefense + contdir + lcaprat + polity21 * polity22, dat = icow_part_cyr);  summary(a)
a <- coxph(Surv(y0, year, ag_end_full) ~ laglntrade + mlntrade + laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + ldefense + contdir + lcaprat + polity21 * polity22 + frailty(dyad), dat = icow_part_cyr);  summary(a)
