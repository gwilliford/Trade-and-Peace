cd "C:\Users\gwill\Dropbox\Research\Dissertation\Data Analysis\data"
use "icow_part_cyr.dta", clear

* create running sum of agreements by claimdy
bys claimdy: gen totagr = sum(agreeiss)
* create claimdy agreement identifier
tostring claimdy totagr, replace 
gen claimdyagr = claimdy + totagr
order totagr claimdyagr, after(claimdy)
*destring claimdyagr, replace
*gen either = midissyr == 1 | agreeiss == 1
sort claimdyagr year
bys claimdyagr: gen agstart = sum(1)

stset agstart, id(claimdyagr) failure(midissyr) enter(agreeiss == 1)
order claimdyagr totagr year agree agreeiss agstart midissyr _st _d _t _t0, after(claimdy)
*stset year, id(claimdy) failure(midissyr) origin(agreeiss) 
* id = claimdyagr
* origin = subject first becomes at risk
* origin = agreeiss == 1
* failure = midissyr == 1
* exit = 
save "icow_final.dta", replace

*destring claimdy, replace
*xtset claimdy year
*gen lamin2 = lamin / 10
*gen b = lag_depdymin_100 * 100
*replace b = 1 if b>1 & b != .
*zinb sagreeiss lamin icowsal samereg lag_pch_gdp_min recmidwt recnowt recyeswt lcaprat ldefense , inflate(lamin contdir trival) cluster(claimdy)



cd "C:\Users\gwill\Dropbox\Research\Dissertation\DataAnalysis\data"
use "icow_part_cyr.dta", clear
xtset claimdy year
gen div = lag_depdymax_100 / lag_depdymin_100
logit attanyp lag_depdymin_100 icowsal riveriss mariss  clstop recmidwt recfatwt recnowt recyeswt  demdy autdy  lag_pch_gdp_min  lcaprat ldefense contdir igosum,cluster(claimdy)
logit agree dee icowsal riveriss mariss  clstop recmidwt recfatwt recnowt recyeswt  demdy autdy  lag_pch_gdp_min  lcaprat ldefense contdir igosum,cluster(claimdy)



gen lcharm = log(charmander)
gen lpsy = log(psyduck)
gen dif = lcharm - lpsy
gen dif2 = log(charmander/psyduck)
gen dif3 = charmander/psyduck
logit agreeiss mac icowsal riveriss mariss clstop recmidwt recnowt recyeswt  demdy autdy  lag_pch_gdp_min  lcaprat ldefense contdir igosum, vce(cluster claimdy)
xtnbreg sagree dee icowsal riveriss mariss clstop recmidwt recnowt recyeswt demdy autdy lcaprat ldefense contdir igosum


stset clstop, id(claimdy) failure(agree)

stcox dee icowsal riveriss mariss recmidwt recfatwt recnowt recyeswt demdy autdy lcaprat ldefense contdir igosum, cluster(claimdy)
