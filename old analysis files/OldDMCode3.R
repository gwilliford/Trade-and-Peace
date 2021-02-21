
	
dat$depdy1 = dat$trade/dat$gdp1 #depdy1 is 1's dependence on 2 (exports/gdpcap)
dat$depdy2 = dat$trade/dat$gdp2  # higher values indicate that 2 is more dependent on trade
dat$depdymax = rowMaxs(cbind(dat$depdy1, dat$depdy2))
dat$depdymin = rowMins(cbind(dat$depdy1, dat$depdy2))
dat$depdy100_1 = dat$trade_100/dat$gdp1 #depdy1 is 1's dependence on 2 (exports/gdpcap)
dat$depdy100_2 = dat$trade_100/dat$gdp2  # higher values indicate that 2 is more dependent on trade

		
dat$a1 <- dat$ln_trade / dat$ln_gdp1
dat$a2 <- dat$ln_trade / dat$ln_gdp2
dat$amin <- rowMins(cbind(dat$a1, dat$a2))
dat$amax <- rowMaxs(cbind(dat$a1, dat$a2))
		
dat$b1 = dat$ln_agg1 / dat$ln_gdp1
dat$b2 = dat$ln_agg2 / dat$ln_gdp2
dat$bmin = rowMins(cbind(dat$b1, dat$b2))
dat$bmax = rowMaxs(cbind(dat$b1, dat$b2))
		
		
c1 - trade/ (gdp / 1000000) ~ c1b - c1 * 100
dmin - lag_depdymin_100 * 100 == f - lag_depdymin * 1000000
e1 (and j) - lbmin * 100
g - lamin / 100 ~ h - lamin * 100



depdy is too small to be of use
depdy_100 works



