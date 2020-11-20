library(dplyr)
library(lattice)

### Global trade trends
tyr <- dat %>% group_by(year) %>% summarize(
  mt = mean(trade, na.rm = T),
  st = sum(trade, na.rm = T)
)
xyplot(mt ~ year, data = tyr, type = 'l', xlab = "Year", ylab = 'Mean Bilateral Trade (millions of US dollars')
xyplot(st ~ year, data = tyr, type = 'l', xlab = "Year", ylab = 'Total Bilateral Trade (millions of US dollars')

# Bilateral trade trends
xyplot(trade ~ year, data = dat, type = 'l', groups = dyad, xlab = "Year",
       ylab = 'Bilateral Trade (in Millions of US Dollars')

# Trade by dyad
mtd <- dat %>% group_by(dyad) %>% summarize(
  mt = mean(trade, na.rm = T)
)




nodes <- transform(with(icow_part_cyr, country.etc = as.factor(country.etc)))
with(nodes, points(long, lat, col = trade, pch=19, cex=rescale(pop, c(1, 8)))) 


####################################
a <- MASS::glm.nb(attemptsp ~ 1 + offset(c), icow_part_cyr)
b <- expand.grid
b <- plot(predict(a))
xyplot(attemptsp ~ c, data = icow_part_cyr)
xyplot(matt ~ c, mcy)
lines(lm(matt ~ c, data = mcy))



cor(cbind(mcy$mmid, mcy$matt))

ggplot(aes(y = magg, x = c), data = mcy) + geom_point() + geom_smooth(method = "loess") + 
  xlab("Length of Dispute (in years)") + ylab("Number of Agreements")
ggplot(aes(y = rat, x = c), data = mcy) + geom_point() + geom_smooth(method = "loess") + 
  xlab("Length of Dispute (in years)") + ylab("Number of Agreements") + ylim(0, 10)
ggplot(aes(y = dif, x = c), data = mcy) + geom_point() + geom_smooth(method = "loess") + 
  xlab("Length of Dispute (in years)") + ylab("Number of Agreements")

