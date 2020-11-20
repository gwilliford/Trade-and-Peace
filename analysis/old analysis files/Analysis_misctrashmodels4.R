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






####################################
a <- MASS::glm.nb(attemptsp ~ 1 + offset(c), icow_part_cyr)
b <- expand.grid
b <- plot(predict(a))
xyplot(attemptsp ~ c, data = icow_part_cyr)
mcy <- icow_part_cyr %>% group_by(c) %>% summarize(
  mmid = mean(nmidiss, na.rm = T),
  matt = mean(attemptsp, na.rm = T), 
  magg = mean(agree, na.rm = T), 
  miss = mean(agreeiss, na.rm = T),
  scl = sum(one), 
  rat = mmid/matt,
  dif = mean(midissyr - attanyp, na.rm = T)
)
xyplot(matt ~ c, mcy)
lines(lm(matt ~ c, data = mcy))


ggplot(aes(y = scl, x = c), data = mcy) + geom_point() + geom_smooth(method = "loess") + 
  xlab("Length of Dispute (in years)") + ylab("Number of Claims")

ggplot(aes(y = matt, x = c), data = mcy) + geom_point(shape = 1, color = "blue") +
  geom_smooth(method = "loess") + 
  geom_point(aes(y = mmid, x = c), color = "red", shape = 1) + 
  geom_smooth(aes(y = mmid, x = c), method = "loess", color = "red") + 
  xlab("Length of Dispute (in years)") + ylab("Number of Peaceful Settlement Attempts")
cor(cbind(mcy$mmid, mcy$matt))

ggplot(aes(y = magg, x = c), data = mcy) + geom_point() + geom_smooth(method = "loess") + 
  xlab("Length of Dispute (in years)") + ylab("Number of Agreements")
ggplot(aes(y = rat, x = c), data = mcy) + geom_point() + geom_smooth(method = "loess") + 
  xlab("Length of Dispute (in years)") + ylab("Number of Agreements") + ylim(0, 10)
ggplot(aes(y = dif, x = c), data = mcy) + geom_point() + geom_smooth(method = "loess") + 
  xlab("Length of Dispute (in years)") + ylab("Number of Agreements")

ggplot(aes(y = miss, x = c), data = mcy) + geom_point() + geom_smooth(method = "loess") + 
  xlab("Length of Dispute (in years)") + ylab("Number of Substantive Agreements")
