library(dplyr)
ibad = read.csv("C:/Users/gwill/Dropbox/Research/Dissertation/Data Management/sourcedata/ibaddyr.csv")
ibad = ibad %>%
  group_by(dyad) %>%
  mutate(maxyear = max(year))

summary(ibad$settle[ibad$year == ibad$maxyear])
