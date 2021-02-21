# Caculates total number of attempts, agreements, and concessions by claim
# Uses icow_set_cldy from claim management formatting file
a = icow_set_cldy %>% group_by(claimdy) %>% summarize(
  sbagree = max(bagree), 
  sbagreeiss = max(bagreeiss),
  sbagconc = max(bagconc)
)
summary(a)
