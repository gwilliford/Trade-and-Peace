library(mlogit)

m <- na.omit(select(icow_set, effect4, ugt, laglntrade, laglngdp1, laglngdp2, laglngdpcap1, laglngdpcap2, lcaprat, W1, W2, polity21, polity22))
ml1 <- mlogit(effect4 ~ ugt + laglntrade + laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + lcaprat + W1 + W2 + polity21 * polity22, data = icow_set, na.rm = T)

library(nnet)
ml1 <- multinom(effect3 ~ ugt + laglntrade + laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + lcaprat + W1 + W2 + polity21 * polity22, data = icow_set, na.rm = T)
m <- brglm::brglm(agreeiss ~ lugt + laglntrade + laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + lcaprat + W1 + W2 + polity21 * polity22, data = icow_set)
m <- brglm::brglm(agree ~ lugt + laglntrade + laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + lcaprat + W1 + W2 + polity21 * polity22, data = icow_set)

dim(icow_set)
summary(icow_set)
# 
# icow_set[is.na(icow_set$agree), "agree"] <-0
# icow_set[is.na(icow_set$agreeiss), "agreeiss"] <-0


m <- glm(agree ~ ugtdep1 + ugtdep2 + laglntrade + laglngdp1 + laglngdp2 + lcaprat + W1 + W2 + polity21 * polity22, data = icow_set, family = binomial(link = "logit")); summary(m)

### Why are there so many NAs in those two variables
m <- glm(agree ~ ugt + laglntrade + laglngdp1 + laglngdp2 + lcaprat + W1 + W2 + polity21 * polity22, data = icow_set, family = binomial(link = "logit")); summary(m)
m <- glm(agree ~ ugt + laglntrade + ltdeptot1 + ltdeptot2 + laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + lcaprat + W1 + W2 + polity21 * polity22, data = icow_set, family = binomial(link = "logit")); summary(m)
m <- glm(agree ~ ugtdep1 + ugtdep2 + laglntrade + ltdeptot1 + ltdeptot2 + laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + lcaprat + W1 + W2 + polity21 * polity22, data = icow_set, family = binomial(link = "logit")); summary(m)


m <- glm(agreeiss ~ ugt + laglntrade + laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + lcaprat + W1 + W2 + polity21 * polity22, data = icow_set, family = binomial(link = "logit")); summary(m)
m <- glm(agreeiss ~ ugt + laglntrade + ltdeptot1 + ltdeptot2 + laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + lcaprat + W1 + W2 + polity21 * polity22, data = icow_set, family = binomial(link = "logit")); summary(m)
m <- glm(agreeiss ~ ugtdep1 + ugtdep2 + ltdeptot1 + ltdeptot2 + laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + lcaprat + W1 + W2 + polity21 * polity22, data = icow_set, family = binomial(link = "logit")); summary(m)

m <- brm(agreeiss ~ ugtdep1 + ugtdep2 + ltdeptot1 + ltdeptot2 + laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + lcaprat + W1 + W2 + polity21 * polity22 + (1 | dyad), data = icow_set, family = "bernoulli", cores = 4); summary(m)
m <- brm(agreeiss ~ ugtdep1 + ugtdep2 + ltdeptot1 + ltdeptot2 + laglngdp1 + laglngdp2 + laglngdpcap1 + laglngdpcap2 + lcaprat + W1 + W2 + polity21 * polity22 + (1 | mm(ccode1, ccode2) + dyad), data = icow_set, family = "bernoulli", cores = 4); summary(m)

