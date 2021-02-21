setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis")
library(tvcure)
library(brms)
library(survminer)
library(xtable)
options(scipen = 999)

# Data
if (!exists("icow_part_cyr")) icow <- readRDS("./data/ICOWFinal.RDS")

#### Descriptives
# at2 <-  glm(update.formula(fullform, attanyp ~ I(lagdee2^2) +.),
#            data = icow)


#### Models
fullform <- as.formula( ~ lagdee2 + icowsal + riveriss + mariss +
                          recmidwt + recnowt + recyeswt + bdymid +
                          lag_pch_gdpmax + lgovcrisesdy + lcw +
                          demdy + autdy +
                          lcaprat + ldefense + contdir + igosum +
                          c + c2 + c3 + 
                          (1 | claimdy) + (1 | year))

set.seed(39458230)
at0 <- brm(update.formula(fullform, attanyp ~ . - (1 | claimdy) - (1 | year)),
           family = "bernoulli",
           prior = set_prior(prior = "student_t(1, 0, 2.5)", class = "b"),
           cores = 4,
           data = icow)
at <-  brm(update.formula(fullform, attanyp ~ .),
          family = "bernoulli",
          prior = set_prior(prior = "student_t(1, 0, 2.5)", class = "b"),
          cores = 4,
          data = icow)
conc <-  brm(update.formula(fullform, conc ~ .),
           family = "bernoulli",
           prior = set_prior(prior = "student_t(1, 0, 2.5)", class = "b"),
           cores = 4,
           data = icow)
ag <-  brm(update.formula(fullform, agterm ~ .),
          family = "bernoulli",
          prior = set_prior(prior = "student_t(1, 0, 2.5)", class = "b"),
          cores = 4,
          data = icow)
ag2 <- brm(update.formula(fullform, agissterm ~ .),
           family = "bernoulli",
           prior = set_prior(prior = "student_t(1, 0, 2.5)", class = "b"),
           cores = 4,
           data = icow)
save.image(file = "./models/Ch3Res20200510.Rdata")

#### Predictions
p <- model.frame(fullform, icow)
p <- apply(p, 2, median, na.rm = T)
p <- rbind(p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p)
# p[, "lagdee2"] <- c(-12, -1)
p[, "c"] <- seq(1, 186, 10)
p[, "c2"] <- seq(1, 186, 10)^2 / 1000
p[, "c3"] <-seq(1, 186, 10)^3 / 1000
p <- as.data.frame(p)
# a <- posterior_predict(at, p, re_formula = NA)
a <- predict(at, p, re_formula = NA)
plot(y = a[, 1], x = p[, "c"])

at_plot <- plot(marginal_effects(at, "lagdee2"), rug = T, plot = F)[[1]] +
  labs(x = "Minimum Economic Dependence",
       y = "Predicted Probability") + theme(panel.grid = element_line("grey70")); at_plot
ggsave("./figures/attempts.png", width = 3, height = 3, units = "in")
conc_plot <- plot(marginal_effects(conc, "lagdee2"), rug = T, plot = F)[[1]] +
  labs(x = "Minimum Economic Dependence",
       y = "Predicted Probability") + theme(panel.grid = element_line("grey70"))
ggsave("./figures/concessions.png", width = 3, height = 3, units = "in")

ag_plot <- plot(marginal_effects(ag, "lagdee2"), rug = T, plot = F)[[1]] +
  labs(x = "Minimum Economic Dependence",
       y = "Predicted Probability") + theme(panel.grid = element_line("grey70"))
ggsave("./figures/agreements.png", width = 3, height = 3, units = "in")

icow$col = ifelse(icow$lagdee2 > -12, 1, 0)
a <- ggplot(mapping = aes(x = lagdee2, y = attanyp), icow[icow$col == 1, ]) +
  geom_jitter(alpha = 0.2, colour = "blue") + 
  geom_rug() + 
  labs(x = "Minimum Economic Dependence",
       y = "Settlement Attempts")  
b <- ggplot(mapping = aes(x = lagdee2, y = attanyp), icow) + geom_jitter() + 
  labs(x = "Minimum Economic Dependence",
       y = "Agreements") + 
  geom_hline(yintercept = 0.5)# + 
  #scale_y_discrete(breaks = c(0, 1))

# ag2_plot <- plot(marginal_effects(at, "lagdee2"), rug = T, plot = F)[[1]] +
#   labs(x = "Minimum Economic Dependence",
#        y = "Probability of Substantive Agreement") + theme(panel.grid = element_line("grey70"))

#### Tables
varlist1 = list("Minimum Economic Dependence" = "lagdee2",
                "Claim Salience" = "icowsal",
                "River Claim" = "riveriss",
                "Maritime Claim" = "mariss",
                "Min. Percent Change in GDP" = "lag_pch_gdpmax",
                "Government Crisis in Either State" = "lgovcrisesdy",
                "Civil War in Either State" = "lcw",
                "Dyadic MID" = "bdymid",
                "Recent MIDs" = "recmidwt",
                "Recent Failed CM Attempts" = "recnowt",
                "Recent Successful CM Attempts" = "recyeswt",
                "Capability Ratio" = "lcaprat",
                "Joint Democracy" = "demdy",
                "Joint Autocracy" = "autdy",
                "Defensive Alliance" = "ldefense",
                "Contiguity" = "contdir",
                "Shared IGO Membership" = "igosum", 
                "Year" = "c", 
                "Year$^2$" = "c2",
                "Year$^3$" = "c3", 
                "$\\alpha_0$" = "Intercept", 
                "$\\alpha_{claim}$" = "claimdy",
                "$\\alpha_{year}$" = "year")
brmtab <- function(mod, varlist, digits = 3) {
  `%notin%` <- Negate(`%in%`)
  f <- summary(mod)$fixed
  f <- f[, 1:2]
  for (i in 1:2) f[, i] <- round(f[, i], digits = digits)
  r <- summary(mod)$random
  for (i in 1:length(r)) r[[i]] <- round(r[[i]], digits = digits)
  rm <- list()
  for (i in 1:length(r)) rm[[i]] <- r[[i]][, 1:2]
  re <- matrix(unlist(rm), nrow = 2)
  rownames(re) = names(r)
  t <- rbind(f, re)
  p <- parameters::p_value(mod)[, 2]
  p <- round(c(p, rep(NA, nrow(t) - length(p))), digits)
  p <- p * 100
  t <- cbind(t, p)
  allnames <- rownames(t)
  index <- match(allnames, varlist)
  vo <- unlist(varlist[!is.na(index)])
  nout <- allnames[allnames %notin% vo]
  names(nout) <- nout
  vo <- c(vo, nout)
  t <- t[match(vo, allnames),] 
  rownames(t) <- names(vo)
  colnames(t) <- c("Mean", "SD", "Percent Above/ \\newline Below Zero")
  t
}
t1  <- brmtab(at, varlist1)[, c(1,3)]
t2  <- brmtab(conc, varlist1)[, c(1,3)]
t3  <- brmtab(ag, varlist1)[, c(1,3)]
# tf  <- cbind(t1, t2, t3)
# tf  <- rbind(tf, c("Model 1", "", "Model 2", "", "Model 3", ""))
# print(xtable(tf), booktabs = T, sanitize.text.function = identity, tabular.environment = "tabularx")
print(xtable(t1), booktabs = T, sanitize.text.function = identity)
print(xtable(t2), booktabs = T, sanitize.text.function = identity)
print(xtable(t3), booktabs = T, sanitize.text.function = identity)

# file = "C:/Users/gwill/Dropbox/Research/Dissertation/Article 0 - Theory Chapter/tab/attempts.tex")


