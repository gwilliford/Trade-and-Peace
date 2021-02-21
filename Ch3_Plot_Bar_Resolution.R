library(ggplot2)
a = read.csv("C:/Users/gwill/Dropbox/Research/Dissertation/Data Management/sourcedata/ICOW Territory Provisional Data 1.01/ICOWprov101.csv")

ggplot(mapping = aes(resolved), data = a) + geom_bar()

a$res2 = ifelse(a$resolved == 1 | a$resolved == 2 | a$resolved == 8 | a$resolved == 9, "Dropped", NA)
a$res2 = ifelse(a$resolved == 4 | a$resolved == 12 | a$resolved == 13 | a$resolved == 14, "Conflict Management", a$res2)
a$res2 = ifelse(a$resolved == 7,  "Military Occupation", a$res2)
a$res2 = ifelse(a$resolved == 5 | a$resolved == 6 | a$resolved == 10 | a$resolved == 11, "Other", a$res2)
a$res2 = ifelse(a$resolved == -9, "Missing", a$res2)

# 1 = dropped, 2 peaceful, 3 military, 4 other, 5 missing

ggplot(mapping = aes(res2), data = a) + geom_bar() +
  xlab("Resolution Method") +
  ylab("Number of Claims")

