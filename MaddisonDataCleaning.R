setwd('C:/Users/gwill/Dropbox/Research/Dissertation/TradeModels')
library(readxl)
library(readr)
library(dplyr)
library(countrycode)

madd <- read_excel("./data/mpd2018.xlsx", sheet = "Full data", n_max = 19357)
madd$ccode <- as.numeric(with(madd, countrycode(countrycode, 'iso3c', 'cown')))
madd$gdp <- madd$rgdpnapc *madd$pop * 1000 # population is measured in 1000s

mp <- read_excel("./data/parmac_madisonGDP_updated.xlsx")

# This is necessary for trade predictions
# Gonna skip this for now
madd[madd$country == "Serbia", "ccode"] <- 345
# Some values were not matched unambiguously: CSK, HKG, PRI, PSE, SRB, SUN, YUG
  # CSK = transitional code for czechoslovakia from June 1993
  # HKG = Hong Kong
  # PRI = Puerto Rico
  # PSE = palestine
  # SRB = serbia
  # SUN = USSR - not assigned a ccode during 
  # YUG = Former Yugoslavia 

  # HKG, PRI, and PSE, YUG are ignored - they won't match to ccodes
  # CSK is recoded below
  # SUN/RUS are taken from Parent Mcdonald Data

### 
# Code FRY
# Figure out difference between CSK and others
# Merge with parent mcdonald data


##### Some of these countries have no gdp entries for 10 year periods
# Give up for now - just assign Serbia to FRY
# The former Yugoslavia
  # Former Yugoslavia only exists for 1500
  # Maddison contains separate entries for members of Former Yugoslavia, including serbia, montenegro, macedonia,
  # slovenia, croatia, and bosnia: These are aggregated to a separate entry for the Former Yugoslavia
  # There is no data for Kosovo (ccode 347) as part of FRY or independently
  # Before 1992 ccode 345 is assigned values of Former Yugoslavia
  # During 1992 ccode 345 is assigned sum of Serbia, Montenegro, and Macedonia
  # After 1992 ccode 345 is assigned sum of Serbia, Montenegro
  # After 2006, ccode 345 is assigned Serbia only
# madd[is.na(madd$ccode), "ccode"] <-  
# madd[madd$country == "Former Yugoslavia", "ccode"] <- 0
# madd[madd$country == "Former Yugoslavia" & madd$year < 1992, "ccode"] <- 345
# madd[madd$country == "Former Yugoslavia" & madd$year == 1992, "cgdppc"] + madd[madd$country == "Serbia" & madd$year == 1992, "cgdppc"]
# madd[madd$country == "Former Yugoslavia" & madd$year = 1992, "ccode"] <- 345
# madd[madd$country == "Former Yugoslavia" & madd$year > 1992 & madd$cyr < 2007, "ccode"] <- 345
# madd[madd$country == "Former Yugoslavia" & madd$year < 2006, "ccode"] <- 345
# 
# 
# 
# fry[ , c("cgdppc", "rgdpnapc", "pop", "gdp")]
# rowS
# fry$fry92 <- fry$cgdppc + ser
# fry$ccode <- 315
# 
#   
# )
# ser <- madd[madd$country == "Serbia", ]
# mont <- madd[madd$country == "Montenegro", ]
# mac <- madd[madd$country == "Macedonia", ]
# 
# fry[fry$year > 2007, c("cgdppc", "rgdpnapc", "pop", "gdp")] <- ser[, c("cgdppc", "rgdpnapc", "pop", "gdp")]
# # FRY (345) exists from 1878-1941, 1944-2011
# fry2 <- data.frame(ccode = NA, year = 1878:2011)
# fry2$cg
# 
# 
# fry$ccode <- NA
# fry$s
# rus$ccode[rus$year < 1960] <- 365
# rus$ccode[rus$year >= 1960] <- 365
# rus <- rus[rus$ccode == 365, ]
# madd <- madd[madd$countrycode != "RUS" & madd$countrycode != "SUN", ]
# madd <- full_join(madd, rus)
# 
# 
# need to drop data from 

#madd <- filter(madd, !is.na(ccode))

# The former Czech republic
    # Maddison contains entries for Czechoslovakia, Slovakia, The Czech Republic
    # Czechoslovakia is ccode 315 and ends in Dec 1992 - maddison values availabel from 1820-1937, 1948-2016
    # Czech Republic/Czechia is ccode 316 and begins in January 1993 (CZE iso code) - maddison data begins in 1970
    # Slovakia's ccode is 317 and begins in January 1993 (SVK iso code) - maddison data begins in 1985
    # Assignment to ccodes:
    # ccode 315 gets Czechoslovakia data through 1992
    # ccode 316 gets Czech Republic data after 1992
    # ccode 317 gets slovakia after 1992
    # *** Recoding complete
    madd[madd$country == "Czechoslovakia", "ccode"] <- 315
    # madd[madd$ccode == 316 & madd$year < 1993, "cgdppc"] <- NA
    # madd[madd$ccode == 316 & madd$year < 1993, "rgdpnapc"] <- NA
    # madd[madd$ccode == 316 & madd$year < 1993, "pop"] <- NA
    # madd[madd$ccode == 317 & madd$year < 1993, "gdp"] <- NA
    # 
    # madd[madd$ccode == 317 & madd$year < 1993, "cgdppc"] <- NA
    # madd[madd$ccode == 317 & madd$year < 1993, "rgdpnapc"] <- NA
    # madd[madd$ccode == 317 & madd$year < 1993, "pop"] <- NA
    # madd[madd$ccode == 317 & madd$year < 1993, "gdp"] <- NA
  
# Russia/USSR 
  # Maddison contains separate entries for RUS and USSR -- cow only has one ccode (365)
  # RUS = Russian federation - data available from 1960-2016
  # SUN = Former USSR - data available from 1885-2016
  # *** COding complete, although I'm not sure this is the correct way to do this
  rus <- madd[madd$countrycode == "RUS" | madd$countrycode == "SUN", c("countrycode", "year", "rgdpnapc", "cgdppc", "pop", "gdp")]
  rus$ccode <- NA
  rus$ccode[rus$year < 1960] <- 365
  rus$ccode[rus$year >= 1960] <- 365
  rus <- rus[rus$ccode == 365, ]
  madd <- madd[madd$countrycode != "RUS" & madd$countrycode != "SUN", ]
  madd <- full_join(madd, rus)
  
# Yemen
  # Maddison only contains one entry for Yemen (from 1950-2016)
  # Yemen Arab Republic (North) - ccode 778 - 1926-1990
  # Yemen (combined) - ccode 679 - 1990 on
  # Yemen People's Republic (South) - 680 - ccode - 1967-1990
  # *** I don't know how to disaggregate this, so I'm just leaving only an entry for Yemen - no recoding necessary
  

# Eritrea and Ethiopia
  # Maddison only contains an entry for Ethiopia
  # Ethiopia -- ccode 530 -- 1898-1936, 1941-2011
  # Eritrea -- ccode 531 -- 1993-2011
  # *** I'm just assigning this data to Ethiopia - no recoding necessary
  

# Austria Hungary
  # Maddison has separate entries for Austria and Hungary
  # Austria-Hungary - ccode 300 -  1816-1918
  # Austria - ccode 305 - 1919-1938, 1955-2011 - already coded
  # Hungary - ccode 310 - Nov 1918-2011 - already coded
  # Assignment to ccodes
  # ccode 305 gets austria data
  # ccode 310 gets hungary datad
  # ccode 300 gets sum of austria and hungary for years less than 1919
  # *** 305 and 310 already coded - assign 300 sum of two
  # no data on auh
madd <- dplyr::select(madd, ccode, year, gdp, rgdpnapc, pop) %>% rename(gdpcap = rgdpnapc)
write_csv(madd, "./data/madd.csv")
