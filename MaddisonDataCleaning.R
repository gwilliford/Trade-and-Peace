setwd('C:\Users/gwill/Dropbox/Research/Dissertation/TradeModels')
library(readxl)
library(countrycode)

mad <- read_excel("mpd2018.xlsx", sheet = "Full data", n_max = 19357)
mad$ccode <- as.numeric(with(mad, countrycode(countrycode, 'iso3c', 'cown')))
mad$gdp <- mad$rgdpnapc/mad$pop

# This is necessary for trade predictions
# Gonna skip this for now

# Some values were not matched unambiguously: CSK, HKG, PRI, PSE, SRB, SUN, YUG
# CSK = transitional code for czechoslovakia from June 1993
# HKG = Hong Kong
# PRI = Puerto Rico
# PSE = palestine
# SRB = serbia
# SUN = USSR - not assigned a ccode during 
# YUG = Former Yugoslavia 

##### Some of these countries have no gdp entries for 10 year periods

# The former Yugoslavia
  # Maddison contains separate entries for members of Former Yugoslavia, including serbia, montenegro, macedonia, slovenia, croatia, and bosnia: These are aggregated to a separate entry for the Former Yugoslavia
  # There is no data for Kosovo (ccode 347) as part of FRY or independently
  # Before 1992 ccode 345 is assigned values of Former Yugoslavia
  # During 1992 ccode 345 is assigned sum of Serbia, Montenegro, and Macedonia
  # After 1992 ccode 345 is assigned sum of Serbia, Montenegro
  # After 2006, ccode 345 is assigned Serbia only
  mad[mad$ccode == 345, ]
a <- with(mad, cbind(ccode, cgdppc, year))
mad$cgdppc[mad$year == 1992 & mad$ccode == 345, "cgdppc"] <- mad$cgdppc[mad$year == 1992 & mad$country == "Serbia"]
temp <- with(mad)

View(mad[mad$ccode == 345, ])

sum(duplicated(mad, mad$ccode, mad$year, incomparables = F))

frydat <- data.frame(year = c(1992:2016))
frydat$cgdppc <- mad$cgdppc[mad$year == 1992 & mad$country == "Serbia"] + mad$cgdppc[mad$year == 1992 & mad$country == "Montenegro"] + mad$cgdppc[mad$year == 1992 & mad$country == "Macedonia"]
frydat


setDT(df)[, Condition := Condition[Trial != "Test"], by = .(Item, ID)]

# The former Czech republic
    # Maddison contains entries for Czechoslovakia, Slovakia, The Czech Republic
    # Czechoslovakia is ccode 315 and ends in Dec 1992 - maddison values availabel from 1820-1937, 1948-2016
    # Czech Republic/Czechia is ccode 316 and begins in January 1993 (CZE iso code) - maddison data begins in 1970
    # Slovakia's ccode is 317 and begins in January 1993 (SVK iso code) - maddison data begins in 1985
    # Assignment to ccodes:
    # ccode 315 gets Czechoslovakia data through 1992
    # ccode 316 gets Czech Republic data after 1992
    # ccode 317 gets slovakia after 1992
    # I don't think this requires any actual recoding

# Russia/USSR 
  # Maddison contains separate entries for RUS and USSR -- cow only has one ccode (365)
  # RUS = Russian federation - data available from 1960-2016
  # SUN = Former USSR - data available from 1885-2016

# Yemen
  # Maddison only contains one entry for Yemen (from 1950-2016)
  # Yemen Arab Republic (North) - ccode 778 - 1926-1990
  # Yemen (combined) - ccode 679 - 1990 on
  # Yemen People's Republic (South) - 680 - ccode - 1967-1990

# Eritrea and Ethiopia
  # Maddison only contains an entry for Ethiopia
  # Ethiopia -- ccode 530 -- 1898-1936, 1941-2011
  # Eritrea -- ccode 531 -- 1993-2011

# Austria Hungary
  # Maddison has separate entries for Austria and Hungary
  # Austria-Hungary - ccode 300 -  1816-1918
  # Austria - ccode 305 - 1919-1938, 1955-2011
  # Hungary - ccode 310 - Nov 1918-2011
  # Assignment to ccodes
  # ccode 305 gets austria data
  # ccode 310 gets hungary datad
  # ccode 300 gets sum of austria and hungary for years less than 1919