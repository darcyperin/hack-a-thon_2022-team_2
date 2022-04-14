# Task 1: General Numbers of the amount of Stations who Improved/Got worse over the
# 20 year period 

# Task 2: Pulling out stations which have two or more causes (and if they
# change over time)

library(tidyverse)
library(readxl)

data18 <- read_xls("data/2018303d_final.xls")
data08 <- read_xls("data/SC_303d_lists_2006to2016/2008303dfinal020608prtyrk.xls")
data98 <- read_xls("data/SC_303d_lists_1998to2004/1998303dfin_al_rec_only.xls")

summary(data18)
summary(data08)
summary(data98)

# Stations who FAILED to improve from 1998 to 2008
stationsFail98to08 <- inner_join(data98, data08, by = "STATION")

# Stations who FAILED to improve from 1998 to 2018
stationsFail20yr <- inner_join(stationsFail98to08, data18, by = "STATION")

# Stations who got better and were REMOVED from 1998 to 2018
stationsPass20yr <- data18[!data18$STATION %in% stationsFail20yr$STATION,]

# Stations who were ADDED between 1998 and 2008 
stationsWorse98to08 <- data08[!data08$STATION %in% data98$STATION,]

# Stations who were ADDED between 1998 and 2018
stationsWorse20yr <- data18[!data18$STATION %in% stationsWorse98to08$STATION,]

# NOTE: added stations may just be a function of expanded funding to actual test
# those bodies of water


# Locations that have one or more issue associated with them over the 20 year
# period

