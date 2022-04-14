# Task 1: General Numbers of the amount of Stations who Improved/Got worse over the
# 20 year period 

# Task 2: Pulling out stations which have two or more causes (and if they
# change over time)

library(tidyverse)
library(readxl)
library(viridisLite)
library(viridis)

data18 <- read_xls("data/2018303d_final.xls")
data08 <- read_xls("data/SC_303d_lists_2006to2016/2008303dfinal020608prtyrk.xls")
data98 <- read_xls("data/SC_303d_lists_1998to2004/1998303dfin_al_rec_only.xls")

summary(data18)
summary(data08)
summary(data98)

# Stations who FAILED to improve from 1998 to 2008
stationsFail98to08 <- inner_join(data98, data08, by = "STATION")
Failed98to08MoreThanOne <- stationsFail98to08 %>% 
  group_by(LOCATION) %>% 
  summarize(count=n())

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
Failed98to18MoreThanOne <- stationsFail20yr %>% 
  group_by(LOCATION) %>% 
  summarize(NumCauses=n()) %>%
  group_by(NumCauses) %>%
  summarize(count2 = n())
Failed98to18MoreThanOne$NumCauses <- sub("^", "N", Failed98to18MoreThanOne$NumCauses)
Failed98to18MoreThanOne$Percent <- c(Failed98to18MoreThanOne$count2[1]/sum(Failed98to18MoreThanOne$count2)*100,
                                     Failed98to18MoreThanOne$count2[2]/sum(Failed98to18MoreThanOne$count2)*100,
                                     Failed98to18MoreThanOne$count2[3]/sum(Failed98to18MoreThanOne$count2)*100,
                                     Failed98to18MoreThanOne$count2[4]/sum(Failed98to18MoreThanOne$count2)*100,
                                     Failed98to18MoreThanOne$count2[5]/sum(Failed98to18MoreThanOne$count2)*100,
                                     Failed98to18MoreThanOne$count2[6]/sum(Failed98to18MoreThanOne$count2)*100)

ggplot(Failed98to18MoreThanOne, aes(x= "", y=Percent, fill=NumCauses)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  scale_fill_viridis(option = "C", discrete = TRUE) +
  theme_void() +
  labs(title = "Number of Causes for Stations Listed from 1998 to 2018")

Pass98to18MoreThanOne <- stationsPass20yr %>% 
  group_by(DESCRIPTION) %>% 
  summarize(NumCauses=n()) %>%
  na.omit() %>%
  group_by(NumCauses) %>%
  summarize(count2 = n()) 
Pass98to18MoreThanOne$NumCauses <- sub("^", "N", Pass98to18MoreThanOne$NumCauses)
Pass98to18MoreThanOne$Percent <- c(Pass98to18MoreThanOne$count2[1]/sum(Pass98to18MoreThanOne$count2)*100,
                                     Pass98to18MoreThanOne$count2[2]/sum(Pass98to18MoreThanOne$count2)*100,
                                     Pass98to18MoreThanOne$count2[3]/sum(Pass98to18MoreThanOne$count2)*100)
                                     

ggplot(Pass98to18MoreThanOne, aes(x= "", y=Percent, fill=NumCauses)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  scale_fill_viridis(option = "C", discrete = TRUE) +
  theme_void() +
  labs(title = "Number of Causes for Stations Removed 1998 to 2018")
# They are doing a shit job of removing stations with more than one issue

Add98to18MoreThanOne <- stationsWorse20yr %>% 
  group_by(DESCRIPTION) %>% 
  summarize(NumCauses=n()) %>%
  na.omit() %>%
  group_by(NumCauses) %>%
  summarize(count2 = n()) 
Add98to18MoreThanOne$NumCauses <- sub("^", "N", Add98to18MoreThanOne$NumCauses)
Add98to18MoreThanOne$Percent <- c(Add98to18MoreThanOne$count2[1]/sum(Add98to18MoreThanOne$count2)*100,
                                   Add98to18MoreThanOne$count2[2]/sum(Add98to18MoreThanOne$count2)*100)


ggplot(Add98to18MoreThanOne, aes(x= "", y=Percent, fill=NumCauses)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  scale_fill_viridis(option = "C", discrete = TRUE) +
  theme_void() +
  labs(title = "Number of Causes for Stations Added 1998 to 2018")


failures <- stationsFail20yr %>% 
  group_by(LOCATION) %>% 
  summarize(NumCauses=n()) %>%
  filter(NumCauses == 8)
# could look up these stations to see their proximity to other things
# Magnolia Gardens is a plantation
# Kingston Lake is next to a recycling plant
# McApline Creek (ironically) next to a construction facility which specializes in retention walls
# Reedy River after a wetland (nature's kidneys)



