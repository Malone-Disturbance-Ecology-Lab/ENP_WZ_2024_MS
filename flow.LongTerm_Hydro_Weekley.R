# Create a weekly long term dataframe and summarize the water level condition 

rm(list = ls())

source("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/ENP_WZ_2024_MS/function.indicator.R" )

library(tidyverse) 

setwd("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/ENP_WZ_2024_MS/data/data_raw")
# TS1 ####
TS1_wl <- read.csv("PHY_Rubio_002.csv") %>% 
  mutate( Date = as.Date(Date, format = "%Y-%m-%d"),
          SITENAME = as.factor(SITENAME),
          Week = week(Date),
          Year = format( Date, "%Y") %>% as.numeric,
          Year_wk = paste(Year, Week, sep = "-" )) %>% 
  filter(SITENAME == "TS/Ph1a") %>%  mutate_all(function(x) ifelse(is.nan(x), NA, x)) %>% mutate(TS1_daily_wl = (WaterDepth*0.01) ) %>%  reframe(.by= Year_wk, TS1_wl = mean(TS1_daily_wl,na.rm=T), Year = mean(Year))  


# TS1
TS1_sal <- read.csv("LT_ND_Rubio_001.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
         SITENAME = as.factor(SITENAME) ,
         Week = week(Date),
         Year = format( Date, "%Y") %>% as.numeric,
         Year_wk = paste(Year, Week, sep = "-" )) %>%
  filter(SITENAME == "TS/Ph1a") %>%  mutate_all(function(x) ifelse(is.nan(x), NA, x)) %>%rename(TS1_sal = Salinity)

TS1_sal$TS1_sal[TS1_sal$TS1_sal <= -9999] <-NA
TS1_sal$TS1_sal[TS1_sal$TS1_sal == 0] <-NA

TS1_sal <- TS1_sal %>%  reframe(.by= Year_wk, TS1_sal = mean(TS1_sal,na.rm=T), Year = mean(Year)) %>%  mutate_all(function(x) ifelse(is.nan(x), NA, x))

TS1 <- TS1_wl %>% full_join(TS1_sal, by = 'Year_wk')
TS1$Year <- TS1$Year.x
TS1$Year[is.na(TS1$Year)== TRUE] <- TS1$Year.y[is.na(TS1$Year)== TRUE] 
TS1 <-TS1 %>% select(Year_wk, Year,Year, TS1_wl,Year,TS1_sal)

# TS7 ####
TS7_wl <- read.csv("PHY_Castaneda_001.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
         SITENAME = as.factor(SITENAME),
         Week = week(Date),
         Year = format( Date, "%Y") %>% as.numeric,
         Year_wk = paste(Year, Week, sep = "-" )) %>% 
  filter(SITENAME == "TS/Ph7") %>% mutate_all(function(x) ifelse(is.nan(x), NA, x)) %>% mutate(TS7_wl = WaterLevel*0.01) %>%
  as.data.frame()%>% distinct()

TS7_wl$TS7_wl[TS7_wl$TS7_wl<= -99.9]<- NA

names(TS7_wl)
TS7_wl <- TS7_wl %>%  reframe(.by= Year_wk, TS7_wl = mean(TS7_wl,na.rm=T), Year = mean(Year)) %>% mutate_all(function(x) ifelse(is.nan(x), NA, x))

TS7_sal <- read.csv("LT_ND_Losada_001-2.csv") %>% 
  mutate( Date = as.Date(Date, format = "%Y-%m-%d"), 
          SITENAME = as.factor(SITENAME),
          Week = week(Date),
          Year = format( Date, "%Y") %>% as.numeric,
          Year_wk = paste(Year, Week, sep = "-" )) %>% 
  filter(SITENAME == "TS/Ph7a") %>% mutate_all(function(x) ifelse(is.nan(x), NA, x)) %>% rename(TS7_sal = Salinity) %>% distinct()

TS7_sal$TS7_sal[TS7_sal$TS7_sal <= -9999] <-NA

TS7_sal <- TS7_sal %>%  reframe(.by= Year_wk, TS7_sal = mean(TS7_sal,na.rm=T), Year = mean(Year)) %>% mutate_all(function(x) ifelse(is.nan(x), NA, x))


TS7 <- TS7_wl %>% full_join(TS7_sal, by = 'Year_wk') %>% mutate_all(function(x) ifelse(is.nan(x), NA, x))

TS7$Year <- TS7$Year.x
TS7$Year[is.na(TS7$Year)== TRUE] <- TS7$Year.y[is.na(TS7$Year)== TRUE] 
TS7 <-TS7 %>% select(Year_wk, Year,Year, TS7_wl,Year,TS7_sal)

summary(TS7)
TS7 <- TS7 %>% filter( Year >= 1999)
TS7$Indx <- indicator(ht=1.5, wl=TS7$TS7_wl) %>% as.factor
TS7$count <- 1


TS1 <- TS1 %>% filter( Year >= 2008)
TS1$Indx <- indicator(ht=1.07, wl=TS1$TS1_wl) %>% as.factor
TS1$count <- 1

TS1.summary <- TS1 %>% mutate_all(function(x) ifelse(is.nan(x), NA, x)) %>% reframe( .by=c(Year, Indx),
                                TS1_wl = mean(TS1_wl, na.rm=T), 
                                TS1_sal = mean(TS1_sal, na.rm=T), 
                                duration = sum(count)/52)  %>% 
  reframe(.by=Indx , 
          TS1_wl = mean_se(TS1_wl),
          TS1_sal = mean_se(TS1_sal),
          duration = mean_se(duration)) %>% as.data.frame %>% mutate(duration.mean= duration$y,duration.se = duration$y - duration$ymin, TS1_wl.mean = TS1_wl$y%>% round(2), TS1_wl.se = TS1_wl$y - TS1_wl$ymin %>% round(4),TS1_sal.mean = TS1_sal$y%>% round(2), TS1_sal.se = TS1_sal$y - TS1_sal$ymin %>% round(4)
                                                                      ) %>% select(Indx, duration.mean, duration.se, TS1_wl.mean, TS1_wl.se, TS1_sal.mean, TS1_sal.se)


TS7.summary <- TS7 %>% mutate_all(function(x) ifelse(is.nan(x), NA, x)) %>% reframe( .by=c(Year, Indx),
                                                                                     TS7_wl = mean(TS7_wl, na.rm=T), 
                                                                                     TS7_sal = mean(TS7_sal, na.rm=T), 
                                                                                     duration = sum(count)/52)  %>% 
  reframe(.by=Indx , 
          TS7_wl = mean_se(TS7_wl),
          TS7_sal = mean_se(TS7_sal),
          duration = mean_se(duration)) %>% as.data.frame %>% mutate(duration.mean= duration$y,duration.se = duration$y - duration$ymin, TS7_wl.mean = TS7_wl$y%>% round(2), TS7_wl.se = TS7_wl$y - TS7_wl$ymin %>% round(4),TS7_sal.mean = TS7_sal$y%>% round(2), TS7_sal.se = TS7_sal$y - TS7_sal$ymin %>% round(4)
          ) %>% select(Indx, duration.mean, duration.se, TS7_wl.mean, TS7_wl.se, TS7_sal.mean, TS7_sal.se)
# Summary for 2021:

summary(TS7)
TS7 <- TS7 %>% filter( Year == 2021)
TS7$Indx <- indicator(ht=1.5, wl=TS7$TS7_wl) %>% as.factor
TS7$count <- 1


TS1 <- TS1 %>% filter( Year == 2021)
TS1$Indx <- indicator(ht=1.07, wl=TS1$TS1_wl) %>% as.factor
TS1$count <- 1


TS7.summary2021 <- TS7 %>% reframe( .by=c(Indx),
                                    TS7_wl = mean_se(TS7_wl), 
                                    TS7_sal = mean_se(TS7_sal),
                                    duration = sum(count)) %>% 
  mutate(TS7_wl.mean = TS7_wl$y,
         TS7_wl.se = TS7_wl$y- TS7_wl$ymin,
         TS7_sal.mean = TS7_sal$y,
         TS7_sal.se = TS7_sal$y- TS7_sal$ymin)

TS1.summary2021 <- TS1 %>% reframe( .by=c(Indx),
                                    TS1_wl = mean_se(TS1_wl), 
                                    TS1_sal = mean_se(TS1_sal),
                                    duration = sum(count)) %>% 
  mutate(TS1_wl.mean = TS1_wl$y,
         TS1_wl.se = TS1_wl$y- TS1_wl$ymin,
         TS1_sal.mean = TS1_sal$y,
         TS1_sal.se = TS1_sal$y- TS1_sal$ymin)

summary(TS1$TS1_wl)
# EOF

