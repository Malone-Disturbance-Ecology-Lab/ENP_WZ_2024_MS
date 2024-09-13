# make a water level and salinity csv for Q2 - comparing ecotone and inland water level and salinity relationships

# last updated: AR 5/16/24

# Clear environment
rm(list = ls())

# load packages
library(tidyverse)

# load marl prairie data (TS1 water depth and surface salinity - 2021) ####
# load TS1 water level from the flux dataset (HOBO logger)
TSPH1_flux_met <- read.csv("data/data_raw/AR_TSPH1_2021.csv")
TSPH1_flux_met <- TSPH1_flux_met[-c(1)]

TSPH1_flux_met$TIMESTAMP <- as.POSIXct(TSPH1_flux_met$TIMESTAMP,
                              format = "%Y-%m-%d %H:%M:%S",
                              tz = "EST") 

TSPH1_flux_met$Date <- as.Date(TSPH1_flux_met$TIMESTAMP,
                          format = "%Y-%m-%d")
TSPH1_flux_wl <- TSPH1_flux_met %>%
  group_by(Date) %>%
  summarize(TS1_tower_daily_wl = mean(wl)) %>% 
  as.data.frame()

TSPH1_flux_wl$Week <- week(TSPH1_flux_wl$Date)
summary(TSPH1_flux_wl)
  
# TS1 surface water salinity
# https://fce-lter.fiu.edu/data/core/metadata/?packageid=knb-lter-fce.1079.15
# LT_ND_Rubio-001
# frequency: composite sample every 3 days
# 1999 - present

TS1_sal <- read_csv("data/data_raw/LT_ND_Rubio_001.csv")
lapply(TS1_sal, class)
TS1_sal$Date <- as.Date(TS1_sal$Date, format = "%Y-%m-%d")
TS1_sal$SITENAME <- as.factor(TS1_sal$SITENAME)

TS1_sal <- TS1_sal %>%
  filter(SITENAME == "TS/Ph1a") %>%
  filter(Date > "2020-12-31" & Date <= "2021-12-31" ) # salinity data from 2003-03 to end of 2021
TS1_sal$Week <- week(TS1_sal$Date)

TS1_sal <- TS1_sal %>%
  select(Date, Week, Salinity)
TS1_sal <- TS1_sal %>% 
  rename(TS1_mean_weekly_Sal = Salinity)

TS1_sal_21_w <- TS1_sal %>%
  group_by(Week) %>%
  summarize(TS1_mean_weekly_Sal= mean(TS1_mean_weekly_Sal, na.rm = TRUE)) 

TS1_wl_sal <- left_join(TSPH1_flux_wl, TS1_sal_21_w, by = "Week")
TS1_wl_sal <- TS1_wl_sal[-c(366),] # drop 2022-01-01

TS1_wl_sal <- TS1_wl_sal %>% 
  rename(date = Date)

# load ecotone data (SE1 water depth and surface salinity - 2021) ####

# water level
se.wl <- read.csv('data/data_raw/SE_WaterLevel_2020_2022.csv') # se.wl.df

se.wl$TIMESTAMP <- as.POSIXct(se.wl$TIMESTAMP,
                                 format = "%Y-%m-%d %H:%M:%S",
                                 tz = "EST")

se.wl.df  <- se.wl %>% 
  subset(TIMESTAMP>=  as.POSIXct("2021-01-01 00:00:00") &
           TIMESTAMP< as.POSIXct("2022-01-01 00:00:00")) %>% as.data.frame()  %>% distinct(TIMESTAMP, .keep_all = TRUE) %>% mutate(wl = wl.corr) %>% select(TIMESTAMP, wl)

# met data
load("data/data_raw/se1_Data_AllData.RDATA") # se1

# fix the timestamps and add a date column to se.wl.df
se1$TIMESTAMP <- as.POSIXct(se1$TIMESTAMP,  
                                   format= "%Y-%m-%d %H:%M:%S",
                                   tz="EST")

se.wl.df$date <- as.Date(as.character(se.wl.df$TIMESTAMP))


# prepare se1 data to be one dataframe #
df <- se1 %>% 
    subset(date >= "2021-01-01" & 
           date < "2022-01-01") %>%
    as.data.frame()

wl_df <- se.wl.df  %>%
          as.data.frame()


names(df)
df2 <- df %>% dplyr::select("TIMESTAMP",  "Year",  "Month", "Day", "Hour", "Doy", "Hour2", "date", 
                            "EC", "Salinity.Hill1986")
names(df2)

# df2 has 22710, but should be 17520 observations --> drop duplicate timestamps in df2
which(duplicated(df2$TIMESTAMP)) # 2021-01-01 until 2021-01-13 14:00:00 are quadrupled
df2 <- df2[!duplicated(df2$TIMESTAMP), ]

# merge water level into df2 
SE1_wl_sal <- left_join(df2, wl_df, by = "TIMESTAMP")
which(duplicated(SE1_wl_sal$TIMESTAMP))

SE1_wl_sal$TIMESTAMP <- as.POSIXct(SE1_wl_sal$TIMESTAMP,  
                                  format= "%Y-%m-%d %H:%M:%S",
                                  tz="EST")

# filter EC and Sal values that are out of range and replace with NAs 
which(SE1_wl_sal$EC==7999) #1648 7999's
SE1_wl_sal$EC.filtered <- SE1_wl_sal$EC

SE1_wl_sal$EC.filtered[SE1_wl_sal$EC==7999] <-NA
which(SE1_wl_sal$EC.filtered==7999)

SE1_wl_sal$Sal.Hill1986.filtered <- SE1_wl_sal$Salinity.Hill1986
SE1_wl_sal$Sal.Hill1986.filtered[SE1_wl_sal$EC==7999] <-NA

# rename the SE1 columns to include site code 
names(SE1_wl_sal) 
SE1_wl_sal <- SE1_wl_sal %>% 
              rename(date = date.x, SE1_EC = EC, SE1_wl = wl, SE1_SalHill = Salinity.Hill1986,
                     SE1_EC.filtered = EC.filtered, SE1_Sal.filtered = Sal.Hill1986.filtered) 

# drop additional date column
SE1_wl_sal <- SE1_wl_sal[-c(12)] #"date.y"
View(SE1_wl_sal)
# summarize daily SE1 water level and salinity statistics
se1daily21 <- SE1_wl_sal %>% 
  group_by(date) %>% 
  summarize(SE1_mean_daily_WL = mean(SE1_wl, na.rm = TRUE), 
            SE1_mean_daily_EC = mean(SE1_EC.filtered, na.rm = TRUE), 
            SE1_mean_daily_Sal = mean(SE1_Sal.filtered, na.rm = TRUE)) %>%
  as.data.frame()

se1daily21 <- se1daily21 %>% mutate_if(is.numeric ,~ifelse(is.nan(.), NA, .)) # converts NaN to NA
se1daily21 <- se1daily21 %>% mutate_if(is.numeric, ~ifelse(is.infinite(.), NA, .)) # converts Inf to NA

# add the daily SE1 summary back into the big dataframe 
SE1_wl_sal <- left_join(SE1_wl_sal, se1daily21, by = "date")


# load mangrove site data (TS7 water depth and surface water salinity - 2021) ####

#TS7 water depth 2001 to present: https://fce-lter.fiu.edu/data/core/metadata/?datasetid=PHY_Castaneda_001 
# NOTE: hourly water level is recorded in centimeters

Mangrove_wl <- read.csv("data/data_raw/PHY_Castaneda_001.csv")

# convert the variables to the proper class
Mangrove_wl$Date <- as.Date(Mangrove_wl$Date, format = "%Y-%m-%d")
Mangrove_wl$SITENAME <- as.factor(Mangrove_wl$SITENAME)

TS7_wl <- Mangrove_wl %>%
  filter(SITENAME == "TS/Ph7") %>% # only TS7
  filter(Date >= "2021-01-01") %>% # only 2021
  mutate("TS7_hourly_wl" = WaterLevel*0.01) %>% # convert the WaterLevel units from centimeters to meters
  as.data.frame()

TS7_wl$TIMESTAMP <- as.POSIXct(paste(TS7_wl$Date, TS7_wl$Time), 
                                  format = "%Y-%m-%d %H:%M:%S",
                                  tz = "EST")
lapply(TS7_wl, class)

TS7_wl$Hour <- lubridate::hour(TS7_wl$TIMESTAMP)
TS7_wl$Hour <- as.numeric(TS7_wl$Hour)

which(TS7_wl$TS7_hourly_wl <= -99.99) # no NA's

TS7_wl_daily <- TS7_wl %>% 
  group_by(Date) %>% 
  summarize(TS7_mean_daily_wl = mean(TS7_hourly_wl, na.rm = TRUE)) %>%
  as.data.frame()

TS7_wl_daily <- TS7_wl_daily %>% mutate_if(is.numeric ,~ifelse(is.nan(.), NA, .)) # converts NaN to NA
TS7_wl_daily <- TS7_wl_daily %>% mutate_if(is.numeric, ~ifelse(is.infinite(.), NA, .)) # converts Inf to NA

TS7_wl_daily <- TS7_wl_daily %>% 
  rename(date = Date)

TS7_wl_2 <- TS7_wl %>%
  select(TIMESTAMP, TS7_hourly_wl)

# TS7 surface salinity from 1996 to 2022: https://fce-lter.fiu.edu/data/core/metadata/?packageid=knb-lter-fce.1074.17 
# Salinity, TN, and TP are 3 day composite. File name: LT_ND_Losada_001-2

TS7_sal <- read_csv("data/data_raw/LT_ND_Losada_001-2.csv")
TS7_sal$SITENAME <- as.factor(TS7_sal$SITENAME)
TS7_sal_21 <- TS7_sal %>%
  filter(SITENAME == "TS/Ph7a") %>%
  filter(Date >= "2021-01-01" & Date <= "2021-12-31" )

TS7_sal_21$Week <- week(TS7_sal_21$Date)

# replace NA value identifier with NA
# find missing data (-9999) and convert to NA
which(TS7_sal_21$Salinity <= -9999) # 1 NA 
TS7_sal_21$Salinity[TS7_sal_21$Salinity<= -9999] <-NA

TS7_sal_21_w <- TS7_sal_21 %>%
  group_by(Week) %>%
  summarize(TS7_mean_weekly_Sal= mean(Salinity, na.rm = TRUE)) 

# join all data together in a new dataframe ####
wl_sal_2021 <- left_join(SE1_wl_sal, TS1_wl_sal, by = "date") 
wl_sal_2021 <- left_join(wl_sal_2021, TS7_wl_2, by = "TIMESTAMP")
wl_sal_2021 <- left_join(wl_sal_2021, TS7_wl_daily, by = "date") 
wl_sal_2021 <- left_join(wl_sal_2021, TS7_sal_21_w, by = "Week") 

# clean the dataframe 

# reorder columns by timestep resolution
names(wl_sal_2021)
wl_sal_2021<- wl_sal_2021[,c(1,8,2:3,18,4,6,5,7,17,19,11,9:10,12:16,20:22)]

# fill the NA's in the time columns (Mar 14, 2021 and Dec 31, 2021) 
wl_sal_2021$Year[is.na(wl_sal_2021$Year)] <-"2021"

which(is.na(wl_sal_2021$Month)) # fill NA's differently to account for gap in March [3461, 3462]
wl_sal_2021$Month[3461:3462] <- "3"
wl_sal_2021$Month[17511:17520] <- "12"

which(is.na(wl_sal_2021$Day)) 
wl_sal_2021$Day[3461:3462] <- "14"
wl_sal_2021$Day[17511:17520] <- "31"

which(is.na(wl_sal_2021$Doy)) 
wl_sal_2021$Doy[3461:3462] <- "73"
wl_sal_2021$Doy[17511:17520] <- "365" 

lapply(wl_sal_2021, class)
wl_sal_2021$Year <- as.numeric(wl_sal_2021$Year)
wl_sal_2021$Month <- as.numeric(wl_sal_2021$Month)
wl_sal_2021$Day <- as.numeric(wl_sal_2021$Day)
wl_sal_2021$Doy <- as.numeric(wl_sal_2021$Doy)

# summarize the daily data into weekly means ####

# weekly
weekly21 <- wl_sal_2021 %>% 
  group_by(Week) %>% 
  summarize(TS1_mean_weekly_WL = mean(TS1_tower_daily_wl, na.rm = TRUE),
            SE1_mean_weekly_WL = mean(SE1_wl, na.rm = TRUE), 
            SE1_mean_weekly_EC = mean(SE1_EC.filtered, na.rm = TRUE), 
            SE1_mean_weekly_Sal = mean(SE1_Sal.filtered, na.rm = TRUE),
            TS7_mean_weekly_WL = mean(TS7_hourly_wl, na.rm = TRUE)) %>%
  as.data.frame()

weekly21 <- left_join(weekly21, TS1_sal_21_w, by = "Week") 
weekly21 <- left_join(weekly21, TS7_sal_21_w, by = "Week") 

weekly21 <- weekly21 %>% mutate_if(is.numeric ,~ifelse(is.nan(.), NA, .)) # converts NaN to NA
weekly21 <- weekly21 %>% mutate_if(is.numeric, ~ifelse(is.infinite(.), NA, .)) # converts Inf to NA

names(weekly21)
weekly21 <- weekly21[,c(1:2,7,3:6,8)]

# export weekly summary as csv ####
write.csv(weekly21, "data/AR_wl_sal_2021_weekly.csv", row.names = FALSE)

# EOF