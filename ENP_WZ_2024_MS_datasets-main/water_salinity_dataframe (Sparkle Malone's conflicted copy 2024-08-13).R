# This Script produces the dataframe for Q2 and to Summarise hydrology for table 2

# last updated: SLM 08/24/2024
rm(list = ls()) 
library(tidyverse)

setwd('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/ENP_WZ_2024_MS')

# Get WL and Salinity data for 2021
# TS1 ####
TSPH1_flux_met <- read.csv("data/data_raw/AR_TSPH1_2021.csv")[-c(1)] # Import Tower data with WL from HOBO Logger

# format timestamp and extract the date:
TSPH1_WL <- TSPH1_flux_met %>% mutate( TIMESTAMP= as.POSIXct(TIMESTAMP,
                                       format = "%Y-%m-%d %H:%M:%S",tz = "EST"),
                                       Date = as.Date(TIMESTAMP,format = "%Y-%m-%d"),
                                       Week = week(Date))  %>% distinct() %>% reframe(.group_by= Week,
                                            TS1_tower_wl = mean(wl, na.rm=T)) %>% distinct() %>% rename(Week = .group_by)
  
# TS1 surface water salinity, LT_ND_Rubio-001, (1999 - present) from:
# https://fce-lter.fiu.edu/data/core/metadata/?packageid=knb-lter-fce.1079.15
# frequency: composite sample every 3 days

TS1_Sal <- read_csv("data/data_raw/LT_ND_Rubio_001.csv") %>% 
  mutate( Date = Date %>% as.Date(format = "%Y-%m-%d"),
  SITENAME <- SITENAME %>% as.factor(),
  Week = Date %>% week()) %>%
  filter(SITENAME == "TS/Ph1a") %>%
  select(Date, Week, Salinity) %>% 
  rename(TS1_mean_weekly_Sal = Salinity) %>%
  filter(Date > "2020-12-31" & Date <= "2021-12-31" ) %>% reframe( .by = Week,
                                     TS1_mean_weekly_Sal= mean(TS1_mean_weekly_Sal, na.rm = TRUE)) %>% distinct

TS1_WL_SAL <- TSPH1_WL %>% left_join(TS1_Sal, by = "Week") %>% filter( Week != 53)

rm(TSPH1_WL, TSPH1_flux_met,TS1_Sal)

# SE1 (Only has contemporary data) ####
load("data/data_raw/SE1_WaterLevel.RDATA") # Water level
load("data/data_raw/se1_Data_AllData.RDATA") # Met data

se1.salinity <- se1 %>% mutate(TIMESTAMP = as.POSIXct(TIMESTAMP, format= "%Y-%m-%d %H:%M:%S", tz="EST")) %>% subset(date >= "2021-01-01" & date < "2022-01-01") %>% 
  dplyr::select("TIMESTAMP","EC", "Salinity.Hill1986")

se1.wl <- se.wl.df %>% mutate(TIMESTAMP = as.POSIXct(TIMESTAMP,format= "%Y-%m-%d %H:%M:%S", tz="EST"),
                                date = as.Date(as.character(TIMESTAMP))) %>% 
  subset (date >= "2021-01-01" & date < "2022-01-01") %>% as.data.frame() %>% distinct()

SE1_wl_sal <- left_join(se1.salinity, se1.wl, by = "TIMESTAMP") %>%  
  mutate(TIMESTAMP  = as.POSIXct( TIMESTAMP,format= "%Y-%m-%d %H:%M:%S",tz="EST"),
         EC.filtered = EC)

SE1_wl_sal$EC.filtered[SE1_wl_sal$EC==7999] <- NA
SE1_wl_sal$Sal.Hill1986.filtered <- SE1_wl_sal$Salinity.Hill1986
SE1_wl_sal$Sal.Hill1986.filtered[SE1_wl_sal$EC==7999] <-NA


# rename the SE1 columns to include site code 
SE1_WL_SAL <- SE1_wl_sal %>% mutate(Week = week(date %>% as.Date)) %>% 
              rename(SE1_EC = EC, SE1_wl = wl, SE1_SalHill = Salinity.Hill1986,
                     SE1_EC.filtered = EC.filtered, SE1_Sal.filtered = Sal.Hill1986.filtered) %>% 
  distinct %>% reframe(.by = Week, SE1_mean_WL = mean(SE1_wl, na.rm = TRUE), 
            SE1_mean_EC = mean(SE1_EC.filtered, na.rm = TRUE), 
            SE1_mean_Sal = mean(SE1_Sal.filtered, na.rm = TRUE)) %>% distinct() %>% mutate_if(is.numeric ,~ifelse(is.nan(.), NA, .)) %>% mutate_if(is.numeric, ~ifelse(is.infinite(.), NA, .)) # converts Inf to NA

rm(  SE1_wl_sal, se1.wl, se.wl.df, se1.salinity, se1)

# TS7load mangrove site data (TS7 water depth and surface water salinity - 2021) ####
#TS7 water depth 2001 to present: https://fce-lter.fiu.edu/data/core/metadata/?datasetid=PHY_Castaneda_001 
# NOTE: hourly water level is recorded in centimeters

TS7_wl <- read.csv("data/data_raw/PHY_Castaneda_001.csv") %>% mutate(
  Date = as.Date( Date, format = "%Y-%m-%d"),
  SITENAME = as.factor( SITENAME) ,
  TIMESTAMP = as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S",tz = "EST"),
  Hour = lubridate::hour(TIMESTAMP),
  Hour = as.numeric(Hour),
  Week = week(Date)) %>%
  filter(SITENAME == "TS/Ph7") %>% # only TS7
  filter(Date >= "2021-01-01") %>% # only 2021
  mutate("TS7_hourly_wl" = WaterLevel*0.01) %>% # convert the WaterLevel units from centimeters to meters
  as.data.frame()

TS7_wl_week <- TS7_wl %>% reframe( .by =Week,
                                    TS7_mean_daily_wl = mean(TS7_hourly_wl, na.rm = TRUE)) %>% distinct() %>% 
  mutate_if(is.numeric ,~ifelse(is.nan(.), NA, .)) %>% 
  mutate_if(is.numeric, ~ifelse(is.infinite(.), NA, .)) %>%
  select(Week, TS7_mean_daily_wl)

# TS7 surface salinity from 1996 to 2022: https://fce-lter.fiu.edu/data/core/metadata/?packageid=knb-lter-fce.1074.17 
# Salinity, TN, and TP are 3 day composite. File name: LT_ND_Losada_001-2

TS7_sal_21 <- read_csv("data/data_raw/LT_ND_Losada_001-2.csv") %>% 
  mutate( SITENAME = SITENAME %>% as.factor(), 
        Week = week(Date %>% as.Date)) %>%
  filter(SITENAME == "TS/Ph7a") %>%
  filter(Date >= "2021-01-01" & Date <= "2021-12-31" )

TS7_sal_21$Salinity[TS7_sal_21$Salinity<= -9999] <-NA

TS7_sal_21_w <- TS7_sal_21 %>% reframe( .by= Week,
                                        TS7_mean_weekly_Sal= mean(Salinity, na.rm = TRUE)) %>%  distinct


TS7_WL_SAL <- TS7_wl_week %>% left_join( TS7_sal_21_w, by = 'Week') 

rm(TS7_wl, TS7_wl_week, TS7_sal_21, TS7_sal_21_w)

# join all data together in a new dataframe ####

WL_Sal_2021 <- TS1_WL_SAL %>% 
  left_join(SE1_WL_SAL, by = "Week") %>% 
  left_join(TS7_WL_SAL, by = "Week")  %>% 
  rename(TS1_mean_weekly_WL = TS1_tower_wl,
         TS1_mean_weekly_Sal = TS1_mean_weekly_Sal,
            SE1_mean_weekly_WL = SE1_mean_WL, 
            SE1_mean_weekly_EC = SE1_mean_EC, 
            SE1_mean_weekly_Sal = SE1_mean_Sal,
            TS7_mean_weekly_WL = TS7_mean_daily_wl,
         TS7_mean_weekly_Sal = TS7_mean_weekly_Sal)

summary(WL_Sal_2021)
# export weekly summary as csv ####
write.csv(WL_Sal_2021, "data/data_raw/AR_wl_sal_2021_weekly.csv", row.names = FALSE)

# Legacy Water level and Salinity information for 

# TS1 Time series:
TS1_Sal <- read_csv("data/data_raw/LT_ND_Rubio_001.csv") %>% 
  mutate( Date = Date %>% as.Date(format = "%Y-%m-%d"),
          SITENAME <- SITENAME %>% as.factor(),
          Week = Date %>% week(),
          Year = Date %>% format( "%Y"),
          YearWk = paste(Week, Year, sep="-" )) %>%
  filter(SITENAME == "TS/Ph1a") %>%
  select(YearWk, Salinity) %>%  reframe( .by = YearWk,
                                         TS1_mean_weekly_Sal= mean(Salinity, na.rm = TRUE)) %>% distinct

TS1_WL <- read_csv("data/data_raw/PHY_Rubio_002.csv") %>% 
  mutate( Date = Date %>% as.Date(format = "%Y-%m-%d"),
          SITENAME <- SITENAME %>% as.factor(),
          Week = Date %>% week(),
          Year = Date %>% format( "%Y"),
          YearWk = paste(Week, Year, sep="-" )) %>%
  filter(SITENAME == "TS/Ph1a") %>%
  select(YearWk, WaterDepth) %>% reframe( .by = YearWk,
                                          TS1_mean_weekly_WL= mean(WaterDepth, na.rm = TRUE)) %>% distinct

TS1_WL_SAL <- TS1_WL %>% left_join( TS1_Sal, by='YearWk')

# TS7:
names(TS7_wl)
TS7_wl <- read.csv("data/data_raw/PHY_Castaneda_001.csv") %>% mutate(
  Date = as.Date( Date, format = "%Y-%m-%d"),
  Year = Date %>% format( "%Y"),
  Week = week(Date),
  YearWk = paste(Week, Year, sep="-" )) %>%
  filter(SITENAME == "TS/Ph7") %>% select(YearWk, WaterLevel)

TS7_wl$WaterLevel[ TS7_wl$WaterLevel == -9999.00] <- NA

TS7_wl2 <-TS7_wl %>% 
  mutate_if(is.numeric ,~ifelse(is.nan(.), NA, .)) %>% 
  mutate_if(is.numeric, ~ifelse(is.infinite(.), NA, .)) %>% 
  mutate(TS7_hourly_wl = WaterLevel*0.01) %>% # convert the WaterLevel units from centimeters to meters
  reframe( .by =YearWk,
           TS7_mean_wl = mean(TS7_hourly_wl, na.rm = TRUE)) %>% distinct()  %>%
  select(YearWk, TS7_mean_wl)

TS7_Sal <- read_csv("data/data_raw/LT_ND_Losada_001-2.csv") %>% 
  mutate( SITENAME = SITENAME %>% as.factor(),
          Date = as.Date( Date, format = "%Y-%m-%d"),
          Year = Date %>% format( "%Y"),
          Week = week(Date),
          YearWk = paste(Week, Year, sep="-" )) %>%
  filter(SITENAME == "TS/Ph7a")

TS7_Sal$Salinity[TS7_Sal$Salinity<= -9999] <-NA

TS7_Sal_wk <- TS7_Sal %>% reframe( .by= YearWk,
                                        TS7_mean_weekly_Sal= mean(Salinity, na.rm = TRUE)) %>%  distinct


TS7_WL_SAL <- TS7_wl2 %>% left_join( TS7_Sal_wk, by = 'YearWk') 

plot(TS7_WL_SAL$TS7_mean_wl)


# EOF