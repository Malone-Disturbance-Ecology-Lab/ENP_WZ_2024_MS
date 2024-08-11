# initial data exploration for DBHYDRO sites &
# dataframe creation for long term water level and salinity analysis (Q1)

# last updated: AR, 5/24/2024

rm(list = ls())
getwd()
# load packages

#devtools::install_github("ropensci/dbhydroR")
install.packages("dbhydroR")
library(dbhydroR)
library(tidyverse)        # everything else
library(terra)            # Spatial analysis to find elevation for TSPh1a and TSPh7

# PART 1: data upload #####

# upload csv's manually downloaded from DBHYDRO and prepare them #####

# inland site NP_EV8 on DBHYDRO or EVER8 on EDEN
# https://my.sfwmd.gov/dbhydroplsql/show_dbkey_info.date_selection?v_js_flag=Y&v_db_request_id=8977494&v_parameter_string=&v_dbkey=15596&v_frequency=&v_sdate=19911224&v_edate=20240524&v_datum=
get_dbkey(category = "SW", stationid = "NP-EV8")

NP_EV8 <- read.csv("~/data_raw/NPEV8_15596.csv", skip = 4)
NP_EV8 <- NP_EV8[-c(1:2,5:19)]
NP_EV8 <- NP_EV8 %>% 
  rename(Date = Daily.Date, `NP-EV8_STG_ft NGVD29` = Data.Value)

lapply(NP_EV8, class)
NP_EV8$Date <- as.Date(NP_EV8$Date, format = "%d-%b-%y")

# coastal site ENPHC water level
# https://my.sfwmd.gov/dbhydroplsql/show_dbkey_info.date_selection?v_js_flag=Y&v_db_request_id=8977677&v_parameter_string=&v_dbkey=39307&v_frequency=&v_sdate=19930729&v_edate=20240524&v_datum=
get_dbkey(category = "SW", stationid = "ENPHC") 
ENPHC_wl <- read.csv("~/data_raw/ENPHC_39307.csv", skip = 4)
ENPHC_wl <- ENPHC_wl[-c(1:2,5:19)]
ENPHC_wl <- ENPHC_wl %>% 
  rename(Date = Daily.Date, `ENPHC_STG88_ft NAVD88` = Data.Value)

lapply(ENPHC_wl, class)
ENPHC_wl$Date <- as.Date(ENPHC_wl$Date, format = "%d-%b-%y")

# coastal site ENPHC surface salinity
#https://my.sfwmd.gov/dbhydroplsql/show_dbkey_info.date_selection?v_js_flag=Y&v_db_request_id=8977677&v_parameter_string=&v_dbkey=63625&v_frequency=&v_sdate=19880714&v_edate=20240524&v_datum=
get_dbkey(category = "WQ", stationid = "ENPHC") 

ENPHC_sal <- read.csv("~/data_raw/ENPHC_63625.csv", skip = 4)
ENPHC_sal <- ENPHC_sal[-c(1:2,5:19)]
ENPHC_sal <- ENPHC_sal %>% 
  rename(Date = Daily.Date, ENPHC_SALI_PSU = Data.Value)

lapply(ENPHC_sal, class)
ENPHC_sal$Date <- as.Date(ENPHC_sal$Date, format = "%d-%b-%y")

# add TS1 long term water level and surface salinity to the dataframe for inland fresh representation ####
# load TS1 long term water level
# TS1 depth from 2006 to 2023: https://fce-lter.fiu.edu/data/core/metadata/?packageid=knb-lter-fce.1098.13 
# PHY_Rubio_002
# NOTE: water level is recorded in centimeters; highest res is daily, there are no missing values for TS1 (-9999)
TS1_wl <- read.csv("data_raw/PHY_Rubio_002.csv")

lapply(TS1_wl, class)
# convert the variables to the proper class
TS1_wl$Date <- as.Date(TS1_wl$Date, format = "%Y-%m-%d")
TS1_wl$SITENAME <- as.factor(TS1_wl$SITENAME)

TS1_wl <- TS1_wl %>%
  filter(SITENAME == "TS/Ph1a") %>% # only TS1
  mutate("TS1_daily_wl" = WaterDepth*0.01) %>% # convert the WaterLevel units from centimeters to meters
  as.data.frame()

which(TS1_wl$WaterDepth==-9999.00) # no missing values
ggplot(TS1_wl) + geom_line(mapping = aes(Date, TS1_daily_wl))

# convert from water depth to water level relative to NAVD88
# according to MDC DEM elevation is 0.948013 m
DEM_fl00 <- terra::rast("data_raw/fl2015_miami_dade_dem_Job965321/fl2015_miami_dade_dem_J965321_000_000.tif")
TSPh1a <- matrix(c(-80.590, 25.424), ncol = 2)
TSPh1a <- vect(TSPh1a, crs="+proj=longlat +datum=WGS84")
TSPh1a_2881 <- project(TSPh1a, "EPSG:2881")
elev_TSPh1a <- terra::extract(DEM_fl00, TSPh1a_2881)
print(elev_TSPh1a*0.3048) 

TS1_wl$TS1_NAVD88 <- NA
TS1_wl$TS1_NAVD88 <- TS1_wl$TS1_daily_wl+0.95
summary(TS1_wl)

TS1_wl <- TS1_wl[,c(2,5)]

# TS1 surface water salinity
# https://fce-lter.fiu.edu/data/core/metadata/?packageid=knb-lter-fce.1079.15
# LT_ND_Rubio-001
# frequency: composite sample every 3 days
# 1999 - present

TS1_sal <- read_csv("data_raw/LT_ND_Rubio_001.csv")
lapply(TS1_sal, class)
TS1_sal$Date <- as.Date(TS1_sal$Date, format = "%Y-%m-%d")
TS1_sal$SITENAME <- as.factor(TS1_sal$SITENAME)

TS1_sal <- TS1_sal %>%
  filter(SITENAME == "TS/Ph1a") %>%
  filter( Date <= "2021-12-31" ) # salinity data from 2003-03 to end of 2021
TS1_sal$Week <- week(TS1_sal$Date)

which(TS1_sal$Salinity ==-9999.00) #7 missing values
TS1_sal$Salinity[TS1_sal$Salinity<= -9999] <-NA

ggplot(TS1_sal) + geom_line(mapping = aes(Date, Salinity))

TS1_sal <- TS1_sal[,c(2,3)]

TS1_sal <- TS1_sal %>% 
  rename(TS1_sal = Salinity)

# add TS7 long term water level and surface salinity to the dataframe for scrub representation ####
# load TS7 long term water level
# https://fce-lter.fiu.edu/data/core/metadata/?datasetid=PHY_Castaneda_001 
# PHY_Castaneda_001
# NOTE: water level is recorded in centimeters; highest res is hourly
TS7_wl <- read.csv("data_raw/PHY_Castaneda_001.csv")

# convert the variables to the proper class
TS7_wl$Date <- as.Date(TS7_wl$Date, format = "%Y-%m-%d")
TS7_wl$SITENAME <- as.factor(TS7_wl$SITENAME)

TS7_wl <- TS7_wl %>%
  filter(SITENAME == "TS/Ph7") %>% # only TS7
  mutate("TS7_hourly_wl" = WaterLevel*0.01) %>% # convert the WaterLevel units from centimeters to meters
  as.data.frame()

TS7_wl$TIMESTAMP <- as.POSIXct(paste(TS7_wl$Date, TS7_wl$Time), 
                                     format = "%Y-%m-%d %H:%M:%S",
                                     tz = "EST")

TS7_wl$Hour <- lubridate::hour(TS7_wl$TIMESTAMP)
TS7_wl$Hour <- as.numeric(TS7_wl$Hour)
lapply(TS7_wl, class)

# make an indicator for missing data (-99.99) and convert to NA
which(TS7_wl$TS7_hourly_wl <= -99.9) # 93954 NA's 
which(TS7_wl$TS7_hourly_wl == -99.99) #none

TS7_wl$TS7_hourly_wl_NA <- TS7_wl$TS7_hourly_wl
TS7_wl$TS7_hourly_wl_NA[TS7_wl$TS7_hourly_wl <= -99.99] <- NA 

summary(TS7_wl)

# convert from water depth to water level relative to NAVD88
# according to MDC DEM elevation is 0 m
#DEM_fl00 <- terra::rast("~/Desktop/Desktop - new mac pro/FIU/Lab/2021-ts1-se1-ts7/fl2015_miami_dade_dem_Job965321/fl2015_miami_dade_dem_J965321_000_000.tif")
TSPh7 <- matrix(c(-80.639, 25.190), ncol = 2)
TSPh7 <- vect(TSPh7, crs="+proj=longlat +datum=WGS84")
TSPh7_2881 <- project(TSPh7, "EPSG:2881") 
elev_TSPh7 <- terra::extract(DEM_fl00, TSPh7_2881) #outside the limits of the raster 

TSPh7_coords <- matrix(c(-80.639, 25.228), ncol = 2)
TSPh7_on_rast <- vect(TSPh7_coords, crs = "+proj=longlat +datum=WGS84")
crs(TSPh7_on_rast)
TSPh7_on_rast_2881 <- project(TSPh7_on_rast, "EPSG:2881")
elev_TSPh7 <- terra::extract(DEM_fl00, TSPh7_on_rast_2881)
print(elev_TSPh7*0.3048) 

TS7_wl$TS7_NAVD88 <- NA
TS7_wl$TS7_NAVD88 <- TS7_wl$TS7_hourly_wl_NA + 0
summary(TS7_wl)

TS7_wl_daily <- TS7_wl %>% 
  group_by(Date) %>% 
  summarize(TS7_daily_wl_NAVD88 = mean(TS7_NAVD88, na.rm = TRUE)) %>%
  as.data.frame()

TS7_wl_daily <- TS7_wl_daily %>% mutate_if(is.numeric ,~ifelse(is.nan(.), NA, .)) # converts NaN to NA
TS7_wl_daily <- TS7_wl_daily %>% mutate_if(is.numeric, ~ifelse(is.infinite(.), NA, .)) # converts Inf to NA

TS7_wl <- left_join(TS7_wl, TS7_wl_daily, by = "Date")

TS7_wl_2 <- TS7_wl[c("Date", "TS7_daily_wl_NAVD88")]

# TS7 surface salinity from 1996 to 2022: https://fce-lter.fiu.edu/data/core/metadata/?packageid=knb-lter-fce.1074.17 
# Salinity, TN, and TP are 3 day composite. File name: LT_ND_Losada_001-2
TS7_sal <- read_csv("data_raw/LT_ND_Losada_001-2.csv")
TS7_sal$Date <- as.Date(TS7_sal$Date, format = "%Y-%m-%d")
TS7_sal$SITENAME <- as.factor(TS7_sal$SITENAME)
TS7_sal <- TS7_sal %>%
  filter(SITENAME == "TS/Ph7a") %>%
  filter(Date >= "1996-04-26" & Date <= "2021-12-31")

# find missing data (-9999) and convert to NA
which(TS7_sal$Salinity <= -9999) #6 na 
TS7_sal$Salinity[TS7_sal$Salinity<= -9999] <-NA

TS7_sal <- TS7_sal %>% 
  rename(TS7_sal = Salinity)
names(TS7_sal)
TS7_sal <- TS7_sal[,c(2,3)]

# PART 2: # dataframe creation for long term water level and salinity (Q1) #####

# remove everything except what we need: (NP_EV8, ENPHC_wl, ENPHC_sal, TS1_wl, TS1_sal)
rm(list = ls()[!ls() %in% c("NP_EV8", "ENPHC_wl", "ENPHC_sal", "TS1_wl", "TS1_sal", "TS7_wl_2", "TS7_sal")])

# make one dataframe for the inland and coastal stations we will use #####
# convert NP-EV8 from NGVD29 to NAVD88 using conversion listed on EDEN
# (https://sofia.usgs.gov/eden/station.php?stn_name=EVER8)
NP_EV8 <- NP_EV8 %>% mutate(NPEV8 = `NP-EV8_STG_ft NGVD29`-1.54)
     
# combine water levels into same dataframe
long_term_data <- full_join(NP_EV8, ENPHC_wl, by = "Date")

# convert from feet to meters
long_term_data <- long_term_data %>% 
  mutate (NPEV8_ngvd29=`NP-EV8_STG_ft NGVD29`*0.3048, NPEV8=NPEV8*0.3048, ENPHC=`ENPHC_STG88_ft NAVD88`*0.3048)

# add ENPHC salinity data to dataframe
long_term_data <- left_join(long_term_data,  ENPHC_sal, by = "Date")
long_term_data <- left_join(long_term_data,  TS1_wl, by = "Date")
long_term_data <- left_join(long_term_data,  TS1_sal, by = "Date")
long_term_data <- left_join(long_term_data,  TS7_wl_2, by = "Date")
long_term_data <- left_join(long_term_data,  TS7_sal, by = "Date")

# drop and rename columns
names(long_term_data)
long_term_data <- long_term_data[-c(2,4,5)]

long_term_data <- long_term_data %>% 
  rename(NPEV8_daily_wl = NPEV8, ENPHC_daily_wl = ENPHC, ENPHC_daily_sal = ENPHC_SALI_PSU)

# add time variables to the long_term_data dataframe (year, month, week) ####
# year
long_term_data$Year <- lubridate::year(long_term_data$Date) 

# month
long_term_data$Month <- lubridate::month(long_term_data$Date) 

# week
long_term_data$Week <- lubridate::week(long_term_data$Date)

# aggregate daily data to lower resolution timesteps ####
monthly <- long_term_data %>% 
  group_by(Year, Month) %>% 
  summarize(NPEV8_mean_mon_WL =  mean(NPEV8_daily_wl, na.rm = TRUE), 
            ENPHC_mean_mon_WL =  mean(ENPHC_daily_wl, na.rm = TRUE), 
            ENPHC_mean_mon_Sal = mean(ENPHC_daily_sal, na.rm = TRUE),
            TS1_mean_mon_WL =    mean(TS1_NAVD88, na.rm = T),
            TS1_mean_mon_Sal =   mean(TS1_sal, na.rm = T),
            TS7_mean_mon_WL =    mean(TS7_daily_wl_NAVD88, na.rm = T),
            TS7_mean_mon_Sal =   mean(TS7_sal, na.rm = T)) %>%
  as.data.frame()

monthly <- monthly %>% mutate_if(is.numeric ,~ifelse(is.nan(.), NA, .)) # converts NaN to NA
monthly <- monthly %>% mutate_if(is.numeric, ~ifelse(is.infinite(.), NA, .)) # converts Inf to NA

#monthly_water <- monthly[c("Year", "Month", "NPEV8_mean_mon_WL", "ENPHC_mean_mon_WL", "TS1_mean_mon_WL", "TS7_mean_mon_WL")]
ggplot(monthly) + geom_line(aes(Year, TS7_mean_mon_WL))
ggplot(long_term_data) + geom_line(aes(Year, TS7_daily_wl_NAVD88))

# make monthly df have the same starting point in time across stations
which(!is.na(monthly$ENPHC_mean_mon_WL)) [1] #20
which(!is.na(monthly$TS1_mean_mon_WL)) [1] #176
which(!is.na(monthly$TS7_mean_mon_WL)) [1] #138

which(!is.na(monthly$ENPHC_mean_mon_Sal))[1] #1
which(!is.na(monthly$TS1_mean_mon_Sal)) [1] #136
which(!is.na(monthly$TS7_mean_mon_Sal))[1]  #53

monthly <- subset(monthly, Year >= 1993) # all long-term data has same start time 

# find maxgaps in the tower/LTER site time series
for( i in seq(3,9,1)) {
  print(i)
  monthly[, i]
  
  #calculate "maxgap" to fill the NAs
  is.na.rle <- rle(is.na(monthly[, i]))
  is.na.rle.t<-is.na.rle$lengths[is.na.rle$values==TRUE] 
  gap<-max(is.na.rle.t)
  print(gap)
}

# monthly water level and salinity
write.csv(monthly, "data_processed/AR_long_term_monthly.csv", row.names = F)
