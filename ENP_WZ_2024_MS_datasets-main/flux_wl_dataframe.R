# Flux data retrieval and dataframe creation

# this script is for pulling flux, met, and water level data together for all sites in 2021
# last updated: AR, 11/5/2023

# Clear environment
rm(list = ls())

# load packages
library(tidyverse)   # everything 

# make se1 csv with fluxes, met and water level information ####

# water level
load("data_raw/SE1_WaterLevel.RDATA") # se.wl.df
lapply(se.wl.df, class)

# met data
load("data_raw/se1_Data_AllData.RDATA") # se1

# prepare se1 data to be one dataframe 
se1$TIMESTAMP <- as.POSIXct(se1$TIMESTAMP,
                            format = "%Y-%m-%d %H:%M:%S",
                            tz = "EST")

df <- se1 %>% 
  subset(date>= "2021-01-01" & 
           date< "2022-01-01") %>%
  as.data.frame()

se.wl.df$TIMESTAMP <- as.POSIXct(se.wl.df$TIMESTAMP,
                            format = "%Y-%m-%d %H:%M:%S",
                            tz = "EST")

#wl_df <- subset (se.wl.df, TIMESTAMP>= "2021-01-01 00:00:00" & 
#                   TIMESTAMP< "2022-01-01 00:00:00") 

which(se.wl.df$TIMESTAMP== "2022-01-01 00:00:00")

wl_df <- as.data.frame(se.wl.df[c(17589:35108),])

which(duplicated(wl_df$TIMESTAMP))
wl_df <- wl_df[!duplicated(wl_df$TIMESTAMP), ]

# only pull out variables we need
df2 <- df %>% dplyr::select("TIMESTAMP",  "NEE.filtered",  "TA.f", "PAR.f")

# df2 has 22710, wl_df has 17520 observations --> drop duplicate timestamps in df2
which(duplicated(df2$TIMESTAMP))
df2 <- df2[!duplicated(df2$TIMESTAMP), ]

# merge water level into df2 
df2 <- left_join(df2, wl_df, by = "TIMESTAMP")
which(duplicated(df2$TIMESTAMP))

lapply(df2, class)
df2$TIMESTAMP<- format(df2$TIMESTAMP, "%Y-%m-%d %H:%M:%S")

# save as csv like other station datasets
write.csv(df2, "data_raw/AR_SE1_2021.csv", row.names = F)

rm(list = ls())

# load flux and met data from the server, TSPH1, TSPH7, SE1 ###########################

TSPH1 <- read.csv("data_raw/AR_TSPH1_2021.csv")
SE1 <- read.csv("data_raw/AR_SE1_2021.csv")
TSPH7 <- read.csv("data_raw/AR_TSPH7_2021.csv")

# drop the additional X columns in all of the files
TSPH1 <- TSPH1[-c(1)]
TSPH7 <- TSPH7[-c(1)]

# rename columns so there are no duplicate names for variables when merging the files
names(TSPH1) <- c("TIMESTAMP", "TS1.NEE.filtered", "TS1.TA.f", "TS1.PAR.f", "TS1.wl") 
names(SE1) <- c("TIMESTAMP", "SE1.NEE.filtered", "SE1.TA.f", "SE1.PAR.f", "SE1.wl") 
names(TSPH7) <- c("TIMESTAMP", "TS7.NEE.filtered", "TS7.TA.f", "TS7.PAR.f") 

# convert TIMESTAMP to POSIXct
lapply(TSPH1, class) # SE1, TSPH7 # all TIMESTAMPS are character

TSPH1$TIMESTAMP <- as.POSIXct(TSPH1$TIMESTAMP,
                                    format = "%Y-%m-%d %H:%M:%S",
                                    tz = "EST") 

SE1$TIMESTAMP <- as.POSIXct(SE1$TIMESTAMP,
                              format = "%Y-%m-%d %H:%M:%S",
                              tz = "EST") 

TSPH7$TIMESTAMP <- as.POSIXct(TSPH7$TIMESTAMP,
                            format = "%Y-%m-%d %H:%M:%S",
                            tz = "EST")

# remove duplicate timestamps in the dataframes #############################################

# TSPH1 - average duplicate water level values together
which(duplicated(TSPH1$TIMESTAMP)) # duplicated entries because 2 water level values associated with every time point until 1198

TS1.wl.avg <- TSPH1 %>%
  group_by(TIMESTAMP) %>%
  summarize(mean(TS1.wl)) 

 TSPH1 <- left_join(TSPH1, TS1.wl.avg, by = "TIMESTAMP") # add averaged water levels back into TSPH1 dataframe

 # drop duplicates based on TIMESTAMP
 TSPH1 <- TSPH1[!duplicated(TSPH1$TIMESTAMP), ]
 
 # drop the original duplicated water level column
 TSPH1 <- TSPH1[-c(5)]
 
 # rename the averaged water level column to TS1.wl
 names(TSPH1) <- c("TIMESTAMP", "TS1.NEE.filtered", "TS1.TA.f", "TS1.PAR.f", "TS1.wl") 
 
 rm(TS1.wl.avg)

# SE1 - add missing datetime back in and remove duplicates for other duplicated values
which(duplicated(SE1$TIMESTAMP))

# TSPH7 - average duplicated values
which(duplicated(TSPH7$TIMESTAMP)) # entries 14880 - 14883 are duplicated with different values for NEE and TA.f

TS7.NEE.avg <- TSPH7 %>%
  group_by(TIMESTAMP) %>%
  summarize(mean(TS7.NEE.filtered)) 

TS7.TA.avg <- TSPH7 %>%
  group_by(TIMESTAMP) %>%
  summarize(mean(TS7.TA.f)) 

TSPH7 <- left_join(left_join(TSPH7, TS7.NEE.avg, by = "TIMESTAMP"), TS7.TA.avg, by = "TIMESTAMP") # add averaged water levels back into TSPH7 dataframe

# drop duplicates based on TIMESTAMP
TSPH7 <- TSPH7[!duplicated(TSPH7$TIMESTAMP), ]

# drop the original duplicated columns
TSPH7 <- TSPH7[-c(2,3)]

# reorder columns to match others
TSPH7 <- TSPH7[,c(1,3,4,2)]

# rename the averaged water level column to TS1.wl
names(TSPH7) <- c("TIMESTAMP", "TS7.NEE.filtered", "TS7.TA.f", "TS7.PAR.f") 

rm(TS7.NEE.avg, TS7.TA.avg)


# pull TS7 water level data from the FCE public database #####
#https://fce-lter.fiu.edu/data/core/metadata/?datasetid=PHY_Castaneda_001 
# NOTE: water level is recorded in centimeters

Mangrove_wl <- read.csv("data_raw/PHY_Castaneda_001.csv")

TSPH7_wl <- Mangrove_wl %>%
  filter(SITENAME == "TS/Ph7") %>% # only TS7
  filter(Date >= "2021") %>% # only 2021
  mutate("TS7_hourly_wl" = WaterLevel*0.01) %>% # convert the WaterLevel units from centimeters to meters
  as.data.frame()

# convert the variables to the proper class
TSPH7_wl$Date <- as.Date(TSPH7_wl$Date, format = "%Y-%m-%d")

# two ways to merge the water level data into the big data freme: via TIMESTAMP2 or via Date and Hour
# make a TIMESTAMP2 variable (so that don't have duplicate named variables when merging the dataframes)
TSPH7_wl$TIMESTAMP2 <- as.POSIXct(paste(TSPH7_wl$Date, TSPH7_wl$Time), 
                                  format = "%Y-%m-%d %H:%M:%S",
                                  tz = "EST")

TSPH7_wl$Hour <- lubridate::hour(TSPH7_wl$TIMESTAMP2)
TSPH7_wl$Hour <- as.numeric(TSPH7_wl$Hour)
lapply(TSPH7_wl, class)

# plot time series of the hourly water level
#ggplot(TSPH7_wl) + geom_line(mapping=aes(x=TIMESTAMP2, y=TS7_hourly_wl))+ geom_hline(yintercept = 0)

# only select the variables we need
TSPH7_wl <- TSPH7_wl[,c(7, 2, 8, 6)]


# put everything in the same dataframe ######################################################

# merge files together into one dataframe based on "TIMESTAMP" ####################
WZ_df <- left_join(left_join(TSPH1, SE1, by = "TIMESTAMP"), TSPH7, by = "TIMESTAMP")

which(duplicated(WZ_df$TIMESTAMP))

# make a date column from TIMESTAMP 
WZ_df$Date <- as.Date(as.character(WZ_df$TIMESTAMP), format = "%Y-%m-%d")

# make an Hour column from TIMESTAMP 
WZ_df$Hour <- lubridate::hour(WZ_df$TIMESTAMP)

# merge TS7 water level by c(date, hour)
WZ_df <- left_join(WZ_df, TSPH7_wl, by = c("Date", "Hour"))  # this way duplicates , but gives the proper values

# summarize hourly water level for TS1 and SE1 and add to the WZ_df dataframe
hourly_wl <- WZ_df %>%
  group_by(Date, Hour) %>%
  summarize(mean(TS1.wl, na.rm = T),
            mean(SE1.wl, na.rm =T)) 

names(hourly_wl) <- c("Date", "Hour", "TS1_hourly_wl", "SE1_hourly_wl") 

# summarize daily water level for all sites and add to the WZ_df dataframe
mean_daily_wl <- WZ_df %>%
  group_by(Date) %>%
  summarize(mean(TS1.wl, na.rm = T),
            mean(SE1.wl, na.rm = T),
            mean(TS7_hourly_wl, na.rm = T))

names(mean_daily_wl) <- c("Date", "TS1_daily_wl", "SE1_daily_wl", "TS7_daily_wl" )


WZ_df <- left_join(left_join(WZ_df, hourly_wl, by = c("Date", "Hour")), mean_daily_wl, by = "Date")  # add hourly and daily averaged water levels back into WZ_df 

WZ_df <- WZ_df[-c(15)] # drop the TIMESTAMP2 variable 

WZ_df <- WZ_df %>% mutate_if(is.numeric ,~ifelse(is.nan(.), NA, .)) # converts NaN to NA
WZ_df <- WZ_df %>% mutate_if(is.numeric, ~ifelse(is.infinite(.), NA, .)) # converts Inf to NA

# reorder columns 
names(WZ_df)
WZ_df <- WZ_df[,c(1,13,14,2:4,6:8,10:12,5,9,16,17,15,18:20)]
summary(WZ_df)


# fill any non-filled data in environmental variables (PAR and TA) ####
summary(WZ_df$TS7.PAR.f) # fill 4 NAs in TS7 PAR and TA as the average between TS1 and SE1

TS1_PAR <- WZ_df$TS1.PAR.f[is.na(WZ_df$TS7.PAR.f)]
SE1_PAR <- WZ_df$SE1.PAR.f[is.na(WZ_df$TS7.PAR.f)]
TS7_PAR <- rowMeans(cbind(TS1_PAR,SE1_PAR))
WZ_df$TS7.PAR.f[is.na(WZ_df$TS7.PAR.f)] <- TS7_PAR
summary(WZ_df$TS7.PAR.f) 

TS1_TA <- WZ_df$TS1.TA.f[is.na(WZ_df$TS7.TA.f)]
SE1_TA <- WZ_df$SE1.TA.f[is.na(WZ_df$TS7.TA.f)]
TS7_TA <- rowMeans(cbind(TS1_TA,SE1_TA))
WZ_df$TS7.TA.f[is.na(WZ_df$TS7.TA.f)] <- TS7_TA
summary(WZ_df$TS7.TA.f) 

rm(TS1_PAR, SE1_PAR, TS7_PAR, TS1_TA, SE1_TA,TS7_TA)

# add water level indicator variables based on proportion of canopy cover at each ecosystem ####
# use daily mean water level variables: `TS1 Water Level`, `SE1 Water Level`, `TS7 Water Level`

# TS1: canopy height = 0.73m (Zhao, 2019; Jimenez, 2012); 1.07m (early 2022 field measures)
#42*2.54 # 42 inches *2.54 = 106.68 cm 
summary(WZ_df$TS1_daily_wl) # min is -0.428 ; max is 0.539, 384 NAs
#0.539/1.07 #0.504, maxes out at 50.4% covered
#1.07*0.25 # 0.2675
#1.07*0.50 # 0.535

# create indicator variable for water level for each site 
WZ_df$TS1WLindicator <- NA
WZ_df$TS1WLindicator[WZ_df$TS1_daily_wl <=0] <- 0  #"dry" # below  soil surface
WZ_df$TS1WLindicator[WZ_df$TS1_daily_wl > 0 & WZ_df$TS1_daily_wl  <= 0.2675] <- 0.25  #"25% coverage" 
WZ_df$TS1WLindicator[WZ_df$TS1_daily_wl > 0.2675 & WZ_df$TS1_daily_wl <= 0.535] <- 0.50 #"50% coverage" 
WZ_df$TS1WLindicator[WZ_df$TS1_daily_wl > 0.535] <- 0.75 #">50% coverage"
class(WZ_df$TS1WLindicator)
WZ_df$TS1WLindicator <- as.factor(WZ_df$TS1WLindicator)
summary(WZ_df$TS1WLindicator)

# SE1: canopy height = 0.84 m (early 2022 field measures)
#33*2.54 # 83.82 cm ;  mean canopy height
#15*2.54 # 38.1 cm ; sawgrass canopy height
#40*2.54 # 101.6 cm ; mangroves canopy height
summary(WZ_df$SE1_daily_wl) # min is 0.373 ; max is 0.822
#0.373/0.84 #0.444, starts at 45% covered
#0.822/0.84 #0.979, maxes out at 98% covered

#0.84*0.25 #0.21
#0.84*0.50 #0.42
#0.84*0.75 #0.63

WZ_df$SE1WLindicator <- NA
WZ_df$SE1WLindicator[WZ_df$SE1_daily_wl > 0.21 & WZ_df$SE1_daily_wl <= 0.42] <- 0.50 #"50% coverage" 
WZ_df$SE1WLindicator[WZ_df$SE1_daily_wl > 0.42 & WZ_df$SE1_daily_wl <= 0.63] <- 0.75 # "75% coverage" 
WZ_df$SE1WLindicator[WZ_df$SE1_daily_wl > 0.63] <- 1 # "75% - 98% coverage" 
WZ_df$SE1WLindicator <- as.factor(WZ_df$SE1WLindicator)
summary(WZ_df$SE1WLindicator)

# TS7: canopy height = 1.5m (Hogan, 2021; Ewe, 2006)
summary(WZ_df$TS7_daily_wl) # min is -0.06, max is 0.403 m
#0.403/1.5 # 0.268 maxes out at 27% covered

#1.5*0.25 #0.375
#1.5*0.50 #0.75

WZ_df$TS7WLindicator <- NA
WZ_df$TS7WLindicator[WZ_df$TS7_daily_wl <=0] <- 0 #"dry" 
WZ_df$TS7WLindicator[WZ_df$TS7_daily_wl > 0 & WZ_df$TS7_daily_wl  <= 0.375] <- 0.25 # "0 - 25% coverage"
WZ_df$TS7WLindicator[WZ_df$TS7_daily_wl > 0.375 & WZ_df$TS7_daily_wl  <= 0.75] <- 0.50 # 25% - 27% coverage"
WZ_df$TS7WLindicator <- as.factor(WZ_df$TS7WLindicator)
summary(WZ_df$TS7WLindicator)


# save as csv for later use ########################################################################
write.csv(WZ_df, "data_processed/AR_flux_sites_2021.csv", row.names = FALSE)

# EOF