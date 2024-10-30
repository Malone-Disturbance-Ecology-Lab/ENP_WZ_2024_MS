rm(list=ls())

library(tidyverse)

df <- read.csv( "data/data_raw/AR_wl_sal_2021_weekly.csv")
summary(df)

source("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/ENP_WZ_2024_MS/function.indicator.R")

# Deal with Missing Values:

df$TS1WLindicator.2 <- 0.5
df$TS1WLindicator.2[ df$TS1WLindicator == 0] <- 0
df$TS1WLindicator.2[ df$TS1WLindicator == 0.25] <- 0.25
df$TS1WLindicator <- df$TS1WLindicator.2
plot(df$TIMESTAMP, df$TS1WLindicator.2 )

df$SE1WLindicator[is.na(df$SE1WLindicator)]<-0.5
plot(df$SE1WLindicator, typ="l")

df$SE1.TA.f [is.na(df$SE1.TA.f ) == T] <- df$TS1.TA.f[is.na(df$SE1.TA.f ) == T]
df$SE1.PAR.f[is.na(df$SE1.PAR.f ) == T] <- df$TS1.PAR.f[is.na(df$SE1.PAR.f ) == T]

# Correct for the missing WL category at TS7:
unique(df$TS7WLindicator)

df$TS7WLindicator.2 <- 0.25
df$TS7WLindicator.2[ df$TS7WLindicator == 0.5] <- 0.25
df$TS7WLindicator.2[ df$TS7WLindicator == 0] <- 0

