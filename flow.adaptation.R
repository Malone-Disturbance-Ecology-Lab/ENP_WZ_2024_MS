# flow.adaptation:

rm(list = ls())

# Evaluate long-term trends in WL and salinity ###
library(ggplot2)
library(tidyverse)
library(zoo)

submergence <- function(df, MCH, index){
  df$WL[df$WL == -99.99]<- NA
  df$WL <- zoo::na.approx(df$WL, x = index(index), na.rm = F)
  df$Submergence <- (df$WL /MCH)*100
  df$WL.CAT <- NA
  df$WL.CAT[df$Submergence < 1] <- 1
  df$WL.CAT[df$Submergence > 1 & df$Submergence < 25] <- 2
  df$WL.CAT[df$Submergence > 25 & df$Submergence < 50] <- 3
  df$WL.CAT[df$Submergence > 50 & df$Submergence < 75] <- 4
  df$WL.CAT[df$Submergence > 75 ] <- 5
  
  summary(df$Submergence[df$WL ==0.0917] )
  df$WL.CAT <- df$WL.CAT %>% as.factor()
  summary(df$WL.CAT)
  df$count <- 1
  return(df)
} 

#__________________________________________________________________________________
# TS1:
df.ts1 <- read.csv('/Users/sm3466/Dropbox (YSE)/Research/ENP_WZ_2024_MS/data/PHY_Rubio_002.csv')


df.ts1$WL <- df.ts1$WaterDepth/100
df.ts1$WL[df.ts1$WL == -99.99]<- NA
df.ts1.m <- df.ts1 %>% group_by(Date) %>% summarise(WL = median(WL, na.rm=T))
df.ts1.m$Date <- as.Date(df.ts1.m$Date); summary(df.ts1.m$Date)
df.ts1.m$Year <- format( df.ts1.m$Date, "%Y")
sub.ts1 <- df.ts1.m[ which(df.ts1.m$Date <  "2021-01-01"),]
sub.duration.ts1 <- submergence(df=sub.ts1, MCH=1.07, index=sub.ts1$Date)
duration.ts1.ann <- sub.duration.ts1 %>% group_by(WL.CAT, Year) %>% summarise(WL=mean(WL, na.rm=T),
                                                                              duration = sum(count)/365*100) %>% mutate(count = 1)
duration.summary.ts1 <- duration.ts1.ann %>% group_by(WL.CAT)%>% summarise(count = sum(count),
                                                                       duration.mean = mean(duration),
                                                                       duration.SE = sd(duration)/sqrt(count),
                                                                       WL.mean = mean(WL),
                                                                       WL.SE = sd(WL)/sqrt(count))
# TS7
df.ts7 <- read.csv('/Users/sm3466/Dropbox (YSE)/Research/ENP_WZ_2024_MS/data/PHY_Castaneda_001.csv')
names(df.ts7)
df.ts7$WL <- df.ts7$WaterLevel/100
df.ts7$WL[df.ts7$WL == -99.99]<- NA
df.ts7.m <- df.ts7 %>% group_by(Date) %>% summarise(WL = median(WL, na.rm=T))
df.ts7.m$Date <- as.Date(df.ts7.m$Date)
df.ts7.m$Year <- format( df.ts7.m$Date, "%Y")
sub.ts7 <- df.ts7.m[ which(df.ts7.m$Date <  "2021-01-01"),]
sub.duration.ts7 <- submergence(df=sub.ts7, MCH=1.07, index=sub.ts7$Date)
duration.ts7.ann <- sub.duration.ts7 %>% group_by(WL.CAT, Year) %>% summarise(WL=mean(WL, na.rm=T),
                                                                              duration = sum(count)/365*100) %>% mutate(count = 1)
duration.summary.ts7 <- duration.ts7.ann %>% group_by(WL.CAT)%>% summarise(count = sum(count),
                                                                           duration.mean = mean(duration),
                                                                           duration.SE = sd(duration)/sqrt(count),
                                                                           WL.mean = mean(WL),
                                                                           WL.SE = sd(WL)/sqrt(count))
