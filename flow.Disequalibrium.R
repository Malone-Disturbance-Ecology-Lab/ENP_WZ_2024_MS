# Disequalibrium:
# This script is used to calculate the numbers used for the simulation
rm(list=ls())

library(tidyverse)
Parms <- read.csv('data/WZ_NEE_Budget_Key_CIs.csv' )

df <- read.csv( 'data/AR_flux_sites_2021.csv')
summary(df$TS1_daily_wl)

names( df)
df$TS_WL <- zoo::na.approx(df$TS1_daily_wl, rule = 2)
plot(zoo::na.approx(df$TS1_daily_wl, rule = 2))

unique(df$TS1WLindicator )
df$TS_WL[ is.na(df$TS1WLindicator) == T]
df$TS1WLindicator[ is.na(df$TS1WLindicator) == T] <- 0.50

df$SE1.TA.f [is.na(df$SE1.TA.f ) == T] <- df$TS1.TA.f [is.na(df$SE1.TA.f ) == T]
df$SE1.PAR.f[is.na(df$SE1.PAR.f ) == T] <- df$TS1.PAR.f[is.na(df$SE1.PAR.f ) == T]

# How to deal with gaps in the Submergence:

# combining submergence parms with site NEE
Parms.TS1 <- Parms %>% filter(Site == 'TS1WLindicator')
Parms.SE1 <- Parms %>% filter(Site == 'SE1WLindicator')
Parms.TS7 <- Parms %>% filter(Site == 'TS7WLindicator')

ts1 <- df %>% dplyr::select("TS1.NEE.filtered",  "TS1.TA.f", "TS1.PAR.f", "TS1WLindicator", 'TS1_daily_wl') %>% rename(NEE = TS1.NEE.filtered,
                                                                                                       TA.f = TS1.TA.f,
                                                                                                       PAR.f = TS1.PAR.f,
                                                                                                       Submergence = TS1WLindicator ) %>% left_join( Parms.TS1, by='Submergence')

se1 <- df %>% dplyr::select("SE1.NEE.filtered",  "SE1.TA.f", "SE1.PAR.f", "SE1WLindicator", 'SE1_daily_wl') %>% rename(NEE = SE1.NEE.filtered,
                                                                                                       TA.f = SE1.TA.f,
                                                                                                       PAR.f = SE1.PAR.f,
                                                                                                       Submergence =SE1WLindicator ) %>% left_join( Parms.SE1, by='Submergence')

ts7 <- df %>% dplyr::select("TS7.NEE.filtered",  "TS7.TA.f", "TS7.PAR.f", "TS7WLindicator", "TS7_daily_wl") %>% rename(NEE = TS7.NEE.filtered,
                                                                                                       TA.f = TS7.TA.f,
                                                                                                       PAR.f = TS7.PAR.f,
                                                                                                       Submergence =TS7WLindicator ) %>% left_join( Parms.TS7, by='Submergence')

#DE
ts1$DE.Pmax <- ( min(ts1$Pmax,na.rm=T)- ts1$Pmax ) / min(ts1$Pmax,na.rm=T)
ts7$DE.Pmax <- ( min(ts7$Pmax,na.rm=T)- ts7$Pmax ) / min(ts7$Pmax,na.rm=T)
se1$DE.Pmax <- ( min(se1$Pmax,na.rm=T)- se1$Pmax ) / min(se1$Pmax,na.rm=T)

names(df)

ggplot( ) + geom_line( data= ts1, aes( x=TS1_daily_wl, y= DE.Pmax), col="darkblue") +
  geom_line( data= se1, aes( x=SE1_daily_wl, y= DE.Pmax), col="#fab255") +
  geom_line( data= ts7, aes( x=TS7_daily_wl, y= DE.Pmax),col= "#43b284")


plot(  ts1$TS_WL,ts1$DE.Pmax, typ="l")
plot(  se1$SE1.wl, se1$DE.Pmax, typ="l")
plot( ts7$DE.Pmax, typ="l")
plot(se1$SE1.wl)

length(ts1$DE.Pmax[ ts1$DE.Pmax > 0] )/ length(!is.na(ts1$DE.Pmax)) *100
length(se1$DE.Pmax[ se1$DE.Pmax > 0] )/ length(!is.na(se1$DE.Pmax)) *100
length(ts7$DE.Pmax[ ts7$DE.Pmax > 0] )/ length(!is.na(ts7$DE.Pmax)) *100
