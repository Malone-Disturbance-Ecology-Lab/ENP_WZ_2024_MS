rm(list = ls())

# Evaluate long-term trends in WL and salinity ###
library(Rbeast)
library(zoo)
library(stringr)
library(xts)
library(ggplot2)
library(tidyverse)
library(patchwork)

setwd('/Users/sm3466/Dropbox (YSE)/Research/ENP_WZ_2024_MS/')

df <- read.csv('data/AR_long_term_monthly.csv')
df$month <- stringr::str_pad(df$Month, 2, pad = "0")
df$YM <- paste( df$Year, df$month, sep="-")

df$yearmon <- as.yearmon(df$YM )

summary(df)

# long term inland
npev8 <- df %>%
  select(yearmon, NPEV8_mean_mon_WL)
npev8 <-npev8[7:348,] 
ts.NPEV8 <- xts(x = npev8$NPEV8_mean_mon_WL, order.by = npev8$yearmon)
NPEV8 <- beast( ts.NPEV8, period=3, start=1993.500, deltat = 1/12)

NPEV8$R2
plot(NPEV8)
plot(NPEV8, vars  = c('slpsgn'), ylab=c('slpSign' ))

NPEV8$data
NPEV8$trend$Y
NPEV8$trend$cp # Change points (Time)
NPEV8$trend$cpPr # Change Point Probabilities
NPEV8$trend$cpAbruptChange
NPEV8$trend$SD

# long term coastal
enphc <- df %>%
  select(yearmon, ENPHC_mean_mon_WL)
enphc <-enphc[7:348,] 
ts.ENPHC <- xts(x = enphc$ENPHC_mean_mon_WL, order.by = enphc$yearmon)
ENPHC <- beast( ts.ENPHC, period=3, start=1993.500, deltat = 1/12)

ENPHC$trend$cp # Change points (Time)
ENPHC$trend$cpPr # Change Point Probabilities

ENPHC$R2
plot(ENPHC)
plot(ENPHC, vars  = c('slpsgn'), ylab=c('slpSign' ), main="ENPHC_wl")


# Add contemporary condition stations: TS1 and TS7
ts1 <- df %>%
select(yearmon, TS1_mean_mon_WL)
ts1 <-ts1[273:348,] #163:348, 2006-07, 2006.5 water level start // 273:348, 2015-09, 2015.667 salinity start
ts.TS1 <- xts(x = ts1$TS1_mean_mon_WL, order.by = ts1$yearmon)
TS1 <- beast( ts.TS1, period=3, start=2015.667, deltat = 1/12) #2006.5
TS1$R2
plot(TS1)
plot(TS1, vars  = c('slpsgn'), ylab=c('slpSign' ), main="TS1_wl")

TS1$time
TS1$trend$cp # Change points (Time)
TS1$trend$cpPr # Change Point Probabilities


ts7 <- df %>%
  select(yearmon, TS7_mean_mon_WL)
ts7 <-ts7[288:348,] #288:348, 2016-12, 2016.917 water level start // 40:348, 1996-04, 1996.250 salintiy start
ts.TS7 <- xts(x = ts7$TS7_mean_mon_WL, order.by = ts7$yearmon)
TS7 <- beast( ts.TS7, period=3, start=2016.917, deltat = 1/12)
TS7$R2
plot(TS7)
plot(TS7, vars  = c('slpsgn'), ylab=c('slpSign' ), main="TS7_wl")

TS7$time
TS7$trend$cp # Change points (Time)
TS7$trend$cpPr # Change Point Probabilities

# Determine the slope of time periods for WL trends ####
# inland long term site
mean(NPEV8$trend$slp)
sd(NPEV8$trend$slp)/ sqrt(length(NPEV8$trend$slp))

NPEV8$trend$slpSD
NPEV8$trend$slpSgnPosPr
NPEV8$trend$slpSgnZeroPr

npev8$time <- NPEV8$time
npev8$NPEV8_slp <- NPEV8$trend$slp
npev8$NPEV8.slpSgnPosPr <- NPEV8$trend$slpSgnPosPr

NPEV8$trend$cp[1]
NPEV8$trend$cp[2]

# Measure the slopes mean and se:
#mean(npev8$NPEV8_slp[ npev8$time <= NPEV8$trend$cp[2]]); sd(npev8$NPEV8_slp[ npev8$time <= NPEV8$trend$cp[2]])/sqrt(length(npev8$NPEV8_slp[ npev8$time <= NPEV8$trend$cp[2]]))

mean(npev8$NPEV8.slpSgnPosPr[ npev8$time <= NPEV8$trend$cp[2]])
sd(npev8$NPEV8.slpSgnPosPr[ npev8$time <= NPEV8$trend$cp[2]])/ sqrt(length(npev8$NPEV8.slpSgnPosPr[ npev8$time <= NPEV8$trend$cp[2]]))

mean(npev8$NPEV8_slp[ npev8$time > NPEV8$trend$cp[2] & npev8$time <= NPEV8$trend$cp[1] ]); 
sd(npev8$NPEV8_slp[ npev8$time > NPEV8$trend$cp[2] & npev8$time <= NPEV8$trend$cp[1] ])/sqrt(length(npev8$NPEV8_slp[ npev8$time > NPEV8$trend$cp[2] & npev8$time <= NPEV8$trend$cp[1] ]))

mean(npev8$NPEV8_slp[ npev8$time > NPEV8$trend$cp[1]])
sd(npev8$NPEV8_slp[ npev8$time > NPEV8$trend$cp[1]])/ sqrt(length(npev8$NPEV8_slp[ npev8$time > NPEV8$trend$cp[1]]))

# coastal long term site
mean(ENPHC$trend$slp)
sd(ENPHC$trend$slp)/ sqrt(length(ENPHC$trend$slp))

ENPHC$trend$slpSD
ENPHC$trend$slpSgnPosPr
ENPHC$trend$slpSgnZeroPr

enphc$time <- ENPHC$time
enphc$ENPHC_slp <- ENPHC$trend$slp
enphc$ENPHC.slpSgnPosPr <- ENPHC$trend$slpSgnPosPr

ENPHC$trend$cp
ENPHC$trend$cpPr

mean(enphc$ENPHC_slp[ enphc$time <= ENPHC$trend$cp[1]])
sd(enphc$ENPHC_slp[ enphc$time <= ENPHC$trend$cp[1]])/ sqrt(length(enphc$ENPHC_slp[ enphc$time <= ENPHC$trend$cp[1]]))

mean(enphc$ENPHC_slp[ enphc$time > ENPHC$trend$cp[1]])
sd(enphc$ENPHC_slp[ enphc$time > ENPHC$trend$cp[1]])/ sqrt(length(enphc$ENPHC_slp[ enphc$time > ENPHC$trend$cp[1]]))

# inland LTER site - wont go back into df because don't have same number of rows
mean(TS1$trend$slp)
sd(TS1$trend$slp)/ sqrt(length(TS1$trend$slp))

TS1$trend$slpSD
TS1$trend$slpSgnPosPr
TS1$trend$slpSgnZeroPr

ts1$time <- TS1$time
ts1$TS1_slp <- TS1$trend$slp
ts1$TS1.slpSgnPosPr <- TS1$trend$slpSgnPosPr
TS1$trend$cp

mean(ts1$TS1_slp[ts1$time <= TS1$trend$cp[1]])
sd(ts1$TS1_slp[ts1$time <= TS1$trend$cp[1]])/ sqrt(length(ts1$TS1_slp[ts1$time <= TS1$trend$cp[1]]))

mean(ts1$TS1_slp[ts1$time > TS1$trend$cp[1]])
sd(ts1$TS1_slp[ts1$time > TS1$trend$cp[1]])/ sqrt(length(ts1$TS1_slp[ts1$time > TS1$trend$cp[1]]))

# coastal LTER site 
mean(TS7$trend$slp)
sd(TS7$trend$slp)/ sqrt(length(TS7$trend$slp))

TS7$trend$slpSD
TS7$trend$slpSgnPosPr
TS7$trend$slpSgnZeroPr

ts7$time <- TS7$time
ts7$TS7_slp <- TS7$trend$slp
ts7$TS7.slpSgnPosPr <- TS7$trend$slpSgnPosPr
TS7$trend$cp

mean(ts7$TS7_slp[ts7$time <= TS7$trend$cp[1]])
sd(ts7$TS7_slp[ts7$time <= TS7$trend$cp[1]])/ sqrt(length(ts7$TS7_slp[ts7$time <= TS7$trend$cp[1]]))

mean(ts7$TS7_slp[ts7$time > TS7$trend$cp[1]])
sd(ts7$TS7_slp[ts7$time > TS7$trend$cp[1]])/ sqrt(length(ts7$TS7_slp[ts7$time > TS7$trend$cp[1]]))

# make WL plots for panel ####
A <- ggplot() + geom_point(aes(y=ENPHC$data, x=ENPHC$time), col= "#43b284", alpha=0.2, size=3) +
  geom_line( aes(x=ENPHC$time, y=ENPHC$trend$Y), col= "#43b284", size=2.0) +
  geom_line( aes(x=ENPHC$time, y=ENPHC$data),col= "#43b284", alpha=0.2) + 
  geom_vline(xintercept=ENPHC$trend$cp[1],col= "#43b284", alpha=0.5 , linetype="dashed", size=1) +
  geom_point(aes(y=NPEV8$data, x=NPEV8$time), col="darkblue", alpha=0.2, size=3) +
  geom_line( aes(x=NPEV8$time, y=NPEV8$trend$Y), col="darkblue", size=2.0) +
  geom_line( aes(x=NPEV8$time, y=NPEV8$data),col="darkblue", alpha=0.2) + 
  geom_vline(xintercept=NPEV8$trend$cp[1:2],col="darkblue", alpha=0.5 , linetype="dashed", size=1) +
  annotate(geom="text", x=2013, y=1.5, hjust = 0, label= "Inland", color= "darkblue", size=7) +
  annotate(geom="text", x=2013, y=1.25, hjust = 0, label= "Coastal", color= "#43b284", size=7) + 
  xlim(1993,2022) + ylim(-0.6,1.6) + 
  ylab("Water Level (m)") + xlab("") + theme_light() + theme(text = element_text(size = 25))

B <- ggplot() + geom_point(aes(y=TS7$data, x=TS7$time), col= "#43b284", alpha=0.2, size=3) +
  geom_line( aes(x=TS7$time, y=TS7$trend$Y), col= "#43b284", size=2.0) +
  geom_line( aes(x=TS7$time, y=TS7$data),col= "#43b284", alpha=0.2) + 
  geom_vline(xintercept=TS7$trend$cp[1],col= "#43b284", alpha=0.5 , linetype="dashed", size=1) +
  geom_point(aes(y=TS1$data, x=TS1$time), col="darkblue", alpha=0.2, size=3) +
  geom_line( aes(x=TS1$time, y=TS1$trend$Y), col="darkblue", size=2.0) +
  geom_line( aes(x=TS1$time, y=TS1$data),col="darkblue", alpha=0.2) + 
  geom_vline(xintercept=TS1$trend$cp[1],col="darkblue", alpha=0.5 , linetype="dashed", size=1) +
  annotate(geom="text", x=1993, y=1.5, hjust = 0, label="Marl Prairie", color="darkblue", size=7) +
  annotate(geom="text", x=1993, y=1.25, hjust = 0, label="Scrub Mangrove", color="#43b284", size=7) + 
  xlim(1993,2022) + ylim(-0.6,1.6) + 
  ylab("Water Level (m)") + xlab("") + theme_light() + theme(text = element_text(size = 25))
  

# Salinity Analysis ####
enphc.sal <- df %>%
  select(yearmon, ENPHC_mean_mon_Sal)
enphc.sal <-enphc.sal[7:348,] 
ts.ENPHC.sal <- xts(x = enphc.sal$ENPHC_mean_mon_Sal, order.by = enphc.sal$yearmon)
ENPHC.sal <- beast( ts.ENPHC.sal, period=3, start=1993.500, deltat = 1/12)

plot(ENPHC.sal)

ENPHC.sal$R2
ENPHC.sal$trend$cp # Change points (Time)
ENPHC.sal$trend$cpPr # Change Point Probabilities

# Add TS1 and TS7
ts1.sal <- df %>%
  select(yearmon, TS1_mean_mon_Sal)
ts1.sal <-ts1.sal[273:348,] #163:348, 2006-07, 2006.5 water level start // 273:348, 2015-09, 2015.667 salinity start
ts.TS1.sal <- xts(x = ts1.sal$TS1_mean_mon_Sal, order.by = ts1.sal$yearmon)
TS1.sal <- beast( ts.TS1.sal, period=3, start=2015.667, deltat = 1/12)
plot(TS1.sal)

TS1.sal$R2
TS1.sal$trend$cp # Change points (Time)
TS1.sal$trend$cpPr # Change Point Probabilities

ts7.sal <- df %>%
  select(yearmon, TS7_mean_mon_Sal)
ts7.sal <-ts7.sal[288:348,] #288:348, 2016-12, 2016.917 water level start // 40:348, 1996-04, 1996.250 salintiy start
ts.TS7.sal <- xts(x = ts7.sal$TS7_mean_mon_Sal, order.by = ts7.sal$yearmon)
TS7.sal <- beast(ts.TS7.sal, period=3, start=2016.917, deltat = 1/12)

plot(TS7.sal)
TS7.sal$R2
TS7.sal$trend$cp 
TS7.sal$trend$cpPr

# Determine the slope of time periods for SAL trends ####
# long-term coastal
mean(ENPHC.sal$trend$slp)
sd(ENPHC.sal$trend$slp)/ sqrt(length(ENPHC.sal$trend$slp))

ENPHC.sal$trend$cp
ENPHC.sal$trend$slpSD
ENPHC.sal$trend$slpSgnPosPr
ENPHC.sal$trend$slpSgnZeroPr

enphc$ENPHC.sal_slp <- ENPHC.sal$trend$slp
enphc$ENPHC.slpSgnPosPr <- ENPHC.sal$trend$slpSgnPosPr

mean(enphc$ENPHC.sal_slp[ enphc$time <= ENPHC.sal$trend$cp[1]])
sd(enphc$ENPHC.sal_slp[ enphc$time <= ENPHC.sal$trend$cp[1]])/ sqrt(length(enphc$ENPHC.sal_slp[ enphc$time <= ENPHC.sal$trend$cp[1]]))

mean(enphc$ENPHC.sal_slp[ enphc$time > ENPHC.sal$trend$cp[1] & enphc$time <= ENPHC.sal$trend$cp[2]])
sd(enphc$ENPHC.sal_slp[ enphc$time > ENPHC.sal$trend$cp[1] & enphc$time <= ENPHC.sal$trend$cp[2]])/ sqrt(length(enphc$ENPHC.sal_slp[ enphc$time > ENPHC.sal$trend$cp[1] & enphc$time <= ENPHC.sal$trend$cp[2] ]))

mean(enphc$ENPHC.sal_slp[ enphc$time > ENPHC.sal$trend$cp[2]])
sd(enphc$ENPHC.sal_slp[ enphc$time > ENPHC.sal$trend$cp[2]])/ sqrt(length(enphc$ENPHC.sal_slp[ enphc$time > ENPHC.sal$trend$cp[2]]))

# inland LTER
mean(TS1.sal$trend$slp)
sd(TS1.sal$trend$slp)/ sqrt(length(TS1.sal$trend$slp))

TS1.sal$trend$slpSD
TS1.sal$trend$slpSgnPosPr
TS1.sal$trend$slpSgnZeroPr

ts1$TS1.sal_slp <- TS1.sal$trend$slp
ts1$TS1.sal.slpSgnPosPr <- TS1.sal$trend$slpSgnPosPr
TS1.sal$trend$cp

mean(ts1$TS1.sal_slp[ts1$time <= TS1.sal$trend$cp[1]])
sd(ts1$TS1.sal_slp[ts1$time <= TS1.sal$trend$cp[1]])/ sqrt(length(ts1$TS1.sal_slp[ts1$time <= TS1.sal$trend$cp[1]]))

mean(ts1$TS1.sal_slp[ts1$time > TS1.sal$trend$cp[1]])
sd(ts1$TS1.sal_slp[ts1$time > TS1.sal$trend$cp[1]])/ sqrt(length(ts1$TS1.sal_slp[ts1$time > TS1.sal$trend$cp[1]]))

# coastal LTER site 
mean(TS7.sal$trend$slp)
sd(TS7.sal$trend$slp)/ sqrt(length(TS7.sal$trend$slp))

TS7.sal$trend$slpSD
TS7.sal$trend$slpSgnPosPr
TS7.sal$trend$slpSgnZeroPr

ts7$TS7.sal_slp <- TS7.sal$trend$slp
ts7$TS7.sal.slpSgnPosPr <- TS7.sal$trend$slpSgnPosPr

mean(ts7$TS7.sal_slp[ts7$time <= TS7.sal$trend$cp[1]])
sd(ts7$TS7.sal_slp[ts7$time <= TS7.sal$trend$cp[1]])/ sqrt(length(ts7$TS7.sal_slp[ts7$time <= TS7.sal$trend$cp[1]]))

mean(ts7$TS7.sal_slp[ts7$time > TS7.sal$trend$cp[1]])
sd(ts7$TS7.sal_slp[ts7$time > TS7.sal$trend$cp[1]])/ sqrt(length(ts7$TS7.sal_slp[ts7$time > TS7.sal$trend$cp[1]]))


# make SAL plots for panel ####
C <- ggplot() + geom_point(aes(y=ENPHC.sal$data, x=ENPHC.sal$time), col="#43b284", alpha=0.2, size=3) +
  geom_line( aes(x=ENPHC.sal$time, y=ENPHC.sal$trend$Y), col="#43b284", size=2.0) +
  geom_line( aes(x=ENPHC.sal$time, y=ENPHC.sal$data),col="#43b284", alpha=0.2) + 
  geom_vline(xintercept=ENPHC$trend$cp[1:2],col="#43b284", alpha=0.5 , linetype="dashed", size=1) +
  ylim(0,50) + ylab("Salinity (PSU)") + xlab("") + theme_light() + theme(text = element_text(size = 25))

D <- ggplot() + geom_point(aes(y=TS7.sal$data, x=TS7.sal$time), col= "#43b284", alpha=0.2, size=3) +
  geom_line( aes(x=TS7.sal$time, y=TS7.sal$trend$Y), col= "#43b284", size=2.0) +
  geom_line( aes(x=TS7.sal$time, y=TS7.sal$data),col= "#43b284", alpha=0.2) + 
  geom_vline(xintercept=TS7.sal$trend$cp[1],col= "#43b284", alpha=0.5 , linetype="dashed", size=1) +
  geom_point(aes(y=TS1.sal$data, x=TS1.sal$time), col="darkblue", alpha=0.2, size=3) +
  geom_line( aes(x=TS1.sal$time, y=TS1.sal$trend$Y), col="darkblue", size=2.0) +
  geom_line( aes(x=TS1.sal$time, y=TS1.sal$data),col="darkblue", alpha=0.2) + 
  geom_vline(xintercept=TS1.sal$trend$cp[1],col="darkblue", alpha=0.5 , linetype="dashed", size=1) +
  xlim(1993,2022) + 
  ylim(0,50) + ylab("Salinity (PSU)") + xlab("") + theme_light() + theme(text = element_text(size = 25))


# patchwork panelling for final figure ####
setwd("figures")
png(filename = "Fig3_Trends.png", 
    width = 8.75, height = 8, units = "in", res = 600, bg = "transparent") 

A+ B+ C+ D + patchwork::plot_layout(nrow = 2, ncol = 2) & patchwork::plot_annotation(tag_levels = "A") &
theme(plot.tag = element_text(size = 19), axis.text=element_text(size=19), axis.title=element_text(size=19), 
      plot.margin = margin(5, 10, 5, 10, unit = "pt"))
dev.off()


#EOF