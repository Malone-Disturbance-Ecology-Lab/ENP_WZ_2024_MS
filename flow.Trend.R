rm(list = ls())

# Evaluate long-term trends in WL and salinity across 4 stations###

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


# long term inland
npev8 <- df %>% select(yearmon, NPEV8_mean_mon_WL)
npev8 <- npev8[7:348,] 

ts.NPEV8 <- xts(x = npev8$NPEV8_mean_mon_WL, order.by = npev8$yearmon)
NPEV8 <- beast( ts.NPEV8, period=3, start=1993.500, deltat = 1/12)

NPEV8$R2
plot(NPEV8)
plot(NPEV8, vars  = c('slpsgn'), ylab=c('slpSign' ))

NPEV8$data
NPEV8$trend$Y %>% plot()
NPEV8$trend$slp %>% plot()
NPEV8$trend$slp %>% median
NPEV8$trend$cp # Change points (Time)
NPEV8$trend$cpPr # Change Point Probabilities
NPEV8$trend$cpAbruptChange
NPEV8$trend$SD

NPEV8 %>% plot( vars  = c('slpsgn'), ylab=c('slpSign' ), main="ENPHC_wl")

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

# Summary Tables: ####
# Determine the slope of time periods for WL trends

trend.summary.periods_3 <- function( Object, cp1, cp2, df ){
  # THis function summairizes the data from each beast analysis by the first two change points
  
  df$time <- Object$time
  
  start.1 <- min( df$yearmon)  
  end.1 <- df$yearmon[ round(as.numeric(df$time),2) == round(as.numeric(Object$trend$cp)[cp1],2)]
  start.2 <-  end.1   
  end.2 <- df$yearmon[ round(as.numeric(df$time),2) == round(as.numeric(Object$trend$cp)[cp2],2)]
  start.3 <- end.2
  end.3 <- max( df$yearmon)
  
  
  period.1.time <- paste(start.1, end.1, sep="-")
  period.2.time <- paste(start.2, end.2, sep="-")
  period.3.time <- paste(start.3, end.3, sep="-")
  
  period.1 <-  Object$time <= Object$trend$cp[cp1]
  period.2 <-  Object$time >= Object$trend$cp[cp1] & Object$time < Object$trend$cp[cp2]
  period.3 <-  Object$time >= Object$trend$cp[cp2]
  
  trend.1 <- mean(Object$trend$Y[  period.1]) %>% round(2)
  trend.2 <- mean(Object$trend$Y[ period.2 ]) %>% round(2)
  trend.3 <- mean(Object$trend$Y[period.3 ]) %>% round(2)
  
  trendSE.1 <- (sd(Object$trend$Y[  period.1])/ sqrt(length(Object$trend$Y[ period.1]))) %>% round(4)
  trendSE.2 <- (sd(Object$trend$Y[  period.2])/ sqrt(length(Object$trend$Y[ period.2]))) %>% round(4)
  trendSE.3 <- (sd(Object$trend$Y[  period.3])/ sqrt(length(Object$trend$Y[ period.3]))) %>% round(4)
  
  trend.slope.1 <- mean(Object$trend$slp[ period.1]) %>% round(3)
  trend.slope.2 <- mean(Object$trend$slp[ period.2])%>% round(3)
  trend.slope.3 <- mean(Object$trend$slp[ period.3])%>% round(3)
  
  trend.slopeSE.1 <- sd(Object$trend$slp[ period.1])/ sqrt(length(Object$trend$slp[ period.1])) %>% round(4)
  trend.slopeSE.2 <- sd(Object$trend$slp[ period.2])/ sqrt(length(Object$trend$slp[ period.2]))%>% round(4)
  trend.slopeSE.3 <- sd(Object$trend$slp[ period.3])/ sqrt(length(Object$trend$slp[ period.3]))%>% round(4)
  
  # Make the table to export
  
  table.df <- data.frame( Period=c(period.1.time, period.2.time, period.3.time),
                          Trend = c(trend.1, trend.2, trend.3 ),
                          Trend.SE = c(trendSE.1, trendSE.2, trendSE.3),
                          Trend_slp = c(trend.slope.1, trend.slope.2, trend.slope.3),
                          Trend_slp.SE = c(trend.slopeSE.1%>% round(4), trend.slopeSE.2%>% round(4), trend.slopeSE.2%>% round(4) ))
  
}
trend.summary.periods_2 <- function( Object, cp1, df ){
  # THis function summairizes the data from each beast analysis by the first two change points
  
  df$time <- Object$time
  
  start.1 <- min( df$yearmon)  
  end.1 <- df$yearmon[ round(as.numeric(df$time),2) == round(as.numeric(Object$trend$cp)[cp1],2)]
  start.2 <-  end.1   
  end.2 <- max( df$yearmon)
  
  period.1.time <- paste(start.1, end.1, sep="-")
  period.2.time <- paste(start.2, end.2, sep="-")
  
  
  period.1 <-  Object$time <= Object$trend$cp[cp1]
  period.2 <-  Object$time >= Object$trend$cp[cp1] 
  
  trend.1 <- mean(Object$trend$Y[  period.1]) %>% round(2)
  trend.2 <- mean(Object$trend$Y[ period.2 ]) %>% round(2)
  
  trendSE.1 <- (sd(Object$trend$Y[  period.1])/ sqrt(length(Object$trend$Y[ period.1]))) %>% round(4)
  trendSE.2 <- (sd(Object$trend$Y[  period.2])/ sqrt(length(Object$trend$Y[ period.2]))) %>% round(4)
  
  trend.slope.1 <- mean(Object$trend$slp[ period.1]) %>% round(3)
  trend.slope.2 <- mean(Object$trend$slp[ period.2]) %>% round(3)
  
  trend.slopeSE.1 <- sd(Object$trend$slp[ period.1])/ sqrt(length(Object$trend$slp[ period.1])) %>% round(4)
  trend.slopeSE.2 <- sd(Object$trend$slp[ period.2])/ sqrt(length(Object$trend$slp[ period.2])) %>% round(4)
  
  # Make the table to export
  
  table.df <- data.frame( Period=c(period.1.time, period.2.time),
                          Trend = c(trend.1, trend.2 ),
                          Trend.SE = c(trendSE.1, trendSE.2),
                          Trend_slp = c(trend.slope.1, trend.slope.2),
                          Trend_slp.SE = c(trend.slopeSE.1 %>% round(4), trend.slopeSE.2 %>% round(4)))
  
}

NPEV8$trend$cp # inland long term site
summary.NPEV8 <- trend.summary.periods_3( Object=NPEV8, cp1=2, cp2=1, df=npev8 ) %>% mutate(site ='NPEV8' )

ENPHC$trend$cp # coastal long term site
summary.ENPHC <- trend.summary.periods_2( Object=ENPHC, cp1=1, df=enphc ) %>% mutate(site ='ENPHC' )

# inland LTER site - wont go back into df because don't have same number of rows
TS1$trend$cp
summary.TS1 <- trend.summary.periods_2( Object=TS1, cp1=1, df=ts1 ) %>% mutate(site ='TS1' )

# coastal LTER site 
TS7$trend$cp
summary.TS7 <- trend.summary.periods_2( Object=TS7, cp1=1, df=ts7 ) %>% mutate(site ='TS7' )

# Add site and merge the tables into one:

Trend.summary <- rbind(summary.NPEV8 ,summary.ENPHC, summary.TS1, summary.TS7) %>% mutate(Type= 'WaterLevel')

# Create a function the summarizes the change point informarion used in the Manuscript:

summary.cp <- function(Object){
  
  cp.df <-  data.frame( cp = Object$trend$cp, cpPr =Object$trend$cpPr ) %>% na.omit %>% filter( cpPr > 0.2)
  
 return( cp.df) 
}

NPEV8.summary.cp <- summary.cp(NPEV8 ) %>% mutate(site ='NPEV8' )
ENPHC.summary.cp <- summary.cp(ENPHC) %>% mutate(site ='ENPHC' )
TS1.summary.cp <- summary.cp(TS1) %>% mutate(site ='TS1' )
TS7.summary.cp <- summary.cp(TS7) %>% mutate(site ='TS7' )

cp.summary <- rbind(NPEV8.summary.cp, ENPHC.summary.cp, TS1.summary.cp,  TS7.summary.cp) %>% mutate(Type= 'WaterLevel')

# Salinity Analysis ####
enphc.sal <- df %>%
  select(yearmon, ENPHC_mean_mon_Sal)
enphc.sal <-enphc.sal[7:348,] 
ts.ENPHC.sal <- xts(x = enphc.sal$ENPHC_mean_mon_Sal, order.by = enphc.sal$yearmon)
ENPHC.sal <- beast( ts.ENPHC.sal, period=3, start=1993.500, deltat = 1/12)

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

# Summaries of the salinity trends in the data ####

trend.summary.periods_2.sal <- function( Object, cp1, df ){
  # THis function summairizes the data from each beast analysis by the first two change points
  
  df$time <- Object$time
  
  start.1 <- min( df$yearmon)  
  end.1 <- df$yearmon[ round(as.numeric(df$time),2) == round(as.numeric(Object$trend$cp)[cp1],2)]
  start.2 <-  end.1   
  end.2 <- max( df$yearmon)
  
  period.1.time <- paste(start.1, end.1, sep="-")
  period.2.time <- paste(start.2, end.2, sep="-")
  
  
  period.1 <-  Object$time <= Object$trend$cp[cp1]
  period.2 <-  Object$time >= Object$trend$cp[cp1] 
  
  trend.1 <- mean(Object$trend$Y[  period.1]) %>% round(4)
  trend.2 <- mean(Object$trend$Y[ period.2 ]) %>% round(4)
  
  trendSE.1 <- (sd(Object$trend$Y[  period.1])/ sqrt(length(Object$trend$Y[ period.1]))) %>% round(6)
  trendSE.2 <- (sd(Object$trend$Y[  period.2])/ sqrt(length(Object$trend$Y[ period.2]))) %>% round(6)
  
  trend.slope.1 <- mean(Object$trend$slp[ period.1]) %>% round(6)
  trend.slope.2 <- mean(Object$trend$slp[ period.2]) %>% round(6)
  
  trend.slopeSE.1 <- sd(Object$trend$slp[ period.1])/ sqrt(length(Object$trend$slp[ period.1])) %>% round(6)
  trend.slopeSE.2 <- sd(Object$trend$slp[ period.2])/ sqrt(length(Object$trend$slp[ period.2])) %>% round(6)
  
  # Make the table to export
  
  table.df <- data.frame( Period=c(period.1.time, period.2.time),
                          Trend = c(trend.1, trend.2 ),
                          Trend.SE = c(trendSE.1, trendSE.2),
                          Trend_slp = c(trend.slope.1, trend.slope.2),
                          Trend_slp.SE = c(trend.slopeSE.1 %>% round(6), trend.slopeSE.2 %>% round(6)))
  
}


ENPHC.sal$trend$cp # coastal long term site
summary.ENPHC.sal <- trend.summary.periods_3( Object=ENPHC.sal, cp1=1, cp2=2, df=enphc.sal ) %>% mutate(site ='ENPHC' )

# inland LTER site - wont go back into df because don't have same number of rows
TS1$trend$cp
summary.TS1.sal <- trend.summary.periods_2.sal( Object=TS1.sal, cp1=1, df=ts1.sal ) %>% mutate(site ='TS1' )

# coastal LTER site 
TS7.sal$trend$cp
summary.TS7.sal <- trend.summary.periods_2(Object=TS7.sal, cp1=1, df=ts7.sal) %>% mutate(site ='TS7' )

# Add site and merge the tables into one:

Trend.summary.sal <- rbind(summary.ENPHC.sal, summary.TS1.sal, summary.TS7.sal) %>% mutate(Type= 'Salinity')

# Create a function the summarizes the change point informarion used in the Manuscript:

ENPHC.summary.cp.sal <- summary.cp(ENPHC.sal) %>% mutate(site ='ENPHC' )
TS1.summary.cp.sal <- summary.cp(TS1.sal) %>% mutate(site ='TS1' )
TS7.summary.cp.sal <- summary.cp(TS7.sal) %>% mutate(site ='TS7' )

cp.summary.sal <- rbind(ENPHC.summary.cp.sal, TS1.summary.cp.sal,  TS7.summary.cp.sal) %>% mutate(Type= 'Salinity')

# Summary to download:

final.trend.summary <- rbind(Trend.summary, Trend.summary.sal)
final.cp.summary <- rbind( cp.summary, cp.summary.sal )

write.csv( final.trend.summary, "final.trend.summary.csv")
write.csv( final.cp.summary, "final.cp.summary.csv")

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
