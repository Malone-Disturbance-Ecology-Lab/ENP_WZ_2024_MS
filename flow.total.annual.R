# This script is used to calculate the numbers used for the simulation
rm(list=ls())

library(tidyverse)
Parms <- read.csv('data/WZ_NEE_Budget_Key_CIs.csv' )

df <- read.csv( 'data/AR_flux_sites_2021.csv')
summary(df$TS1_daily_wl)

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

ts1 <- df %>% dplyr::select("TS1.NEE.filtered",  "TS1.TA.f", "TS1.PAR.f", "TS1WLindicator") %>% rename(NEE = TS1.NEE.filtered,
                                                                                                TA.f = TS1.TA.f,
                                                                                                PAR.f = TS1.PAR.f,
                                                                                                Submergence = TS1WLindicator ) %>% left_join( Parms.TS1, by='Submergence')

se1 <- df %>% dplyr::select("SE1.NEE.filtered",  "SE1.TA.f", "SE1.PAR.f", "SE1WLindicator") %>% rename(NEE = SE1.NEE.filtered,
                                                                                                TA.f = SE1.TA.f,
                                                                                                PAR.f = SE1.PAR.f,
                                                                                                Submergence =SE1WLindicator ) %>% left_join( Parms.SE1, by='Submergence')

ts7 <- df %>% dplyr::select("TS7.NEE.filtered",  "TS7.TA.f", "TS7.PAR.f", "TS7WLindicator") %>% rename(NEE = TS7.NEE.filtered,
                                                                                                TA.f = TS7.TA.f,
                                                                                                PAR.f = TS7.PAR.f,
                                                                                                Submergence =TS7WLindicator ) %>% left_join( Parms.TS7, by='Submergence')


gapfill <- function(data.frame){
  
data.frame$NEE.day <- data.frame$Reco + ((data.frame$PAR.f * data.frame$alpha * data.frame$Pmax)/ (data.frame$PAR.f * data.frame$alpha + data.frame$Pmax))  

  data.frame$NEE.night <- data.frame$R0 * exp(data.frame$b * data.frame$TA.f)
  
  data.frame$NEE.Modeled <- data.frame$NEE.day
  data.frame$NEE.Modeled[ data.frame$PAR.f == 0] <- data.frame$NEE.night[data.frame$PAR.f == 0]
  

  
  data.frame$NEE.gf <- data.frame$NEE
  data.frame$NEE.gf[ is.na(data.frame$NEE.gf) ==T] <- data.frame$NEE.Modeled[ is.na(data.frame$NEE.gf) ==T] 
  
  data.frame$NEE.gc <- (12.0107*data.frame$NEE.gf)/1000000*1800
  return( data.frame)
}

ts1.gf <-gapfill(data.frame = ts1)
ts7.gf <-gapfill(data.frame = ts7)
se1.gf <-gapfill(data.frame = se1)

sum( ts1.gf$NEE.gc, na.rm=T)
sum( se1.gf$NEE.gc, na.rm=T)
sum( ts7.gf$NEE.gc, na.rm=T)

# Uncertainty estimation ####

# PARM table
# combining submergence parms with site NEE

new.parms <- function( Parms.df) {
  Parms.N <- Parms.df %>% select(Submergence)
  
  for( i in 1: length(Parms.N$Submergence)){
    
    Parms.N$alpha[i] = runif(3, Parms.TS1$alphaLowCI[i], Parms.TS1$alphaHiCI[i])
    Parms.N$Pmax[i] = runif(3, Parms.TS1$PmaxLowCI[i], Parms.TS1$PmaxHiCI[i])
    Parms.N$Reco[i] = runif(3, Parms.TS1$RecoLowCI[i], Parms.TS1$RecoHiCI[i])
    Parms.N$R0[i] = runif(3, Parms.TS1$R0LowCI[i], Parms.TS1$R0HiCI[i])
    Parms.N$b[i] = runif(3, Parms.TS1$bLowCI[i], Parms.TS1$bHiCI[i])
  }
  return(Parms.N )
} # creates a new parm DF with randomly selected new parms

# Create data frames
ts1.met <- df %>% dplyr::select("TS1.NEE.filtered",  "TS1.TA.f", "TS1.PAR.f", "TS1WLindicator") %>% rename(NEE = TS1.NEE.filtered, TA.f = TS1.TA.f, PAR.f = TS1.PAR.f, Submergence = TS1WLindicator )

se1.met <- df %>% dplyr::select("SE1.NEE.filtered",  "SE1.TA.f", "SE1.PAR.f", "SE1WLindicator") %>% rename(NEE = SE1.NEE.filtered, TA.f = SE1.TA.f, PAR.f = SE1.PAR.f, Submergence =SE1WLindicator )

ts7.met <- df %>% dplyr::select("TS7.NEE.filtered",  "TS7.TA.f", "TS7.PAR.f", "TS7WLindicator") %>% rename(NEE = TS7.NEE.filtered, TA.f = TS7.TA.f, PAR.f = TS7.PAR.f, Submergence =TS7WLindicator )


uncertainty <- function( parms.df, met.df){
  new.parms.df <- new.parms( parms.df)
  new.data <- met.df %>%  left_join( new.parms.df, by= 'Submergence') 
  new.data.gf <-gapfill(data.frame = new.data)
  total <- sum( new.data.gf$NEE.gc, na.rm=T)
  return(total)
}

uncertainty.annual <-function( parms.df, met.df, n) {
 
   list.annual <- c()

  for( i in 1:n){
   try(new <- uncertainty( parms.df, met.df), silent=T)
    list.annual <- c(new, list.annual)
  }
  return( list.annual)
}

TS7.uncertainty.annual <- uncertainty.annual(parms.df= Parms.TS7, met.df =ts7.met , n=10000)
TS1.uncertainty.annual <- uncertainty.annual(parms.df= Parms.TS1, met.df =ts1.met , n=10000)
SE1.uncertainty.annual <- uncertainty.annual(parms.df= Parms.SE1, met.df =se1.met , n=100000)


sd(TS7.uncertainty.annual )/ sqrt(length( TS7.uncertainty.annual ))
sd(TS1.uncertainty.annual )/ sqrt(length( TS1.uncertainty.annual ))
sd(SE1.uncertainty.annual )/ sqrt(length( SE1.uncertainty.annual ))

save(TS7.uncertainty.annual,
     TS1.uncertainty.annual,
     SE1.uncertainty.annual, 
     file = 'data/UncertaintyAnnual.RDATA')

