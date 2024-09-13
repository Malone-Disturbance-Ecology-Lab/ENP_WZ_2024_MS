# This script is used to calculate the uncertainty in the annual estimates for each site.
rm(list=ls())

library(tidyverse)
library(brms) 

load("data/WZ_NLM_RESULTS.RDATA")

# Creates parameter files from the model objects:
TS1 <- cbind( rbind(brms::fixef(ts1.00)[,1], 
      brms::fixef(ts1.25)[,1],
      brms::fixef(ts1.5)[,1]),
      rbind(brms::fixef(ts1.00)[,3], 
              brms::fixef(ts1.25)[,3],
              brms::fixef(ts1.5)[,3]) %>% as.data.frame %>% rename( alpha_LowCI =a1_Intercept, Pmax_LowCI = ax_Intercept, Reco_LowCI=r_Intercept) ,
        rbind(brms::fixef(ts1.00)[,4], 
             brms::fixef(ts1.25)[,4],
             brms::fixef(ts1.5)[,4]) %>% as.data.frame %>% rename( alpha_HighCI =a1_Intercept, Pmax_HighCI = ax_Intercept, Reco_HighCI=r_Intercept) ) %>% mutate( Submergence = c(0.00, 0.25, 0.50), site = 'TS1')

SE1 <- cbind( rbind(brms::fixef(se1.5)[,1],
                    brms::fixef(se1.75)[,1]),
              rbind(brms::fixef(se1.5)[,3],
                    brms::fixef(se1.75)[,3]) %>% as.data.frame %>% rename( alpha_LowCI =a1_Intercept, Pmax_LowCI = ax_Intercept, Reco_LowCI=r_Intercept) ,
              rbind(brms::fixef(se1.5)[,4],
                    brms::fixef(se1.75)[,4]) %>% as.data.frame %>% rename( alpha_HighCI =a1_Intercept, Pmax_HighCI = ax_Intercept, Reco_HighCI=r_Intercept) ) %>% mutate( Submergence = c( 0.50, 0.75), site='SE1')

TS7 <- cbind( rbind(brms::fixef(TS7.00)[,1],
                    brms::fixef(TS7.25)[,1]),
              rbind(brms::fixef(TS7.00)[,3],
                    brms::fixef(TS7.25)[,3]) %>% as.data.frame %>% rename( alpha_LowCI =a1_Intercept, Pmax_LowCI = ax_Intercept, Reco_LowCI=r_Intercept) ,
              rbind(brms::fixef(TS7.00)[,4],
                    brms::fixef(TS7.25)[,4]) %>% as.data.frame %>% rename(alpha_HighCI =a1_Intercept, Pmax_HighCI = ax_Intercept, Reco_HighCI=r_Intercept) ) %>% mutate( Submergence = c( 0.00, 0.25), site='TS7')

Parms <- rbind( TS1, SE1, TS7)

TS1.trc <- cbind( rbind(brms::fixef(ts1.00.trc)[,1], 
                    brms::fixef(ts1.25.trc)[,1],
                    brms::fixef(ts1.5.trc)[,1]) %>% as.data.frame %>% rename( R0 =a_Intercept, b = b_Intercept) ,
              rbind(brms::fixef(ts1.00.trc)[,3], 
                    brms::fixef(ts1.25.trc)[,3],
                    brms::fixef(ts1.5.trc)[,3]) %>% as.data.frame %>% rename( R0_LowCI =a_Intercept, b_LowCI = b_Intercept) ,
              rbind(brms::fixef(ts1.00.trc)[,4], 
                    brms::fixef(ts1.25.trc)[,4],
                    brms::fixef(ts1.5.trc)[,4]) %>% as.data.frame %>% rename(R0_HighCI =a_Intercept, b_HighCI = b_Intercept) ) %>% mutate( Submergence = c(0.00, 0.25, 0.50), site = 'TS1')

SE1.trc <- cbind( rbind(brms::fixef(se1.5.trc)[,1],
                    brms::fixef(se1.75.trc)[,1])%>% as.data.frame %>% rename( R0 =a_Intercept, b = b_Intercept) ,
              rbind(brms::fixef(se1.5.trc)[,3],
                    brms::fixef(se1.75.trc)[,3]) %>% as.data.frame %>% rename( R0_LowCI =a_Intercept, b_LowCI = b_Intercept) ,
              rbind(brms::fixef(se1.5.trc)[,4],
                    brms::fixef(se1.75.trc)[,4]) %>% as.data.frame %>% rename( R0_HighCI =a_Intercept, b_HighCI = b_Intercept) ) %>% mutate( Submergence = c( 0.50, 0.75), site='SE1')

TS7.trc <- cbind( rbind(brms::fixef(TS7.00.trc)[,1],
                    brms::fixef(TS7.25.trc)[,1])%>% as.data.frame %>% rename( R0 =a_Intercept, b = b_Intercept) ,
              rbind(brms::fixef(TS7.00.trc)[,3],
                    brms::fixef(TS7.25.trc)[,3]) %>% as.data.frame %>% rename( R0_LowCI =a_Intercept, b_LowCI = b_Intercept) ,
              rbind(brms::fixef(TS7.00.trc)[,4],
                    brms::fixef(TS7.25.trc)[,4]) %>% as.data.frame %>% rename(R0_HighCI =a_Intercept, b_HighCI = b_Intercept) ) %>% mutate( Submergence = c( 0.00, 0.25), site='TS7')

Parms.trc <- rbind( TS1.trc, SE1.trc, TS7.trc)

Parms <- Parms %>% left_join(Parms.trc , by=c("Submergence" ,"site"))

# Imports Flux data needed:
df <- read.csv( 'data/data_raw/AR_flux_sites_2021.csv')
df$TIMESTAMP <- as.POSIXct( df$TIMESTAMP)
summary(df)
# Fill missing values with NA:
df$SE1.TA.f[ is.na(df$SE1.TA.f )] <- df$TS7.TA.f[ is.na(df$SE1.TA.f )]
df$SE1.PAR.f[ is.na(df$SE1.PAR.f )] <- df$TS7.PAR.f[ is.na(df$SE1.PAR.f )]

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

plot(df$TIMESTAMP, df$TS7WLindicator.2 )
df$TS7WLindicator <- df$TS7WLindicator.2


# How to deal with gaps in the Submergence:
names(df)
# combining submergence parms with site NEE
Parms.TS1 <- Parms %>% filter(site == 'TS1')
Parms.SE1 <- Parms %>% filter(site == 'SE1')
Parms.TS7 <- Parms %>% filter(site == 'TS7')

ts1 <- df %>% dplyr::select("TIMESTAMP", 
                            "TS1.NEE.filtered",  
                            "TS1.TA.f", 
                            "TS1.PAR.f", 
                            "TS1WLindicator", 
                            "TS1_daily_wl") %>% rename(NEE = TS1.NEE.filtered,
                                                                                                TA.f = TS1.TA.f,
                                                                                                PAR.f = TS1.PAR.f,
                                                                                                Submergence = TS1WLindicator ) %>% left_join( Parms.TS1, by='Submergence')

se1 <- df %>% dplyr::select("TIMESTAMP", 
                            "SE1.NEE.filtered",  
                            "SE1.TA.f", "SE1.PAR.f", 
                            "SE1WLindicator", 
                            "SE1_daily_wl") %>% rename(NEE = SE1.NEE.filtered,
                                                                                                TA.f = SE1.TA.f,
                                                                                                PAR.f = SE1.PAR.f,
                                                                                                Submergence =SE1WLindicator ) %>% left_join( Parms.SE1, by='Submergence')
ts7 <- df %>% dplyr::select("TIMESTAMP", 
                            "TS7.NEE.filtered",  
                            "TS7.TA.f", 
                            "TS7.PAR.f", 
                            "TS7WLindicator", 
                            "TS7_daily_wl") %>% rename(NEE = TS7.NEE.filtered,
                                                       TA.f = TS7.TA.f,
                                                       PAR.f = TS7.PAR.f,
                                                       Submergence =TS7WLindicator ) %>% left_join( Parms.TS7, by='Submergence')
# Prepare datafrome to calulate the optimum:
opt.parms <- function( df){
  
  df <- df %>% mutate(alpha =a1_Intercept,
                      Pmax = ax_Intercept,
                      Reco = r_Intercept,
                      Pmax.opt = min(ax_Intercept, na.rm=T),
                      alpha.opt = min(a1_Intercept, na.rm=T),
                      Reco.opt = r_Intercept,
                      R0.opt =R0,
                      b.opt = b,
                      R0 =R0,
                      b = b)
  
  #df$Pmax.opt <- min(df$ax_Intercept, na.rm=T)
  #df$alpha.opt <-df$a1_Intercept
  #df$Reco.opt <-df$r_Intercept
  
  #df$alpha.opt <-max(df$alpha[df$Pmax == min(df$Pmax, na.rm=T)], na.rm=T)
  #df$Reco.opt <-max(df$Reco[df$Pmax == min(df$Pmax, na.rm=T)], na.rm=T)
  #df$R0.opt <-mean(df$R0[df$Pmax == min(df$Pmax, na.rm=T)], na.rm=T)
  #df$b.opt <-mean(df$b[df$Pmax == min(df$Pmax, na.rm=T)], na.rm=T)
  
  #df$R0.opt <-df$a_Intercept
  #df$b.opt <-df$b_Intercept
  return(df)
}

ts1 <- opt.parms(ts1)
se1 <- opt.parms(se1)
ts7 <- opt.parms(ts7)

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

# Optimum:
gapfill.opt <- function(data.frame){
  
  data.frame$NEE.day.opt <- data.frame$Reco.opt + ((data.frame$PAR.f * data.frame$alpha.opt * data.frame$Pmax.opt)/ (data.frame$PAR.f * data.frame$alpha.opt + data.frame$Pmax.opt))  
  
  data.frame$NEE.night.opt <- data.frame$R0.opt * exp(data.frame$b.opt * data.frame$TA.f)
  
  data.frame$NEE.Modeled.opt <- data.frame$NEE.day.opt
  data.frame$NEE.Modeled.opt[ data.frame$PAR.f == 0] <- data.frame$NEE.night.opt[data.frame$PAR.f == 0]
   
  data.frame$NEE.Modeled.opt.gc <- (12.0107*data.frame$NEE.Modeled.opt)/1000000*1800
  return( data.frame)
}

ts1.gf.opt <-gapfill.opt(data.frame = ts1.gf)
ts7.gf.opt <-gapfill.opt(data.frame = ts7.gf)
se1.gf.opt <-gapfill.opt(data.frame = se1.gf )

sum( ts1.gf.opt$NEE.Modeled.opt.gc, na.rm=T)
sum( se1.gf.opt$NEE.Modeled.opt.gc, na.rm=T)
sum( ts7.gf.opt$NEE.Modeled.opt.gc, na.rm=T)

# Daily summariesf for the plots:
ts1.gf.opt %>% group_by(TIMESTAMP) %>% summarise(NEE.gc = sum(NEE.gc) , NEE.Modeled.opt.gc = sum(NEE.Modeled.opt.gc)) %>% 
  ggplot() + geom_line( aes(x=TIMESTAMP , y=cumsum( NEE.gc)), col="darkblue")+
  geom_line( aes(x=TIMESTAMP , y=cumsum( NEE.Modeled.opt.gc)), col="blue") + theme_bw()

se1.gf.opt %>% group_by(TIMESTAMP) %>% summarise(NEE.gc = sum(NEE.gc) , NEE.Modeled.opt.gc = sum(NEE.Modeled.opt.gc)) %>%  
  ggplot() + geom_line( aes(x=TIMESTAMP , y=cumsum( NEE.gc)), col="#fab255")+
  geom_line( aes(x=TIMESTAMP , y=cumsum( NEE.Modeled.opt.gc)), col="#fab255") + theme_bw()

ts7.gf.opt %>% group_by(TIMESTAMP) %>% summarise(NEE.gc = sum(NEE.gc) , NEE.Modeled.opt.gc = sum(NEE.Modeled.opt.gc)) %>%
  ggplot() + geom_line( aes(x=TIMESTAMP , y=cumsum( NEE.gc)), col="#43b284")+
  geom_line( aes(x=TIMESTAMP , y=cumsum( NEE.Modeled.opt.gc)), col="#43b284") + theme_bw()

save(ts1.gf.opt,
     se1.gf.opt,
     ts7.gf.opt, file='data/Budget_Opt.RDATA' )
# Uncertainty estimation ####

# Create data frames

ts1.met <- df %>% dplyr::select("TS1.NEE.filtered",  "TS1.TA.f", "TS1.PAR.f", "TS1WLindicator") %>% rename(NEE = TS1.NEE.filtered, TA.f = TS1.TA.f, PAR.f = TS1.PAR.f, Submergence = TS1WLindicator )

se1.met <- df %>% dplyr::select("SE1.NEE.filtered",  "SE1.TA.f", "SE1.PAR.f", "SE1WLindicator") %>% rename(NEE = SE1.NEE.filtered, TA.f = SE1.TA.f, PAR.f = SE1.PAR.f, Submergence =SE1WLindicator )

ts7.met <- df %>% dplyr::select("TS7.NEE.filtered",  "TS7.TA.f", "TS7.PAR.f", "TS7WLindicator") %>% rename(NEE = TS7.NEE.filtered, TA.f = TS7.TA.f, PAR.f = TS7.PAR.f, Submergence =TS7WLindicator )

# PARM table random sample:
new.parms <- function( parms.df) {
  Parms.N <- parms.df
  
  for( i in 1: length(Parms.N$Submergence)){
    
    Parms.N$alpha[i] = runif(3, Parms.N$alpha_LowCI[i], Parms.N$alpha_HighCI[i])
    Parms.N$Pmax[i] = runif(3, Parms.N$Pmax_LowCI[i], Parms.N$Pmax_HighCI[i])
    Parms.N$Reco[i] = runif(3, Parms.N$Reco_LowCI[i], Parms.N$Reco_HighCI[i])
    Parms.N$R0[i] = runif(3, Parms.N$R0_LowCI[i], Parms.N$R0_HighCI[i])
    Parms.N$b[i] = runif(3, Parms.N$b_LowCI[i], Parms.N$b_HighCI[i])
  }
  return(Parms.N )
} # creates a new parm DF with randomly selected new parms

# calculate uncertainty
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

