# Resilience:

# This script is used to calculate the numbers used for the simulation
rm(list=ls())

library(tidyverse)
library(ggplot2)
library(terra)
library(tidyterra)

setwd('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/ENP_WZ_2024_MS')

load('data/Budget_Opt.RDATA' )
load('data/Landscape_C.RDATA')
load('data/UncertaintyAnnual.RDATA')

#DE
ts1.gf.opt$DE.Pmax <- ( min(ts1.gf.opt$Pmax,na.rm=T)- ts1.gf.opt$Pmax ) / min(ts1.gf.opt$Pmax,na.rm=T) *100
ts7.gf.opt$DE.Pmax <- ( min(ts7.gf.opt$Pmax,na.rm=T)- ts7.gf.opt$Pmax ) / min(ts7.gf.opt$Pmax,na.rm=T)*100
se1.gf.opt$DE.Pmax <- ( min(se1.gf.opt$Pmax,na.rm=T)- se1.gf.opt$Pmax ) / min(se1.gf.opt$Pmax,na.rm=T)*100



TS1.DE <- rbind(ts1.gf.opt %>% group_by(Submergence) %>% summarise( 
  DE.Pmax = mean(DE.Pmax),
  WL = round(min(TS1_daily_wl, na.rm=T), 1)) %>% na.omit() ,
  ts1.gf.opt %>% group_by(Submergence) %>% summarise( 
    DE.Pmax = mean(DE.Pmax),
    WL = round(max(TS1_daily_wl, na.rm=T), 1)) %>% na.omit() ) %>% mutate( 
      Debt = case_when( DE.Pmax == 0 ~ 0,
                        DE.Pmax != 0 ~ WL - 0)) 

SE1.DE <- rbind(se1.gf.opt %>% group_by(Submergence) %>% summarise( 
  DE.Pmax = mean(DE.Pmax),
  WL = min(SE1_daily_wl, na.rm=T)) %>% na.omit(), se1.gf.opt %>% group_by(Submergence) %>% summarise( 
    DE.Pmax = mean(DE.Pmax),
    WL = max(SE1_daily_wl, na.rm=T)) %>% na.omit()) %>% 
  mutate( Debt = case_when( DE.Pmax == 0 ~ 0,
                            DE.Pmax != 0 ~ WL - 0))



TS7.DE <- rbind(ts7.gf.opt %>% group_by(Submergence) %>% summarise( 
  DE.Pmax = mean(DE.Pmax),
  WL = min(TS7_daily_wl, na.rm=T)) %>% na.omit(),
  ts7.gf.opt %>% group_by(Submergence) %>% summarise( 
    DE.Pmax = mean(DE.Pmax),
    WL = max(TS7_daily_wl, na.rm=T)) %>% na.omit()) %>% 
  mutate( Debt = case_when( DE.Pmax == 0 ~ 0,
                            DE.Pmax != 0 ~ WL - 0))



Extrap <- rbind(data.frame(Site = c("TS1", "TS1"), 
                     WL=c(-0.5, 0), 
                     DE.Pmax = min(TS1.DE$DE.Pmax), 
                     line=1),
                data.frame(Site = c("TS1", "TS1"), 
                           WL=c(0.5, 1), 
                           DE.Pmax = max(TS1.DE$DE.Pmax), 
                           line=2),
                data.frame(Site = c("SE1", "SE1"), 
                           WL=c(-0.5, 0.25), 
                           DE.Pmax = min(SE1.DE$DE.Pmax), 
                           line=1),
                data.frame(Site = c("SE1", "SE1"), 
                           WL=c(0.5, 1), 
                           DE.Pmax = max(SE1.DE$DE.Pmax), 
                           line=2),
                data.frame(Site = c("TS7", "TS7"), 
                           WL=c(0.5, 1), 
                           DE.Pmax = min(SE1.DE$DE.Pmax), 
                           line=1),
                data.frame(Site = c("TS7", "TS7"), 
                           WL=c(-0.5, 0), 
                           DE.Pmax = max(SE1.DE$DE.Pmax), 
                           line=2))

Plot.DE.Pmax <- ggplot( ) + geom_line( data= TS1.DE %>% arrange(Submergence), aes( x=WL, y= DE.Pmax), col="darkblue", linewidth=2) + xlim(-0.5, 1)+
  geom_line( data=SE1.DE %>%arrange(Submergence), aes( x=WL, y= DE.Pmax), col="#fab255", linewidth=2) +
  geom_line( data= TS7.DE %>%arrange(Submergence), aes( x=WL, y= DE.Pmax),col= "#43b284", linewidth=2) +
  geom_line( data= Extrap %>% filter(Site == "TS1", line==1), aes( x=WL, y= DE.Pmax), col="darkblue", linewidth=2, linetype = "dotted") +
  geom_line( data= Extrap %>% filter(Site == "TS1", line==2), aes( x=WL, y= DE.Pmax), col="darkblue", linewidth=2, linetype = "dotted") +
  geom_line( data= Extrap %>% filter(Site == "SE1", line==1), aes( x=WL, y= DE.Pmax), col="#fab255", linewidth=2, linetype = "dotted") +
  geom_line( data= Extrap %>% filter(Site == "SE1", line==2), aes( x=WL, y= DE.Pmax), col="#fab255", linewidth=2, linetype = "dotted") +
  geom_line( data= Extrap %>% filter(Site == "TS7", line==1), aes( x=WL, y= DE.Pmax), col="#43b284", linewidth=2, linetype = "dotted") +
  geom_line( data= Extrap %>% filter(Site == "TS7", line==2), aes( x=WL, y= DE.Pmax), col="#43b284", linewidth=2, linetype = "dotted") +
  theme_bw() + ylab( expression(paste(Delta,"Pmax (%)"))) + xlab( "Water Level (m)") + theme(text = element_text(size = 25))




names(ts1.gf.opt)
ts1.gf.opt$count <- 1
se1.gf.opt$count <- 1
ts7.gf.opt$count <- 1

# Add Debt to the 
ts1.gf.opt$TS1_daily_wl
duration <- rbind(ts1.gf.opt %>% 
                    group_by(Submergence) %>% 
                    summarise(duration = sum(count )/17520*100, 
                              DE= unique(DE.Pmax),
                              WL = mean(TS1_daily_wl, na.rm=T)) %>% mutate(site="TS1"),
                  se1.gf.opt %>% group_by(Submergence) %>% 
                    summarise(duration = sum(count )/17520*100, 
                              DE= unique(DE.Pmax),
                              WL = mean(SE1_daily_wl, na.rm=T)) %>% mutate(site="SE1"),
                  ts7.gf.opt %>% group_by(Submergence) %>% 
                    summarise(duration = sum(count )/17520*100, 
                              DE= unique(DE.Pmax),
                              WL = mean(TS7_daily_wl, na.rm=T)) %>% mutate(site="TS7"))

#duration$DE.PMAX <- duration$duration * duration$DE
#duration.DE <- duration %>% group_by(site) %>% summarise(DE.PMAX= sum(DE.PMAX) )

duration$WL[ duration$WL < 0] <- 0

WL.opt <- duration %>% filter( DE == 0) %>% mutate(WL.opt = WL) %>%  select(site, WL.opt)

debt <- duration %>% left_join(WL.opt, by= 'site' ) %>% mutate(Debt = WL - WL.opt*(duration/100) ) %>% reframe( .by=site, Debt = sum(Debt) %>% round(2)) %>% 
  mutate(eco = case_when(site == "TS1"~"Marl Prairie",
                         site == "SE1"~"Ecotone",
                         site == "TS7"~"Scrub Mangrove"))

debt$Debt[debt$eco == "Scrub Mangrove"]<- 0.004# show up a tiny bit

debt$eco <- factor(debt$eco , levels = c( "Marl Prairie","Ecotone", "Scrub Mangrove" ))

debt.plot <- debt %>% ggplot() + geom_col(aes( x= eco, y = Debt, fill=site)) + theme_bw() +scale_fill_manual(values=c( "#fab255","darkblue", "#43b284")) + ylab("Climate Debt") + xlab("") + theme(text = element_text(size = 20),axis.text=element_text(size=18))+  theme(legend.position="none")



# Daily summaries for the plots:

ts1.gf.opt$Date <- ts1.gf.opt$TIMESTAMP %>% as.Date

plot.ts1.dde <- ts1.gf.opt %>% group_by(Date) %>% summarise(NEE.gc = sum(NEE.gc) , NEE.Modeled.opt.gc = sum(NEE.Modeled.opt.gc)) %>% 
  ggplot() + geom_line( aes(x=Date , y=cumsum( NEE.gc)), col="darkblue",linewidth=2) +
  geom_line( aes(x=Date , y=cumsum( NEE.Modeled.opt.gc)), col="darkblue", linetype = "dotted",linewidth=2) + theme_bw() + xlab("") + 
  ylab(expression(paste('NEE ( g C  m'^2, ' day'^-1, ')'))) + theme(text = element_text(size = 20),axis.text=element_text(size=20) , axis.text.x=element_text(angle = 0, vjust = 1, hjust = 1))+
  scale_x_date(date_breaks = "3 month", date_labels = "%b")

se1.gf.opt$Date <- se1.gf.opt$TIMESTAMP %>% as.Date

plot.se1.dde <- se1.gf.opt %>% group_by(Date) %>% summarise(NEE.gc = sum(NEE.gc) , NEE.Modeled.opt.gc = sum(NEE.Modeled.opt.gc)) %>%  
  ggplot() + geom_line( aes(x=Date , y=cumsum( NEE.gc)), col="#fab255",linewidth=2)+
  geom_line( aes(x=Date , y=cumsum( NEE.Modeled.opt.gc)), col="#fab255", linetype = "dotted",linewidth=2) + theme_bw()+ xlab("") + 
  ylab(expression(paste('NEE ( g C  m'^2, ' day'^-1, ')')))+ theme(text = element_text(size = 20),axis.text=element_text(size=20) , axis.text.x=element_text(angle = 0, vjust = 1, hjust = 1)) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b")

ts7.gf.opt$Date <- ts7.gf.opt$TIMESTAMP %>% as.Date

plot.ts7.dde <- ts7.gf.opt %>% group_by(Date) %>% summarise(NEE.gc = sum(NEE.gc) , NEE.Modeled.opt.gc = sum(NEE.Modeled.opt.gc)) %>%
  ggplot() + geom_line( aes(x=Date, y=cumsum( NEE.gc)), col="#43b284",linewidth=2)+
  geom_line( aes(x=Date , y=cumsum( NEE.Modeled.opt.gc)), col="#43b284", linetype = "dotted",linewidth=2) + theme_bw()+ xlab("") + 
  ylab(expression(paste('NEE ( g C  m'^2, ' day'^-1, ')'))) + theme(text = element_text(size = 20),axis.text=element_text(size=20) , axis.text.x=element_text(angle = 0, vjust = 1, hjust = 1)) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b")

save(duration,Plot.DE.Pmax, plot.ts1.dde, plot.se1.dde, plot.ts7.dde  ,  file='data/Disequalibrium.RDATA')

theme(legend.position = "top") + theme(axis.text=element_text(size=6.5)) 

# Landscape C:
veg <- rast('data/landscapeC.tif')
dist.fwm_raster <- rast('data/landscapeC_dist.tif')

load('data/Landscape_C.RDATA')

library(sf)
enp.se <- read_sf("data/shapefiles/Southeastern Everglades_AOI.kml" ) %>% st_transform( 3857)
ENP.shp <- read_sf("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/ENP_WZ_2024_MS/data/shapefiles/ENP.shp") %>%  st_transform( 3857)


simulation.1 <- ggplot() + geom_sf(data = ENP.shp, fill='Transparent', col="black", linewidth=2) + geom_spatraster(data = veg$veg.p, na.rm = TRUE,show.legend = T) +  
  scale_fill_manual(na.value = "transparent", 
                    values= c("#43b284", "#000099", "#fab255"),
                    labels =c("","","", ""))+
  labs( fill="" ) +
  geom_spatraster_contour(data = dist.fwm_raster, breaks =seq(1, 50, 1.5), na.rm = TRUE, col="lightblue4") +
  geom_sf(data= enp.shade, col='black', alpha=0.7, linewidth=2) + theme_bw()+  
  theme(axis.text=element_text(size=20), 
        text = element_text(size = 25), 
        axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(legend.position="none")

simulation.1

library(gridExtra)

png('figures/Figure7_flow.Disequalibrium.png',width = 1450, height =900)
grid.arrange(  Plot.DE.Pmax+labs(tag="A"), debt.plot+labs(tag="B"),
               simulation.1+labs(tag="C"),
              plot.ts1.dde+labs(tag="D"),
              plot.se1.dde+labs(tag="E"), 
              plot.ts7.dde +labs(tag="F") ,
              layout_matrix = rbind(c(1,2,3),
                                    c(4,5,6)))

dev.off()

# Annual NEE realized:

sum( ts1.gf.opt$NEE.gc, na.rm=T)
sum( se1.gf.opt$NEE.gc, na.rm=T)
sum( ts7.gf.opt$NEE.gc, na.rm=T)

sum( ts1.gf.opt$NEE.gc, na.rm=T)- sum( ts1.gf.opt$NEE.Modeled.opt.gc, na.rm=T)
sum( se1.gf.opt$NEE.gc, na.rm=T)- sum( se1.gf.opt$NEE.Modeled.opt.gc, na.rm=T)
sum( ts7.gf.opt$NEE.gc, na.rm=T)- sum( ts7.gf.opt$NEE.Modeled.opt.gc, na.rm=T)


sd(TS7.uncertainty.annual )/ sqrt(length( TS7.uncertainty.annual ))
sd(TS1.uncertainty.annual )/ sqrt(length( TS1.uncertainty.annual ))
sd(SE1.uncertainty.annual )/ sqrt(length( SE1.uncertainty.annual ))

save(TS7.uncertainty.annual,
     TS1.uncertainty.annual,
     SE1.uncertainty.annual, 
     file = 'data/UncertaintyAnnual.RDATA')


