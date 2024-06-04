# Disequalibrium:
# This script is used to calculate the numbers used for the simulation
rm(list=ls())

library(tidyverse)
library(ggplot2)
library(terra)
library(tidyterra)

load('data/Budget_Opt.RDATA' )
load('data/Landscape_C.RDATA')

#DE
ts1.gf.opt$DE.Pmax <- ( min(ts1.gf.opt$Pmax,na.rm=T)- ts1.gf.opt$Pmax ) / min(ts1.gf.opt$Pmax,na.rm=T)
ts7.gf.opt$DE.Pmax <- ( min(ts7.gf.opt$Pmax,na.rm=T)- ts7.gf.opt$Pmax ) / min(ts7.gf.opt$Pmax,na.rm=T)
se1.gf.opt$DE.Pmax <- ( min(se1.gf.opt$Pmax,na.rm=T)- se1.gf.opt$Pmax ) / min(se1.gf.opt$Pmax,na.rm=T)

unique(ts7.gf.opt$DE.Pmax)

extension <- data.frame( DE.Pmax.ts1 = 0.3421 , WL.ts1 = c(0.5, 1),
                         DE.Pmax.ts1.2 = 0 , WL.ts1.2 = c(-0.5, 0),
                         DE.Pmax.se1 = c(0.20134,0.20134)   , WL.se1 = c(-0.5,0.40),
                         DE.Pmax.se1.2 = c(0.1521253, 0.1521253)   , WL.se1.2 = c( 0.75, 1),
                         DE.Pmax.ts7 = c(0.07065217 ,0.07065217 ) , WL.ts7 = c(-0.5,0),
                         DE.Pmax.ts7.2 = c(0 ,0 ) , WL.ts7.2 = c(0,1))
names(df)
Plot.DE.Pmax <- ggplot( ) + geom_line( data= ts1.gf.opt, aes( x=TS1_daily_wl, y= DE.Pmax), col="darkblue", linewidth=1) +
  geom_line( data= se1.gf.opt, aes( x=SE1_daily_wl, y= DE.Pmax), col="#fab255", linewidth=1) +
  geom_line( data= ts7.gf.opt, aes( x=TS7_daily_wl, y= DE.Pmax),col= "#43b284", linewidth=1) + 
  geom_line( data= extension, aes( x=WL.ts1, y= DE.Pmax.ts1), col="darkblue", linewidth=1, linetype = "dotted") +
  geom_line( data= extension, aes( x=WL.ts1.2, y= DE.Pmax.ts1.2), col="darkblue", linewidth=1, linetype = "dotted") +
  geom_line( data= extension, aes( x=WL.se1, y= DE.Pmax.se1), col="#fab255", linewidth=1, linetype = "dotted") +
  geom_line( data= extension, aes( x=WL.se1.2, y= DE.Pmax.se1.2), col="#fab255", linewidth=1, linetype = "dotted") +
  geom_line( data= extension, aes( x=WL.ts7, y= DE.Pmax.ts7),col= "#43b284", linewidth=1, linetype = "dotted") + 
  geom_line( data= extension, aes( x=WL.ts7.2, y= DE.Pmax.ts7.2),col= "#43b284", linewidth=1, linetype = "dotted") +
  theme_bw() + ylab( "Disequalibrium (Pmax)") + xlab( "Water Level (m)") + theme(text = element_text(size = 25))


names(ts1.gf.opt)
ts1.gf.opt$count <- 1
se1.gf.opt$count <- 1
ts7.gf.opt$count <- 1


duration <- rbind(ts1.gf.opt %>% group_by(Submergence) %>% summarise(duration = sum(count )/17520*100, DE= unique(DE.Pmax)) %>% mutate(site="TS1"),
                  se1.gf.opt %>% group_by(Submergence) %>% summarise(duration = sum(count )/17520*100, DE= unique(DE.Pmax)) %>% mutate(site="SE1"),
                  ts7.gf.opt %>% group_by(Submergence) %>% summarise(duration = sum(count )/17520*100, DE= unique(DE.Pmax)) %>% mutate(site="TS7"))

duration$DE.PMAX <- duration$duration * duration$DE
duration.DE <- duration %>% group_by(site) %>% summarise(DE.PMAX= sum(DE.PMAX) )
# Debt: the difference between realized NEE and the NEE expected from the optimum

# Daily summaries for the plots:


plot.ts1.dde <- ts1.gf.opt %>% group_by(TIMESTAMP) %>% summarise(NEE.gc = sum(NEE.gc) , NEE.Modeled.opt.gc = sum(NEE.Modeled.opt.gc)) %>% 
  ggplot() + geom_line( aes(x=TIMESTAMP , y=cumsum( NEE.gc)), col="darkblue")+
  geom_line( aes(x=TIMESTAMP , y=cumsum( NEE.Modeled.opt.gc)), col="darkblue", linetype = "dotted") + theme_bw() + xlab("") + 
  ylab(expression(paste('NEE ( g C  m'^2, ' day'^-1, ')'))) + theme(text = element_text(size = 20),axis.text=element_text(size=15) , axis.text.x=element_text(angle = 60, vjust = 1, hjust = 1))

plot.se1.dde <- se1.gf.opt %>% group_by(TIMESTAMP) %>% summarise(NEE.gc = sum(NEE.gc) , NEE.Modeled.opt.gc = sum(NEE.Modeled.opt.gc)) %>%  
  ggplot() + geom_line( aes(x=TIMESTAMP , y=cumsum( NEE.gc)), col="#fab255")+
  geom_line( aes(x=TIMESTAMP , y=cumsum( NEE.Modeled.opt.gc)), col="#fab255", linetype = "dotted") + theme_bw()+ xlab("") + 
  ylab(expression(paste('NEE ( g C  m'^2, ' day'^-1, ')')))+ theme(text = element_text(size = 20),axis.text=element_text(size=15) , axis.text.x=element_text(angle = 60, vjust = 1, hjust = 1))

plot.ts7.dde <- ts7.gf.opt %>% group_by(TIMESTAMP) %>% summarise(NEE.gc = sum(NEE.gc) , NEE.Modeled.opt.gc = sum(NEE.Modeled.opt.gc)) %>%
  ggplot() + geom_line( aes(x=TIMESTAMP , y=cumsum( NEE.gc)), col="#43b284")+
  geom_line( aes(x=TIMESTAMP , y=cumsum( NEE.Modeled.opt.gc)), col="#43b284", linetype = "dotted") + theme_bw()+ xlab("") + 
  ylab(expression(paste('NEE ( g C  m'^2, ' day'^-1, ')'))) + theme(text = element_text(size = 20),axis.text=element_text(size=15) , axis.text.x=element_text(angle = 60, vjust = 1, hjust = 1))

save(duration,Plot.DE.Pmax, plot.ts1.dde, plot.se1.dde, plot.ts7.dde  ,  file='data/Disequalibrium.RDATA')

theme(legend.position = "bottom") + theme(axis.text=element_text(size=6.5)) 

# Landscape C:
veg <- rast('data/landscapeC.tif')
dist.fwm_raster <- rast('data/landscapeC_dist.tif')

load('data/Landscape_C.RDATA')

simulation.1 <- ggplot() + geom_spatraster(data = veg$veg.p, na.rm = TRUE,show.legend = T) +  
  scale_fill_manual(na.value = "transparent", values= c("#43b284", "#000099", "#fab255"),
                    labels =c("Freshwater Marl Praire",  "Brakish Ecotone","Saline Scrub Mangrove", ""))+
  labs( fill="" ) +
  geom_spatraster_contour(data = dist.fwm_raster, breaks =seq(1, 50, 1.5), na.rm = TRUE, col="lightblue4")+
  geom_sf(data= enp.shade, alpha=0.5, col="transparent") + theme_bw()+theme(legend.position = "bottom") + theme(axis.text=element_text(size=15), text = element_text(size = 25))


library(gridExtra)

png('figures/Disequalibrium_CC.png',width = 1450, height =900)
grid.arrange( Plot.DE.Pmax+labs(tag="A"), plot.ts1.dde+labs(tag="C"),
              plot.se1.dde+labs(tag="D"), plot.ts7.dde +labs(tag="E") ,simulation.1+labs(tag="B") , layout_matrix = rbind(c(1,1,1,5,5,5),
                                                                                                                          c(1,1,1,5,5,5),
                                    c(2,2,3,3,4,4)))

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


