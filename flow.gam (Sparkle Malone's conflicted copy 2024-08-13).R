
# Fit Models: #####
library(brms) 
library(cmdstanr)
library(ggplot2)


setwd('/Users/sm3466/Dropbox (YSE)/Research/ENP_WZ_2024_MS')
df <- read.csv( 'data/data_raw/AR_wl_sal_2021_weekly.csv')
names(df)
df$lowWL <- 0
df$lowWL[df$TS7_mean_weekly_WL < -0.50]<- 1
df$lowWL <- as.factor(df$lowWL)

prior1 <-get_prior(bf(TS7_mean_weekly_Sal ~ s(TS7_mean_weekly_WL) + arma(p= 1, q=0) ) ,data = df,  family = gaussian())

tsph7 <- brm( bf(TS7_mean_weekly_Sal ~ s(TS7_mean_weekly_WL) + arma( p= 1, q=0) ) , prior = prior1,
           data = df, backend = "cmdstanr",iter = 20000, family = gaussian(),
           cores=10, seed=101, refresh = 0, thin=30, chains = 10,
           control = list(adapt_delta = 0.99))

summary(tsph7)
pp_check(tsph7,ndraws = 100) 
pp_check(tsph7, type = "ecdf_overlay")
mcmc_plot(tsph7, type = "pairs")
mcmc_plot(tsph7, type = "acf") # check for auto correlation issues

plot.tsph7 <- conditional_effects(tsph7, effects="TS7_mean_weekly_WL", prob = 0.89) 
plot.tsph7.df  <- as.data.frame(plot.tsph7[[1]])

prior.se1 <- get_prior(bf(SE1_mean_weekly_Sal ~ s(SE1_mean_weekly_WL) + arma(p= 0, q=1, time=Week)),data = df,  family = gaussian())

# Date is notin the dfso i used week
se1 <- brm( bf(SE1_mean_weekly_Sal ~ s(SE1_mean_weekly_WL) + arma(p= 0, q=1)), prior = prior.se1 ,
              data = df, family='gaussian', backend = "cmdstanr",iter = 20000, thin = 20,chains=10,
              cores=10, seed=100, refresh = 0, warmup=10000,
              control = list(adapt_delta = 0.99), normalize=T)

summary(se1)

pp_check(se1,ndraws = 100) 
pp_check(se1, type = "ecdf_overlay",ndraws = 100)
mcmc_plot(se1, type = "pairs")
mcmc_plot(se1, type = "acf") # check for auto correlation issues


plot.se1 <- conditional_effects(se1, effects="SE1_mean_weekly_WL", prob = 0.89) 
plot.se1.df  <- as.data.frame(plot.se1[[1]])


# TS1:

prior.ts1 <- get_prior(bf(TS1_mean_weekly_Sal ~ s(TS1_mean_weekly_WL) + arma(p= 0, q=1, time=Week)),data = df,  family = gaussian())

ts1 <- brm( bf(TS1_mean_weekly_Sal ~ s(TS1_mean_weekly_WL) + arma(p= 0, q=1)), prior = prior.ts1 ,
            data = df, family='gaussian', backend = "cmdstanr",iter = 20000, thin = 20,chains=10,
            cores=10, seed=100, refresh = 0, warmup=10000,
            control = list(adapt_delta = 0.99), normalize=T)

summary(ts1)

pp_check(ts1,ndraws = 100) 
pp_check(ts1, type = "ecdf_overlay",ndraws = 100)
mcmc_plot(ts1, type = "pairs")
mcmc_plot(ts1, type = "acf") # check for auto correlation issues


plot.ts1 <- conditional_effects(ts1, effects="TS1_mean_weekly_WL", prob = 0.89) 
plot.ts1.df  <- as.data.frame(plot.ts1[[1]])


save( df, ts1, plot.ts1, plot.ts1.df,tsph7, plot.tsph7, se1, plot.se1, plot.se1.df, plot.tsph7.df, file="GAMs_Analysis_2024.RDATA"  )

# Figures:    #####
rm(list=ls())

load("~/YSE Dropbox/Sparkle Malone/Research/ENP_WZ_2024_MS/GAMs_Analysis_2024 (Sparkle Malone's conflicted copy 2024-08-12).RDATA")

# Look at model results
ts1
se1
tsph7







tsph7.plot <- ggplot() +geom_point(data=df,
                     aes(TS1_mean_weekly_WL, TS1_mean_weekly_Sal) , alpha=0.3 , color="#000099")  +
  theme_minimal() + xlab('Water Level (m)') + ylab('Salinity (PSU)') + theme(text = element_text(size = 18)) + ylim(0, 1 )  


tsph7.plot <- ggplot() +
  geom_line(data=plot.tsph7.df,
            aes(TS7_mean_weekly_WL, estimate__), color="#43b284", size=1.5) +
  geom_ribbon(data=plot.tsph7.df, aes(x=TS7_mean_weekly_WL, y=estimate__, 
                                      ymin=estimate__ + se__, ymax=estimate__- se__),
              alpha=0.1, fill = "#43b284",  color="transparent") +
  geom_point(data=df,
             aes(TS7_mean_weekly_WL, TS7_mean_weekly_Sal) , alpha=0.3 , color="#43b284")  +
  theme_minimal() + xlab('Water Level (m)') + ylab('Salinity (PSU)') +  theme(text = element_text(size = 18))   

se1.plot <- ggplot() +
  geom_line(data=plot.se1.df,
            aes(SE1_mean_weekly_WL, estimate__), color="#fab255", size=1.5) +
  geom_ribbon(data=plot.se1.df, aes(x=SE1_mean_weekly_WL, y=estimate__, 
                                    ymin=estimate__ + se__, ymax=estimate__- se__),
              alpha=0.1, fill = "#fab255",  color="transparent") +
  geom_point(data=df,
             aes(SE1_mean_weekly_WL, SE1_mean_weekly_Sal) , alpha=0.3 , color="#fab255")  +
  theme_minimal() + xlab('Water Level (m)') + ylab('Salinity (PSU)') + theme(text = element_text(size = 18))   

ts1.plot <- ggplot() +
  geom_line(data=plot.ts1.df,
            aes(TS1_mean_weekly_WL, estimate__), color="#000099", size=1.5) +
  geom_ribbon(data=plot.ts1.df, aes(x=TS1_mean_weekly_WL, y=estimate__, 
                                    ymin=estimate__ + se__, ymax=estimate__- se__),
              alpha=0.1, fill = "#000099",  color="transparent") +
  geom_point(data=df,
             aes(TS1_mean_weekly_WL, TS1_mean_weekly_Sal) , alpha=0.3 , color="#000099")  +
  theme_minimal() + xlab('Water Level (m)') + ylab('Salinity (PSU)') + theme(text = element_text(size = 18))   +
  ylim(0,1) + xlim(0.3, 0.525)


library(ggpubr)

png(file="figures/WaterLvesvsSalinity_2024.png",
    width=800, height=300)
ggarrange(ts1.plot, se1.plot,tsph7.plot,  ncol = 3, labels = c("A","B", "C"))
dev.off()

