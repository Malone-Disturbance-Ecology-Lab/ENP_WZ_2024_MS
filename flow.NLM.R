
library(brms) 
library(cmdstanr)
library(ggplot2)
library(beepr)
library(tidybayes)
library(tidyverse)
library(ggpubr)

df <- read.csv( 'data/AR_flux_sites_2021.csv')
df <-na.omit(df);summary(df)


names(df$date)
df$date <- as.Date(df$Date)


# Light Response Curves: #####
# nee ~ (a1 * PAR * ax)/(a1 * PAR + ax) + r

names(df)
ts1.df <- df[,c("TIMESTAMP" , "TS1.NEE.filtered","TS1.TA.f", "TS1.PAR.f", "TS1WLindicator" )]
names(ts1.df ) <- c("TIMESTAMP" , "nee","TA", "PAR", "I.wl" )

unique(ts1.df$I.wl)

priors.ts1 <- get_prior(bf(nee ~ (a1 * PAR * ax)/(a1 * PAR + ax) + r, a1+ax+r ~ 1, nl=TRUE),
                        data = ts1.df[which(ts1.df$I.wl == 0.00 & ts1.df$PAR >0),],
                        family = poisson())

priors.ts1 <-  prior(normal(-0.01, 0.1), nlpar = "a1", lb=-0.2, ub= 0) +
  prior(normal( -7.65 ,  0.33), nlpar = "ax", lb=-8, ub= -5) +
  prior(normal(2.10, 0.11), nlpar = "r", lb=1.9, ub= 2.2)

ts1.00 <- brm( bf(nee ~ (a1 * PAR * ax)/(a1 * PAR + ax) + r, a1+ax+r ~ 1, nl=TRUE),
            prior = priors.ts1 , data = ts1.df[which(ts1.df$I.wl == 0.00 & ts1.df$PAR >0 ),], 
            backend = "cmdstanr", iter = 500000, cores =4, seed=101)

summary(ts1.00)
plot(conditional_effects(ts1.00), points=T)
pp_check(ts1.00,ndraws = 100) 
pp_check(ts1.00, type = "ecdf_overlay")
mcmc_plot(ts1.00, type = "pairs")


ts1.25 <- brm( bf(nee ~ (a1 * PAR * ax)/(a1 * PAR + ax) + r, a1+ax+r ~ 1, nl=TRUE),
               prior = priors.ts1 , data = ts1.df[which(ts1.df$I.wl ==0.25 & ts1.df$PAR >0 ),], 
               backend = "cmdstanr", iter = 50000, cores =4, seed=101)

summary(ts1.25)
plot(conditional_effects(ts1.25), points=T)
pp_check(ts1.25,ndraws = 100) 
pp_check(ts1.25, type = "ecdf_overlay")
mcmc_plot(ts1.25, type = "pairs")


ts1.5 <- brm( bf(nee ~ (a1 * PAR * ax)/(a1 * PAR + ax) + r, a1+ax+r ~ 1, nl=TRUE),
               prior = priors.ts1 , data = ts1.df[which(ts1.df$I.wl ==0.5 & ts1.df$PAR >0),], 
               backend = "cmdstanr", iter = 50000, cores =4, seed=101)


summary(ts1.5)
plot(conditional_effects(ts1.5), points=T)
pp_check(ts1.5,ndraws = 100) 
pp_check(ts1.5, type = "ecdf_overlay")
mcmc_plot(ts1.5, type = "pairs")

#_________________________________________________

names(df)
SE1.df <- df[,c("TIMESTAMP" , "SE1.NEE.filtered","SE1.TA.f", "SE1.PAR.f", "SE1WLindicator" )]
names(SE1.df ) <- c("TIMESTAMP" , "nee","TA", "PAR", "I.wl" )

unique(SE1.df$I.wl)

priors.se1 <-  prior(normal(-0.01, 0.1), nlpar = "a1", lb=-0.2, ub= 0) +
  prior(normal( -3 ,  0.65), nlpar = "ax", lb=-8, ub= -3) +
  prior(normal(2, 0.5), nlpar = "r", lb=1.9, ub= 2.2)


se1.5 <- brm( bf(nee ~ (a1 * PAR * ax)/(a1 * PAR + ax) + r, a1+ax+r ~ 1, nl=TRUE),
              prior = priors.se1 , data = SE1.df[which(SE1.df$I.wl ==0.5 & ts1.df$PAR >0 ),], 
              backend = "cmdstanr", iter = 50000, cores =4, seed=101)


summary(se1.5)
plot(conditional_effects(se1.5), points=T)



se1.75 <- brm( bf(nee ~ (a1 * PAR * ax)/(a1 * PAR + ax) + r, a1+ax+r ~ 1, nl=TRUE),
              prior = priors.se1 , data = SE1.df[which(SE1.df$I.wl == 0.75 & ts1.df$PAR >0 ),], 
              backend = "cmdstanr", iter = 50000, cores =4, seed=101)


summary(se1.75)
plot(conditional_effects(se1.75), points=T)


se1.1.00 <- brm( bf(nee ~ (a1 * PAR * ax)/(a1 * PAR + ax) + r, a1+ax+r ~ 1, nl=TRUE),
               prior = priors.se1 , data = SE1.df[which(SE1.df$I.wl == 1.00 & ts1.df$PAR >0),], 
               backend = "cmdstanr", iter = 50000, cores =4, seed=101)


summary(se1.1.00)
plot(conditional_effects(se1.1.00), points=T)

#__________________________________

names(df)
TS7.df <- df[,c("TIMESTAMP" , "TS7.NEE.filtered","TS7.TA.f", "TS7.PAR.f", "TS7WLindicator" )]
names(TS7.df ) <- c("TIMESTAMP" , "nee","TA", "PAR", "I.wl" )

unique(TS7.df$I.wl)

priors.TS7 <-  prior(normal(-0.02, 0.1), nlpar = "a1", lb=-0.2, ub= 0) +
  prior(normal( -11 ,  5), nlpar = "ax", lb=-11, ub= -7) +
  prior(normal(2, 0.5), nlpar = "r", lb=1.9, ub= 2.2)


TS7.25 <- brm( bf(nee ~ (a1 * PAR * ax)/(a1 * PAR + ax) + r, a1+ax+r ~ 1, nl=TRUE),
              prior = priors.TS7 , data = TS7.df[which(TS7.df$I.wl == 0.25 & ts1.df$PAR >0),], 
              backend = "cmdstanr", iter = 50000, cores =4, seed=101)
summary(TS7.25)
plot(conditional_effects(TS7.25), points=T)
beep(3)


TS7.00 <- brm( bf(nee ~ (a1 * PAR * ax)/(a1 * PAR + ax) + r, a1+ax+r ~ 1, nl=TRUE),
               prior = priors.TS7 , data = TS7.df[which(TS7.df$I.wl == 0.00 & ts1.df$PAR >0),], 
               backend = "cmdstanr", iter = 50000, cores =4, seed=101)
summary(TS7.00)
plot(conditional_effects(TS7.00), points=T)
beep(3)

# Sample sizes:

length(ts1.00$data$nee)
length(ts1.25$data$nee)
length(ts1.5$data$nee)

length(se1.5$data$nee)
length(se1.75$data$nee)
length(se1.1.00$data$nee)

length(TS7.00$data$nee)
length(TS7.25$data$nee)


# extract distributions and plot models:

library(tidybayes)
library(tidyverse)


# See the variable in the model object
get_variables(TS7.00)

# Extract the posterior distributions of the parameters from the objects:
posterior.data <- function(model, model.name){
  
  a1 <-model %>%
    spread_draws(b_a1_Intercept) %>% select(contains('b_a1_Intercept'))
  ax <-model %>%
    spread_draws(b_ax_Intercept) %>% select(contains('b_ax_Intercept'))
  r <-model %>%
    spread_draws(b_r_Intercept) %>% select(contains('b_r_Intercept'))
  
  p.dist <- cbind( a1, ax, r )
  names( p.dist) <- c(paste( model.name, 'a1', sep="."), paste( model.name, 'ax', sep="."), paste( model.name, 'r', sep="."))
  
  return( p.dist )
}

posterior.LRC <- cbind( posterior.data(ts1.00, "ts1.00"), 
                        posterior.data(ts1.25, "ts1.25"),
                        posterior.data(ts1.5, "ts1.5"),
                        
                        posterior.data(se1.5, "se1.5"),
                        posterior.data(se1.75, "se1.75"),
                        posterior.data(se1.1.00, "se1.1.00"),
                        
                        posterior.data(TS7.25, "TS7.25"),
                        posterior.data(TS7.00, "TS7.00"))

# make predictions into a data frame:
lrc.predictions <- data.frame(PAR=seq(0, 2000, 10))

lrc.predictions$ts1.00 <- predict( ts1.00 , newdata=lrc.predictions)[,1]
lrc.predictions$ts1.00.err <- predict( ts1.00 , newdata=lrc.predictions)[,2]
lrc.predictions$ts1.25 <- predict( ts1.25 , newdata=lrc.predictions)[,1]
lrc.predictions$ts1.25.err <- predict( ts1.25 , newdata=lrc.predictions)[,2]
lrc.predictions$ts1.5 <- predict( ts1.5 , newdata=lrc.predictions)[,1]
lrc.predictions$ts1.5.err <- predict( ts1.5 , newdata=lrc.predictions)[,2]

lrc.predictions$se1.5 <- predict( se1.5 , newdata=lrc.predictions)[,1]
lrc.predictions$se1.5.err <- predict( se1.5 , newdata=lrc.predictions)[,2]
lrc.predictions$se1.75 <- predict( se1.75 , newdata=lrc.predictions)[,1]
lrc.predictions$se1.75.err <- predict( se1.75 , newdata=lrc.predictions)[,2]
lrc.predictions$se1.1.00 <- predict( se1.1.00 , newdata=lrc.predictions)[,1]
lrc.predictions$se1.1.00.err <- predict( se1.1.00 , newdata=lrc.predictions)[,2]

lrc.predictions$TS7.25 <- predict( TS7.25 , newdata=lrc.predictions)[,1]
lrc.predictions$TS7.25.err <- predict( TS7.25 , newdata=lrc.predictions)[,2]
lrc.predictions$TS7.00 <- predict( TS7.00 , newdata=lrc.predictions)[,1]
lrc.predictions$TS7.00.err <- predict( TS7.00 , newdata=lrc.predictions)[,2]

# Temperature Response Curves: #####

# Functions nee ~ a * exp(b*TA)

priors.ts1.trc <-  prior(normal(0.2 , 1), nlpar = "a", lb=0.1, ub= 1) +
  prior(normal( 0.5 ,  0.03), nlpar = "b", lb=0.001, ub= 0.09)

ts1.00.trc <- brm( bf(nee ~ a * exp(b*TA),a+b ~ 1, nl=TRUE),
               prior = priors.ts1.trc , data = ts1.df[which(ts1.df$I.wl == 0.00 & ts1.df$PAR == 0),], 
               backend = "cmdstanr", iter = 50000, cores =4, seed=101)

summary(ts1.00.trc)
plot(conditional_effects(ts1.00.trc), points=T)

priors.ts1.trc <-  prior(normal(0.2 , 1), nlpar = "a", lb=0.1, ub= 1) +
  prior(normal( 0.5 ,  0.03), nlpar = "b", lb=0.001, ub= 0.09)

ts1.25.trc <-brm( bf(nee ~ a*exp(b*TA) ,a+b ~ 1, nl=TRUE),
              prior = priors.ts1.trc , data = ts1.df[which(ts1.df$I.wl == 0.25 & ts1.df$PAR == 0),], 
              backend = "cmdstanr", iter = 50000, cores =4, seed=101)
summary(ts1.25.trc)
plot(conditional_effects(ts1.25.trc), points=T)


priors.ts1.trc <-  prior(normal(0.2 , 1), nlpar = "a", lb=0.1, ub= 1) +
  prior(normal( 0.5 ,  0.03), nlpar = "b", lb=0.001, ub= 0.09)

ts1.5.trc <-brm( bf(nee ~ a * exp(b*TA), a+b ~ 1, nl=TRUE),
             prior = priors.ts1.trc , data = ts1.df[which(ts1.df$I.wl == 0.5 & ts1.df$PAR == 0),], 
             backend = "cmdstanr", iter = 50000, cores =4, seed=101)

summary(ts1.5.trc)
plot(conditional_effects(ts1.5.trc), points=T)

# se1:

priors.se1.trc <-  prior(normal(0.2 , 1), nlpar = "a", lb=0.1, ub= 1) +
  prior(normal( 0.5 ,  0.03), nlpar = "b", lb=0.001, ub= 0.09)

se1.5.trc <-brm( bf(nee ~ a * exp(b*TA), a+b ~ 1, nl=TRUE),
                 prior = priors.se1.trc , data = SE1.df[which(SE1.df$I.wl == 0.5 & SE1.df$PAR == 0),], 
                 backend = "cmdstanr", iter = 50000, cores =4, seed=101)

summary(se1.5.trc)
plot(conditional_effects(se1.5.trc), points=T)


se1.75.trc <-brm( bf(nee ~ a * exp(b*TA), a+b ~ 1, nl=TRUE),
                 prior = priors.se1.trc , data = SE1.df[which(SE1.df$I.wl == 0.75 & SE1.df$PAR == 0),], 
                 backend = "cmdstanr", iter = 50000, cores =4, seed=101)

summary(se1.5.trc)
plot(conditional_effects(se1.75.trc), points=T)


se1.1.00.trc <-brm( bf(nee ~ a * exp(b*TA), a+b ~ 1, nl=TRUE),
                  prior = priors.se1.trc , data = SE1.df[which(SE1.df$I.wl == 1.00 & SE1.df$PAR == 0),], 
                  backend = "cmdstanr", iter = 50000, cores =4, seed=101)

summary(se1.1.00.trc)
plot(conditional_effects(se1.1.00.trc), points=T)

# TS7

priors.TS7.trc <-  prior(normal(0.2 , 1), nlpar = "a", lb=0.1, ub= 1) +
  prior(normal( 0.5 ,  0.03), nlpar = "b", lb=0.001, ub= 0.09)

TS7.25.trc <-brm( bf(nee ~ a * exp(b*TA), a+b ~ 1, nl=TRUE),
                  prior = priors.TS7.trc , data = TS7.df[which(TS7.df$I.wl == 0.25 & TS7.df$PAR == 0),], 
                  backend = "cmdstanr", iter = 50000, cores =4, seed=101)

summary(TS7.05.trc)
plot(conditional_effects(TS7.05.trc), points=T)

TS7.00.trc <-brm( bf(nee ~ a * exp(b*TA), a+b ~ 1, nl=TRUE),
                  prior = priors.TS7.trc , data = TS7.df[which(TS7.df$I.wl == 0.00 & TS7.df$PAR == 0),], 
                  backend = "cmdstanr", iter = 50000, cores =4, seed=101)

summary(TS7.00.trc)
plot(conditional_effects(TS7.00.trc), points=T)
model <-ts1.00.trc
get_variables(ts1.00.trc)

posterior.data.trc <- function(model, model.name){
  
  a <-model %>%
    spread_draws(b_a_Intercept) %>% select(contains('b_a_Intercept'))
  b <-model %>%
    spread_draws(b_b_Intercept) %>% select(contains('b_b_Intercept'))
  
  p.dist <- cbind( a, b )
  names( p.dist) <- c(paste( model.name, 'a', sep="."), paste( model.name, 'b', sep="."))
  
  return( p.dist )
}

posterior.trc <- cbind( posterior.data.trc(ts1.00.trc, "ts1.00"), 
                        posterior.data.trc(ts1.25.trc, "ts1.25"),
                        posterior.data.trc(ts1.5.trc, "ts1.5"),
                        
                        posterior.data.trc(se1.5.trc, "se1.5"),
                        posterior.data.trc(se1.75.trc, "se1.75"),
                        posterior.data.trc(se1.1.00.trc, "se1.1.00"),
                        posterior.data.trc(TS7.00.trc, "TS7.00"),
                        posterior.data.trc(TS7.25.trc, "TS7.25"))

# make predictions into a data frame:
trc.predictions <- data.frame(TA=seq(0, 40, 2))

trc.predictions$ts1.00 <- predict( ts1.00.trc , newdata=trc.predictions)[,1]
trc.predictions$ts1.00.err <- predict( ts1.00.trc , newdata=trc.predictions)[,2]
trc.predictions$ts1.25 <- predict( ts1.25.trc , newdata=trc.predictions)[,1]
trc.predictions$ts1.25.err <- predict( ts1.25.trc , newdata=trc.predictions)[,2]
trc.predictions$ts1.5 <- predict( ts1.5.trc , newdata=trc.predictions)[,1]
trc.predictions$ts1.5.err <- predict( ts1.5.trc, newdata=trc.predictions)[,2]

trc.predictions$se1.5 <- predict( se1.5.trc , newdata=trc.predictions)[,1]
trc.predictions$se1.5.err <- predict( se1.5.trc , newdata=trc.predictions)[,2]
trc.predictions$se1.75 <- predict( se1.75.trc , newdata=trc.predictions)[,1]
trc.predictions$se1.75.err <- predict( se1.75.trc , newdata=trc.predictions)[,2]
trc.predictions$se1.1.00 <- predict( se1.1.00.trc , newdata=trc.predictions)[,1]
trc.predictions$se1.1.00.err <- predict( se1.1.00.trc , newdata=trc.predictions)[,2]

trc.predictions$TS7.25 <- predict( TS7.25.trc , newdata=trc.predictions)[,1]
trc.predictions$TS7.25.err <- predict( TS7.25.trc , newdata=trc.predictions)[,2]
trc.predictions$TS7.00 <- predict( TS7.00.trc , newdata=trc.predictions)[,1]
trc.predictions$TS7.00.err <- predict( TS7.00.trc , newdata=trc.predictions)[,2]

# Save all models, posterior data and curve data for figure development####
# files: lrc.predictions, posterior.LRC 
# models:  'ts1.00, ts1.25, ts1.5, se1.5, se1.75, se1.1.00, TS7.25, TS7.00'

save(trc.predictions, posterior.TRC, lrc.predictions, posterior.LRC ,
     ts1.00, ts1.25, ts1.5, se1.5, se1.75, se1.1.00, TS7.25, TS7.00,
     ts1.00.trc, ts1.25.trc, ts1.5.trc, se1.5.trc, se1.75.trc, se1.1.00.trc, 
     TS7.25.trc, TS7.00.trc, file= "data/WZ_NLM_RESULTS.RDATA")


# Figures ####

load("data/WZ_NLM_RESULTS.RDATA")

lrc.reformat <- function(df, model, model.err, group){
  df2 <- df[,c( 'PAR', model, model.err)]
  df2$group <- group
  names(df2) <- c("PAR", "Estimate", "Error", "group")
  return (df2)
}

lrc.predictions.ts1 <- rbind( lrc.reformat(lrc.predictions, 'ts1.00', 'ts1.00.err', "0%" ), 
                              lrc.reformat(lrc.predictions, 'ts1.25', 'ts1.25.err', "0-25%"),
                              lrc.reformat(lrc.predictions, 'ts1.5', 'ts1.5.err', "25-50%" ))

lrc.predictions.se1 <- rbind( lrc.reformat(lrc.predictions, 'se1.5', 'se1.5.err', "25-50%" ), 
                              lrc.reformat(lrc.predictions, 'se1.75', 'se1.75.err', "50-75%"),
                              lrc.reformat(lrc.predictions, 'se1.1.00', 'se1.1.00.err', "75-100%" ))

lrc.predictions.TS7 <- rbind( lrc.reformat(lrc.predictions, 'TS7.00', 'TS7.00.err', "0%" ), 
                              lrc.reformat(lrc.predictions, 'TS7.25', 'TS7.25.err', "0-25%"))


ts1.lrc.p <- ggplot( data =lrc.predictions.ts1 ) + 
  geom_line( aes( x= PAR, y =Estimate, group=group, linetype=group), colour= "#000099", size=1) +
  geom_ribbon(aes( x= PAR, ymin = Estimate-Error, ymax= Estimate +Error, group = group), alpha=0.1 , fill="#000099") +
  ylab(expression(paste('NEE'[day], ' (', mu, 'mol m'^2, 's'^-1, ')' ))) +
  xlab(expression(paste( 'PAR (', mu, 'mol m'^2, 's'^-1, ')' ))) + ylim(-9, 13)+
  scale_linetype_manual(values = c("0%" = 1,"0-25%" = 2, "25-50%"=3 , "50-75%"=4, "75-100%"=5)) +
  scale_linetype_discrete(limits=c("0%","0-25%", "25-50%", "50-75%", "75-100%" ), name="% Submergence") +
  theme(text = element_text(size=20),
        legend.position = c(.95, .99),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(3, 3, 3, 3),
        legend.background = element_rect(fill="transparent"))


se1.lrc.p <- ggplot( data =lrc.predictions.se1 ) + 
  geom_line( aes( x= PAR, y =Estimate, group=group, linetype=group), colour="#fab255", size=1) +
  geom_ribbon(aes( x= PAR, ymin = Estimate-Error, ymax= Estimate +Error, group = group), alpha=0.1 , fill="#fab255") +
  ylab(expression(paste('NEE'[day], ' (', mu, 'mol m'^2, 's'^-1, ')' ))) +
  xlab(expression(paste( 'PAR (', mu, 'mol m'^2, 's'^-1, ')' ))) + ylim(-9, 13)+
  scale_linetype_manual(values = c("0%" = 1,"0-25%" = 2, "25-50%"=3 , "50-75%"=4, "75-100%"=5)) +
  scale_linetype_discrete(limits=c("0%","0-25%", "25-50%", "50-75%", "75-100%" ), name="% Submergence")+
  theme(text = element_text(size=20),
        legend.position = 'none')

ts7.lrc.p <- ggplot( data =lrc.predictions.TS7 ) + 
  geom_line( aes( x= PAR, y =Estimate, group=group, linetype=group), colour="#43b284", size=1) +
  geom_ribbon(aes( x= PAR, ymin = Estimate-Error, ymax= Estimate +Error, group = group), alpha=0.1 , fill="#43b284") +
  ylab(expression(paste('NEE'[day], ' (', mu, 'mol m'^2, 's'^-1, ')' ))) +
  xlab(expression(paste( 'PAR (', mu, 'mol m'^2, 's'^-1, ')' ))) + ylim(-9, 13)+
  scale_linetype_manual(values = c("0%" = 1,"0-25%" = 2, "25-50%"=3 , "50-75%"=4, "75-100%"=5)) +
  scale_linetype_discrete(limits=c("0%","0-25%", "25-50%", "50-75%", "75-100%" ), name="% Submergence")+
  theme(text = element_text(size=20),
        legend.position = 'none')

# Figures by site:

ts1.a1.p <- ggplot(data=posterior.LRC) + geom_density(aes(x=ts1.00.a1, y = after_stat(scaled)),linetype=1, colour="#000099", fill="#000099", alpha=0.2) + 
  geom_density(aes(x=ts1.25.a1, y = after_stat(scaled)),linetype=2, colour="#000099", alpha=0.2, fill="#000099") + 
  geom_density(aes(x=ts1.5.a1, y = after_stat(scaled)),linetype=3, colour="#000099", alpha=0.2, fill="#000099") + xlab(expression(alpha)) + ylab("Density") +
  theme(text = element_text(size=20),
        legend.position = 'none')

ts1.ax.p <- ggplot(data=posterior.LRC) + geom_density(aes(x=ts1.00.ax, y = after_stat(scaled)),linetype=1, colour="#000099", fill="#000099", alpha=0.2) + 
  geom_density(aes(x=ts1.25.ax, y = after_stat(scaled)),linetype=2, colour="#000099", alpha=0.2, fill="#000099") + 
  geom_density(aes(x=ts1.5.ax, y = after_stat(scaled)),linetype=3, colour="#000099", alpha=0.2, fill="#000099")+ xlim(-9, -4)+ xlab(expression(P[max])) + ylab("Density") +
  theme(text = element_text(size=20),
        legend.position = 'none')


ts1.r.p <- ggplot(data=posterior.LRC) + geom_density(aes(x=ts1.00.r, y = after_stat(scaled)),linetype=1, colour="#000099", fill="#000099", alpha=0.2) + 
  geom_density(aes(x=ts1.25.r, y = after_stat(scaled)),linetype=2, colour="#000099", alpha=0.2, fill="#000099") + 
  geom_density(aes(x=ts1.5.r, y = after_stat(scaled)),linetype=3, colour="#000099", alpha=0.2, fill="#000099")+ xlim(1.8, 2.25)+ xlab(expression(R[eco])) + ylab("Density") +
  theme(text = element_text(size=20),
        legend.position = 'none')



se1.a1.p <- ggplot(data=posterior.LRC) + geom_density(aes(x=se1.5.a1, y = after_stat(scaled)),linetype=1, colour="#fab255", fill="#fab255", alpha=0.2) + 
  geom_density(aes(x=se1.75.a1, y = after_stat(scaled)),linetype=2, colour="#fab255", alpha=0.2, fill="#fab255") + 
  geom_density(aes(x=se1.1.00.a1, y = after_stat(scaled)),linetype=3, colour="#fab255", alpha=0.2, fill="#fab255") + xlim(-0.25, 0) + xlab(expression(alpha)) + ylab("Density") +
  theme(text = element_text(size=20),
        legend.position = 'none')


se1.ax.p <- ggplot(data=posterior.LRC) + geom_density(aes(x=se1.5.ax, y = after_stat(scaled)),linetype=1, colour="#fab255", fill="#fab255", alpha=0.2) + 
  geom_density(aes(x=se1.75.ax, y = after_stat(scaled)),linetype=2, colour="#fab255", alpha=0.2, fill="#fab255") + 
  geom_density(aes(x=se1.1.00.ax, y = after_stat(scaled)),linetype=3, colour="#fab255", alpha=0.2, fill="#fab255")+ xlim(-5, -2.5) + xlab(expression(P[max])) + ylab("Density")+
  theme(text = element_text(size=20),
        legend.position = 'none')


se1.r.p <- ggplot(data=posterior.LRC) + geom_density(aes(x=se1.5.r, y = after_stat(scaled)),linetype=1, colour="#fab255", fill="#fab255", alpha=0.2) + 
  geom_density(aes(x=se1.75.r, y = after_stat(scaled)),linetype=2, colour="#fab255", alpha=0.2, fill="#fab255") + 
  geom_density(aes(x=se1.1.00.r, y = after_stat(scaled)),linetype=3, colour="#fab255", alpha=0.2, fill="#fab255")+ xlim(1.8, 2.25)+ xlab(expression(R[eco])) + ylab("Density")+
  theme(text = element_text(size=20),
        legend.position = 'none')



TS7.a1.p <- ggplot(data=posterior.LRC) + geom_density(aes(x=TS7.00.a1, y = after_stat(scaled)),linetype=1, colour="#43b284", fill="#43b284", alpha=0.2) + 
  geom_density(aes(x=TS7.25.a1, y = after_stat(scaled)),linetype=2, colour="#43b284", alpha=0.2, fill="#43b284") + xlab(expression(alpha)) + ylab("Density")+
  theme(text = element_text(size=20),
        legend.position = 'none')


TS7.ax.p <- ggplot(data=posterior.LRC) + geom_density(aes(x=TS7.00.ax, y = after_stat(scaled)),linetype=1, colour="#43b284", fill="#43b284", alpha=0.2) + 
  geom_density(aes(x=TS7.25.ax, y = after_stat(scaled)),linetype=2, colour="#43b284", alpha=0.2, fill="#43b284") + xlim(-11.5, -6)+ xlab(expression(P[max])) + ylab("Density") +
  theme(text = element_text(size=20),
        legend.position = 'none')


TS7.r.p <- ggplot(data=posterior.LRC) + geom_density(aes(x=TS7.00.r, y = after_stat(scaled)),linetype=1, colour="#43b284", fill="#43b284", alpha=0.2) + 
  geom_density(aes(x=TS7.25.r, y = after_stat(scaled)),linetype=2, colour="#43b284", alpha=0.2, fill="#43b284") + xlim(1.7, 2.5)+ xlab(expression(R[eco])) + ylab("Density") +
  theme(text = element_text(size=20),
        legend.position = 'none')


png('figures/LRC.png',width = 1000, height = 1100,)
ggarrange( ts1.lrc.p, se1.lrc.p, ts7.lrc.p,
              ts1.a1.p, se1.a1.p, TS7.a1.p,
              ts1.ax.p, se1.ax.p, TS7.ax.p,
              ts1.r.p , se1.r.p, TS7.r.p, 
           labels =c('A', 'B', 'C',
                     'D', 'E', 'F',
                     'G', 'H', 'I',
                     'J', 'K', 'L'),nrow=4, ncol=3)
dev.off()



# Figures by % Submergence:

ggplot(data=posterior.LRC) + geom_density(aes(x=ts1.00.a1, y = after_stat(scaled)),linetype=1, colour="#000099", fill="#000099", alpha=0.2)+ 
  geom_density(aes(x=se1.1.00.a1, y = after_stat(scaled)),linetype=2, colour="#fab255", alpha=0.2, fill="#fab255")+ 
  geom_density(aes(x=TS7.00.a1, y = after_stat(scaled)),linetype=3, colour="#43b284", alpha=0.2, fill="#43b284") + xlim(-0.25, 0)

# Create a dataset for violin plots
names(posterior.LRC)
df <- posterior.LRC
model.parm <- 'ts1.00.a1'
sub.site.LRC <- function(df,model.parm, site, cc,parm){
  df1 <- data.frame(df[,c(model.parm)])
  df1$site <- site
  df1$cc <- cc
  df1$parm <- parm
  names(df1) <- c('Estimate', 'site', 'cc', 'parm')
  return(df1)
  
}


lrc.vio.df <- rbind(sub.site.LRC(posterior.LRC,'ts1.00.a1', 'ts1', '0%','a1' ),
                    sub.site.LRC(posterior.LRC,'ts1.25.a1', 'ts1', '0-25%','a1'),
                    sub.site.LRC(posterior.LRC,'ts1.5.a1', 'ts1', '25-50%','a1'),
                    sub.site.LRC(posterior.LRC,'ts1.00.ax', 'ts1', '0%','ax' ),
                    sub.site.LRC(posterior.LRC,'ts1.25.ax', 'ts1', '0-25%','ax'),
                    sub.site.LRC(posterior.LRC,'ts1.5.ax', 'ts1', '25-50%','ax'),
                    sub.site.LRC(posterior.LRC,'ts1.00.r', 'ts1', '0%','r' ),
                    sub.site.LRC(posterior.LRC,'ts1.25.r', 'ts1', '0-25%','r'),
                    sub.site.LRC(posterior.LRC,'ts1.5.r', 'ts1', '25-50%','r'),
                    
                    sub.site.LRC(posterior.LRC,'se1.5.a1', 'se1', '25-50%','a1' ),
                    sub.site.LRC(posterior.LRC,'se1.75.a1', 'se1', '50-75%','a1'),
                    sub.site.LRC(posterior.LRC,'se1.1.00.a1', 'se1', '75-100%','a1'),
                    sub.site.LRC(posterior.LRC,'se1.5.ax', 'se1', '25-50%','ax' ),
                    sub.site.LRC(posterior.LRC,'se1.75.ax', 'se1', '50-75%','ax'),
                    sub.site.LRC(posterior.LRC,'se1.1.00.ax', 'se1', '75-100%','ax'),
                    sub.site.LRC(posterior.LRC,'se1.5.r', 'se1', '25-50%','r' ),
                    sub.site.LRC(posterior.LRC,'se1.75.r', 'se1', '50-75%','r'),
                    sub.site.LRC(posterior.LRC,'se1.1.00.r', 'se1', '75-100%','r'))


ggplot(lrc.vio.df[which(lrc.vio.df$parm == "a1"),], aes(x=cc, y=Estimate, color=site)) + 
  geom_violin()

# figure by cc:

lrc.predictions.ts1$site <- 'TS1'
lrc.predictions.se1$site <- 'SE1'
lrc.predictions.TS7$site <- 'TS7'

cc.0 <- rbind(lrc.predictions.ts1[which( lrc.predictions.ts1$group == '0%'),],
              lrc.predictions.TS7[which( lrc.predictions.TS7$group == '0%'),])

cc.25 <- rbind(lrc.predictions.ts1[which( lrc.predictions.ts1$group == '0-25%'),],
               lrc.predictions.se1[which( lrc.predictions.se1$group == '0-25%'),],
              lrc.predictions.TS7[which( lrc.predictions.TS7$group == '0-25%'),])

cc.50 <- rbind(lrc.predictions.ts1[which( lrc.predictions.ts1$group == '25-50%'),],
               lrc.predictions.se1[which( lrc.predictions.se1$group == '25-50%'),],
               lrc.predictions.TS7[which( lrc.predictions.TS7$group == '25-50%'),])

cc.75 <- rbind(lrc.predictions.ts1[which( lrc.predictions.ts1$group == '50-75%'),],
               lrc.predictions.se1[which( lrc.predictions.se1$group == '50-75%'),],
               lrc.predictions.TS7[which( lrc.predictions.TS7$group == '50-75%'),])

cc.100 <- rbind(lrc.predictions.ts1[which( lrc.predictions.ts1$group == '75-100%'),],
               lrc.predictions.se1[which( lrc.predictions.se1$group == '75-100%'),],
               lrc.predictions.TS7[which( lrc.predictions.TS7$group == '75-100%'),])




cc0.lrc.p <- ggplot( data =cc.0 ) + 
  geom_line( aes( x= PAR, y =Estimate, color=site, linetype=group), size=1) +
  geom_ribbon(aes( x= PAR, ymin = Estimate-Error, ymax= Estimate +Error, fill=site), alpha=0.15 ) +
  ylab(expression(paste('NEE'[day], ' (', mu, 'mol m'^-2, 's'^-1, ')' ))) +
  xlab(expression(paste( 'PAR (', mu, 'mol m'^-2, 's'^-1, ')' ))) + ylim(-9, 5) +
  scale_color_manual(values = c("TS1" = "#000099","SE1" = "#fab255", "TS7"="#43b284"))+
  scale_fill_manual(values = c("TS1" = "#000099","SE1" = "#fab255", "TS7"="#43b284")) +
  scale_linetype_manual(values = c("0%" = 1,"0-25%" = 2, "25-50%"=3 , "50-75%"=4, "75-100%"=5)) +
  scale_linetype_discrete(limits=c("0%","0-25%", "25-50%", "50-75%", "75-100%" ), name="% Submergence")+
  theme(legend.position="none",
        panel.background = element_rect("white", "white", linetype=1, color="black"),
        panel.grid.major = element_line( linetype = 'solid', colour = "grey90"),
        text = element_text(size=18))


cc25.lrc.p <- ggplot( data =cc.25 ) + 
  geom_line( aes( x= PAR, y =Estimate, group=site, color=site, linetype=group), size=1) +
  geom_ribbon(aes( x= PAR, ymin = Estimate-Error, ymax= Estimate +Error, fill=site), alpha=0.15 ) +
  ylab(expression(paste('NEE'[day], ' (', mu, 'mol m'^-2, 's'^-1, ')' ))) +
  xlab(expression(paste( 'PAR (', mu, 'mol m'^-2, 's'^-1, ')' ))) + ylim(-9, 5) +
  scale_color_manual(values = c("TS1" = "#000099","SE1" = "#fab255", "TS7"="#43b284"))+
  scale_fill_manual(values = c("TS1" = "#000099","SE1" = "#fab255", "TS7"="#43b284" )) +
  scale_linetype_manual(values = c("0%" = 1,"0-25%" = 2, "25-50%"=3 , "50-75%"=4, "75-100%"=5)) +
  scale_linetype_discrete(limits=c("0%","0-25%", "25-50%", "50-75%", "75-100%" ), name="% Submergence")+
  theme(legend.position="none",
        panel.background = element_rect("white", "white", linetype=1, color="black"),
        panel.grid.major = element_line( linetype = 'solid', colour = "grey90"),
        text = element_text(size=18))


cc50.lrc.p <- ggplot( data =cc.50 ) + 
  geom_line( aes( x= PAR, y =Estimate, group=site, color=site, linetype=group), size=1) +
  geom_ribbon(aes( x= PAR, ymin = Estimate-Error, ymax= Estimate +Error, fill=site), alpha=0.15) +
  ylab(expression(paste('NEE'[day], ' (', mu, 'mol m'^-2, 's'^-1, ')' ))) +
  xlab(expression(paste( 'PAR (', mu, 'mol m'^-2, 's'^-1, ')' ))) + ylim(-9, 5) +
  scale_color_manual(values = c("TS1" = "#000099","SE1" = "#fab255", "TS7"="#43b284" ))+
  scale_fill_manual(values = c("TS1" = "#000099","SE1" = "#fab255", "TS7"="#43b284")) +
  scale_linetype_manual(values = c("0%" = 1,"0-25%" = 2, "25-50%"=3 , "50-75%"=4, "75-100%"=5)) +
  scale_linetype_discrete(limits=c("0%","0-25%", "25-50%", "50-75%", "75-100%" ), name="% Submergence")+
  theme(legend.position="none",
        panel.background = element_rect("white", "white", linetype=1, color="black"),
        panel.grid.major = element_line( linetype = 'solid', colour = "grey90"),
        text = element_text(size=18))

cc75.lrc.p <- ggplot( data =cc.75 ) + 
  geom_line( aes( x= PAR, y =Estimate, group=site, color=site, linetype=group), size=1) +
  geom_ribbon(aes( x= PAR, ymin = Estimate-Error, ymax= Estimate +Error, fill=site), alpha=0.15 ) +
  ylab(expression(paste('NEE'[day], ' (', mu, 'mol m'^-2, 's'^-1, ')' ))) +
  xlab(expression(paste( 'PAR (', mu, 'mol m'^-2, 's'^-1, ')' ))) + ylim(-9, 5) +
  scale_color_manual(values = c("TS1" = "#000099","SE1" = "#fab255", "TS7"="#43b284"))+
  scale_fill_manual(values = c("TS1" = "#000099","SE1" = "#fab255", "TS7"="#43b284"))+
  scale_linetype_manual(values = c("0%" = 1,"0-25%" = 2, "25-50%"=3 , "50-75%"=4, "75-100%"=5)) +
  scale_linetype_discrete(limits=c("0%","0-25%", "25-50%", "50-75%", "75-100%" ), name="% Submergence")+
  theme(legend.position="none",
        panel.background = element_rect("white", "white", linetype=1, color="black"),
        panel.grid.major = element_line( linetype = 'solid', colour = "grey90"),
        text = element_text(size=18))

cc100.lrc.p <- ggplot( data =cc.100 ) + 
  geom_line( aes( x= PAR, y =Estimate, group=site, color=site, linetype=group), size=1) +
  geom_ribbon(aes( x= PAR, ymin = Estimate-Error, ymax= Estimate +Error, fill=site), alpha=0.15 ) +
  ylab(expression(paste('NEE'[day], ' (', mu, 'mol m'^-2, 's'^-1, ')' ))) +
  xlab(expression(paste( 'PAR (', mu, 'mol m'^-2, 's'^-1, ')' ))) + ylim(-9, 5) +
  scale_color_manual(values = c("TS1" = "#000099","SE1" = "#fab255", "TS7"="#43b284" ))+
  scale_fill_manual(values = c("TS1" = "#000099","SE1" = "#fab255", "TS7"="#43b284"))+
  scale_linetype_manual(values = c("0%" = 1,"0-25%" = 2, "25-50%"=3 , "50-75%"=4, "75-100%"=5)) +
  scale_linetype_discrete(limits=c("0%","0-25%", "25-50%", "50-75%", "75-100%" ), name="% Submergence")+
  theme(legend.position="none",
        panel.background = element_rect("white", "white", linetype=1, color="black"),
        panel.grid.major = element_line( linetype = 'solid', colour = "grey90"),
        text = element_text(size=18))


 
cc.df <- rbind(cc.0,cc.25,cc.50,cc.75,cc.100)

unique(cc.df$site)
cc.df$group <- factor(cc.df$group, levels = c("0%", "0-25%", "25-50%", "50-75%","75-100%") )
cc.df$site <- factor(cc.df$site, levels=c("TS1", "SE1","TS7"))



legend.p <- ggplot( data = cc.df ) + 
  geom_line( aes( x= PAR, y =Estimate, color=site, linetype=group), size=1) +
  geom_ribbon(aes( x= PAR, ymin = Estimate-Error, ymax= Estimate +Error, fill=site), alpha=0.1 ) +
  ylab(expression(paste('NEE'[day], ' (', mu, 'mol m'^-2, 's'^-1, ')' ))) +
  xlab(expression(paste( 'PAR (', mu, 'mol m'^2, 's'^-1, ')' ))) + ylim(-9, 9) +
  scale_color_manual(values = c("TS1" = "#000099","SE1" = "#fab255", "TS7"="#43b284" ))+
  scale_fill_manual(values = c("TS1" = "#000099","SE1" = "#fab255", "TS7"="#43b284"))+
  theme(text = element_text(size=20),legend.direction='vertical', 
        legend.text=element_text(size=19)) + 
  guides(color=guide_legend(title="Site"),
         linetype=guide_legend(title="% Submergence"),
         fill=guide_legend(title="Site"))


# Extract the legend. Returns a gtable
leg <- get_legend(legend.p)

# Convert to a ggplot and print
as_ggplot(leg)


png('figures/LRC_Site_CC.png',width = 900, height = 625,)
ggarrange( as_ggplot(leg), cc0.lrc.p, cc25.lrc.p,
           cc50.lrc.p, cc75.lrc.p,
           cc100.lrc.p, font.label = list(size = 18),
           labels =c('','A', 'B', 'C',
                     'D', 'E'),nrow=2, ncol=3)
dev.off()

# Temperature Response Curve Figures: ####
# # Temperature Response Curve Figures: ####

trc.reformat <- function(df, model, model.err, group){
  df2 <- df[,c( 'TA', model, model.err)]
  df2$group <- group
  names(df2) <- c("PAR", "Estimate", "Error", "group")
  return (df2)
}

trc.predictions.ts1 <- rbind( trc.reformat(trc.predictions, 'ts1.00', 'ts1.00.err', "0%" ), 
                              trc.reformat(trc.predictions, 'ts1.25', 'ts1.25.err', "0-25%"),
                              trc.reformat(trc.predictions, 'ts1.5', 'ts1.5.err', "25-50%" ))

trc.predictions.se1 <- rbind( trc.reformat(trc.predictions, 'se1.5', 'se1.5.err', "25-50%" ), 
                              trc.reformat(trc.predictions, 'se1.75', 'se1.75.err', "50-75%"),
                              trc.reformat(trc.predictions, 'se1.1.00', 'se1.1.00.err', "75-100%" ))

trc.predictions.TS7 <- rbind( trc.reformat(trc.predictions, 'TS7.00', 'TS7.00.err', "0%" ), 
                              trc.reformat(trc.predictions, 'TS7.25', 'TS7.25.err', "0-25%"))


ts1.trc.p <- ggplot( data =trc.predictions.ts1 ) + 
  geom_line( aes( x= PAR, y =Estimate, group=group, linetype=group), colour="#000099") +
  geom_ribbon(aes( x= PAR, ymin = Estimate-Error, ymax= Estimate +Error, group = group), alpha=0.1 , fill="#000099") +
  ylab(expression(paste('NEE'[night], ' (', mu, 'mol m'^2, 's'^-1, ')' ))) +
  xlab(expression(paste( 'TA ('^ degree, 'C)' ))) + ylim(-4, 9) +
  scale_linetype_manual(values = c("0%" = 1,"0-25%" = 2, "25-50%"=3 , "50-75%"=4, "75-100%"=5)) +
  scale_linetype_discrete(limits=c("0%","0-25%", "25-50%", "50-75%", "75-100%" ), name="% Submergence") +
  theme(text = element_text(size=20),
        legend.position = c(.75, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(3, 3, 3, 3),
        legend.background = element_rect(fill="transparent"))


se1.trc.p <- ggplot( data =trc.predictions.se1 ) + 
  geom_line( aes( x= PAR, y =Estimate, group=group, linetype=group), colour="#fab255") +
  geom_ribbon(aes( x= PAR, ymin = Estimate-Error, ymax= Estimate +Error, group = group), alpha=0.1 , fill="#fab255") +
  ylab(expression(paste('NEE'[night], ' (', mu, 'mol m'^2, 's'^-1, ')' ))) +
  xlab(expression(paste( 'TA ('^ degree, 'C)' ))) + ylim(-4, 9) +
  scale_linetype_manual(values = c("0%" = 1,"0-25%" = 2, "25-50%"=3 , "50-75%"=4, "75-100%"=5)) +
  scale_linetype_discrete(limits=c("0%","0-25%", "25-50%", "50-75%", "75-100%" ), name="% Submergence") +
  theme(text = element_text(size=20),
        legend.position = 'none')

ts7.trc.p <- ggplot( data =trc.predictions.TS7 ) + 
  geom_line( aes( x= PAR, y =Estimate, group=group, linetype=group), colour="#43b284") +
  geom_ribbon(aes( x= PAR, ymin = Estimate-Error, ymax= Estimate +Error, group = group), alpha=0.1 , fill="#43b284") +
  ylab(expression(paste('NEE'[night], ' (', mu, 'mol m'^2, 's'^-1, ')' ))) +
  xlab(expression(paste( 'TA ('^ degree, 'C)' ))) + ylim(-4, 9) +
  scale_linetype_manual(values = c("0%" = 1,"0-25%" = 2, "25-50%"=3 , "50-75%"=4, "75-100%"=5)) +
  scale_linetype_discrete(limits=c("0%","0-25%", "25-50%", "50-75%", "75-100%" ), name="% Submergence") +
  theme(text = element_text(size=20),
        legend.position = 'none')


# Figures by site:
ts1.a.p <- ggplot(data=posterior.trc) + geom_density(aes(x=ts1.00.a, y = after_stat(scaled)),linetype=1, colour="#000099", fill="#000099", alpha=0.2) + 
  geom_density(aes(x=ts1.25.a, y = after_stat(scaled)),linetype=2, colour="#000099", alpha=0.2, fill="#000099") + 
  geom_density(aes(x=ts1.5.a, y = after_stat(scaled)),linetype=3, colour="#000099", alpha=0.2, fill="#000099") + xlab(expression(R[eco])) + ylab("Density")+
  theme(text = element_text(size=20))

ts1.b.p <- ggplot(data=posterior.trc) + geom_density(aes(x=ts1.00.b, y = after_stat(scaled)),linetype=1, colour="#000099", fill="#000099", alpha=0.2) + 
  geom_density(aes(x=ts1.25.b, y = after_stat(scaled)),linetype=2, colour="#000099", alpha=0.2, fill="#000099") + 
  geom_density(aes(x=ts1.5.b, y = after_stat(scaled)),linetype=3, colour="#000099", alpha=0.2, fill="#000099")+ xlab(expression(b)) + ylab("Density")+
  theme(text = element_text(size=20))


se1.a.p <- ggplot(data=posterior.trc) + geom_density(aes(x=se1.5.a, y = after_stat(scaled)),linetype=1, colour="#fab255", fill="#fab255", alpha=0.2) + 
  geom_density(aes(x=se1.75.a, y = after_stat(scaled)),linetype=2, colour="#fab255", alpha=0.2, fill="#fab255") + 
  geom_density(aes(x=se1.1.00.a, y = after_stat(scaled)),linetype=3, colour="#fab255", alpha=0.2, fill="#fab255") + xlab(expression(R[eco])) + ylab("Density")+
  theme(text = element_text(size=20))


se1.b.p <- ggplot(data=posterior.trc) + geom_density(aes(x=se1.5.b, y = after_stat(scaled)),linetype=1, colour="#fab255", fill="#fab255", alpha=0.2) + 
  geom_density(aes(x=se1.75.b, y = after_stat(scaled)),linetype=2, colour="#fab255", alpha=0.2, fill="#fab255") + 
  geom_density(aes(x=se1.1.00.b, y = after_stat(scaled)),linetype=3, colour="#fab255", alpha=0.2, fill="#fab255")+ xlab(expression(b)) + ylab("Density")+
  theme(text = element_text(size=20))


TS7.a.p <- ggplot(data=posterior.trc) + geom_density(aes(x=TS7.00.a, y = after_stat(scaled)),linetype=1, colour="#43b284", fill="#43b284", alpha=0.2) + 
  geom_density(aes(x=TS7.25.a, y = after_stat(scaled)),linetype=2, colour="#43b284", alpha=0.2, fill="#43b284") + xlab(expression(R[eco])) + ylab("Density")+
  theme(text = element_text(size=20))


TS7.b.p <- ggplot(data=posterior.trc) + geom_density(aes(x=TS7.00.b, y = after_stat(scaled)),linetype=1, colour="#43b284", fill="#43b284", alpha=0.2) + 
  geom_density(aes(x=TS7.25.b, y = after_stat(scaled)),linetype=2, colour="#43b284", alpha=0.2, fill="#43b284") + xlab(expression(b)) + ylab("Density")+
  theme(text = element_text(size=20))


png('figures/trc.png',width = 1000, height = 1000,)
ggarrange( ts1.trc.p, se1.trc.p, ts7.trc.p,
           ts1.a.p, se1.a.p, TS7.a.p,
           ts1.b.p, se1.b.p, TS7.b.p,
           labels =c('A', 'B', 'C',
                     'D', 'E', 'F',
                     'G', 'H', 'I'),nrow=3, ncol=3)
dev.off()

# Figures by % Submergence:

trc.predictions.ts1$site <- 'TS1'
trc.predictions.se1$site <- 'SE1'
trc.predictions.TS7$site <- 'TS7'

cc.0.trc <- rbind(trc.predictions.ts1[which( trc.predictions.ts1$group == '0%'),],
                  trc.predictions.TS7[which( trc.predictions.TS7$group == '0%'),])

cc.25.trc <- rbind(trc.predictions.ts1[which( trc.predictions.ts1$group == '0-25%'),],
                   trc.predictions.se1[which( trc.predictions.se1$group == '0-25%'),],
                   trc.predictions.TS7[which( trc.predictions.TS7$group == '0-25%'),])

cc.50.trc <- rbind(trc.predictions.ts1[which( trc.predictions.ts1$group == '25-50%'),],
                   trc.predictions.se1[which( trc.predictions.se1$group == '25-50%'),],
                   trc.predictions.TS7[which( trc.predictions.TS7$group == '25-50%'),])

cc.75.trc <- rbind(trc.predictions.ts1[which( trc.predictions.ts1$group == '50-75%'),],
                   trc.predictions.se1[which( trc.predictions.se1$group == '50-75%'),],
                   trc.predictions.TS7[which( trc.predictions.TS7$group == '50-75%'),])

cc.100.trc <- rbind(trc.predictions.ts1[which( trc.predictions.ts1$group == '75-100%'),],
                    trc.predictions.se1[which( trc.predictions.se1$group == '75-100%'),],
                    trc.predictions.TS7[which( trc.predictions.TS7$group == '75-100%'),])


cc0.trc.p <- ggplot( data =cc.0.trc ) + 
  geom_line( aes( x= PAR, y =Estimate, color=site, linetype=group), size=1) +
  geom_ribbon(aes( x= PAR, ymin = Estimate-Error, ymax= Estimate +Error, fill=site), alpha=0.15 ) +
  ylab(expression(paste('NEE'[night], ' (', mu, 'mol m'^-2, 's'^-1, ')' ))) +
  xlab(expression(paste( 'TA ('^ degree, 'C)' ))) + ylim(-4, 10) +
  scale_color_manual(values = c("TS1" = "#000099","SE1" = "#fab255", "TS7"="#43b284"))+
  scale_fill_manual(values = c("TS1" = "#000099","SE1" = "#fab255", "TS7"="#43b284")) +
  scale_linetype_manual(values = c("0%" = 1,"0-25%" = 2, "25-50%"=3 , "50-75%"=4, "75-100%"=5)) +
  scale_linetype_discrete(limits=c("0%","0-25%", "25-50%", "50-75%", "75-100%" ), name="% Submergence")+
  theme(legend.position="none",
        panel.background = element_rect("white", "white", linetype=1, color="black"),
        panel.grid.major = element_line( linetype = 'solid', colour = "grey90"),
        text = element_text(size=18))


cc25.trc.p <- ggplot( data =cc.25.trc ) + 
  geom_line( aes( x= PAR, y =Estimate, group=site, color=site, linetype=group), size=1) +
  geom_ribbon(aes( x= PAR, ymin = Estimate-Error, ymax= Estimate +Error, fill=site), alpha=0.15 ) +
  ylab(expression(paste('NEE'[night], ' (', mu, 'mol m'^-2, 's'^-1, ')' ))) +
  xlab(expression(paste( 'TA ('^ degree, 'C)' ))) + ylim(-4, 10) +
  scale_color_manual(values = c("TS1" = "#000099","SE1" = "#fab255", "TS7"="#43b284"))+
  scale_fill_manual(values = c("TS1" = "#000099","SE1" = "#fab255", "TS7"="#43b284" )) +
  scale_linetype_manual(values = c("0%" = 1,"0-25%" = 2, "25-50%"=3 , "50-75%"=4, "75-100%"=5)) +
  scale_linetype_discrete(limits=c("0%","0-25%", "25-50%", "50-75%", "75-100%" ), name="% Submergence")+
  theme(legend.position="none",
        panel.background = element_rect("white", "white", linetype=1, color="black"),
        panel.grid.major = element_line( linetype = 'solid', colour = "grey90"),
        text = element_text(size=18))


cc50.trc.p <- ggplot( data =cc.50.trc ) + 
  geom_line( aes( x= PAR, y =Estimate, group=site, color=site, linetype=group), size = 1) +
  geom_ribbon(aes( x= PAR, ymin = Estimate-Error, ymax= Estimate +Error, fill=site), alpha=0.15) +
  ylab(expression(paste('NEE'[night], ' (', mu, 'mol m'^-2, 's'^-1, ')' ))) +
  xlab(expression(paste( 'TA ('^ degree, 'C)' ))) + ylim(-4, 10) +
  scale_color_manual(values = c("TS1" = "#000099","SE1" = "#fab255", "TS7"="#43b284" ))+
  scale_fill_manual(values = c("TS1" = "#000099","SE1" = "#fab255", "TS7"="#43b284")) +
  scale_linetype_manual(values = c("0%" = 1,"0-25%" = 2, "25-50%"=3 , "50-75%"=4, "75-100%"=5)) +
  scale_linetype_discrete(limits=c("0%","0-25%", "25-50%", "50-75%", "75-100%" ), name="% Submergence")+
  theme(legend.position="none",
        panel.background = element_rect("white", "white", linetype=1, color="black"),
        panel.grid.major = element_line( linetype = 'solid', colour = "grey90"),
        text = element_text(size=18))

cc75.trc.p <- ggplot( data =cc.75.trc ) + 
  geom_line( aes( x= PAR, y =Estimate, group=site, color=site, linetype=group), size = 1) +
  geom_ribbon(aes( x= PAR, ymin = Estimate-Error, ymax= Estimate +Error, fill=site), alpha=0.15 ) +
  ylab(expression(paste('NEE'[night], ' (', mu, 'mol m'^-2, 's'^-1, ')' ))) +
  xlab(expression(paste( 'TA ('^ degree, 'C)' ))) + ylim(-4, 10) +
  scale_color_manual(values = c("TS1" = "#000099","SE1" = "#fab255", "TS7"="#43b284"))+
  scale_fill_manual(values = c("TS1" = "#000099","SE1" = "#fab255", "TS7"="#43b284"))+
  scale_linetype_manual(values = c("0%" = 1,"0-25%" = 2, "25-50%"=3 , "50-75%"=4, "75-100%"=5)) +
  scale_linetype_discrete(limits=c("0%","0-25%", "25-50%", "50-75%", "75-100%" ), name="% Submergence")+
  theme(legend.position="none",
        panel.background = element_rect("white", "white", linetype=1, color="black"),
        panel.grid.major = element_line( linetype = 'solid', colour = "grey90"),
        text = element_text(size=18))

cc100.trc.p <- ggplot( data =cc.100.trc ) + 
  geom_line( aes( x= PAR, y =Estimate, group=site, color=site, linetype=group), size = 1) +
  geom_ribbon(aes( x= PAR, ymin = Estimate-Error, ymax= Estimate +Error, fill=site), alpha=0.15 ) +
  ylab(expression(paste('NEE'[night], ' (', mu, 'mol m'^-2, 's'^-1, ')' ))) +
  xlab(expression(paste( 'TA ('^ degree, 'C)' ))) + ylim(-4, 10) +
  scale_color_manual(values = c("TS1" = "#000099","SE1" = "#fab255", "TS7"="#43b284" ))+
  scale_fill_manual(values = c("TS1" = "#000099","SE1" = "#fab255", "TS7"="#43b284"))+
  scale_linetype_manual(values = c("0%" = 1,"0-25%" = 2, "25-50%"=3 , "50-75%"=4, "75-100%"=5)) +
  scale_linetype_discrete(limits=c("0%","0-25%", "25-50%", "50-75%", "75-100%" ), name="% Submergence")+
  theme(legend.position="none",
        panel.background = element_rect("white", "white", linetype=1, color="black"),
        panel.grid.major = element_line( linetype = 'solid', colour = "grey90"),
        text = element_text(size=18))

cc.trc.df <- rbind(cc.0.trc,cc.25.trc,cc.50.trc,cc.75.trc,cc.100.trc)

unique(cc.trc.df$site)
cc.trc.df$group <- factor(cc.trc.df$group, levels = c("0%", "0-25%", "25-50%", "50-75%","75-100%") )
cc.trc.df$site <- factor(cc.trc.df$site, levels=c("TS1", "SE1","TS7"))


legend.p <- ggplot( data = cc.trc.df ) + 
  geom_line( aes( x= PAR, y =Estimate, color=site, linetype=group)) +
  geom_ribbon(aes( x= PAR, ymin = Estimate-Error, ymax= Estimate +Error, fill=site), alpha=0.1 ) +
  ylab(expression(paste('NEE'[day], ' (', mu, 'mol m'^-2, 's'^-1, ')' ))) +
  xlab(expression(paste( 'TA ('^ degree, 'C)' ))) + ylim(-4, 10) +
  scale_color_manual(values = c("TS1" = "#000099","SE1" = "#fab255", "TS7"="#43b284" ))+
  scale_fill_manual(values = c("TS1" = "#000099","SE1" = "#fab255", "TS7"="#43b284"))+
  theme(text = element_text(size=20),legend.direction='vertical', 
        legend.text=element_text(size=19)) + 
  guides(color=guide_legend(title="Site"),
         linetype=guide_legend(title="% Submergence"),
         fill=guide_legend(title="Site"))


# Extract the legend. Returns a gtable
leg <- get_legend(legend.p)

# Convert to a ggplot and print
as_ggplot(leg)


png('figures/trc_Site_CC.png',width = 900, height = 625,)
ggarrange( as_ggplot(leg), cc0.trc.p, cc25.trc.p,
           cc50.trc.p, cc75.trc.p,
           cc100.trc.p, font.label = list(size = 18),
           labels =c('','A', 'B', 'C',
                     'D', 'E'),nrow=2, ncol=3)
dev.off()