# Duration Figure
library(ggplot2)
library(dplyr)
library(ggpubr)

duration <- read.csv('~/YSE Dropbox/Sparkle Malone/Research/ENP_WZ_2024_MS/data/Duration.csv' )
duration %>% names
duration %>% ggplot() +
  geom_col(aes(x= Submergence, y=Duration_C))

duration$Ecosystem <- factor( duration$Ecosystem, levels=c( "Scrub Mangrove", "Ecotone", "Marl Prairie"))


plot.duration <- duration %>%  ggplot() +
  geom_bar(aes(x =Duration_C , y = Ecosystem, fill = Submergence), 
           position = "stack", stat = "identity") + xlab("Duration") +
  scale_fill_manual(values=c("azure3", "cadetblue",
                             "cyan4", "darkblue")) + 
  theme_bw() + ylab("") +theme(text = element_text(size = 18))

