# Summarize the data by the site for the 
df$count <- 1
ts1.summary <- df %>% reframe(.by= TS1WLindicator,
                              WL= mean(TS1_daily_wl) %>% round(2),
                              WL.se= sd(TS1_daily_wl)/sqrt(length(TS1_daily_wl))%>% round(2),
                              total.count = sum(count),
                              SAL= mean(TS1_mean_weekly_Sal) %>% round(2),
                              SAL.se= sd(TS1_mean_weekly_Sal)/sqrt(length(TS1_mean_weekly_Sal))%>% round(2)) %>%  mutate( duration = total.count/ sum(total.count)*100)