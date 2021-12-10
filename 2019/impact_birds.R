library(tidyverse)
library(readr)

wildlife_impacts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-23/wildlife_impacts.csv")

(wildlife_impacts %>% 
    select(incident_year, incident_month, height) %>% 
    group_by(incident_month) %>% 
    arrange(incident_year, incident_month) %>% 
    filter(height > 0) %>% 
    summarise(mean_height = mean(height)) -> impacts_month)


(wildlife_impacts %>% 
    select(incident_month, incident_year, height) %>% 
    group_by(incident_year, incident_month) %>% 
    filter(height > 0) %>% 
    summarise(mean_height = mean(height)) -> impacts_month_yr)


ggplot(impacts_month, aes(incident_month, mean_height)) + 
    geom_line(data = impacts_month_yr, aes(incident_month, mean_height, group = incident_year), 
              alpha = 0.2, color = "#5888d6")+
    geom_line(color = "#002054") +
    geom_point(color = "#002054") +
    theme_bw() + 
    theme(
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank()
    ) + 
    scale_x_continuous(breaks = seq(1,12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                                                       "Sep", "Oct", "Nov", "Dec")) + 
    labs(
        y = "Average Height of Impact once Airborne (Feet)", 
        title = "Wildlife Impacts from Airbone in the US from 1990 - 2018") + 
    ggsave("impact_height.png", width = 9, height = 6, units = "in", dpi = 800)
    
    
    
