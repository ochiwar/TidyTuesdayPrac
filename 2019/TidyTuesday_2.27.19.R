---
title: "french_trains"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
library(tidyverse)

full_trains <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-26/full_trains.csv")

```{r}
november_2018 <- full_trains %>% 
    filter(year == 2018, month == 11) %>% 
    mutate(pct_late_at_departure = num_late_at_departure/total_num_trips, 
           departure_station = str_to_title(departure_station),
           arrival_station = str_to_title(arrival_station)
           ) 

november_2018 %>% 
    ggplot(aes(pct_late_at_departure)) + 
    geom_histogram(binwidth =  0.05) + 
    scale_x_continuous(labels = scales::percent_format()) + 
    theme_light()

november_2018 %>%
    mutate(departure_station = fct_lump(departure_station, 3)) %>% 
    ggplot(aes(departure_station, pct_late_at_departure)) + 
    geom_boxplot() + 
    scale_y_continuous(labels = scales::percent_format()) + 
    theme_light()

november_2018 %>% 
    filter(departure_station == "Lyon Part Dieu")
    
november_2018 %>%
    mutate(arrival_station = fct_infreq(fct_lump(arrival_station, prop = .01))) %>% 
    mutate(departure_station = fct_infreq(fct_lump(departure_station, prop = .01))) %>% 
    group_by(arrival_station, departure_station) %>% 
    summarise(pct_late_at_departure = sum(num_late_at_departure/sum(total_num_trips))) %>%  
    # filter(departure_station == "Other", arrival_station == "Paris Lyon") %>% 
    ggplot(aes(arrival_station, departure_station, fill = pct_late_at_departure)) + 
    geom_tile() + 
    scale_fill_gradient2(low = "blue", high = "red", midpoint = 0.25) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    labs(
        x = "Arrival station",
        y = "Departure station",
        fill = "% late at departure",
        title = "Which routes have the most delayed trains in November 2018?",
        subtitle = "Stations with only one arriving/departing route where lumped into 'Other' "
        
    )
   
```

Reordering the arrangement


```{r}
november_2018 %>%
    mutate(arrival_station = fct_reorder(fct_lump(arrival_station, prop = .01), pct_late_at_departure)) %>% 
    mutate(departure_station = fct_reorder(fct_lump(departure_station, prop = .01), pct_late_at_departure)) %>% 
    group_by(arrival_station, departure_station) %>% 
    summarise(pct_late_at_departure = sum(num_late_at_departure/sum(total_num_trips))) %>%  
    # filter(departure_station == "Other", arrival_station == "Paris Lyon") %>% 
    ggplot(aes(arrival_station, departure_station, fill = pct_late_at_departure)) + 
    geom_tile() + 
    scale_fill_gradient2(low = "blue", high = "red", midpoint = 0.25, labels = scales::percent_format()) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    labs(
        x = "Arrival station",
        y = "Departure station",
        fill = "% late at departure",
        title = "Which routes have the most delayed trains in November 2018?",
        subtitle = "Stations with only one arriving/departing route where lumped into 'Other' "
        
    )

```

