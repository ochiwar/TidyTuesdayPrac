
# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(extrafont)
library(readxl)
library(ggthemes)


# Load Data ---------------------------------------------------------------


sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')


# Data Wrangling ----------------------------------------------------------

sf_trees %>% pull(address)
sf_trees %>% pull(legal_status) %>% head(n=100)


#How many of the tress are privately owned or not 

sf_trees %>% 
    count(caretaker) %>% 
    arrange(desc(n)) %>% 
    mutate(caretaker = caretaker %>% as_factor() %>% fct_rev()) %>% 
    slice(1:10) %>% 
    ggplot(aes(y = caretaker, x = n, label = scales::comma(n,accuracy = 1))) + 
    geom_col() + 
    geom_text(color = "black", nudge_x = -0.2, family = "Arial Narrow") + 
    scale_x_continuous(trans = "log10", labels = scales::number_format(big.mark = ",")) +
    labs(
        y = "", 
        x = "\n(Count in log scale)", 
        title = "Top 10 Owners of Trees in San Francisco"
    )+
    theme_gray() + 
    theme(
        text = element_text(family = "Corbel", size = 14), 
        axis.text.x = element_text(family = "Arial Narrow", size = 10),
        axis.title.x = element_text(size = 10, hjust = 1)
    )

#How many trees where planted by year

sf_trees_year_month <- sf_trees %>% 
    select(date) %>% 
    mutate(year_planted = floor_date(date, unit = "year"), month_planted = month(date, label = T, abbr = T)) 
    
    
    
sf_trees_year_month %>% 
    filter(between(year_planted, as.Date("2010-01-01"), as.Date('2019-12-01'))) %>% 
    group_by(year_planted) %>% 
    summarise(trees_planted = n()) %>% 
    ggplot(aes(year_planted, trees_planted)) + 
    geom_line(size = 0.5) + 
    geom_point(shape = 16, size = 4) + 
    labs(
        title = "Tree Planting in San Francisco in the past decade", 
        subtitle = "Number of trees planted in SF has continued to decline in past 6 years",
        y = "Number of Trees planted", 
        x = "year"
    ) + 
    scale_y_continuous(limits = c(0,2500), breaks = c(0, 1000, 2000, 2500)) + 
    scale_x_date(breaks = c(seq(as.Date("2010/1/1"), as.Date("2019/1/1"), "2 years"), as.Date("2019/1/1")), date_labels = "%Y") +
    theme(
        text = element_text(family = "Corbel", size = 14), 
        axis.text = element_text(family = "Arial Narrow", size = 12),
        axis.title.y = element_text(size = 11, hjust = 1, vjust = 3),
        axis.title.x = element_text(size = 11, hjust = 1, vjust = 1)
    )

#What is the favorite day(s) for planting trees 

sf_trees_wday <-  sf_trees %>% 
    select(date) %>% 
    mutate(day_of_week = wday(date, label = T)) %>% 
    filter(between(date, as.Date("1956-01-01"), as.Date('2019-12-01'))) %>% 
    group_by(day_of_week) %>% 
    summarise(day_total = n()) 

sf_trees_wday %>% 
     
    ggplot(aes(day_of_week, day_total)) + 
    geom_col() + 
    geom_col(data = sf_trees_wday %>% filter(day_of_week %in% c("Mon","Tue", "Wed", "Thu", "Fri")), 
                                             aes(day_of_week, day_total), fill = "sky blue", show.legend = F) +
    labs(
        y = "count", 
        x = "", 
        title = "Number of Trees planted in San Francisco by day of week since 1956",
        subtitle = "Bay Area folks are more likely to plant their trees on a weekday"
    )+
    
    scale_y_continuous( limits = c(0,15000), labels = scales::comma) + 
    
    theme_gray() + 
    theme(
        text = element_text(family = "Corbel", size = 14), 
        axis.text.y = element_text(family = "Arial Narrow", size = 12),
        axis.title.y = element_text(size = 10, hjust = 1)
    )
     

#Trying Maps

ca_counties <- map_data("county", "california") %>% 
    select(lon = long, lat, group, id = subregion) %>% 
    filter(id %in%  c("san francisco", "santa cruz", "san benito"))

# ggplot(ca_counties , aes(lon, lat, group = group)) +
#     geom_polygon(fill = "white", colour = "grey50") + 
#     coord_quickmap()
# 
# ggplot(sf_trees %>% filter(!is.na(longitude | latitude), latitude < 40, latitude > 37.7), aes(longitude, latitude)) + 
#     geom_point(size = .25, show.legend = FALSE) +
#     coord_quickmap()



    
