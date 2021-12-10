#Load Packages ----
library(tidyverse)
library(scales)
library(forcats)
library(extrafont)

# 1.0 Read CSV File ----
#For Number of PhDs Awarded in US
broad_phd <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-19/phd_by_field.csv") 
broad_phd_tbl <- broad_phd %>% 
    as.tbl()

# 1.1 How many PhDs in 2017  ----
broad_phd_2017_tbl <- broad_phd_tbl %>% 
    filter(year == 2017) %>% 
    count(broad_field , wt = n_phds, sort = T) %>% 
    arrange(-n)

#1.2 Visualize Data for 2017----
broad_phd_2017_tbl%>% 
    mutate(broad_field = fct_reorder(broad_field,n)) %>% 
    ggplot(aes(broad_field, n, fill = broad_field)) + 
    geom_col(show.legend = F, col = "black")+
    geom_text(aes(label = comma(n)), hjust = -.20)+
    scale_y_continuous(limits = c(0,25000), expand = c(0,0))+
    labs(
        title = "2017 PhD Graduates by Broad Field of Study ",
        x = "Broad Field of Study",
        y = "\nNo. of PhD Graduates in the year 2017",
        caption = " Plot for #TidyTuesday"
    )+
    coord_flip() + 
    theme_light()  
 
broad_phd_tbl %>% 
    # select(broad_field, year, n_phds) %>% 
    mutate(
        year = as.double(year),
        label = broad_field %>% as.character()
    ) %>% 
    filter (label != "Other")
  
#1.3 Facet View change in Phd Graduates by Broad Field
broad_phd_group_tbl <- broad_phd_tbl %>% 
    # select(broad_field, year, n_phds) %>% 
    mutate(
        year = as.double(year),
        label = broad_field %>% as.character()
        ) %>% 
    filter(label != "Other") %>% 
    group_by(year, broad_field) %>% 
    summarise(total = sum(n_phds, na.rm = TRUE))%>% 
    ungroup() 

broad_field_facet_plot <- broad_phd_group_tbl %>% 
    ggplot(aes(year,total)) +
    geom_line()+
    geom_point()+
    scale_x_continuous(breaks = seq(2008, 2017, by = 3))+
    
    facet_wrap(~broad_field, scales = "free_y") + 
    expand_limits(y = 0) + 
    theme_minimal() + 
    
    labs(
        title = "Number of PhDs Awards by Broad field ",
        subtitle = "Some fields have seen renewed interest while some haven't",
        x = " ",
        y = "Number of PhDs",
        caption = "Source: National Science Foundation"
    ) + 
    theme(
        strip.text = element_text(
            face = "bold"
        ), 
        text = element_text(family = "Corbel" )
            )
broad_field_facet_plot

