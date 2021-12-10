#Load Libraries ----

library(tidyverse)
library(tidytuesdayR)
library(showtext)


# Add font ----


font_add_google("Indie Flower", "Indie")

# Open showtex

showtext.opts(dpi = 300)
showtext.auto(enable = TRUE)

fontfam <- "Indie"

# Get Data ----

tuesdata <- tt_load(2021, week = 9)

earn <- tuesdata$earn

glimpse(earn)


# EDA ----

library(skimr)

skimr::skim(earn)


# Data Wrangling ----

earn_df <- earn %>%  
    #filter only the 25 to 54 years age group 
    filter (age == "25 to 54 years") %>% 
    #remove all origins group
    filter(race != "All Races") %>% 
    #remove all sexes
    filter(sex != "Both Sexes") %>% 
    select(race, sex, year, median_weekly_earn) %>% 
    group_by(year, race, sex) %>% 
    summarise(year_avg = mean(median_weekly_earn)) %>% 
    ungroup() %>% 
    group_by(year, race) %>% 
    mutate(ribbon_max = (year_avg - lag(year_avg))*-1) %>% 
    mutate(race = factor(race, level = c("Black or African American",
                                         "White",
                                         "Asian")))
    
fill_color <- viridis::cividis(2)

text_labels <-  data.frame(
    race = factor(unique(earn_df$race), level = c("Black or African American",
                                                  "White",
                                                  "Asian")), 
    position = c(1510, 1010, 1210))

# Labels 

title <- "GENDER GAP BY RACE IN THE US"
subtitle <- "Mean Weekly earnings in the US for people ages 25 and over"
caption <- "Source: US Bereau of Labor Statistics | Visualization: @MrOchiwar"


# Plotting ----

ggplot(earn_df, aes(year, year_avg, col = sex, group = sex)) +
    geom_line(size = 3) +
    geom_ribbon(data = earn_df %>%  filter (sex == "Women"), 
                aes(year, ymin = year_avg, ymax = year_avg + ribbon_max), 
                inherit.aes = F, fill = "grey85", alpha = 0.4, show.legend = F) + 
    scale_color_manual(values = fill_color) +
    scale_x_continuous(breaks = seq(2010, 2020, 2), 
                       labels = c("2010", "'12", "'14", "'16", "'18", "'20"), name = NULL) +
    scale_y_continuous(breaks = seq(0,1600,200),
                       labels = scales::dollar_format(scale = 1), name = NULL) + 
    geom_text(data = text_labels, 
              aes(x = 2014, y = position, label = race), 
              inherit.aes = F, 
              fontface = "bold", family = fontfam, size = 3) +
    facet_wrap(~race) +
    labs(
        x = "Year",
        title = title,
        subtitle = subtitle,
        caption = caption
    ) + 
    theme(
        text = element_text(size = 9),
        panel.background = element_rect(fill = "white", 
                                        color = NULL),
        panel.grid.major.y = element_line(size = 1, colour = "grey95", linetype = "dashed"),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal',
        legend.text = element_text(family = fontfam, size = 9),
        plot.caption = element_text(hjust = 1, face = "italic", colour = "darkgrey", size = 3),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14, family = fontfam), 
        plot.subtitle = element_text(hjust = 0.5, size = 5),
        axis.text.x = element_text(colour = "darkgrey", size = 8),
        axis.text.y = element_text(hjust = 1, vjust = -1),
        strip.text = element_blank()
    )


