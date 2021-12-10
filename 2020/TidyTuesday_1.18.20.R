---
title: "TidyTuesday_02_20_2020"
author: "Ochiwar"
date: "2/22/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Loading Libraries 

```{r include=FALSE}
library(tidyverse)
library(ggforce)

```

## Load In Tidy Data


```{r echo=FALSE}
tuesdata <- tidytuesdayR::tt_load("2020-02-18")
food_consumption <- tuesdata$food_consumption

```

## Wrangling Data

You can also embed plots, for example:

```{r echo=FALSE}


food_consumption %>% 
    count(country)

food_consumption %>% 
    count(food_category)

info <- food_consumption %>% 
    group_by(food_category) %>% 
    filter(co2_emmission == min(co2_emmission))


food_consumption %>%
  mutate(food_category = str_wrap(food_category, 20)) %>% 
  mutate(food_category = fct_reorder(food_category, -co2_emmission, mean)) %>% 
    ggplot(aes(x = food_category, co2_emmission)) + 
    geom_sina(aes(color = co2_emmission), method = "c", show.legend = F, size = 1, scale = "width") + 
    scale_y_continuous(trans =  "log1p", limits = c(0, 1800), breaks = c(0, 1800)) +
    #scale_y_log10() +
    coord_flip() + 
    
    scale_color_viridis_c(
    option = "plasma",
    trans = "log10",
    begin = 0.05
  ) + 
    
      labs(
          x = "",
    y = bquote("CO"[2]~emission~(Kg~CO[2]~person^{-1}~year^{-1})),
    title = str_wrap("CO2 emission for various food categories around the World", 35),
    subtitle = str_wrap("Each point corresponds to one of the 130 countries included in the data", 40),
    caption = "Tidytuesday 2020 week #8\nData: https://www.nu3.de/blogs/nutrition/food-carbon-footprint-index-2018\n@philmassicotte"
  ) + 
    
    theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#3c3c3c"),
    panel.background = element_rect(fill = "#3c3c3c"),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(size = 0.25, color = "gray30"),
    #plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, size = 18),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(color = "gray75", size = 6)
  )
    ```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
