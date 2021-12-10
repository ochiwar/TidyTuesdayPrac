library(tidyverse)
library(countrycode)
library(tsibble)
library(tidyr)

tuesdata <- tidytuesdayR::tt_load('2020-02-11')

hotels <- tuesdata$hotels

unique_country <- unique(hotels$country)



hotels %>% 
    select(hotel, arrival_date_year, adults, children, babies, country) %>% 
    mutate(country = countrycode(country, "iso3c", "country.name")) %>% 
    filter( country != "Portugal") %>% 
    
    group_by(arrival_date_year) %>% 
    
    summarise(
        travelling_adult_count = sum(adults), 
        travelling_dependents = sum(children, na.rm = T) + sum(babies,  na.rm = T), 
        travelling_children = sum(children,  na.rm = T), 
        travelling_babies = sum(babies,  na.rm = T)
    ) 
    

hotels2 <- hotels %>% 
    
    mutate(country = countrycode(country, "iso3c", "country.name")) %>% 
    filter( country != "Portugal") %>% 
    
    mutate(date = glue::glue("{arrival_date_year}-{arrival_date_month}-{arrival_date_day_of_month}"),
           date = parse_date(date, format = "%Y-%B-%d")) %>% 
    mutate(guests=children+babies+adults) %>%
    mutate(kids=children+babies) %>%
    mutate(has_kids=(kids>0)) %>%
    mutate(kids_per_adult=kids/adults) %>%
    mutate(kids_coverage=case_when(
        kids == 0 ~ "0, Free",
        kids_per_adult > 0 & kids_per_adult <= 1 ~ "0-1, One-one",
        kids_per_adult > 1  ~ ">1, Zone")) %>%
    mutate(children_per_adult=children/adults) %>%
    mutate(babies_per_adult=babies/adults) %>%
    mutate(assigned_reserved=(reserved_room_type==assigned_room_type))

hotels2 %>% filter(!is.na(kids_coverage)) %>% 
    group_by(date,kids_coverage) %>% 
    summarize(reservations=n()) %>% 
    #fill(reservations, 0) %>% 
    ungroup() %>% 
    
    spread(kids_coverage, reservations) %>% 
    
    mutate(`>1, Zone` = replace_na(`>1, Zone`, replace = 0),
           `0-1, One-one` = replace_na(`0-1, One-one`, replace = 0),
               `0, Free` = replace_na( `0, Free`, replace = 0)
           )   %>%  
    
    gather(kids_coverage, reservations, - date) %>% 
    
    group_by(kids_coverage) %>% 
    mutate (
        rma_1 = rollmean(reservations, 14, na.pad = T, align="right")
    ) %>%  
    
    ggplot(aes(date, rma_1)) + 
    
    geom_line() + 
    
    scale_x_date(breaks = "2 months") +
    
    facet_grid(kids_coverage ~ ., scales = "free_y")
    
    
    
    
