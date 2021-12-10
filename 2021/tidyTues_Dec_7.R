# Working Libraries 

library(tidytuesdayR)
library(tidyverse)

# Download Data ----

tuesdata <- tidytuesdayR::tt_load('2021-12-07')

spiders <- tuesdata$spiders

glimpse(spiders)

# Top 10 Species With the Most Geographic Reach ----

convert_function <- function(x) {
    as.numeric(x != "0")
}

geo_location_spiders <- spiders %>% 
    
    #Separate the distribution field into different countries
    separate(col = distribution, 
             into = str_c("country_", 1:16),
             sep = ",",  
             remove = F) %>% 
    filter(!is.na(country_10)) |> 
    select(-c(1,2,3,4,6,7,8,9,10)) |> 
    mutate(species = as.factor(species)) |> 
    mutate_if(is.character, ~replace_na(.x, 0)) 

summary(geo_location_spiders)

# Preparing to Count by Geographic Spread 
# 
# Identifying Duplicated Species 
geo_location_spiders |> 
    group_by(species) |> 
    filter(n() > 1) |> 
    summarise(n = n())

#Proceed to continue counting 

geo_location_spiders |> 
    mutate_if(is.character, convert_function) |> 
    group_by(species) |> 
    #Only the first duplicate is used using the fromLast attribute
    filter(duplicated(species, fromLast = T) | n() == 1) |> 
    rowwise() |> 
    mutate(countries_found = sum(across(country_1:country_16))) |> 
    select(species, countries_found ) |> 
    arrange(desc(countries_found ))

