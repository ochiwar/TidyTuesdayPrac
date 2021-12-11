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

top_9_species <- geo_location_spiders |> 
    mutate_if(is.character, convert_function) |> 
    group_by(species, ) |> 
    #Only the first duplicate is used using the fromLast attribute
    filter(duplicated(species, fromLast = T) | n() == 1) |> 
    rowwise() |> 
    mutate(countries_found = sum(across(country_1:country_16))) |> 
    select(species, countries_found ) |> 
    arrange(desc(countries_found )) |>
    ungroup() |> 
    slice_head(n = 9)


# Putting in a Table 
# 

library(gt)

top_9_species |> 
    mutate(species = str_to_title(species)) |> 
    gt() |> 
    tab_options(table.font.names = "calibri") |> 
    tab_header(
        title = md("**Spider Species With The Most Geographic Spread**"),
        subtitle = md("Species found in at least *14* countries")
    ) |> 
    tab_style(
        style = list(
            cell_text(
            font = "calibri",
            weight = "bold"
        )
    ),
    locations = list(
        cells_title(
            groups = "title"
        )
    )) |> 
    
    tab_source_note(
        source_note =md("TABLE: @Ochiwar<br>Source: World Spider Database. https://wsc.nmbe.ch")
        
    ) |> 
    
    cols_align( 
        align = "left",
        columns = species) |> 

    tab_style(
        style = list(
            cell_borders(
                sides = "bottom",
                color = "black",
                weight = px(3)
            ),
            cell_text(
                font = "calibri"
            )
        ),
        locations = list(
            cells_column_labels(
                columns = gt::everything()
            )
        )
    ) |> 
    cols_label(
        species = "Species",
        countries_found = "Countries"
    )
    

    
