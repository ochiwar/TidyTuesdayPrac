#-----load libraries-----
library(tidyverse)
library(countrycode)
library(gt)
library(paletteer)

#---Load Data ----

tuesdata <- tidytuesdayR::tt_load(2021, week = 2)

transit_cost <- tuesdata$transit_cost

#-- Clean Data ---

transit_cost_clean <- transit_cost %>% 
    mutate(
        continent = case_when(
            country != "UK" ~ countrycode(country, origin = "genc2c", destination = "continent"), 
            country == "UK" ~ "Europe"
        ), 
        start_year = as.double(start_year), 
        end_year = as.double(end_year),
        continent = factor(continent)
    ) %>%  
    filter(!is.na(country)) 
 

topbottom5 <- transit_cost_clean %>% 
    filter(dense_rank(cost_km_millions) <= 5 | dense_rank(desc(cost_km_millions)) <= 5 ) %>% 
    arrange(-cost_km_millions)


#-- Make table ----

my_theme <- function(data) {
    tab_options(
        data = data,
        heading.subtitle.font.size = 14,
        heading.align = "left",
        row_group.border.top.width = px(10),
        row_group.border.top.color = alpha("white", 0),
        row_group.border.bottom.width = px(3),
        row_group.border.bottom.color = "lightgrey",
        table.border.top.color = "white",
        table.border.top.width = px(5),
        table.border.bottom.color = "white",
        column_labels.border.bottom.color = "black",
        column_labels.border.bottom.width = px(1),
        column_labels.border.top.width = px(10),
        column_labels.border.top.color = "white"
    )
}

costcolours <- c("lightblue", "white", "thistle2")
scales_costscale <- scales::col_bin(costcolours, 
                                    domain = c(min(topbottom5$cost_km_millions),
                                               max(topbottom5$cost_km_millions)),
                                    bins = c(0, 10, 20, 30, 40, 50, 1500, 1800, 2100, 2400, 2700, 3000,
                                             3300, 3600, 3900, 4200))

table <- 
    topbottom5 %>%
    mutate(
        cost_km_millions = round(cost_km_millions, 2),
        length = round(length, 1)
    ) %>%
    select(
        City = city,
        "Start year" = start_year,
        Line = line,
        "Length (km)" = length,
        "Cost (millions) per km" = cost_km_millions
    ) %>%
    gt() %>%
    data_color(
        columns = 5,
        colors = scales_costscale
    ) %>%
    tab_row_group(
        group = "Five least expensive",
        rows = 6:10
    ) %>%
    tab_row_group(
        group = "Five most expensive transit projects",
        rows = 1:5
    ) %>%
    tab_header(
        title = md("**The Five most expensive transit projects are all from New York**"),
        subtitle = md("**whereas none of the five least expensive are in America**")
    ) %>%
    tab_source_note("Data: Transit Costs Project | Visualisation: @ochiwar") %>%
    tab_footnote(footnote = "'Expensive' here means real cost per kilometre",
                 locations = cells_title(
                     groups = "title"
                 )
    ) %>% 
    tab_style(
        style = cell_text(font = "IBM Plex Sans"),
        locations = cells_body(columns = c(2, 4:5))
    ) %>%
    tab_style(
        style = cell_text(font = "IBM Plex Sans"),
        locations = cells_body(columns = c(1, 3))
    ) %>%
    tab_style(
        style = cell_text(font = "IBM Plex Sans"),
        locations = cells_column_labels(columns = everything())
    ) %>%
    tab_style(
        style = cell_text(weight = "bold",font = "IBM Plex Sans" ),
        locations = cells_row_groups()
    ) %>%
    tab_style(
        style = cell_text(weight = "bold", font = "IBM Plex Sans"),
        locations = cells_column_labels(gt::everything())
    ) %>%
    cols_align(
        align = "left",
        columns = 2
    ) %>%
    fmt_currency(
        columns = 5,
        rows = c(1:10)
    ) %>%
    my_theme() %>%
    cols_width(vars(Line) ~ px(210),
               5 ~ px(190),
               c(1:2, 4) ~ px(120))

table


#---Table Practice 2 ----

scales::show_col(c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"))

playoff_salary <- read_csv("https://raw.githubusercontent.com/jthomasmock/radix_themockup/master/_posts/2020-05-13-qb-salaries-vs-playoff-appearances/playoff_salary.csv")

glimpse(playoff_salary)

playoff_salary %>% 
    #head() %>% 
    gt() %>% 
    data_color(
        columns = vars(Total),
        colors = scales::col_numeric(
            # custom defined values - notice that order matters!
            palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
            domain = NULL
        )) %>% 
    data_color(
        columns = vars(salary),
        colors = scales::col_numeric(
            # custom defined values - notice that order matters!
            palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
            domain = NULL
        )) %>% 
    fmt_currency(
        columns = vars(salary),
        decimals = 1,
        pattern = "{x} M"
        
    ) %>% 
    cols_align(align = "right", 
               columns = vars(salary)) %>% 
    
    tab_style(
        style = list(
            cell_borders(
                sides = "left",
                color = "black",
                weight = px(3)
            )
            
        ),
        locations = list(
            cells_body(
                columns = vars(Total)
            )
        )
    ) %>% 
    
    tab_style(
        style = list(
            cell_borders(
                sides = "bottom",
                color = "black",
                weight = px(3)
            )
        ),
        locations = list(
            cells_column_labels(
                columns = gt::everything()
            )
        )
    ) %>% 
    
    cols_label(
        player = "Player",
        salary = "Salary"
    ) %>% 
    
    tab_source_note("Table: @MrOchiwar") %>% 
    tab_header(
        title = md("**2014-2018 Salary and Playoff Appearances**"),
        subtitle = "QBS limited to playoff games where they threw a pass"
    )



island_tbl <- data.frame(name = names(islands), 
           size = islands) %>% 
    arrange(desc(size)) %>% 
    slice(1:10)

island_tbl %>% 
    gt()
    
        