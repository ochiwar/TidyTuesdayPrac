# The packages
library(tidyr)
library(dplyr)
library(readr)
library(gt)

# The data
passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')

# Cleaning up the data

# Summarize raw data based on password category, removing strength outliers
passwords_category <- passwords %>%
    group_by(category) %>%
    filter(strength <= 10) %>%
    summarize(avg_strength = mean(strength), 
              avg_offline = mean(offline_crack_sec,
                                 avg_guess_min = mean(value))) %>%
    mutate(category = str_to_title(category)) %>%
    ungroup() %>%
    drop_na()

# Determining the 3 most common bad passwords for each category
password_examples <- passwords %>%
    select(rank, password, category) %>%
    mutate(category = str_to_title(category)) %>%
    group_by(category) %>%
    top_n(3, desc(rank)) %>%
    arrange(category) %>%
    mutate(rank = rep(c(1,2,3))) %>%
    pivot_wider(names_from = rank, values_from = password)

# Combining the tables and cleaning up entries
passwords_full <- passwords_category %>%
    left_join(password_examples, by = "category") %>%
    mutate(category = str_replace(category, "-", " "))


# Making the table
password_table <- passwords_full %>%
    gt() %>%
    tab_header(
        title = "Bad Passwords",
        subtitle = "Common themes for bad passwords and how long they take to crack"
    ) %>%
    cols_label(
        category = "Category",
        avg_strength = "Avg. Rel. Strength",
        avg_offline = "Avg. Hacking Time (s)",
        '1' = "First",
        '2' = "Second",
        '3' = "Third") %>%
    tab_spanner(
        label = "Most Popular Passwords",
        columns = 4:6) %>%
    fmt_number(columns = 2:3, decimals = 1) %>%
    tab_source_note(source_note = "Information is Beautiful: 
                  https://informationisbeautiful.net/visualizations/top-500-passwords-visualized/") %>%
    tab_footnote(footnote = "Passwords ranked on a scale of 1-10 relative to 500 other bad passwords
               (outliers over 10 removed)",
                 locations = cells_column_labels(2)) %>%
    tab_footnote(footnote = "The amount of time it would take to crack the password offline",
                 locations = cells_column_labels(3)) %>%
    
    cols_align(align = "center") %>% 
    
    tab_options(heading.background.color = "lightgray",
        source_notes.background.color = "lightgray",
    )

# View the table
password_table
