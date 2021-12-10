library(tidyverse)
library(extrafont)

tuesdata <- tidytuesdayR::tt_load("2020-01-14")
passwords <- tuesdata$passwords

pw <- passwords[1:500, ]

pw <- pw %>% 
    mutate(char = nchar(password),
           only_password = grepl("^[a-z]+$", .$password),
           only_numbers = grepl("^[0-9]+$", .$password), 
           alphanumeric = !(only_numbers | only_password)
           )


pw_vis <- pw %>% 
    select(char, password, only_password, only_numbers, alphanumeric, offline_crack_sec) %>% 
    gather("cat", "is_cat", -char, -offline_crack_sec, -password) %>%
    filter(is_cat) %>% 
    mutate(
        cat = str_to_title(
            str_replace(.$cat, "_", " ")
        )
    ) %>% 
    group_by(char, cat) %>% 
    summarise(crack_mean = mean(offline_crack_sec), 
              example = first(password)) %>% 
    ungroup()

x_breaks <- 10^c(-6:1)
x_labels <- format(x_breaks, scientific = FALSE) %>% 
    stringr::str_trim() %>% str_replace("(\\.{0,1}0+$)", "")

pw_vis %>% 
    ggplot(aes(crack_mean, char, color = cat, label = example)) +
    geom_point(size = 2, shape = 15) + 
    geom_line(alpha = 0.5) + 
    
    geom_text(show.legend = FALSE, nudge_x = 0.2, nudge_y = -0.15, family = "PT Sans", size = 3) +
    
    scale_x_log10(breaks = x_breaks,labels = x_labels) + 
    scale_y_continuous() + 
    scale_color_brewer(palette = "Dark2") + 
    
    labs(
        x = "Seconds to Crack",
        y = "Characters",
        title = "Bad passwords are cracked fast",
        subtitle =  paste("Shown is the time it takes to crack bad passwords", 
                          "with examples of how a password for the given type", 
                          "and length migh look like"), 
        color = "Type"
    ) + 
    
    theme_minimal() + 
    
    theme(
        legend.position = "bottom"
    ) + 
    
    theme(
        text = element_text(family = "PT Sans"), 
        axis.title = element_text (size = 12), 
        axis.title.x = element_text(margin = margin(t = 10)), 
        axis.text = element_text(size = 9), 
        plot.title = element_text(size = 14, face = "bold"), 
        panel.grid.minor = element_blank()
    )
ggsave("2020_03_passwords.png")    
    
    
