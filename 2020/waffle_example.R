# Load Modules ----
library(waffle)
library(tidyverse)

# Dataset ----
parts <- c(80, 30, 20, 10)

savings <- c(`Mortgage\n($84,911)` = 84911, 
             `Auto\ntuition loans\n($14,414)` = 14414,
             `Home equity loans\n($10,062)` = 10062,
             `Credit Cards\n($8,565)` = 8565)

# Plot ----

waffle(parts = parts, rows = 8)

waffle(savings/392, rows = 7, size = 0.5, legend_pos = "bottom", flip = T, 
       colors = c("#c7d4b6", "#a3aabd", "#a0d0de", "#97b5cf"))


# Second Example 

graf3 <- data.frame(
    tamaño = c("Micro","Pequeño", "Mediano", "Grande"),
    porcentaje = c(42,13,37,8),
    stringsAsFactors = FALSE)

ggplot(data = graf3, aes(fill = tamaño, values = porcentaje))  +
    geom_waffle(n_rows = 10, size = 1, flip = TRUE, color="white") +
    coord_equal() +
    theme_minimal() +
    theme_enhance_waffle() +
    scale_fill_brewer(palette = "Set2")
