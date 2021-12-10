library(tidytuesdayR)
library(tidyverse)
library(extrafont)
library(lubridate)


tuesdata <- tidytuesdayR::tt_load('2020-01-21')

spotify_songs <- tuesdata$spotify_songs

head(spotify_songs)


annual_trends <- spotify_songs  %>% 
    distinct(track_name, track_artist, .keep_all = TRUE) %>% 
    
    mutate(year_released = ymd(track_album_release_date, truncated = 2L)) %>% 
    
    mutate(year_released = year(year_released)) %>% 
    
    filter(year_released >= 1980) %>% 
    
    mutate(duration_min = duration_ms / 60000) %>% 
    
    group_by(year_released) %>% 
    
    summarise(
        Danceability = mean(danceability),
        Energy       = mean(energy),
        "Loudness(db)" = mean(loudness), 
        "Happiness (Valence)"= mean(valence), 
        "Tempo (BPM)" = mean(tempo), 
        "Duration (Min)"= mean(duration_min)
        
    ) %>%  
    
    gather(parameter, score, -year_released)

annual_trends %>% 
    
    ggplot(aes(year_released, score)) + 
    geom_point() + 
    geom_smooth(se = FALSE) + 
    
    facet_wrap(~parameter, scales = "free_y") +
    
    theme_minimal() + 
    
    theme(text = element_text(size=14, family = "Calibri" , color = "#F1AE86"),
          strip.background = element_rect(color="#7A7676", fill="#FDF7C0", size = 0.5, linetype ="solid"),
          plot.background = element_rect(fill = "azure1"),
          axis.text = element_text(colour = "#667682"),
          plot.title = element_text(size=22, family = "Calibri"  , face="bold"),
          plot.subtitle=element_text(size=12, family = "Calibri")
          #plot.margin=unit(c(0.5,1.5,0.5,0.5),"cm")
          
          )  + 
    
    labs(x = "",
         y = "",
         title = "SONG ATTRIBUTE TRENDS 1980-2020", 
         subtitle = 'Recently songs have trended more danceable, shorter,\n less energetic, sader, louder, and faster.' ,
         caption = 'Based on annual averages\nData by Spotify via spotifyr package\n#TidyTuesday   @MrOchiwar') 
    
  
 

