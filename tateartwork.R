library(tidyverse)    
library(lubridate)     
library(ggthemes)      
library(gghighlight)
library(ggtext)
library(gganimate)


artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')
artists <- readr::read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv")

artwork %>% 
  summarize(n_distinct(year))



art_gganim_start <- artwork %>% 
  drop_na(width, height) %>% 
  select(id, year, width, height) %>% 
#  sample_n(size = 10000) %>% 
  mutate(left = -width/2,
         right = width/2,
         top = height/2,
         bottom = -height/2) %>% 
  ggplot(aes(xmin  = left,
             xmax = right,
             ymin = bottom,
             ymax = top,
             group = id)) +
  geom_rect(alpha = 0, color = "gray") +
  theme_void() +
  transition_states(year) +
  labs(title = "Size of Artwork in Tate Art Museum",
       subtitle = "Year: {closest_state}",
       caption = "Viz credit to @lisalendway")


animate(art_gganim_start,
        nframes = 370,
        duration = 60)

anim_save("art_anim.gif", path = "images/")