library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)

df = readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-08/state_stations.csv')

states = map_data("state") %>% 
  mutate(state = str_to_title(region)) %>% 
  select(long, lat, group, order, state)

df %>% 
  mutate(state = str_replace(state, "_"," ")) %>% 
  count(state, format, sort = T) %>% 
  group_by(state) %>% 
  slice_max(1,with_ties = F) %>% 
  left_join(states, by = "state") %>% 
  ggplot(aes(long, lat, group = group))+
  geom_polygon(aes(fill = format), color = "gray90", size = 0.1)+
  coord_map(projection = "albers", lat0 = 45, lat1 = 55)+
  scale_fill_brewer(type = "qual", palette = 6)+
  labs(title = "Most popular format of Radio Station by state")+
  theme(legend.position="bottom",
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_rect(fill = alpha("skyblue", 0.1)),
        panel.border=element_blank(),
        panel.grid=element_blank(),
        legend.title = element_blank(),
        legend.background = element_rect(fill = alpha("blue", 0.1)),
        plot.title = element_text(color = "gray40", size=14, face="bold.italic", hjust = 0.5)
  )


