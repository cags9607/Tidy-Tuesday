library(tidyverse)
library(scales)
library(lubridate)

bytes_total = readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/bytes_total.csv')%>% 
  mutate(date = ymd(date))

col1 = "#435E55"
col2 = "#5B84B1"

bytes_total %>% 
  mutate(client = if_else(client == "desktop", "???????  Desktop", "???? Mobile"),
         across(c(p50),~.x*1024)) %>% 
  ggplot(aes(date, p50, fill = client, color = client))+
  geom_line(size = 1)+
  scale_y_continuous(labels = label_bytes(units = "kB"),
                     limits = c(0, 2.5e6))+
  theme_minimal()+
  scale_color_manual(values = c(col1, col2)) +
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "grey95", color = NA),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 25),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray80"))+
  labs(color = "",
       x = "",
       y = "",
       title = "Average webpage weight",
       caption = "Source: HTTPArchive | github.com/cags9607 | #TidyTuesday")
