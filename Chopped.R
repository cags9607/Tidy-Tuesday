library(ggtext)
library(tidyverse)

chopped <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')

note = "The first season is the highest rated season (average = 8.87)"

chopped %>% 
  filter(!is.na(episode_rating)) %>% 
  group_by(season) %>% 
  summarize(rating = mean(episode_rating),
            n_episodes = n()) %>% 
  ggplot(aes(season, rating))+
  geom_point(aes(size = n_episodes), color = "darkgray")+
  geom_line(color = "gray60")+
  annotate(
    "curve",
    xend = 0.7,
    yend = 8.88,
    x = 0,
    y = 9.1,
    curvature = .4,
    arrow = arrow(type = "closed", length = unit(.5, "lines")),
    colour = "blue"
  )+
  annotate(
    "curve",
    xend = 40,
    yend = 8.89,
    x = 37,
    y = 9.1,
    curvature = -.4,
    arrow = arrow(type = "closed", length = unit(.5, "lines")),
    colour = "blue"
  )+
  annotate(
    "curve",
    xend = 33,
    yend = 7.48,
    x = 30,
    y = 7.5,
    curvature = -.4,
    arrow = arrow(type = "closed", length = unit(.5, "lines")),
    colour = "red"
  )+
  annotate(
    "text",
    x = 0,
    y = 9.1,
    label = note,
    color = "white",
    hjust = 0
  )+
  annotate(
    "text",
    x = 37,
    y = 9.1,
    label = "After a long period of decreasing ratings, \n the 40th season is the second most favorite (average = 8.86)",
    color = "white",
    hjust = 1
  )+
  annotate(
    "text",
    x = 30,
    y = 7.55,
    label = "The 33th season is the least favorite; \n it featured a celebrity tournament with 16 celebrities competing to win $50,000 for charity.\n Contestants in this heat were internet personalities with a considerable online following. \n (average = 7.46)",
    color = "white",
    hjust = 1
  )+
  labs(x = "Season",
       y = "Average IMDB rating",
       title = "Chopped rating across seasons",
       subtitle = "The size of the dots represents the number of episodes within the season (ranging from 1 to 19)",
       caption = "Data from <b>'Chopped: 10+ Years of Episode Data' </b>(kaggle.com/jeffreybraun/chopped-10-years-of-episode-data)<br>
       Code found at <b>github.com/cags9607</b>")+
  ggthemes::theme_clean()+
  theme(
    rect = element_rect(fill = "#373F51"),
    text = element_text(colour = "white"),
    line = element_line(colour = "white"),
    title = element_text(colour = "white"),
    axis.text = element_text(colour = "white"),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "#373F51"),
    axis.line = element_blank(),
    legend.position = "none",
    panel.grid.major = element_line(colour = "#3e4c6d"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "darkgray"),
    plot.caption = element_markdown(hjust = 0.5),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.title = element_text(size = 17)
    )
  
