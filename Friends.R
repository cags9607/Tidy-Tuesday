library(tidyverse)
library(tidytext)
library(tidylo)
library(cowplot)
library(ggtext)

friends_emotions = readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_emotions.csv')
friends_info = readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_info.csv')

my_theme = theme(
  title = element_text(color = "white", size = 17),
  rect = element_rect(fill = "#333333"),
  panel.background = element_rect(fill = "darkorchid2"),
  strip.background = element_rect(fill = "#9912FF"),
  strip.text = element_text(color = "white", size = 17),
  axis.text = element_text(color = "white", size = 12), 
  legend.text = element_text(color = "white"),
  plot.subtitle = element_markdown(),
  plot.caption = element_text(size = 10, color = "gray80")
  )

p1 = friends_emotions %>% 
  mutate(season = paste("Season", season)) %>% 
  count(emotion, season, sort = T) %>% 
  bind_log_odds(emotion, season, n) %>% 
  arrange(-log_odds_weighted) %>% 
  mutate(emotion = reorder_within(emotion, abs(log_odds_weighted), season)) %>% 
  ggplot(aes(abs(log_odds_weighted), emotion))+
  geom_col(aes(fill = ifelse(log_odds_weighted<0,"Neg","Pos")),
           show.legend = F,
           alpha = 0.95)+
  geom_vline(xintercept = qnorm(0.975),
             linetype = "dashed", 
             size = 1.7, 
             color = "firebrick1",
             alpha = 0.5)+
  scale_y_reordered()+
  scale_fill_manual(values = c("firebrick","forestgreen"))+
  facet_wrap(~season, scales = "free_y", ncol= 4)+
  my_theme+
  labs(x = "Absolute value of log odds ratio (z-scores), weighted by uninformative Dirichlet prior",
       title = "How the utterance emotions evolved for the first 4 seasons of the TV comedy show Friends?",
       subtitle = "<span style = 'color:forestgreen;'>Neutral</span> utterances were <span style = 'color:forestgreen;'>positively associated </span> with the first season, with<span style = 'color:firebrick;'> peaceful </span>and <span style = 'color:firebrick;'> powerful </span> utterances <br> being <span style = 'color:firebrick;'>negatively associated</span>.<br> This pattern reverted for the 4th season, where utterances were <span style = 'color:forestgreen;'>powerful</span> and <span style = 'color:forestgreen;'>peaceful</span> with a lack of <span style = 'color:firebrick;'>neutral</span> utterances.",
       y = "",
       caption = "The dashed line shows the usual threshold (1.959...) for significance of standardized z-scores for a two-tailed test")

p2 = friends_info %>% 
  filter(season %in% 1:4) %>%
  group_by(season) %>% 
  summarize(imdb_rating = mean(imdb_rating),
            total_views = sum(us_views_millions)) %>% 
  ggplot(aes(season, imdb_rating))+
  geom_point(size = 6, color = "darkblue", alpha = 0.9)+
  geom_line(color = "firebrick", size = 1.3)+
  scale_x_continuous(labels = function(x) paste("Season",x))+
  my_theme+
  labs(x = "", y = "IMDb rating",
       caption = "Code found at: https://github.com/cags9607 \n Data source: https://github.com/EmilHvitfeldt/friends",
       subtitle = "IMDb rating had its lowest value on the most neutral season (1st), while the highest rating was achieved on the most powerful season (4th).")

plot_grid(p1,p2, nrow = 2)
