library(ggwordcloud)
library(ggtext)
library(tidyverse)
library(tidytext)
library(tidylo)

# If you don't have the font:
#library(showtext)
#showtext_auto()
#font_add(family = "friends", regular = "GABRWFFR.TTF")

friends = readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv')

friends %>% 
  filter(speaker %in% c("Rachel Green","Ross Geller","Chandler Bing",
                        "Monica Geller","Joey Tribbiani","Phoebe Buffay")) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  count(speaker, word) %>% 
  filter(n>100) %>% 
  bind_log_odds(speaker, word, n) %>%
  mutate(sign = ifelse(log_odds_weighted<0, "Neg","Pos")) %>% 
  arrange(-log_odds_weighted) %>% 

ggplot(aes(
         label = word,
         size = abs(log_odds_weighted),
         color = sign,
         alpha = abs(log_odds_weighted)
       )) +
  geom_text_wordcloud_area(area_corr_power = 1, show.legend = F) +
  facet_wrap(~speaker)+
  scale_radius(range = c(3, 20)) +
  theme_void() +
  scale_color_manual(values = c("firebrick1", "skyblue"))+ 
  scale_alpha(range = c(.5, 1))+
  theme(
    plot.background = element_rect(fill = "#393536ff", color = NA),
    strip.text = element_text(
      family = "friends",
      size = 20,
      color = "white"
    )
,
    plot.margin = unit(rep(1, 4), "cm"),
    panel.spacing = unit(.5, "cm"),
    plot.title = element_text(
      family = "friends",
      size = 20,
      color = "#f4c93cff",
      hjust = .5,
      vjust = .5
    )
,
    plot.subtitle = element_markdown(
      hjust = .5,
      color = "white",
      size = 10
    )
,
    plot.caption = element_markdown(
      hjust = .5,
      vjust = .5,
      color = "gray80"
    )
  ) +
  labs(title = "Friends (most iconic words by character)",
       subtitle = "<span style='color:skyblue'>Blue</span> words are more likely to be spoken by the character, while <span style='color:firebrick1'>red</span> words are less likely.<br> (The size and transparency are proportional to the absolute value of the log odds ratio weighted by uninformative Dirichlet prior, and the color is based on the sign)<br>",
       caption = "Code found at: github.com/cags9607 <br> Data source: github.com/EmilHvitfeldt/friends"
       )
