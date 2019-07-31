# LIBRARY -----------------------------------------------------------------

library(tidyverse)
library(scico)
library(lubridate)
library(showtext)

# Data And Font --------------------------------------------------------------------

video_games_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv")

video_games <- video_games_raw %>% 
  mutate(release_date = ifelse(release_date == "8 Oct, 2014", "Oct 8, 2014", release_date),
         release_date = mdy(release_date),
         release_year = year(release_date),
         release_month = month(release_date, label = T),
         game = ifelse(str_detect(game,pattern = "^(Nioh: Complete)"), "Nioh: Complete Edition", game)
  ) 

font_add_google("Merriweather","Merriweather")
font_add_google("Roboto Mono", "Roboto Mono")

# Theme-Set ---------------------------------------------------------------

mytheme <- theme(
  text = element_text( family= "Merriweather", color = "black"),
  axis.text = element_text(color = "black"),
  axis.title = element_text( face = "bold"),
  plot.background = element_rect(fill = "grey40", color = NA),
  panel.background = element_rect(fill = "grey40", color = NA),
  panel.grid= element_blank(),
  legend.position = "bottom"
)

theme_set(theme_minimal()+mytheme)

# barplot -----------------------------------------------------------------

barplot <- video_games %>% 
  top_n(n = 20,wt = average_playtime) %>% 
  ggplot(aes(fct_reorder(game, average_playtime), average_playtime, fill = factor(release_year)))+
  geom_col()+
  scale_y_continuous(breaks = seq(0,5000, 1000))+
  scale_fill_scico_d(palette = "bamako", begin =  0.2, end= 0.8, direction = 1)+
  guides( fill = guide_legend(title = "Released Year",
                              title.position = "top",
                              label.position = "bottom",
                              nrow = 1))+
  coord_flip()+
  theme(
    axis.title.x = element_text(color = "grey20"),
    panel.grid.major.x = element_line(linetype = 2, color = "grey30"),
    legend.key.height = unit(0.5, "lines"),
    legend.key.width = unit(2, "lines")
  )+
  labs(y = "\n(play-time in minutes)", x = "", 
       title = "Top 20 Games with Longest Average play-time")


# Heatmap -----------------------------------------------------------------

heatmap <- video_games %>% 
  group_by(release_year,release_month) %>% 
  summarise(avg = median(price, na.rm = T)) %>% 
  ggplot(aes(x = factor(release_year),y =  release_month, fill = avg))+
  geom_raster(interpolate =F)+
  scale_fill_scico(palette = "bamako")+
  guides(fill = guide_colorbar(title = "Median Price of Games",
                               title.position = "top",
                               barwidth = unit(15, "lines"),
                               ticks.colour = "grey10"))+
  labs(x = NULL,
       y = "",title = NULL)

# Header ann Footer -------------------------------------------------------

header <- ggplot()+
  labs(title = "Video Games Played in the Last Two Weeks",
       subtitle = str_wrap("Median Price of Video Games has decreased in recent years
       and It seems that Most of the Games with longest Average playtime released in recent years",
                           width = 100))+
    theme(plot.title = element_text(size = 25,margin = margin(t = 2,b =1,unit = "mm")),
          plot.subtitle = element_text(family = "Roboto Mono", size = 16, color = "grey10"))

footer <- ggplot()+
  labs(caption = "Viz-by: @shafyet_shafee | Data Source: @brightcdns via Steam Spy")+
  theme(plot.caption = element_text(family = "Roboto Mono", size = 8))
# Assemble ----------------------------------------------------------------

png("Video Games.png", width = 1500, height = 700, res = 144, bg = "transparent")

showtext_auto()
gridExtra::grid.arrange(
  header,
  heatmap,
  barplot,
  footer,
  widths = c(55,45),
  heights = c(25,65,5),
  layout_matrix = rbind(
    c(1,1),
    c(2,3),
    c(4,4)
  )
)
