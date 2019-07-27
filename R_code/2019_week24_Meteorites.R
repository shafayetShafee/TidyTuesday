# LIBRARY -----------------------------------------------------------------

library(tidyverse)
library(showtext)
library(sysfonts)
library(ggalt)


# DATA --------------------------------------------------------------------

meteorites_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv")

meteorites <- meteorites_raw %>% 
  mutate(decade = year - year%%10) %>% 
  filter(decade>=1750, decade<=2010) %>% 
  select(name,mass,fall, year,decade, lat,long) %>% 
  drop_na()

# adding Google fonts to showtext database
font_add_google("Merriweather","Merriweather")
font_add_google("Roboto Mono","Roboto Mono")

# Line-plot ---------------------------------------------------------------

meteorites_count <- meteorites %>% filter(fall == 'Fell') %>%  count(decade) # filtering the fallen meteorites only

lineplot <- meteorites_count %>% 
  ggplot(aes(decade,n))+
  geom_line(size=1.2,color = "grey10")+
  geom_point(color = "#FFFF00FF",size = 3)+
  theme_minimal()+
  scale_x_continuous(expand = c(0,0),limits = c(1745,2015))+
  theme(
    text = element_text(family = "Merriweather",color = "white"),
    panel.background = element_rect(fill = "grey20", color = "grey20"),
    plot.background = element_rect(fill = "grey20", color = "grey20"),
    panel.grid.major.x = element_line(size = .02,linetype = 1,color = "grey40"),
    panel.grid.minor =element_blank(),
    panel.border = element_rect(color = "grey20", fill = NA),
    panel.grid.major.y =  element_line(size = .02,linetype = 1,"grey40"),
    axis.text = element_text(family = "Merriweather",color = "white",size = 14),
    axis.title.x = element_text(size =16,color = "white")
  )+
  labs(x = "\nNumber of Meteorites per decade",y = NULL,title = "",subtitle = "")


# MAP-PLOT ----------------------------------------------------------------

mapplot <- ggplot(data = map_data("world"))+
  geom_map(map=  map_data("world"),aes(long,lat,map_id = region),fill ="black")+
  coord_proj("+proj=robin")+
  geom_jitter(data = filter(meteorites,fall == "Fell"),aes(long,lat),color = "#0924ed", alpha = 0.6)+
  theme(panel.background = element_rect(fill = "grey20",color = "grey20"),
        plot.background = element_rect(fill = "grey20",color = "grey20"),
        panel.border = element_rect(color = "grey20", fill = NA),
        legend.position = "none",
        panel.grid.major = element_line(size = 0.15,color = "grey40"),
        axis.text = element_text(family = "Merriweather",color = "white",size = 14)
  )+
  labs(x = NULL, y = "\n\n")


# BAR-PLOT ----------------------------------------------------------------
options(scipen=999)

bar_plot <- meteorites %>% 
  filter(fall == "Fell",
         year > 2000) %>% 
  top_n(16,mass) %>% 
  mutate(name = paste0(name,"(",year,")"),
         name = fct_reorder(name,mass)) %>% 
  ggplot(aes(name,mass,fill = mass))+
  geom_col()+
  geom_hline(yintercept = 0,color = "grey40",size = 0.15)+
  scale_fill_gradientn(colors = heat.colors(16,rev = TRUE),
                       labels = function(x)paste0(round(x/1000)))+
  guides(fill = guide_colorbar(title = "mass (per Tonne)",
                               title.position = "left",
                               barheight = unit(8,"lines")))+
  coord_flip()+
  scale_y_continuous(labels = function(x)paste0(round(x),"kg"), expand = c(0,0))+
  theme(panel.background = element_rect(fill = "grey20",color = "grey20"),
        plot.background = element_rect(fill = "grey20",color = "grey20"),
        legend.background = element_rect(fill="grey20",color = "grey20"),
        text = element_text(family = "Merriweather",color = "white"),
        panel.grid = element_blank(),
        panel.grid.major.x =element_line(color = "grey40",size = 0.15),
        panel.border = element_rect(color = "grey20", fill = NA),
        axis.ticks = element_blank(),
        axis.text = element_text(family = "Merriweather",color = "white",size = 13),
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 13),
        legend.title = element_text(angle = 90, hjust = 0.5, vjust = 0.5,size = 14)
        )+
  labs(y = "\nName and Mass of 16 Most heavy Meteorites (fallen) from 2001 to 2010          ",
       x= "",
       title = "", 
       subtitle = "")



# TITLE AND CAPTION -------------------------------------------------------

title <- ggplot()+
  xlim(-1,1)+
  ylim(-1,1)+
  labs(x = NULL,y  = NULL,
       title = "\nFallen Meteorites Across the World Since 1750",
       subtitle = glue::glue("\nHere considered only those meteorites- which were seen to fall" ," from the sky and tracked down successfully\n\n"))+
  theme(
    line = element_blank(),
    plot.title = element_text(family = "Merriweather", size = 35,color = "white"),
    plot.subtitle = element_text(family = "Roboto Mono", size = 25, color = "grey98"),
    panel.background = element_rect(fill = "grey20",color = "grey20"),
    plot.background = element_rect(fill = "grey20",color = "grey20"),
    panel.border = element_blank(),
    axis.text = element_blank(),
  )

caption <- ggplot()+
  labs(x = NULL,y = NULL,
       caption = "ViZ by: @Shafayet_shafee  | Data-Source: NASA")+
  theme(line = element_blank(),
        plot.background = element_rect(fill = "grey20",color = "grey20"),
        panel.background = element_rect(fill = "grey20",color = "grey20"),
        panel.border = element_rect(fill = "grey20", color = "grey20"),
        axis.text = element_blank(),
        plot.caption = element_text(size = 8, family = "Roboto Mono", color = "white"))

null_plot <- ggplot()

# THEME_SET ---------------------------------------------------------------

my_theme <- theme(
  panel.background = element_rect(fill = "grey20",color = "grey20"),
  plot.background = element_rect(fill = "grey20",color = "grey20"),
  panel.border = element_blank()
)

theme_set(theme_minimal()+my_theme)

# ASSEMBLE ----------------------------------------------------------------

png("Fallen_Meteorites.png", height = 900, width = 2800, res = 144, bg = "grey20")

showtext_auto()
gridExtra::grid.arrange(
  title,
  null_plot,
  lineplot,
  mapplot,
  bar_plot,
  caption,
  heights = c(13,2,5,75,5),
  widths = c(26,43,31),
  layout_matrix = rbind(
    c(1,1,1),
    c(2,2,2),
    c(3,4,2),
    c(3,4,5),
    c(6,6,6)
  )
)
#dev.off()
