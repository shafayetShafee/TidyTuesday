# LIBRARY -----------------------------------------------------------------

library(tidyverse)
library(ggchicklet)
library(ggrepel)
library(ggthemes)
library(scico)
library(LaCroixColoR)
library(showtext)
library(gridExtra)

# DATA --------------------------------------------------------------------

media_franchises <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-02/media_franchises.csv")

media_franchises_year <- media_franchises %>% 
  mutate(franchise = paste0(franchise,"(", as.integer(year_created),")")) %>% 
  group_by(franchise) %>% 
  mutate(total = sum(revenue)) %>% 
  ungroup() %>% 
  group_by(owners) %>% 
  mutate(gross = sum(revenue)) %>% 
  ungroup() 

font_add_google("B612 Mono","B612 Mono") # adding fonts to showtext database


# Highest Grossing Media Stack-plot ---------------------------------------

top_franchise <- media_franchises_year %>% 
  group_by(franchise) %>% 
  summarise(total = sum(revenue)) %>% 
  arrange(desc(total)) %>% 
  top_n(10, total) %>% 
  pull(franchise)

chick_plot <- media_franchises_year %>% 
  filter(franchise %in% top_franchise) %>% 
  group_by(franchise) %>% 
  mutate(total = sum(revenue)) %>% 
  ungroup() %>% 
  arrange(desc(total)) %>% 
  mutate(franchise = fct_reorder(franchise,total)) %>% 
  ggplot(aes(franchise, revenue, fill=revenue_category))+
  geom_chicklet(width = 0.75)+
  geom_text(aes(x = franchise, y = total, label = paste0(" $ ",as.character(round(total,1))," B"), hjust = 0), size = 5, color = "grey45")+
  theme(legend.position = "right")+
  coord_flip()+
  labs(x =" ", y = " ",
       title ="Top Ten Highest Grossing Media Franchises",
       subtitle ="")+
  scale_y_continuous(breaks = c(0,20,40,60,80),labels = function(x)paste0("$",x,"B"), limits = c(0,95))+
  ggthemes::scale_fill_tableau(name = "Revenue Earned from:")+
  guides(fill = guide_legend(title.position = "top"))+
  theme(
    text = element_text(family = "B612 Mono"),
    plot.background = element_rect(fill = "#d7ddf7", color = "#d7ddf7"),
    panel.background = element_rect(fill = "#d7ddf7", color = "#d7ddf7"),
    plot.title = element_text(size = 16, face = "italic", color = "grey5", family = "B612 Mono"),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(linetype = 1, color = "grey50"),
    axis.text = element_text(color = "black", family = "B612 Mono", size = 13),
    axis.ticks.length.y =unit(0, "mm") ,
    axis.title.y = element_text(color = "black", family = "B612 Mono", size=14),
    axis.text.y = element_text(hjust = 1, size = 14),
    legend.background =  element_rect(fill = "#d7ddf7", color = "#d7ddf7"),
    legend.key = element_rect(color = "transparent", fill = "transparent"),
    legend.key.size = unit(2,"lines"),
    legend.spacing.x = unit(0.7,"lines"),
    legend.title = element_text(face = "italic", size = 14),
    legend.text = element_text(color = "grey2",size=12)
  )


# Lolipop plot ------------------------------------------------------------

lolipop <- media_franchises_year %>% 
  group_by(original_media) %>% 
  summarise(g = sum(revenue)) %>% 
  arrange(desc(g)) %>% 
  ungroup() %>% 
  mutate(original_media = fct_reorder(original_media, g)) %>% 
  ggplot(aes(original_media, g, color = original_media))+
  geom_point(size = 4)+
  geom_segment(aes(x = original_media, xend = original_media, y = 0, yend = g),lwd = 1.8)+
  coord_flip()+
  scale_y_continuous(expand =c(0,0) ,label = function(x)paste0("$",x," B"), limits = c(0,340))+
  labs(y = "\nNet Revenues",
       title = "\nOriginal Sources of franchises and Net revenue",x = "")+
  theme(text = element_text(family = "B612 Mono"),
        axis.title = element_text(family=  "B612 Mono", size = 14, color = "black"),
        axis.text.y = element_text(family = "B612 Mono", color = "black", size = 13),
        axis.text.x = element_text(family = "B612 Mono", size =12,color = "black"),
        legend.position = "none",
        panel.grid.major.y = element_line(color = "grey50", linetype = 2),
        plot.title = element_text(family = "B612 Mono", face = "italic",size = 16, color = "black")
  )


# Media Category Stack-plot -----------------------------------------------

Media_cat <- media_franchises_year %>% 
  mutate(decade = year_created - year_created%%10) %>% 
  group_by(decade) %>% 
  mutate(decade_total = sum(revenue)) %>% 
  ungroup() %>% 
  group_by(decade, revenue_category) %>% 
  mutate(cat_t = sum(revenue)) %>% 
  arrange(revenue_category, .by_group = T) %>% 
  ungroup() %>% distinct(revenue_category,decade, cat_t,decade_total) %>% 
  ggplot(aes(x = decade, y = cat_t, fill = revenue_category))+
  geom_chicklet()+
  geom_text(aes(x = decade, y = decade_total, label = paste0(" $ ",as.character(round(decade_total,1))," B"), hjust = 0), size = 5, color = "grey45")+
  coord_flip()+
  scale_x_continuous(breaks = seq(1920,2010,10))+
  scale_y_continuous(expand=c(0.005,0.005),label = function(x)paste0("$",x," B"), limits = c(0,390))+
  theme(legend.position = "none",
        panel.grid.major.x = element_line(color = "grey50", linetype = 2),
        plot.title = element_text(size = 16, face = "italic", color = "grey5", family = "B612 Mono"),
        plot.subtitle = element_text(family = "B612 Mono",size = 13, color = "grey5"),
        axis.text = element_text(color = "black", family = "B612 Mono", size = 13),
        axis.ticks.length.y =unit(0, "mm") ,
        axis.title.y = element_text(color = "black", family = "B612 Mono", size=14),
        axis.text.y = element_text(hjust = 1, size = 14)
  )+
  scale_fill_tableau()+
  labs(x = NULL, y = " ",
       title = "Revenues Earned from different category in each decade",
       subtitle = "**WWII lasted from 1939 to 1945** And 1940s has the lowest gross       ")



# Lowest Grossing  Media --------------------------------------------------

low <- media_franchises_year%>% 
  group_by(franchise) %>% 
  summarise(total = sum(revenue),
            cat = unique(original_media)) %>% 
  arrange(total) %>% 
  top_n(-10, total) %>% 
  mutate(franchise = fct_reorder(franchise,total)) %>% 
  ggplot(aes(franchise, total, fill = cat))+
  geom_chicklet()+
  theme(text = element_text(family = "B612 Mono"),
        panel.grid.major.x = element_line(color = "grey50", linetype = 2),
        plot.title = element_text(size = 16, face = "italic", color = "grey5", family = "B612 Mono"),
        plot.subtitle = element_text(family = "B612 Mono",size = 13, color = "grey5"),
        axis.text = element_text(color = "black", family = "B612 Mono", size = 13),
        axis.ticks.length.y =unit(0, "mm") ,
        axis.title.y = element_text(color = "black", family = "B612 Mono", size=14),
        axis.text.y = element_text(hjust = 1, size = 14),
        legend.position = "none",
  )+
  coord_flip()+
  labs(x = NULL, y = NULL, 
       title = "\nLowest Grossing Media Franchises")+
  scale_y_continuous(labels = function(x)paste0("$",x,"B"))+
  scale_fill_scico_d(palette = "imola")

# Setting up a theme ------------------------------------------------------

my_theme <- theme(
  plot.background = element_rect(fill = "#d7ddf7", color = "#d7ddf7"),
  panel.background = element_rect(fill = "#d7ddf7", color = "#d7ddf7"),
  panel.grid = element_blank(),
  panel.border = element_blank()
)

theme_set(theme_grey() + my_theme)


# Header and caption ------------------------------------------------------

header <- ggplot()+
  labs(title = " Media Franchise (1924-2013)",
       subtitle =" How do different media franchises stack up with their revenue streams?")+
  theme(plot.title = element_text(family = "B612 Mono", face = "bold",size = 37, color = "grey10"),
        plot.subtitle = element_text(family = "B612 Mono", face = "italic",size = 25, color = "grey10"),
        panel.border = element_blank())

caption <- ggplot()+
  labs(x = NULL,y = NULL,
       caption = "Visualization by Shafayet_shafee  | Data-Source: Wikipedia")+
  theme(line = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        plot.caption = element_text(size = 10, family = "B612 Mono", color = "black"))

null_plot <- ggplot()
# Assemble ----------------------------------------------------------------

png("Media-franchise-01.png", width = 2800, height = 1300, res = 100, bg = "white")

showtext_auto()

grid.arrange(
  header,
  chick_plot,
  Media_cat,
  lolipop,
  null_plot,
  low,
  caption,
  heights = c(15,40,40,4),
  widths = c(40,10,5,5,40),
  layout_matrix = rbind(
    c(1,1,1,1,1),
    c(2,2,2,3,3),
    c(4,4,5,6,6),
    c(7,7,7,7,7)
  ))

#dev.off()

# TOP 5 Owners ------------------------------------------------------------

### finding the income of each owner per revenue category
owners <- media_franchises_year %>% 
  group_by(owners,revenue_category) %>% 
  summarise(cat_inc = sum(revenue)) %>% 
  arrange(desc(cat_inc),.by_group=T) %>% 
  ungroup() %>% 
  group_by(owners) %>% 
  mutate(income = sum(cat_inc),
         prop = (cat_inc/income)*100) %>% 
  arrange(desc(income)) %>% 
  ungroup()

### selecting top 5 owners
topOwners <- owners %>% 
  distinct(owners, income) %>% 
  top_n(5) %>% 
  pull(owners)

point_plot <- media_franchises_year %>% 
  filter(owners %in% topOwners) %>% 
  mutate(owners = if_else(owners == "Nintendo (trademark) The Pokémon Company (Nintendo, Game Freak, Creatures) (copyright)","Nintendo(The Pokémon Company)", owners),
         owners = if_else(owners == "Lucasfilm (The Walt Disney Company)","Lucasfilm",owners),
         owners = paste0(owners,"\n($", round(gross, 1)," B)")) %>% 
  group_by(owners) %>% 
  arrange(total,year_created, .by_group = T) %>% 
  mutate( franchise = if_else(revenue == max(revenue), paste(franchise,"\nType : ",original_media),""),
          franchise = if_else(revenue == max(revenue), paste(franchise,"Revenue category : Merchandise\n",sep = "\n"),"")) %>% 
  ggplot(aes(owners, revenue, label = franchise))+
  geom_point(aes(color = owners), size = 2)+
  geom_text_repel(aes(color = owners),
                  family = "B612 Mono",
                  fontface = "italic",
                  direction = "y",
                  vjust = 1,
                  size=3.5,
                  min.segment.length = unit(5,"lines")
  )+
  annotate("text",x=1,y = 1, label = "Anpanman(1973)\nType : Manga\nRevenue category : Box Office\n", size = 3.5, family = "B612 Mono", color = "#C70E7B" ,vjust = 0)+
  annotate("text",x = 3, y = 29, label ="Pokémon(1996)\nType : Games\nRevenue category : Video Games\n",size = 3.5, family = "B612 Mono", color = "#A6E000" ,vjust = 0, hjust = 0.2 )+
  annotate("text", x = 5, y = 45, label = "Disney Princess(2000)\nType : Animated Series\nRevenue category : Merchandise\n",size = 3.5, family = "B612 Mono", color = "#6C6C9D" ,vjust = 0 , hjust = 0.9)+
  scale_color_manual(values = lacroix_palette("PassionFruit"))+
  scale_y_continuous(limits = c(0,90),breaks = seq(0,80,20), labels = function(x)paste0("$",x,"B"))+
  theme(text = element_text(family = "B612 Mono"),
        plot.background = element_rect(fill = "#d7ddf7", color = "#d7ddf7"),
        panel.background = element_rect(fill = "#d7ddf7", color = "#d7ddf7"),
        panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, color = "grey20"),
        legend.position = "none",
        axis.text.y = element_text(family = "B612 Mono", color = "grey20"),
        axis.text.x = element_text(family = "B612 Mono", size =10, face = "bold", color = "grey20"),
        axis.title = element_text(family = "B612 Mono", color = "grey20", size = 11),
        axis.ticks.x = element_blank(),
        plot.title= element_text(family = "B612 Mono", face = "bold",size = 16, color = "grey10"),
        plot.caption = element_text(family = "B612 Mono", size = 8, color = "grey30")
      
  )+
  labs(x = "\nOwners and their total revenue earned ", y = "Revenue from different revenue category\n",
       title = "              Top Five Highest Grossing Franchises Owners",
       subtitle = "",
       caption =  "Visualization by Shafayet_shafee  | Data-Source: Wikipedia")

ggsave("media-point.png", dpi=100, height = 7, width=15)
