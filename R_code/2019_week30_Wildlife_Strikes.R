
# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(ggchicklet)
library(lubridate)
library(showtext)
library(ggsci)
library(scico)


# DATA --------------------------------------------------------------------

bird_impacts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-23/wildlife_impacts.csv")

birds <- bird_impacts %>% 
  mutate(incident_month = month(ymd(incident_date),label = TRUE))


# Adding some google fonts to showtextdb ----------------------------------

font_add_google("Atma","Atma Light")
font_add_google("Roboto Mono","Roboto Mono")
font_add_google("Merriweather","Merriweather")

# YEAR-MONTH-TILEMAP ------------------------------------------------------

yearMOnth <- birds %>% 
  count(incident_year, incident_month = fct_rev(incident_month)) %>%  
  ggplot(aes(x = factor(incident_year),y = incident_month))+
  geom_tile(aes(fill = n))+
  scale_fill_gsea()+
  labs(x = NULL, y = "",
       subtitle = "Bird's collision with airplanes Since 1990",
       title = "Reported Wildlife collision Since 1990 With The Big Four USA Airlines")+
  guides(fill = guide_colorbar(title = "Number of Strikes", title.position = "right",
                               barheight  = unit(10,"lines"),
                               label.position = "left" 
                               ))+
  theme_minimal()+
  theme(
        text = element_text(family = "Roboto Mono"),
        legend.position = "right",
        axis.ticks = element_blank(),
        axis.text = element_text(color = "grey20", face = "plain"),
        legend.text = element_text(color = "grey20", face = "bold", 
                                   angle = 90, vjust = 0.5, hjust = 0.5),
        legend.title = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        plot.subtitle = element_text(size = 14, color = "grey20"),
        plot.title = element_text(size = 24, face = "bold"),
        )


# top frequent birds ------------------------------------------------------

# creating a function to grab the top 5 most frequent species in each year
# excluded the unknown species 
top5 <- function(x){
  return(birds %>% 
           filter(!(species == str_extract_all(species,"^(Unknown)[[a-z],\\s,-]+")) & incident_year == x) %>%    count(species) %>% 
           top_n(5, wt = n) %>% pull(species))
}

bird2001 <- birds %>% 
  filter(species %in% top5(2001)  & incident_year == 2001) %>% 
  count(incident_year,incident_month, species)

for (i in 2002:2018) {
  data <- birds %>% 
    filter(species %in% top5(i)  & incident_year == i) %>% 
    count(incident_year,incident_month, species)
  
  bird2001 <- bind_rows(bird2001,data)
}

topBird <- bird2001 %>% 
  filter(incident_year > 2014) %>% 
  ggplot(aes(x = incident_month, y = n))+
  geom_line(size = 1,aes(color = species, group = species))+
  geom_point(pch = 21, aes(color = species, fill = species), size = 4)+
  scale_color_manual(values = c(
    "Barn swallow" = "#fc3b19","Mourning dove" = "#2b48ff", # red and blue
    "Horned lark" = "#480080", "Cliff swallow" = "#49a315", # violet and dark green
    "American kestrel" = "#3ad9fc", "Perching birds (y)" = "#4d5557", # light blue and dark grey
    "Killdeer" = "#ed2176", "Chimney swift" = "#8cfc14", # pink and light green
    "European starling" = "#520100", "Brazilian free-tailed bat" = "#fbff0f" # orange and yellow
  ))+
  scale_fill_manual(values = c(
    "Barn swallow" = "#fc3b19","Mourning dove" = "#2b48ff", # red and blue
    "Horned lark" = "#480080", "Cliff swallow" = "#49a315", # violet and dark green
    "American kestrel" = "#3ad9fc", "Perching birds (y)" = "#4d5557", # light blue and dark grey
    "Killdeer" = "#ed2176", "Chimney swift" = "#8cfc14", # pink and light green
    "European starling" = "#520100", "Brazilian free-tailed bat" = "#fbff0f" # orange and yellow
  ))+
  guides(fill = guide_legend(title.position = "top"), color = guide_legend(title.position = "top"
                                                                           ))+
  facet_wrap(~incident_year, scales = "free")+
  theme_minimal()+
  labs(x = "", y = "Number of Strikes",
       subtitle = "\nWhat are most common Bird Species involved in Collisions with Airplanes in recent years??"
       )+
  theme(legend.position = "right",
        panel.grid = element_line(color = "grey90"),
        panel.grid.minor.y = element_blank(),
        #panel.grid.major.y = element_blank(),
        panel.spacing = unit(0.7,"lines"),
        #legend.spacing.x = unit(2, "lines"),
        text = element_text(family = "Roboto Mono"),
        strip.text = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 14, color = "grey18")
  )


# Phase of flt ------------------------------------------------------------

# cleaning the phase_of_flt column a bit 
df <- birds %>% 
  mutate(phase_of_flt = case_when(
    phase_of_flt %in%  c("TAKE-OFF RUN","Take-off run","take-off run","Take-off Run")~"Take-off Run",
    phase_of_flt %in% c("Landing Roll","landing roll","LANDING ROLL","Landing roll")~"Landing Roll",
    phase_of_flt %in% c("DEPARTURE")~"Departure",
    phase_of_flt %in% c("CLIMB","climb")~"Climb",
    phase_of_flt %in% c("ARRIVAL")~"Arrival",
    phase_of_flt %in% c("approach","APPROACH")~"Approach",
    TRUE~phase_of_flt) 
  ) %>% 
  filter(!phase_of_flt %in% c("Unknown",NA) )

phase <- df %>% 
  count(operator, phase_of_flt) %>% 
  mutate(operator = str_to_title(operator),
        operator = str_replace(operator," ","\n")) %>% 
  ggplot(aes(operator, phase_of_flt))+
  geom_raster(aes(fill = n),interpolate = T)+
  # scale_fill_gsea()
  scale_fill_gradient2(low = "#04bfd4", high = "#FF3200",mid = "#faec82", midpoint = 4000)+
  guides(fill = guide_colorbar(title = "Number of Strikes", title.position = "right",
                               barheight  = unit(8,"lines"),
                               label.position = "left" ))+
  theme_minimal()+
  labs(y = NULL, x = NULL,
       subtitle = "In Which Flight-state planes are most likely to strike birds??")+
  theme(legend.position = "right",
        legend.text = element_text(color = "grey20", face = "bold", 
                                   angle = 90, vjust = 0.5, hjust = 0.5),
        legend.title = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        text = element_text(family = "Roboto Mono"),
        plot.subtitle = element_text(color = "grey20", size = 12))

# time of day and sky conditions ------------------------------------------

con <- birds %>% 
  count(operator, time_of_day, sky) %>% filter(!is.na(time_of_day) & !is.na(sky))

time_sky <- con %>% ggplot(aes(time_of_day,sky, fill = n))+
  geom_tile()+
  scale_fill_viridis_c(option = "plasma", end =0.95)+
  #scale_fill_gradientn(colors =wes_palette("Zissou1", 10, type = "continuous"))+
  guides(fill = guide_colorbar(title = "Number of Strikes", title.position = "right",
                               barheight  = unit(8,"lines"),
                               label.position = "left" ))+
  facet_wrap(~operator, nrow = 2)+
  theme_minimal()+
  labs(y = " ", x = "")+
  theme(legend.position = "right",
        legend.text = element_text(color = "grey20", face = "bold", 
                                   angle = 90, vjust = 0.5, hjust = 0.5),
        legend.title = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        axis.ticks = element_blank(),
        text = element_text(family = "Roboto Mono")
        )


# Desccription ------------------------------------------------------------

description <- ggplot()+
  xlim(-1,1)+
  ylim(-1,1)+
  annotate("text",
           x = -1, 
           y = 0,
           hjust = 0,
           vjust = 0.5,
           lineheight = 0.85, 
           size = 8,
           fontface = "plain",
           family = "Merriweather",
           color = "grey10",
           label = str_wrap(
             c("The number of Wildlife Strikes  have increased in some Recent years and occurred 
               mostly from July to October. Interestingly these incidences happened mostly during Daytime and
               with Fresh sky conditions and near the ground level.
               And what about the Bird species involved in these strikes in recent years ??"),
             width = 20))+
  theme_void()+
  theme(
    axis.text = element_blank(),
    panel.grid = element_blank()
  )

footer <- ggplot()+
  xlim(-1,1)+
  ylim(-1,1)+
  annotate("text",
           x = 1, 
           y = 0,
           hjust = 0.8,
           vjust = 0.5,
           lineheight = 0.85, 
           size = 4,
           family = "Atma Light",
           label = "Data Source: The FAA Wildlife Strike Database\nViz by: @shafayet_shafee"
           )+
  theme_void()+
  theme(
    axis.text = element_blank(),
    panel.grid = element_blank()
  )
  

# Assemble ----------------------------------------------------------------

png("wildlife_strikes-01.png", width = 1650, height = 2000, res = 144, bg = "white")

showtext_auto()
gridExtra::grid.arrange(
  yearMOnth,
  time_sky,
  phase,
  description,
  topBird,
  footer,
  heights = c(26,21,21,30,2),
  widths = c(80,20),
  layout_matrix = rbind(
    c(1,1),
    c(2,4),
    c(3,4),
    c(5,5),
    c(6,6)
  )
  
)

# Total and most dmg ------------------------------------------------------


birds %>% 
  count(operator)%>% drop_na() %>% 
  mutate(operator = fct_reorder(operator,n)) %>%  
  ggplot(aes(operator,n, fill = operator))+
  geom_col()+
  scale_fill_scico_d(palette = "devon", direction = -1,begin=0.2, end = 0.5)+
  coord_flip()+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text =element_text(color = "grey20", face = "bold"))+
  labs(x = NULL, y = "Number of times the Airlines faced birds strike")



birds %>% 
  count(operator,damage) %>% drop_na() %>% filter(!damage == "N") %>% 
  mutate(operator = fct_reorder(operator,n)) %>% 
  ggplot(aes(operator, n, fill = damage))+
  geom_chicklet()+
  scale_fill_manual(values = c("S" = "#800503", "M" = "#010c5e", "M?" = "#3d3737"),
                    labels = c("M" = "Minor", "M?" = "Uncertain", "S" = "Substantial"),
                    name = "Damage level")+
  guides(fill = guide_legend(title.position = "top", label.hjust = 0.5))+
  coord_flip()+
  labs(y = "Counts of strikes", x = NULL)+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.spacing.x = unit(0.9,"lines"))


# Top 10 most frequent bird species with the height at impact -------------

# finding most frequent species name
topSpecies_name <- birds %>%  
  count(species,sort = T) %>% 
  filter(!(species == str_extract_all(species,"^(Unknown)[[a-z],\\s,-]+"))) %>% 
  top_n(10) %>% pull(species)

# finding the total occurance of those most frequent birds
most_freq <-  birds %>% 
  count(species,sort = T) %>% 
  filter(!(species == str_extract_all(species,"^(Unknown)[[a-z],\\s,-]+"))) %>% 
  top_n(10) 

data <- birds %>% 
  filter(species %in% topSpecies_name & !(species == str_extract_all(species,"^(Unknown)[[a-z],\\s,-]+")))

final <- left_join(data,most_freq , by = "species")


final%>% 
  mutate(species = fct_reorder(species,n)) %>% 
  ggplot()+
  geom_jitter(aes(x = species, y = height, color = n), width = 0.25)+
  scale_color_viridis_c()


