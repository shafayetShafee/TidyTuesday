
# PACKAGES ----------------------------------------------------------------

library(tidyverse)
library(ggforce)
library(lubridate)
library(gridExtra)
library(showtext)

# DATA --------------------------------------------------------------------

r4ds_members <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-16/r4ds_members.csv") %>% 
  mutate(
    date = ymd(date),
    year = factor(year(date)),
    month = factor(month(date, label = TRUE), levels = month.abb),
    weekday = factor(wday(date, label = TRUE))
  )
font_add_google("B612 Mono","B612 Mono")


# DAILY-ACTIVITY ----------------------------------------------------------

daily <- ggplot(r4ds_members, aes(date, daily_active_members))+
  geom_line(lwd = 1)+
  facet_wrap(~year, ncol = 1, scales = "free")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  theme_minimal()+
  theme(
    axis.text = element_text(color = "grey5", family = "B612 Mono", size = 10),
    axis.title = element_text(color = "grey5", family = "B612 Mono", size =14),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#f5f1bc", color = "#f5f1bc"),
    plot.background = element_rect(fill = "#f5f1bc", color = "#f5f1bc"),
    panel.border = element_rect(color = "grey20", fill =NA, size = 1),
    panel.spacing.y = unit(.5,"lines"),
    strip.text = element_text(size = 13, face = "bold", family = "B612 Mono")
  )+
  labs(x = NULL, y = "Number of Daily Active Members", 
       title =" ")

# Total-Activity ----------------------------------------------------------

total_data <- r4ds_members %>% 
  filter(mday(date) %in% c(1,30)) 

total <- ggplot(data = r4ds_members, aes(date,total_membership))+
  geom_line(color = "grey30", size = 1)+
  geom_point(data = total_data, aes(date,total_membership), color = "grey10", size = 2)+
  scale_x_date(date_breaks = "3 month", date_labels = "%b\n%Y", expand = c(0.05,0.05))+
  scale_y_continuous(labels = function(x)paste0(x/1000,"K"))+
  theme_minimal()+
  theme(
        text = element_text(family = "B612 Mono"),
        axis.text = element_text(color = "grey5"),
        axis.title = element_text(size = 13),
        panel.grid = element_line(color = "grey20", linetype = 2),
        # panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#f5f1bc", color = "#f5f1bc"),
        plot.background = element_rect(fill = "#f5f1bc", color = "#f5f1bc"),
        panel.border = element_rect(color = "grey20", fill =NA),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 13, face = "bold")
  )+
  labs(
    x = NULL, subtitle = "\nTotal Membership", y  = "Number of Members", 
     title = "R4DS Online Learning Community Member's Stats"
    
  )


# Proportion --------------------------------------------------------------

prop <- ggplot(r4ds_members, aes(date, daily_active_members/total_membership))+
  geom_line(lwd = 1, color = "grey20")+
  geom_mark_circle(aes(
    filter = (daily_active_members/total_membership) == min(daily_active_members/total_membership), 
    label = paste0("On ",mday(date)," ",month,",", year),
    description = paste0("the percentage of active members reached its lowest (",round(min(daily_active_members/total_membership)*100,2),"%)")),
    expand = unit(1.5, "mm"),label.fill = "#f5f1bc",
    con.colour = "grey5",
    con.size = 0.8,
    con.linetype = 1,
    label.buffer = unit(10,"mm"),
    label.family = c("B612 Mono","B612 Mono"),
    label.fontface = c("bold","italic"),
    label.fontsize = c(11,9)
    )+
  scale_y_continuous(labels = scales::percent_format())+
  scale_x_date(date_breaks = "2 month", date_labels = "%b\n%Y")+
  theme(
        text =   element_text(family = "B612 Mono"),
        axis.text = element_text(color = "grey5"),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#f5f1bc", color = "#f5f1bc"),
        plot.background = element_rect(fill = "#f5f1bc", color = "#f5f1bc"),
        plot.subtitle = element_text(size = 13, face = "bold"),
        panel.border = element_rect(color = "grey20", fill =NA)
  )+
  labs(x = NULL, y = NULL,
       title = "",
       subtitle = "Percentage of Active Members Out of Total members",
       caption = "ViZ_by: @shafayet_shafee")
  


# Desccription ------------------------------------------------------------
windows()
showtext_auto()

description <- ggplot()+
  xlim(-1,1)+
  ylim(-1,1)+
  annotate("text",
           x = -1, 
           y = 0,
           hjust = 0,
           vjust = 0.5,
           lineheight = 0.85, 
           size = 10,
           fontface = "plain",
           color = "grey10",
           label = str_wrap(
                      c("The number of Total members has increased to 3K 
                        since it's founding through the start of July 2017.
                        But the number of daily active members remained quite same
                        Hence the proportion of active members to Total members has declined over time"),
                            width = 20))+
  theme_void()+
  theme(
    text =   element_text(family = "B612 Mono"),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#f5f1bc", color = "#f5f1bc"),
    plot.background = element_rect(fill = "#f5f1bc", color = "#f5f1bc")
    )



# patch-up ----------------------------------------------------------------

png("R4DS_daily_activity.png", width = 1650, height = 1650, res = 144, bg = "transparent")

showtext_auto()
grid.arrange(
  total,
  daily,
  description,
  prop,
  heights = c(25,50,25),
  widths = c(80,30),
  layout_matrix = rbind(
    c(1,1),
    c(2,3),
    c(4,4)
  )
)


