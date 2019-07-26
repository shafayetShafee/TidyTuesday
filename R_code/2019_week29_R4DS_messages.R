
# LIBRARY -----------------------------------------------------------------

library(tidyverse)
library(ggrepel)
library(lubridate)
library(waffle)
library(showtext)

font_add_google("Quattrocento","Quattrocento Oswald")
font_add_google("Roboto Mono","Roboto Mono")


# DATA --------------------------------------------------------------------


r4ds_members <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-16/r4ds_members.csv") %>% 
  mutate(
    date = ymd(date),
    year = factor(year(date)),
    month = factor(month(date, label = TRUE), levels = month.abb),
    weekday = factor(wday(date, label = TRUE))
  )

# WEEK-day  activity ----------------------------------------------------------------

p1 <- ggplot(r4ds_members)+
  geom_area(aes(date, weekly_members_posting_messages, color =" Weekly members posting messages", fill = " Weekly members posting messages"), alpha = 0.3)+
  geom_area( aes(date, weekly_active_members,color = "Weekly active members", fill = "Weekly active members"), alpha = 0.3)+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  facet_wrap(~year, scales = "free", ncol = 1)+
  theme_minimal()+
  labs(x = "",
       y = "")+
  scale_color_manual("", values = c(  " Weekly members posting messages" = "red","Weekly active members" = "#a805e8"),
                      )+
  scale_fill_manual("", values = c(" Weekly members posting messages" = "red", "Weekly active members" = "#a805e8"))+
  guides(fill = guide_legend(reverse = T), color = guide_legend(reverse = T))+
  theme(legend.position = "top",
        panel.spacing.y = unit(0.5,"lines"),
        text = element_text(family = "Roboto Mono")
        )


# DAILY-MESSSAGES ---------------------------------------------------------

top_activity <- r4ds_members %>% 
  select(date, daily_active_members)  %>% 
  arrange(desc(daily_active_members))

p2 <- ggplot(r4ds_members)+
  geom_area( aes(date, daily_members_posting_messages ,color = "Daily Active Members posting messages", fill = "Daily Active Members posting messages"), alpha = 0.3)+
  geom_area(aes(date, daily_active_members, color ="Daily Active Members", fill = "Daily Active Members"), alpha = 0.3)+
  theme_minimal()+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  facet_wrap(~year, scales = "free", ncol = 1)+
  labs(x = NULL,
       y = "Number of Members")+
  scale_color_manual("", values = c( "Daily Active Members posting messages" = "#ff0000", "Daily Active Members" = "#ff8800")
  )+
  scale_fill_manual("", values = c( "Daily Active Members posting messages" = "#cf2525","Daily Active Members" = "#f2901f"))+
  theme(legend.position = "top",
        panel.spacing.y = unit(0.5,"lines"),
        text = element_text(family = "Roboto Mono"),
        legend.spacing.x = unit(1, "lines")
  )


# Waffle-chart ------------------------------------------------------------

messages <- r4ds_members %>% 
  group_by(year, month) %>% 
  summarise(
    public_mess = sum(messages_in_public_channels),
    private_mess = sum(messages_in_private_channels),
    shared_mess  = sum(messages_in_shared_channels),
    dms = sum(messages_in_d_ms),
    total = sum(public_mess,private_mess,shared_mess,dms)
  ) %>% 
  mutate(
    date = myd(paste(month, year,"1"))
  )

levels <- messages %>% mutate(date = paste(month,"\n", year)) %>% pull(date) %>% unique()


mess <- gather(messages, key = "message", value = "number", -year, -month,-total,-date) %>% 
  ungroup() %>% 
  mutate(date = paste(month,"\n",year),
         date = factor(date, levels = levels))

windows()
p3 <- ggplot(data = mess,aes(fill = message, values = number))+
  geom_waffle(color = "white",size = 0.10, n_rows = 10, flip = TRUE, make_proportional = TRUE)+
  facet_wrap(~date, nrow = 1, strip.position = "bottom")+
  scale_x_discrete()+
  scale_y_continuous(labels = function(x)paste0(x * 10,"%"),
                     expand = c(0,0)) +
  scale_fill_manual(values = c('dms' = "#a67872",
                               "private_mess" = "#332323",
                               "public_mess" = "#ed2207"),
                    labels = c("dms" = "Direct Message",
                               "private_mess" = "Private Messages",
                               "public_mess" = "Public Message"),
                    name = NULL)+
  theme_minimal()+
  theme(
    text = element_text(family = "Roboto Mono"),
    legend.position = "bottom",
    panel.grid = element_blank(),
    plot.subtitle = element_text(face = "bold", size = 12)
  )+
  labs(x = NULL, y = "Percentages",
       caption = "VIZ by @shafayet_shafee", 
       title = "",
       subtitle = "Proportion of Messages Posted in Different Channels")


# header ------------------------------------------------------------------

windows()
showtext_auto()
p0 <- ggplot()+
  xlim(-1,1)+
  ylim(-1,1)+
  labs(title = " R4DS Slack Messages",
       subtitle = " There are total 60k messages posted on Slack Since July 2017\n")+
  theme_void()+
  theme(text = element_text(family = "Roboto Mono"),
        plot.title = element_text(face = "bold", size = 24),
        plot.subtitle = element_text(size = 14))



# ASSEMBLE ----------------------------------------------------------------

png("R4DS_messages01.png", width = 1650, height = 1400, res = 144, bg = "white")

windows()
showtext_auto()
gridExtra::grid.arrange(p0,p2,p1,p3,
                        widths = c(50,50),
                        heights = c(7,29,29,35),
                        layout_matrix = rbind(
                         c(1,1),
                         c(2,3),
                         c(2,3),
                         c(4,4)
                        ))
