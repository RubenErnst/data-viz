rm(list = ls())

library(tidyverse)
library(showtext)
library(MetBrewer)
library(ggside)

df <- read_csv("https://raw.githubusercontent.com/rusergroupstgallen/rusergroupstgallen.github.io/main/San%20Francisco%20Rent%20Exercise/rent_san_francisco_download.csv")

# Font
font <- "Montserrat"
font_add_google(family=font, font, db_cache = FALSE)
showtext_auto(enable = TRUE) 
theme_set(theme_minimal(base_family = font))
bg <- "#494949"
txt_col <- "#DEA2A2"

# Data wrangling
df <- df %>%
  select(year, price, county) %>% 
  drop_na() %>% 
  group_by(year, county) %>% 
  summarize(median_price = median(price)) %>% 
  ungroup()

df$county[df$county == "marin"] <- "Marin"
df$county[df$county == "napa"] <- "Napa"
df$county[df$county == "san francisco"] <- "San Francisco"
df$county[df$county == "santa clara"] <- "Santa Clara"
df$county[df$county == "alameda"] <- "Alameda"
df$county[df$county == "sonoma"] <- "Sonoma"
df$county[df$county == "santa cruz"] <- "Santa Cruz"
df$county[df$county == "san mateo"] <- "San Mateo"
df$county[df$county == "solano"] <- "Solano"
df$county[df$county == "contra costa"] <- "Contra Costa"

# Plot
df %>%
  ggplot(aes(x = year, y = median_price, group = county)) +
  geom_line(aes(color = county)) +
  geom_point(data = df %>% filter(year == 2018),
             aes(color = county)) +
  geom_text(data = df %>% filter(year == 2018),
            aes(label = county, color = county),
            hjust = -0.2,
            family = font,
            size = 4) +
  coord_cartesian(clip = "off") +
  scale_color_manual(
    values = met.brewer(name = "Benedictus",
                        type = "discrete",
                        n = 10)) +
  scale_y_continuous(limits = c(0,4500),
                     expand = c(0,0)) +
  labs(title = "Median rent prices\nin the Bay Area",
       caption = "Ruben Ernst | Data: Kate Pennington & Census.gov",
       x = "",
       y = "Median rent price (USD)") +
  theme(panel.grid = element_blank(),
        axis.text = element_text(color = txt_col,
                                 size = 10),
        axis.title = element_text(color = txt_col,
                                  size = 12),
        plot.title = element_text(size = 36,
                                  hjust =.5,
                                  color = txt_col,
                                  face = "bold",
                                  margin = margin(0,0,30,0)),
        plot.caption = element_text(hjust = .5, color= txt_col),
        legend.position = "none",
        plot.margin = margin(50, 100, 50, 50),
        plot.background = element_rect(fill = bg, color = bg))
