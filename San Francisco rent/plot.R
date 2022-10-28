rm(list = ls())

library(tidyverse)
library(showtext)
library(MetBrewer)
library(patchwork)

df <- read_csv("https://raw.githubusercontent.com/rusergroupstgallen/rusergroupstgallen.github.io/main/San%20Francisco%20Rent%20Exercise/rent_san_francisco_download.csv")

# Font
font <- "Montserrat"
font_add_google(family=font, font, db_cache = FALSE)
showtext_auto(enable = TRUE) 
theme_set(theme_minimal(base_family = font))
bg <- "#0D1117"
txt_col <- "#FFFFFF"

# Data wrangling
df <- df %>%
  select(year, price, county) %>% 
  drop_na() %>% 
  group_by(year, county) %>% 
  summarize(median_price = median(price)) %>% 
  ungroup()

# Set fixed color range
col_range <- c(500, 3500)

ca_map <- map_data("county", "california") %>% 
  select(lon = long, lat, county = subregion) %>% 
  filter(county %in% unique(df$county))

ca_map_2001 <- merge(ca_map, df %>% filter(year == 2001),
                     by = "county", all.x = TRUE)

ca_map_2018 <- merge(ca_map, df %>% filter(year == 2018),
                     by = "county", all.x = TRUE)

# Map plots
ca_plot_2001 <- ca_map_2001 %>% 
  ggplot(aes(lon, lat, group = county)) +
  geom_polygon(aes(fill = median_price), colour = bg) +
  scale_fill_gradientn(colours = met.brewer(name = "Benedictus",
                                            type = "discrete",
                                            n = 11,
                                            direction = -1),
                       limits = col_range, breaks = seq(col_range[1], col_range[2], (col_range[2] - col_range[1]) / 4)) +
  coord_quickmap() +
  labs(title="2001") +
  theme(
    panel.grid = element_blank(),
    axis.title  = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(size = 20, color = txt_col, hjust = 0.5, lineheight = 1, face = "bold", margin = margin(0, 0, 30, 0)),
    plot.caption = element_text(size = 8, color = txt_col, hjust = 0.5, face = "bold", margin = margin(20, 0, 0, 0)),
    plot.background = element_rect(color = bg, fill = bg),
    plot.title.position = "panel",
    plot.margin = margin(0, 0, 0, 0),
    legend.title = element_text(size = 12, color = txt_col, hjust = 0.5),
    legend.text = element_text(size = 12, color = txt_col)) +
  guides(fill = guide_colorbar(title = "Median Rent (USD)", ticks = FALSE, nbin = 1000, title.position = "top", barwidth = 14))

ca_plot_2001

ca_plot_2018 <- ca_map_2018 %>% 
  ggplot(aes(lon, lat, group = county)) +
  geom_polygon(aes(fill = median_price), colour = bg) +
  scale_fill_gradientn(colours = met.brewer(name = "Benedictus",
                                            type = "discrete",
                                            n = 11,
                                            direction = -1),
                       limits = col_range, breaks = seq(col_range[1], col_range[2], (col_range[2] - col_range[1]) / 4)) +
  coord_quickmap() +
  labs(title="2018") +
  theme(
    panel.grid = element_blank(),
    axis.title  = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(size = 20, color = txt_col, hjust = 0.5, lineheight = 1, face = "bold", margin = margin(0, 0, 30, 0)),
    plot.caption = element_text(size = 8, color = txt_col, hjust = 0.5, face = "bold", margin = margin(20, 0, 0, 0)),
    plot.background = element_rect(color = bg, fill = bg),
    plot.title.position = "panel",
    plot.margin = margin(0, 0, 0, 0),
    legend.title = element_text(size = 12, color = txt_col, hjust = 0.5),
    legend.text = element_text(size = 12, color = txt_col)) +
  guides(fill = guide_colorbar(title = "Median Rent (USD)", ticks = FALSE, nbin = 1000, title.position = "top", barwidth = 14))

ca_plot_2018

# Combining
ca_plot_2001 + ca_plot_2018 +
  plot_annotation(title = "Median Monthly Rent in the Bay Area",
                  caption = "Plot: Ruben Ernst | Data: Kate Pennington") +
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom',
        plot.title = element_text(size = 20, color = txt_col, hjust = 0.5, lineheight = 1, face = "bold", margin = margin(0, 0, 30, 0)),
        plot.caption = element_text(hjust = 0.5, margin = margin(20, 0, 0, 0), size = 10, color = txt_col, face = "bold"),
        plot.margin = margin(10, 10, 10, 10),
        plot.background = element_rect(color = bg, fill = bg)
  )

# Save
showtext_opts(dpi = 320) 

ggsave("Bay Area Rent.png",
       height = 10,
       width = 10,
       dpi=320, device = cairo_pdf()
)  

showtext_auto(FALSE)
