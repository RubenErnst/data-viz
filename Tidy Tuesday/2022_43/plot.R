rm(list = ls()) 

### Tidy Tuesday 2022 Week 43: Great British Bake Off
library(tidyverse)
library(ggridges)
library(showtext)
library(MetBrewer)
library(patchwork)

# Get the data
ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-25/ratings.csv")

# Font
font <- "Roboto"
font_add_google(family=font, font, db_cache = FALSE)
showtext_auto(enable = TRUE) 
theme_set(theme_minimal(base_family = font))
bg <- "#f5e8d0"
txt_col <- "#000000"

# Data wrangling
ratings <- ratings %>% 
  select(series, episode, viewers_7day, viewers_28day) %>% 
  mutate(series = factor(paste("Season", series), levels = c("Season 1", "Season 2", "Season 3", "Season 4", "Season 5", "Season 6", "Season 7", "Season 8", "Season 9", "Season 10")))

# Plot
plot_43 <- ratings %>% 
  ggplot(aes(x = episode, y = series, fill = series)) +
  geom_density_ridges2(aes(height = viewers_7day), stat = "identity", alpha = 1, scale = 3, show.legend = FALSE) +
  scale_fill_manual(values = met.brewer(name = "Hiroshige", type = "discrete", n = 10)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0), limits = c(1,10), breaks = 1:10) +
  coord_cartesian(clip="off") +
  labs(title = "Viewer numbers of the Great British Bake Off",
       subtitle = "Finales attract the most viewers",
       caption = "Plot: Ruben Ernst | Data: Alison Hill, Chester Ismay, and Richard Iannone",
       x= "Episode"
  ) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_text(size = 14, color = txt_col, margin = margin(10, 0, 0, 0), hjust = 0.5),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12, color = txt_col),
        axis.text.x = element_text(margin = margin(20, 0, 0, 0)),
        axis.text.y = element_text(vjust = -0.5, margin = margin(0, 20, 0, 0)),
        plot.title = element_text(size = 20, color = txt_col, hjust = 0.5, lineheight = 1, margin = margin(0, 0, 10, 0)),
        plot.sub = element_text(size = 16, color = txt_col, hjust = 0.5, lineheight = 1, margin = margin(0, 0, 20, 0)),
        plot.caption = element_text(size = 10, color = txt_col, hjust = 0.5, margin = margin(30, 0, 0, 0), lineheight = 1.4),
        plot.background = element_rect(color=bg, fill=bg),
        plot.title.position = "plot",
        plot.margin = margin(30, 60, 30, 30))

plot_43

showtext_opts(dpi = 320) 

ggsave(filename = "Tidy Tuesday/2022_43/tidy_tuesday_2022_43.pdf",
       plot = plot_43,
       height = 10,
       width = 10,
       dpi = 320,
       device = cairo_pdf)

showtext_auto(FALSE)
