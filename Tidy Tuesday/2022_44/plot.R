rm(list = ls())

### Tidy Tuesday 2022 Week 44: Horror Movies
library(tidyverse)
library(ggnetwork)
library(tnet)
library(network)
library(showtext)
library(patchwork)

# Get the data
horror_movies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-01/horror_movies.csv")

# Font
font <- "Roboto Condensed"
font_add_google(family=font, font, db_cache = FALSE)
showtext_auto(enable = TRUE) 
theme_set(theme_minimal(base_family = font))
txt_col <- "#312e1b"
label_col <- "#e4f3e0"
bg <- "#b0bf8f" # "#e2ebce"

btbr_theme <- c("#eaa480", "#eb9173", "#de7f5f", "#d46248", "#c65237", "#ac2c13", "#94301e", "#881c08", "#671006", "#4c0d06", "#17120d")

# Data wrangling
weighted_edge_list <- horror_movies$genre_names %>% 
  str_split(", ") %>% 
  lapply(function(x){expand.grid(x, x, weight = 1 / length(x), stringsAsFactors = FALSE)}) %>%
  bind_rows

weighted_edge_list <- apply(weighted_edge_list[, -3], 1, str_sort) %>%
  t %>%
  data.frame(stringsAsFactors = FALSE) %>%
  mutate(weight = weighted_edge_list$weight) %>% 
  group_by(X1, X2) %>% 
  summarize(weight = sum(weight)) %>% 
  filter(X1 != X2)

# Generate non-directional graph
horror_network <- network(weighted_edge_list[, -3], directed = FALSE)
nrow(weighted_edge_list) == network.edgecount(horror_network) # Should be TRUE

# Set weights as edge attributes
set.edge.attribute(horror_network, "weight", weighted_edge_list$weight)

# Convert to symmetric tnet
horror_tnet <- as.edgelist(horror_network, attrname = "weight") %>%
  symmetrise_w %>%
  as.tnet %>%
  degree_w

nrow(horror_tnet) == network.size(horror_network) # Should be TRUE

set.vertex.attribute(horror_network, "degree_w", horror_tnet[, "output" ])

# Remove genres below median weighted degree
horror_network_trunc <- horror_network %v% "degree_w"
horror_network_trunc <- ifelse(horror_network_trunc >= median(horror_network_trunc), network.vertex.names(horror_network), NA)

length(horror_network_trunc) == network.size(horror_network) # Should be TRUE
set.vertex.attribute(horror_network, "label", horror_network_trunc)


# Plot
set.seed(9)

plot_44 <- horror_network %>% 
  ggplot(aes(x, y, xend = xend, yend = yend)) +
  geom_edges(aes(alpha = weight), size = 1.6, color = btbr_theme[11]) +
  scale_size_continuous(range = c(5, 50)) +
  geom_nodes(aes(color = label, size = degree_w)) +
  geom_text(aes(label = label), family = font, color = label_col, size = 4) +
  scale_fill_manual(values = c("Action" = btbr_theme[1],
                               "Animation" = btbr_theme[2],
                               "Comedy" = btbr_theme[3],
                               "Crime" = btbr_theme[4],
                               "Drama" = btbr_theme[9],
                               "Mystery" = btbr_theme[6],
                               "Science Fiction" = btbr_theme[7],
                               "Thriller" = btbr_theme[8],
                               "Fantasy" = btbr_theme[5],
                               "Horror" = btbr_theme[10]),
                    na.value = "grey50") +
  scale_color_manual(values = c("Action" = btbr_theme[1],
                               "Animation" = btbr_theme[2],
                               "Comedy" = btbr_theme[3],
                               "Crime" = btbr_theme[4],
                               "Drama" = btbr_theme[9],
                               "Mystery" = btbr_theme[6],
                               "Science Fiction" = btbr_theme[7],
                               "Thriller" = btbr_theme[8],
                               "Fantasy" = btbr_theme[5],
                               "Horror" = btbr_theme[10]),
                    na.value = "#1c1a10") +

  coord_cartesian(clip = "off") +
  labs(title = "Most frequent genre crossovers with horror",
       subtitle = "Undirected network of above median weighted degree genre combinations",
       caption = "Plot: Ruben Ernst | Data: Tanya Shapiro & TMDB"
  ) +
  theme(panel.grid = element_blank(),
        axis.title  = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size = 26, color = txt_col, hjust = 0.5, lineheight = 1, face = "bold", margin = margin(0, 0, 10, 0)),
        plot.subtitle = element_text(size = 15, color = txt_col, hjust = 0.5, lineheight = 1, margin = margin(0, 0, 20, 0)),
        plot.caption = element_text(size = 12, color = txt_col, hjust = 0.5, face = "bold", margin = margin(20, 0, 0, 0)),
        plot.background = element_rect(color = bg, fill = bg),
        plot.title.position = "panel",
        legend.position = "none",
        plot.margin = margin(50, 50, 50, 50))

plot_44

# Save
showtext_opts(dpi = 320) 

ggsave("Tidy Tuesday/2022_44/tidy_tuesday_2022_44.png",
       plot = plot_44,
       height = 10,
       width = 10,
       dpi=320)

showtext_auto(FALSE)
