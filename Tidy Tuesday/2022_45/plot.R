rm(list = ls())

### Tidy Tuesday 2022 Week 45:
library(tidyverse)
library(showtext)
library(patchwork)

# Get the data


# Font
font <- "Anek Latin"
font_add_google(family=font, font, db_cache = FALSE)
showtext_auto(enable = TRUE) 
theme_set(theme_minimal(base_family = font))
txt_col <- "#"
bg <- "#"
ba_theme <- c("#c4adb5", "#b99ba3", "#ad898d", "#986e70", "#864f4c", "#bfb1b4", "#928890", "#8fa3c", "#718aa9", "#f1ad49", "#231f1f")
