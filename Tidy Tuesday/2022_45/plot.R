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

