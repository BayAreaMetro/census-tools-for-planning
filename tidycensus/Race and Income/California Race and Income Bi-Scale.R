library(tidyverse)
library(janitor)
library(biscale)
library(sf)
library(cowplot)
library(tidycensus)
library(ggtext)

# data

data <- get_acs(geography = "tract",
                variables = c(total = "B03002_001",
                              white_alone = "B03002_003",
                              median_household_income = "B19013_001"),
                output = "wide",
                state = c("CA"),
                county = c("Alameda"),
                year = 2021,
                geometry = T) %>% 
  mutate(white_percent = white_aloneE / totalE)

# map

bi_data <- bi_class(data, x = white_percent, y = median_household_incomeE, style = "quantile", dim = 3)

bi_map <- ggplot() +
  geom_sf(data = bi_data, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "BlueGold", dim = 4) +
  bi_theme() +
  labs(title = "<span style='color:#0072B2;'>Race</span> and <span style='color:#D4A017;'>income</span> in the Pacific Northwest",
       caption = "@GabeOsterhout | ACS tables B02001 (race) & B19013 (household income) by census tract") +
  theme(plot.title = element_markdown())

legend <- bi_legend(pal = "BlueGold",
                    dim = 3,
                    xlab = "Higher % white",
                    ylab = "Higher income ",
                    size = 12)

plot <- ggdraw() +
  draw_plot(bi_map, 0, 0, 1, 1) +
  draw_plot(legend, 0.75, .65, 0.2, 0.2)

plot
