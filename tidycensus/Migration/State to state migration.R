library(tidycensus)
library(tidyverse)
library(tigris)
options(tigris_use_cache = TRUE)

migrants <- get_pums(
  variables = c("MIGSP", "PUMA"),
  state = "all",
  year = 2021,
  survey = "acs1",
  recode = TRUE,
  variables_filter = list(
    MIG = 3
  )
)

state_flows <- migrants %>%
  filter(as.character(ST_label) != as.character(MIGSP_label)) %>%
  rename(from = MIGSP_label, to = ST_label) %>%
  group_by(from, to) %>%
  summarize(flow = sum(PWGTP, na.rm = TRUE)) %>%
  ungroup()

# Prepare a chart
library(showtext)
font_add_google("Montserrat")
showtext_auto()

state_flows %>%
  mutate(label = paste0(str_extract(from, "^[^/]*"),
                        " to ",
                        str_extract(to, "^[^/]*"))) %>%
  slice_max(order_by = flow, n = 20) %>%
  ggplot(aes(x = flow, y = reorder(label, flow))) +
  geom_col(alpha = 0.8, color = "darkblue", fill = "darkblue", width = 0.9) +
  theme_minimal(base_family = "Montserrat", base_size = 18) +
  scale_x_continuous(labels = scales::label_comma()) +
  labs(title = "Largest state-to-state migration flows in 2021",
       subtitle = "2021 1-year ACS PUMS data",
       caption = "tidycensus R package | @kyle_e_walker",
       x = "Estimated number of migrants",
       y = "")
