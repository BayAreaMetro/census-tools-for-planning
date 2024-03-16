# commute_decline.r

# Libraries

library(tidycensus)
suppressMessages(library(tidyverse))
library(extrafont)

# Bring in Bay Area place equivalency

place_eq <- read.csv("M:/Data/Census/corrlib/Census2010_BayArea_Places.csv") %>% 
  mutate(GEOID=as.character(paste0("0",StatePlace))) %>% 
  select(GEOID,Place_Name)

var <- "DP03_0025"

commute19 <- get_acs(
  geography = "place",
  variables = var,
  survey = "acs1",
  year = 2019
) %>%
  select(GEOID, NAME, est2019 = estimate)

commute21 <- get_acs(
  geography = "place",
  variables = var,
  survey = "acs1",
  year = 2021
) %>%
  select(GEOID, NAME, est2021 = estimate)

shift <- commute19 %>%
  left_join(commute21, by = c("GEOID", "NAME")) %>%
  mutate(shift = round(est2021 - est2019, 1),
         mn = round(((est2021 + est2019) / 2), 1)) %>%
  arrange(desc(shift)) %>% 
  left_join(.,place_eq,by="GEOID") %>% 
  filter(!(is.na(Place_Name))) %>% 
  mutate(NAME=sub(" city, California","",NAME))

top15 <- slice_min(shift, shift, n = 15) %>%
  pivot_longer(est2019:est2021, names_to = "year", 
               names_prefix = "est")

top15 %>% 
  ggplot(aes(x= value, y= reorder(NAME,-shift))) +
  geom_line(aes(group = NAME),color="black")+
  geom_point(aes(color=year), size=4) +
  geom_text(aes(label = format(shift,nsmall=1), x = mn), nudge_y = 0.3) + 
  scale_x_continuous(breaks = c(25, 30, 35, 40, 45)) + 
  labs(x = "Mean travel time to work (minutes)",
       y = "",
       color = "",
       title = "Largest Bay Area declines in mean commute time, 2019-2021",
       subtitle = "Census-designated places with population 65,000+",
       caption = "2019 and 2021 1-year ACS | tidycensus R package | MTC") + 
  theme_minimal(base_size = 14, base_family = "Roboto")+
  theme(legend.position="bottom", panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_color_brewer(palette="Set1", direction=-1)
