library(tidyverse)
library(sf)

seattle_tract <- tigris::tracts("WA", county = "King", class="sf") %>% 
  select(GEOID, geometry) %>%
  filter(as.numeric(str_sub(GEOID, -5, -1)) < 13000 | GEOID == 53033026500) %>%
  filter(GEOID != 53033005302) %>%
  st_transform(3689)
save(seattle_tract, file = "./data/derived/seattle_tract.RData")

seattle_bg <- tigris::block_groups("WA", county = "King", class="sf") %>% 
  select(GEOID, geometry) %>% 
  filter(as.numeric(str_sub(GEOID, -6, -1)) < 130000 | str_detect(GEOID, "530330265001")) %>%
  filter(str_sub(GEOID, 1, -2) != "53033005302") %>%
  st_transform(3689)
save(seattle_bg, file = "./data/derived/seattle_bg.RData")
