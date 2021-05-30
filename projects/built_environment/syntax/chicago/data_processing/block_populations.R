library(tidyverse)
library(sf)
library(tidycensus)
library(areal)
source("./syntax/project_functions.R")
load("./data/chicago/derived/il_block.RData")
load("./data/chicago/derived/crosswalks/complete_crosswalk.RData")

# dec_vars <- load_variables(2010, "sf1")

block_pops <- get_decennial("block", state = "IL", county = "Cook",
                          geometry  = FALSE, year = 2000,
                          variables = c("P001001"),
                          output = "wide")
block_pops <- block_pops %>% 
  mutate(tract_block = case_when(
    nchar(GEOID) == 13 ~ paste0(str_sub(GEOID, -8,  -5), "00", str_sub(GEOID, -4,  -1)),
    nchar(GEOID) == 15 ~ str_sub(GEOID, -10, -1)
  ))
block_shape <- tigris::blocks("IL", "Cook", year = 2000)
block_shape <- block_shape %>% mutate(tract_block = str_sub(BLKIDFP00, -10, -1)) 

block_shape_pop <- block_shape %>% inner_join(block_pops) %>% 
  st_transform(3435) %>%
  select(tract_block, population = P001001, geometry)

blocks_1990 <- il_block %>% 
  filter(FIPSSTCO == "17031") %>% select(TRACT, BLOCK, geometry) %>%
  mutate(tract_block = paste0(TRACT, BLOCK)) %>%
  st_make_valid()

blocks_1990_interpolated_sum <- aw_interpolate(blocks_1990, 
                                                 tid = tract_block, source = block_shape_pop, 
                                                 sid = tract_block, extensive = "population", 
                                                 weight = "sum", output = "sf")

# blocks_1990_interpolated_total <- aw_interpolate(blocks_1990, 
#                                            tid = tract_block, source = block_shape_pop, 
#                                            sid = tract_block, extensive = "population", 
#                                            weight = "total", output = "sf")

# data.frame(tract = blocks_1990_interpolated_total$TRACT, block = blocks_1990_interpolated_total$BLOCK, p_total = blocks_1990_interpolated_total$population, p_sum = blocks_1990_interpolated_sum$population) %>%
#   filter(p_total > p_sum*1.1 | p_total < p_sum*0.9)

block_pop_estimates_2000 <- complete_crosswalk %>% 
  inner_join(blocks_1990_interpolated_sum %>% 
               mutate(area = as.numeric(units::set_units(st_area(geometry), "mi^2")),
                      density = as.numeric(population / area)) %>% 
               st_drop_geometry() %>% 
               select(census_tract_6 = TRACT, census_block = BLOCK, population, area, density))

save(block_pop_estimates_2000, file = "./data/chicago/derived/block_pop_estimates_2000.RData")
