il_block_raw <- sf::read_sf("D:/Projects/dissertation_data/chicago/illinois_boundaries/blocks_1990/IL_block_1990.shp") %>%
  st_transform(3435)

# Fixing 8104-813, which is one block in CCAHS but two separate blocks in census

il_block_810400813 <- il_block_raw %>% filter(TRACT == "810400" & BLOCK %in% c("813A", "813B")) %>%
  summarize(across(c(FIPSSTCO, TRACT, STFID, GISJOIN), ~ first(.)), BLOCK = "813", geometry = st_union(geometry))
il_block <- 
  il_block_raw %>% filter(!(TRACT == "810400" & BLOCK %in% c("813A", "813B"))) %>% rbind(il_block_810400813)

save(il_block, file = "./data/chicago/derived/il_block.RData")