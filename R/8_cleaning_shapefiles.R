## loading in the localities from eBird shapefile

library(sf)
library(dplyr)

localities <- st_read("Data/ebird_hotspots/localities_clean.shp")

localities_chk <- localities %>%
  mutate(chunk = ((seq_len(nrow(localities)) - 1) %/% 250))

write_sf(localities_chk, "Data/ebird_hotspots/localities_chunked.shp")
