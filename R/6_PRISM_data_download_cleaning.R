#PRISM preciptation data download
# and clenaing for eBird locations

library(dplyr)
library(sf)
library(terra)
library(readr)
library(prism)
library(exactextractr)
library(tidyr)

#prism_set_dl_dir("Data/prism_data/")

# No need to run again, this does the trick
#get_prism_monthlys(type = "ppt", 
#                   years = 2010:2026,
#                   resolution = "800m")



### So we need to create a function
### that reads in each monthly file
### intersects it with our eBird shapefile
### grabs the value within a 5KM buffer around that hotspot
### then stores it in a data frame for us to use, and probably exports it I'd assume (?)

#ebird shapefile
ebird <- st_read("Data/ebird_hotspots/localities_clean.shp") 

ebird <- st_transform(ebird, 5070)

ebird_buff <- st_buffer(ebird, dist = 5000)

## now let's re-project it again and save it to match the PRISM data
ebird_buff <- st_transform(ebird_buff, "EPSG:4269")




prism_archive_ls()
files <- prism_archive_subset("ppt", "monthly", years = 2010:2026, resolution = "800m")
ppt_paths <- pd_to_file(files)  # get full file paths
ppt_stack <- rast(ppt_paths)   

#checking to make sure it look like we have the right number of layers
nlyr(ppt_stack) 
names(ppt_stack)

## now we can extract the values
monthly_precip <- exact_extract(ppt_stack, ebird_buff, 'mean')

# the column names here are pretty difficult to read

# once we want to bind them together
hotspots_with_precip <- cbind(ebird_buff, monthly_precip)

## tidy-ing this df

hotspots_no_geo <- st_drop_geometry(hotspots_with_precip)

hotspots_long <- hotspots_no_geo %>%
  pivot_longer(
    cols = matches("\\d{6}"),          # grabs any column with a 6-digit date in its name
    names_to = "date_raw",
    values_to = "precip_mean"
  ) %>%
  mutate(
    date = sub(".*?(\\d{6}).*", "\\1", date_raw),   # extract just the YYYYMM
    date = paste0(substr(date, 1, 4), "-", substr(date, 5, 6)),  # format as YYYY-MM
    date = as.Date(paste0(date, "-01"))             # make it an actual Date
  ) %>%
  dplyr::select(lclty_d, date, precip_mean)

# let's export thsi object

saveRDS(hotspots_long, "Data/for_analysis/precip_ebird.RDS")

