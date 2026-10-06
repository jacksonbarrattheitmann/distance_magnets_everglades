## Script to grab the wading bird data

library(dplyr)
library(tidyverse)
library(auk)
library(remotes)
library(wader)
library(slider)
library(ggplot2)
library(wader)
library(scales)
#remotes::install_github("weecology/wader")
# set the path for your data to be stored
# usethis::edit_r_environ()

download_observations()

## As per Micheal's recommendation, let's use imagecounts

wading <- read.csv("../wader_data/EvergladesWadingBird/Counts/image_counts.csv")

## So just as we've done with our eBird data
## let's get some monthly counts by species in the entire system.

wading_sum <- wading %>%
  mutate(date = ymd(date),
         month = floor_date(date, unit = "month"),
         year = floor_date(date, unit = "year")) %>%
  filter(species %in% c("greg", "whib", "wost", "smwh", "rosp", "sneg")) %>%
  group_by(species, month, year) %>%
  summarize(max_n = max(count),
            avg_max_n = mean(count))

## now we've got our metrics that mimic exactly what we calculated from eBird

saveRDS(wading_sum, "Data/for_analysis/wading_bird_EV.RDS")
