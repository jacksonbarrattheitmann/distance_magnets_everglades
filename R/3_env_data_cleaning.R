## ENV Data cleaning and aggregating

library(dplyr)
library(readr)
library(terra)
library(stars)
library(wader)
library(tsibble)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(edenR)
library(sf)
library(stars)
library(corrplot)
library(tidyr)
library(broom)

## download the eden data
#download_eden_depths(eden_path = "Data/eden/")

#test <- terra::rast("Data/eden/1992_q2_depth.nc")

subregions <- read_sf("../../wader_data/EvergladesWadingBird/SiteandMethods/regions/subregions.geojson")

colonies <- read_sf("../../wader_data/EvergladesWadingBird/SiteandMethods/colonies/colonies.geojson")

colonies_csv <- read_csv("../../wader_data/EvergladesWadingBird/SiteandMethods/colonies.csv") %>%
  rename(Name = colony) %>%
  select(Name, region, subregion)

eden_water <- get_eden_covariates(eden_path = "Data/eden/", level = "colonies", 
                                  years = c(2015:2026))

water_depth <- get_eden_depths(eden_path = "Data/eden/",
                               level = "colonies",
                years = c(2016:2026),
                colony_buffers = list(
                  "gbhe" = 250069
                ))

### need to create some summarized data from all these daily water depths

water_depth_avg <- water_depth %>%
  mutate(date = ymd(date),
         month = floor_date(date, "month")) %>%
  group_by(month, region) %>%
  summarise(mean_depth_avg = mean(depth_mean, na.rm = TRUE),
            sd_depth_avg = mean(depth_sd, na.rm = TRUE),
            max_depth_avg = mean(depth_max, na.rm = TRUE),
            min_depth_avg = mean(depth_min, na.rm = TRUE))

water_depth_avg <- water_depth_avg %>%
  rename(date = month,
         Name = region)

#### GEE LAND COVER DATA ####

file_list <- list.files(path = "Data/GEE_Everglades_Landcover/", pattern = "*.csv", full.names = TRUE)

data_list <- lapply(file_list, read.csv)

combined <- do.call(rbind, data_list)

## Now we need to clean this file at the very least
combined <- combined %>%
  select(-system.index, -altitudeMo, -begin, -descriptio, -end, -extrude, -icon, -tessellate, -timestamp, -visibility)

## making a date column
combined <-  combined %>%
  mutate(date = ymd(export_date)) %>%
  select(-export_date)

# let's mutate and make this into 
# realtive changes in land cover through time

combined_long <- combined %>%
  pivot_longer(cols = c(bare, flooded_vegetation, water, grass, shrub_and_scrub, trees, built, crops),
               names_to = "land_cover",
               values_to = "value") %>%
  drop_na(value)

combined_long <- combined_long %>%
  group_by(Name, buffer_size_m, land_cover) %>%
  mutate(avg_value = mean(value),
         rel_diff = value - avg_value) %>%
  ungroup()

combined_long$buffer_size_m <- as.factor(combined_long$buffer_size_m)

## save this file for other uses

saveRDS(combined_long, "Data/gee_combined_lulc.RDS")

combined_long <- readRDS("Data/gee_combined_lulc.RDS")


### let's aggregate this data for Alex quickly

gee_summaries <- combined_long %>%
  filter(buffer_size_m == 2500) %>%
  left_join(colonies_csv, by = "Name") %>%
  ungroup()

region_summaries <- gee_summaries %>%
  select(Name, region, subregion, land_cover, date, value) %>%
  group_by(region, date, land_cover) %>%
  summarize(mean_land_cover_region = mean(value, na.rm = TRUE),
            sd_land_cover_region = sd(value, na.rm = TRUE)) %>%
  ungroup()

## save to send to Alex
#saveRDS(region_summaries, "Data/For_alex/region_summaries.RDS")

###we're getting NAs and I'm not sure why, let's investigate
region_summaries %>%
  filter(is.na(mean_land_cover_region) | is.na(sd_land_cover_region))
### 
### it's because the 'other' region has NA values in for sd in 2017-04-01
### probably because there is only one value


subregion_summaries <- gee_summaries %>%
  select(Name, region, subregion, land_cover, date, value) %>%
  group_by(subregion, date, land_cover) %>%
  summarize(mean_land_cover_subregion = mean(value, na.rm = TRUE),
            sd_land_cover_subregion = sd(value, na.rm = TRUE))

## save to send to Alex
#saveRDS(subregion_summaries, "Data/For_alex/subregion_summaries.RDS")


### Joining the water depth data
### and the LULC data from GEE

land_cover_dat <- combined %>%
  left_join(water_depth_avg, by = c("date", "Name"))

## here's the filtered version with the exact same scale
land_cover_dat_filt <- land_cover_dat %>%
  filter(buffer_size_m == 2500) %>%
  select(date, Name, water, mean_depth_avg, sd_depth_avg)

### export this dataframe
saveRDS(land_cover_dat_filt, "Data/land_cover_eden.RDS")


### a quick and dirty check on the r value to see if they are colinear
r <- cor(land_cover_dat_filt$water, land_cover_dat_filt$mean_depth_avg,
         use = "complete.obs")
1 / (1 - r^2)   # VIF

## plots
ggplot(land_cover_dat_filt, aes(x = water, y = mean_depth_avg)) +
  geom_point() +
  geom_smooth()

ggplot(land_cover_dat_filt, aes(x = mean_depth_avg, y = sd_depth_avg)) +
  geom_point()

####### CORRELATION PLOT ###########

## this checks the r2 value within a colony

water_corr <- land_cover_dat_filt %>%
  group_by(Name) %>%
  filter(sum(complete.cases(water, mean_depth_avg)) >= 4) %>% 
  summarise(tidy(cor.test(water, mean_depth_avg))) %>%
  select(Name, estimate, p.value, conf.low, conf.high)


## a plot to visualize

water_corr %>%
  mutate(
    Name = reorder(factor(Name), estimate),
    sig = ifelse(p.value < 0.05, "p < 0.05", "n.s.")
  ) %>%
  ggplot(aes(x = estimate, y = Name, color = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
  scale_color_manual(values = c("p < 0.05" = "#d95f02", "n.s." = "grey60")) +
  scale_x_continuous(limits = c(-1, 1)) +
  labs(x = "Correlation (r) between water and mean depth",
       y = "Name", color = NULL) +
  theme_bw()












## let's try a subset of just 1 colony


lox <- combined_long %>%
  filter(Name == "lox99")

ggplot(data = lox, aes(x = date, y = value, color = buffer_size_m)) +
  geom_point() +
  geom_line() +
  facet_wrap(~land_cover) +
  theme_bw()


## let's just look at water for all of the colonies combined
water <- combined_long %>%
  filter(land_cover == "water")

## need a summary average df for plotting

water_error <- water %>%
  group_by(buffer_size_m, date) %>%
  summarize(global_avg = mean(value),
            sd_avg = sd(value),
            hi = global_avg + sd_avg,
            low = global_avg - sd_avg)

## big years to highlight
highlight_years <- data.frame(
  xmin = as.Date(c("2018-01-01", "2021-01-01")),
  xmax = as.Date(c("2018-06-30", "2021-06-30"))
)

## plot for water
ggplot() +
  geom_rect(data = highlight_years,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "blue", alpha = 0.2, inherit.aes = FALSE) +
  geom_point(data = water_error, aes(x = date, y = global_avg), color = "red") +
  geom_errorbar(data = water_error, aes(x = date, ymin = low, ymax = hi), width = 0.2) +
  facet_wrap(~buffer_size_m) +
  geom_line(data = water_error, aes(x = date, y = global_avg), color = "red") +
  theme_bw() +
  labs(x = "Date",
       y = "Regional Average (%)")

## flooded vegetation too
flooded_veg <- combined_long %>%
  filter(land_cover == "flooded_vegetation")

veg_error <- flooded_veg %>%
  group_by(buffer_size_m, date) %>%
  summarize(global_avg = mean(value),
            sd_avg = sd(value),
            hi = global_avg + sd_avg,
            low = global_avg - sd_avg) +
  

ggplot() +
  geom_rect(data = highlight_years,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "blue", alpha = 0.2, inherit.aes = FALSE) +
  geom_point(data = veg_error, aes(x = date, y = global_avg), color = "red") +
  geom_errorbar(data = veg_error, aes(x = date, ymin = low, ymax = hi), width = 0.2) +
  geom_line(data = veg_error, aes(x = date, y = global_avg), color = "red") +
  facet_wrap(~buffer_size_m) +
  theme_bw() +
  labs(x = "Date",
       y = "Regional Average (%)")
