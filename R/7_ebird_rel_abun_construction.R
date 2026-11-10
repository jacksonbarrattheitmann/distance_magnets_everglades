# eBird data relative abundance modelling

library(lubridate)
library(sf)
library(raster)
library(dggridR)
library(pdp)
library(mgcv)
library(fitdistrplus)
library(viridis)
library(fields)
library(tidyverse)
library(dplyr)
library(spAbundance)

#load data

### we need to do a preliminary filter for just 2021 as the last year
### because we don't have data yet later than 2021
### FIX AFTER WE GET MORE DATA 
###
###

chk_df <- readRDS("Data/for_analysis/wost_full.RDS") %>%
  filter(year %in% 2010:2021)

#remove super long distances
chk_df_short <- chk_df %>%
  filter(dist_km < 5) %>%
  mutate(day_of_year = yday(dt))

## let's check out the na values
## for some reason the bcr codes didn't carry over for a bunch of checklists
chk_na <- chk_df_short %>%
  filter(!is.na(bcr)) %>%
  mutate(dist_km = if_else(protocol != "Traveling", 
                           0, dist_km))


## create a dataframe to inner_join
seasons <- data.frame(month = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                      season = c("breeding", "breeding", "breeding", 
                                 "post-breeding migration","post-breeding migration", "post-breeding migration", 
                                 "non-breeding", "non-breeding", 
                                 "non-breeding", "pre-breeding migration", "pre-breeding migration", 
                                 "breeding"))
chk_na <- chk_na %>%
  left_join(seasons, by = "month")

## add the water env data

water_dat <- readRDS("Data/landcover_dat/combined_monthly_water_GEE.RDS") %>%
  rename(locality_id = lclty_d)

## precipt data
precip <- readRDS("Data/for_analysis/precip_ebird.RDS") %>%
  rename(locality_id = lclty_d) %>%
  mutate(year = year(date),
         month = month(date))

# need to remove the 'date' column because we already have it in ebird df
precip <- precip[, c(1, 3:5)]

## and now do a join with the eBird data so we have everythign we need in a single dataframe

bird_full <- chk_na %>%
  left_join(water_dat, by = c("locality_id", "month", "year")) %>%
  left_join(precip, by = c("locality_id", "month", "year"))

# We need to remove possible checklists where we can't get any of the landcover due to clod cover
# being too high. 
bird_full_filt <- bird_full %>%
  filter(valid_frac > 0.75)

# construct grid to build the relative abundance model
dggs <- dgconstruct(spacing = 5)

checklist_cell <- bird_full_filt %>% 
  mutate(cell = dgGEO_to_SEQNUM(dggs, lon, lat)$seqnum)

ebird_ss <- checklist_cell %>%
  group_by(year, season, cell) %>%
  sample_n(5, replace = TRUE) %>%
  ungroup()



### Now we have a sampled data frame
### let's cut this up and try building
### an spAbundance model

### from what I understand, I think we should build the model 
### for each separate year, and then we can include a different











######## PLOTTING THE ENV COVARIATES FOR EXPLORATORY PURPOSES ##########

bcr_targets <- c(26, 27, 31, 37)

bcr_labels <- c("26" = "Mississippi Alluvial Valley", "27" = "Southeastern Coastal Plain",
                "31" = "Peninsular Florida", "37" = "Gulf Coastal Prairie")

# count by monthly water cover
ebird_ss %>%
  filter(detected == TRUE & bcr %in% bcr_targets) %>%
  ggplot(aes(x = water_frac, y = count, group = cell)) +
  geom_point() +
  geom_line() +
  facet_grid(~bcr) +
  theme_bw()

## straight up water cover by time in each bcr
ebird_ss %>%
  filter(detected == TRUE & bcr %in% bcr_targets) %>%
  ggplot(aes(x = date, y = seas, group = cell)) +
  geom_point() +
  geom_line() +
  facet_grid(~bcr, labeller = as_labeller(bcr_labels)) +
  theme_bw()


#### How about just precip by water cover
ebird_ss %>%
  filter(detected == TRUE &  bcr %in% bcr_targets) %>%
  ggplot(aes(x = seas, y = precip_mean)) +
  geom_point() +
  facet_grid(~bcr, labeller = as_labeller(bcr_labels)) +
  geom_smooth(method = "lm") +
  theme_bw() +
  xlab ("Ephemeral Water Cover (%)") +
  ylab( "Monthly Precipitation (mm)")
