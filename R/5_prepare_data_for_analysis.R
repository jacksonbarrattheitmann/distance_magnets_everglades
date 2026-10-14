## Big join

library(dplyr)
library(readr)
library(tidyverse)

## eBird
ebird <- readRDS("Data/for_analysis/ebird_metrics.RDS") %>%
  group_by(STATE_CODE, COMMON_NAME) %>%
  mutate(global_max = max(max_n),
         global_max_avg = max(avg_max_n)) %>%
  ungroup()

ebird <- ebird %>%
  mutate(max_n_norm = 1-((global_max - max_n) / global_max),
         max_n_avg_norm = 1-((global_max_avg - avg_max_n) / global_max_avg))

## let's visualize

ebird %>%
  filter(COMMON_NAME == "Wood Stork") %>%
ggplot(aes(x = month, y = max_n_avg_norm, color = max_n_avg_norm)) +
  geom_point() +
  geom_line() +
  facet_wrap(~STATE_CODE) +
  theme_bw()


## ebird ENV
env <- readRDS("Data/for_analysis/ebird_env.RDS")

## eBird effort data
agg <- readRDS("Data/for_analysis/agg_dataframe.RDS")


agg_sum <- agg %>%
  rename(LOCALITY_ID = locality_id) %>%
  filter(year > 2015) %>%
  left_join(env, by = "LOCALITY_ID") %>%
  filter(!is.na(STATE_CODE))

agg_sum2 <- agg_sum %>%
  group_by(species, STATE_CODE, year, month) %>%
  summarize(total_checklists = sum(n),
         total_checklists_present = sum(k),
         prop_checklists_detected = total_checklists_present / total_checklists) %>%
  mutate(date = make_date(year, month, 1))

## let's visualize

agg_sum2 %>%
  filter(species == "Wood Stork") %>%
  ggplot(aes(x = date, y = prop_checklists_detected, color = prop_checklists_detected)) +
  geom_point() +
  geom_line() +
  facet_wrap(~STATE_CODE) +
  theme_bw()



## land cover + edenR
lulc <- readRDS("Data/for_analysis/land_cover_eden.RDS")

## wading_bird_data
wading <- readRDS("Data/for_analysis/wading_bird_EV.RDS") %>%
  group_by(species) %>%
  mutate(global_max = max(max_n),
         global_max_avg = max(avg_max_n)) %>%
  ungroup()

wading <- wading %>%
  mutate(max_n_norm = 1-((global_max - max_n) / global_max),
         max_n_avg_norm = 1-((global_max_avg - avg_max_n) / global_max_avg))


#### We need to still do more with our data before it's ready for
#### a model. We need to bin the eBird data seasonally so that we can use it ap

