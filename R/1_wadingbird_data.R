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

#download_observations()

## let's take a peek at the wading bird.csv files

max_count <- load_datafile("Counts/maxcounts.csv")

nest_suc <- load_datafile("Nesting/nest_success_summary.csv")

species_int <- c("greg", "gbhe", "whib", "wost", "smhe", "rosp", "sneg", "lbhe")

## load our GEE land cover data
gee <- readRDS("Data/gee_combined_lulc.RDS") 

gee <- gee %>%
  rename(colony = Name) %>%
  mutate(year = year(date),
         month = month(date),
         day = day(date)) %>%
  select (-.geo) %>%
  as.data.frame()

## let's do some cleaning and filtering

nest_suc <- nest_suc %>%
  filter(species %in% species_int &
           year > 2011)

max_count <- max_count %>%
  filter(species %in% species_int)

## Plot of total nesting effort and nest success
ggplot(nest_suc, aes(x = incubation_k, y = overall_p)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  xlab("Total Nests") +
  ylab("Nesting Success (%)")

# quick dumb model to see what things look like
mod <- lm(overall_p ~ incubation_k + species, data = nest_suc)

summary(mod)

## Plot of year and nest success
ggplot(nest_suc, aes(x = year, y = overall_p, color = species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  facet_wrap(~species) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  xlab("Year") +
  ylab("Nesting Success (%)")

## let's remove the species coloration and look at overall
ggplot(nest_suc, aes(x = year, y = overall_p)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  xlab("Year") +
  ylab("Nesting Success (%)")


##### NOW LETS ADD MAX COUNT ####

max_count_filt <- max_count %>%
  select(year, species, count) %>%
  filter(year > 2012) %>%
  group_by(year, species) %>%
  summarize(max_count = sum(count))

## now let's do a quick join

count_nest <- nest_suc %>%
  left_join(max_count_filt, by = c("species", "year"))

## now let's make some more summary plots quickly
ggplot(count_nest, aes(x = max_count, y = incubation_k, color = species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~species) +
  theme_bw() +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  xlab("Max Count") +
  ylab("Total Nests")


# how about just max count, per year
ggplot(count_nest, aes(x = year, y = max_count, color = species)) +
  geom_point() +
  geom_line() +
  facet_wrap(~species, scales = "free_y") +
  theme_bw()



##### LAND COVER AND NEST SUCCCESS #####

#### 6th_bridge, alley_north, and lox99 seem to be good candidates to test ###

# first let's combine
# and make year into a generic date at the end of the breeding season
nest_filt <- nest_suc %>%
  filter(year > 2015) %>%
  filter(colony == "6th_bridge") %>%
  mutate(date = as.Date(paste0(year, "-06-30")))

# filter gee the same way

gee_filt <- gee %>%
  filter(colony == "6th_bridge") %>%
  select(-rel_diff, -avg_value) %>%
  group_by(colony, buffer_size_m, land_cover) %>%
  mutate(col_avg = mean(value),
         col_sd = sd(value),
         rel_diff = value - col_avg) %>%
  ungroup()

gee_filt <- gee_filt %>%
  group_by(colony, year, buffer_size_m, land_cover) %>%
  mutate(col_avg_seas = mean(value),
         col_sd_seas = sd(value),
         rel_diff_seas = value - col_avg_seas) %>%
  ungroup() 

# let's just take flooded veg and water for now
gee_filt <- gee_filt %>%
  filter(land_cover == "flooded_vegetation" | land_cover == "water")

## big years to highlight
highlight_years <- data.frame(
  xmin = as.Date(c("2018-01-01", "2021-01-01")),
  xmax = as.Date(c("2018-06-30", "2021-06-30"))
)


### Now we can think about plotting potentially
ggplot(data = gee_filt, aes(x = date, y = rel_diff_seas, color = land_cover)) +
  geom_point() +
  geom_line() +
  facet_wrap(~colony) +
  theme_bw() +
  geom_rect(data = highlight_years,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "blue", alpha = 0.2, inherit.aes = FALSE)



## trying a harder plot
ggplot() +
  geom_point(data = gee_filt, aes(x = date, y = rel_diff_seas, color = land_cover)) +
  geom_line(data = gee_filt, aes(x = date, y = rel_diff_seas, color = land_cover)) +
  #geom_point(data = nest_filt, aes(x = date, y = overall_p, shape = species), 
  #           color = "black", size = 2) +
  #geom_line(data = nest_filt, aes(x = date, y = overall_p, group = species), 
  #          color = "black", linetype = "dashed") +
  facet_wrap(~buffer_size_m) +
  theme_bw() +
  labs(y = "Relative Difference / Nest Success (p)") +
  geom_rect(data = highlight_years,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "blue", alpha = 0.2, inherit.aes = FALSE)
