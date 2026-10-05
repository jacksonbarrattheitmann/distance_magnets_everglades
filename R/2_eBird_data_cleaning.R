## Cleaning and sorting the eBird data

library(dplyr)
library(ggplot2)
library(lubridate)
library(tsibble)
library(sf)
library(usmap)
library(purrr)

## here's some test data, so we can create the pipeline
## then fix it later with all of the data

test_ebird <- readRDS("Data/eBird_test/ebd_wading_SE copy.RDS") %>%
  rename(LOCALITY_ID = `LOCALITY ID`,
         COMMON_NAME = `COMMON NAME`,
         OBSERVATION_COUNT = `OBSERVATION COUNT`)

## let's remove TX for now from our pipeline
test_ebird_2 <- test_ebird %>%
  filter(!`STATE CODE` == "US-TX")

### Now let's chunk up Florida into 3 separate STATE CODES
### so we can plot some more interesting trends

## start by grabbing just the Florida data
fl <- test_ebird_2 %>%
  filter(`STATE CODE` == "US-FL") %>%
  select(LOCALITY_ID, LONGITUDE, LATITUDE) %>%
  distinct(.keep_all = TRUE)

## Now create a few different objects we can filter on

fl_south <- fl %>%
  filter(LATITUDE < 27) %>%
  mutate(`STATE CODE` = "US-FL-S") %>%
  select(LOCALITY_ID, `STATE CODE`)

fl_central <- fl %>%
  filter(LATITUDE > 27 & LATITUDE < 29) %>%
  mutate(`STATE CODE` = "US-FL-C") %>%
  select(LOCALITY_ID, `STATE CODE`)

fl_north <- fl %>%
  filter(LATITUDE > 29) %>%
  mutate(`STATE CODE` = "US-FL-N") %>%
  select(LOCALITY_ID, `STATE CODE`)

## make this into one table
## to append

fl_table <- fl_south %>%
  rbind(fl_central) %>%
  rbind(fl_north)

full_table <- test_ebird_2 %>%
  filter(`STATE CODE` == "US-FL") %>%
  select(-`STATE CODE`) %>%
  left_join(fl_table, by = "LOCALITY_ID")

ebird_state <- test_ebird_2 %>%
  filter(!`STATE CODE` == "US-FL") %>%
  rbind(full_table)

## First let's create a df that summarizes the total number of checklists at 
## a hotspot within each month, within each year

temp_ebird <- ebird_state %>%
  mutate(month = floor_date(`OBSERVATION DATE`, unit = "month"),
         year = floor_date(`OBSERVATION DATE`, unit = "year"))

temp_ebird2 <- temp_ebird %>%
  group_by(LOCALITY_ID, COMMON_NAME, year, month) %>%
  summarize(checks_mon_yr = n_distinct(`SAMPLING EVENT IDENTIFIER`)) %>%
  ungroup()


temp_ebird3 <- temp_ebird2 %>%
  group_by(LOCALITY_ID, year, COMMON_NAME) %>%
  summarize(n_month_sampled = n_distinct(month)) %>%
  ungroup()

## now let's calculate how many year x hotspots combos have at least 1 year of data
temp_ebird_yr <- temp_ebird3 %>%
  filter(n_month_sampled == 12) %>%
  group_by(LOCALITY_ID, COMMON_NAME) %>%
  summarize(total_yrs = n_distinct(year))

## Now why don;t we just take hotspots with at least 5 years with of data from every month

temp_ebird_hotspots <- temp_ebird_yr %>%
  filter(total_yrs > 4)

##### MAP OF HOTSPOTS #######
## Now let's see if we can just plot where these hotspots are spatially

ebird_sf <- ebird_state %>%
  select(LOCALITY_ID, LONGITUDE, LATITUDE) %>%
  distinct(.keep_all = TRUE) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

transformed_ebird <- usmap_transform(ebird_sf)

# Plot the South region of the US

plot_usmap(include = c(.south_atlantic, .east_south_central), labels = FALSE) +
  geom_sf(data = transformed_ebird) +
  geom_sf_label(data = transformed_ebird, aes(label = LOCALITY_ID), size = 3,
                label.padding = unit(0.45, "lines")) +
  labs(title = "The Southeast Region of the United States") +
  theme(panel.background = element_rect(color = "black", fill = "white"))

####### PLOTS OF COUNTS ########

ebird_state <- readRDS("Data/eBird_test/eBird_clean.RDS")

ebird_state$OBSERVATION_COUNT <- as.numeric(ebird_state$OBSERVATION_COUNT)

## now a summary data frame with monthly max counts of each species at each hotspot
ebird_summary <- ebird_state %>%
  mutate(month = floor_date(`OBSERVATION DATE`, unit = "month"),
         year = floor_date(`OBSERVATION DATE`, unit = "year")) %>%
  filter(year > "2015-12-31") %>%
  group_by(LOCALITY_ID, COMMON_NAME, month, year, `STATE CODE`) %>%
  summarise(max_n = max(OBSERVATION_COUNT, na.rm = TRUE))

## now some regional high counts
ebird_summary_region <- ebird_state %>%
  mutate(month = floor_date(`OBSERVATION DATE`, unit = "month"),
         year = floor_date(`OBSERVATION DATE`, unit = "year")) %>%
  filter(!is.na(OBSERVATION_COUNT),
         COMMON_NAME %in% c("Great Egret", "White Ibis", "Wood Stork", "Snowy Egret",
                          "Roseate Spoonbill"),
         year > "2015-12-31") %>%
  group_by(`STATE CODE`, COMMON_NAME, month, year) %>%
  summarise(max_n = max(OBSERVATION_COUNT, na.rm = TRUE))

### Right now we are ignoring and spatio-temporal replication 
### beyond the high count in any one 'region'

ggplot(data = ebird_summary_region, aes(x = month, y = max_n, color = `STATE CODE`)) +
  geom_point() +
  geom_line() +
  facet_wrap(~COMMON_NAME, scales = "free_y") +
  theme_bw()


### let's try and get at some of the overall trends in the data
## Let's just take a look at a single species GREG

ebird_summary_WOST <- ebird_summary_region %>%
  filter(COMMON_NAME == "White Ibis") %>%
  group_by(`STATE CODE`) %>%
  mutate(avg_max_total = mean(max_n)) %>%
  ungroup()

ebird_summary_WOST <- ebird_summary_WOST %>%
  mutate(rel_diff_yr = (max_n - avg_max_total)/(max_n + avg_max_total))

## let's try this plot

## big years to highlight
highlight_years <- data.frame(
  xmin = as.Date(c("2018-01-01", "2021-01-01")),
  xmax = as.Date(c("2018-06-30", "2021-06-30"))
)


ggplot(data = ebird_summary_WOST, aes(x = month, y = rel_diff_yr, color = rel_diff_yr)) +
  geom_point() +
  geom_line() +
  facet_wrap(~`STATE CODE`, scales = "free_y") +
  theme_bw() +
  scale_color_gradient(low = "black", high = "white") +
  geom_rect(data = highlight_years,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "blue", alpha = 0.2, inherit.aes = FALSE)


##### PLOT OF ALL SPECIES at just a single LOCALITY_ID #######
ebird_loc <- ebird_state %>%
  filter(LOCALITY_ID == "L143084") %>%
  mutate(month = floor_date(`OBSERVATION DATE`, unit = "month"),
         year = floor_date(`OBSERVATION DATE`, unit = "year")) %>%
  group_by(COMMON_NAME, month, year) %>%
  summarize(global_avg = mean(OBSERVATION_COUNT),
            sd_avg = sd(OBSERVATION_COUNT),
            hi = global_avg + sd_avg,
            low = global_avg - sd_avg) %>%
  ungroup() %>%
  filter(year > "2015-12-31")

## let's try and plot it
ggplot() +
  geom_rect(data = highlight_years,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "blue", alpha = 0.2, inherit.aes = FALSE) +
  geom_point(data = ebird_loc, aes(x = month, y = global_avg), color = "red") +
  geom_errorbar(data = ebird_loc, aes(x = month, ymin = low, ymax = hi), width = 0.2) +
  facet_wrap(~COMMON_NAME, scales = "free_y") +
  geom_line(data = ebird_loc, aes(x = month, y = global_avg), color = "red") +
  theme_bw() +
  labs(x = "Date",
       y = "Mean Abundance",
       title = "WAKODAHATCHEE eBird")








#### Plotting relative change at a single hotspot scale #####

ebird_summary_WOST <- ebird_summary %>%
  filter(COMMON_NAME == "Wood Stork") %>%
  group_by(LOCALITY_ID, year) %>%
  mutate(avg_max_total = mean(max_n)) %>%
  ungroup()

ebird_summary_WOST <- ebird_summary_WOST %>%
  mutate(rel_diff_yr = (max_n - avg_max_total)/(max_n + avg_max_total))


## now let's make an average  df ##

ebird_summary_WOST_error <- ebird_summary_WOST %>%
  group_by(COMMON_NAME, month, `STATE CODE`) %>%
  summarize(mean_dif)


ggplot(data = ebird_summary_WOST, aes(x = month, y = rel_diff_yr, color = rel_diff_yr)) +
  geom_point() +
  geom_line() +
  facet_wrap(~`STATE CODE`, scales = "free_y") +
  theme_bw() +
  scale_color_gradient(low = "black", high = "white") +
  geom_rect(data = highlight_years,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "blue", alpha = 0.2, inherit.aes = FALSE)