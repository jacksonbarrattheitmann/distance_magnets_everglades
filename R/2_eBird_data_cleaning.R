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


#### Now we need to deal with the issue of group checklists. 

non_groups <- ebird_state %>%
  filter(is.na(`GROUP IDENTIFIER`))

groups <- ebird_state %>%
  filter(!is.na(`GROUP IDENTIFIER`)) %>%
  group_by(`GROUP IDENTIFIER`) %>%
  slice(1) %>%
  ungroup()

# check to make sure this worked
n_distinct(groups$`GROUP IDENTIFIER`)

### Now we have to do a hacky thing where we subset the 
### to get group checklists out, then re-add the groups that pass.

ebird_state <- rbind(non_groups, groups)


#saveRDS(ebird_state, "Data/eBird_test/eBird_clean.RDS")

ebird_state <- readRDS("Data/eBird_test/eBird_clean.RDS")

### We now from other work that both Max Count and Average Max Count can be good 
### proxy measures for wading bird abundance. 

## So, let's create a table that calculates some metrics for us
## and we can use the data_agg.RDS file for our effort metrics

## At this point I think regional metrics are going to be the best for our purposes. 
## So, let's generate some metrics at this scale:
## Max Count Monthly in Region

ebird_state$OBSERVATION_COUNT <- as.numeric(ebird_state$OBSERVATION_COUNT)

ebird_sum <- ebird_state %>%
  mutate(month = floor_date(`OBSERVATION DATE`, unit = "month"),
         year = floor_date(`OBSERVATION DATE`, unit = "year")) %>%
  filter(!OBSERVATION_COUNT == "X",
         COMMON_NAME %in% c("Great Egret", "White Ibis", "Wood Stork", "Snowy Egret",
                            "Roseate Spoonbill"),
         year > "2015-12-31") %>%
  group_by(`STATE CODE`, COMMON_NAME, month, year) %>%
  summarise(max_n = max(OBSERVATION_COUNT, na.rm = TRUE))


## Average Max across all LOCALITY_IDs

ebird_loc <- ebird_state %>%
  mutate(month = floor_date(`OBSERVATION DATE`, unit = "month"),
         year = floor_date(`OBSERVATION DATE`, unit = "year")) %>%
  filter(!OBSERVATION_COUNT == "X",
         COMMON_NAME %in% c("Great Egret", "White Ibis", "Wood Stork", "Snowy Egret",
                            "Roseate Spoonbill"),
         year > "2015-12-31") %>%
  group_by(LOCALITY_ID, COMMON_NAME, month, year, `STATE CODE`) %>%
  summarise(max_n = max(OBSERVATION_COUNT, na.rm = TRUE)) %>%
  ungroup()

ebird_loc_avg <- ebird_loc %>%
  group_by(COMMON_NAME, month, year, `STATE CODE`) %>%
  summarise(avg_max_n = mean(max_n)) %>%
  ungroup()
  
#### FOR NOW THESE ARE THE EMTRICS WE"LL WORK WITH ####
# Let's append them, and then export for analysis

ebird_metrics <- ebird_sum %>%
  left_join(ebird_loc_avg, by = c("COMMON_NAME", "month", "year", "STATE CODE")) %>%
  rename(STATE_CODE = `STATE CODE`)

## Saving this object for our analysis script
#saveRDS(ebird_metrics, "Data/eBird_test/ebird_metrics.RDS")


## we also just need a df that has the STATE_CODE and the LOCALITY_ID

ebird_env <- ebird_state %>%
  select(LOCALITY_ID, `STATE CODE`) %>%
  distinct(LOCALITY_ID, .keep_all = TRUE) %>%
  rename(STATE_CODE = `STATE CODE`)

# save this for prepare data anlysis
#saveRDS(ebird_env, "Data/for_analysis/ebird_env.RDS")

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



