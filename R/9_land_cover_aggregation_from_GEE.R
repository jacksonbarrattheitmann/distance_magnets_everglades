## Reading in env covariate data to prepare data 
## for analysis

library(tidyverse)
library(dplyr)
library(readr)
library(arrow)
library(stringr)

## we have the parquet file
## that already has all the compiled 
## monthly water data for each locality_id

monthly <- read_parquet("Data/landcover_dat/gsw_monthly_long.parquet")

## then we also have the static images that are annual
## that contain the water permanence metrics
## we need to aggregate these and convert them to long format

test <- read_csv("Data/landcover_dat/drive-download-20261103T163131Z-1-001/gsw_annual_c000_2010_2015.csv") %>%
  select(-.geo, -`system:index`, -chunk, -stat_cd)

test_long <- test %>%
  pivot_longer(
    cols = -lclty_d,
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(Year = str_extract(Variable, "(?<=_).*"),
         Water_var = str_extract(Variable, ".*(?=_)")) %>%
  select(-Variable)

## need to make a function that rbind this together
## after each subsequent file is read

bind_files <- function(csv_paths) {
  
  df <- data.frame()
  
  for (i in seq_along(csv_paths)) {
  
    test <- read_csv(csv_paths[i]) %>%
    select(-.geo, -`system:index`, -chunk, -stat_cd)
  
  test_long <- test %>%
    pivot_longer(
      cols = -lclty_d,
      names_to = "Variable",
      values_to = "Value"
    ) %>%
    mutate(year = str_extract(Variable, "(?<=_).*"),
           Water_var = str_extract(Variable, ".*(?=_)")) %>%
    select(-Variable) %>%
    pivot_wider(names_from = "Water_var",
                values_from = "Value")
  
  df <- rbind(df, test_long)
  
         }

  return(df)
  
}

files <- list.files(path = "Data/landcover_dat/drive-download-20261103T163131Z-1-001/", pattern = "\\.csv$", full.names = TRUE)

big_df <- bind_files(files)

big_df$year <- as.integer(big_df$year)

## Now we have this whole thing built 
## we need to do a join of the bigger monthly df
## and the static annual one

water_dat <- monthly %>%
  left_join(big_df, by = c("lclty_d", "year"))

## now let's save this

saveRDS(water_dat, "Data/landcover_dat/combined_monthly_water_GEE.RDS")
