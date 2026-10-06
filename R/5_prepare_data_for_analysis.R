## Big join

library(dplyr)
library(readr)
library(tidyverse)

## eBird
ebird <- readRDS("Data/for_analysis/ebird_metrics.RDS")

## eBird effort data
agg <- readRDS("Data/for_analysis/agg_dataframe.RDS")

## land cover + edenR
lulc <- readRDS("Data/for_analysis/land_cover_eden.RDS")

## wading_bird_data
wading <- readRDS("Data/for_analysis/wading_bird_EV.RDS")
