## Big join

library(dplyr)
library(readr)
library(tidyverse)

## eBird

ebird <- readRDS("Data/eBird_test/eBird_clean.RDS")

## land cover + edenR
lulc <- readRDS("Data/land_cover_eden.RDS")

## wading_bird_data
wading <- read.csv("../../wader_data/EvergladesWadingBird/Counts/image_counts.csv")
