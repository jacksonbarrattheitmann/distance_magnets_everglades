### Bayesian Model for eBird data

library(readr)
library(dplyr)
library(ggplot2)
library(brms)
library(purrr)

### our data
dat <- readRDS("Data/eBird_test/agg_dataframe.RDS")

## let's join the region/state data
env <- readRDS("Data/eBird_test/env_table.RDS")

dat_full <- dat %>%
  left_join(env, by = c("locality_id", "locality_type"))

## let's check to make sure everything looks good
## and let's also get rid of personal hotspots, since they'll have serious observer bias

dat_2 <- dat_full |>
  filter(locality_type == "H")

### Let's also create some nice preliminary filters for only hotspots that are well-sampled temporally.
keep <- dat_2 |>
  filter(species == "Wood Stork") |>
  group_by(locality_id, year) |>
  summarise(m = n_distinct(month), .groups = "drop") |>
  filter(m >= 8) |>
  count(locality_id, name = "n_years") |>
  filter(n_years >= 10)

### now let's filter our data to only this subset
### to make the bayesian models run a bit quicker to start

WOST <- dat_2 %>%
  filter(species == "Wood Stork" & locality_id %in% keep$locality_id & year > 2015)


WOST |> dplyr::group_by(locality_id) |> dplyr::summarise(k = sum(k)) |> dplyr::count(k == 0)


hs_k <- WOST |>
  dplyr::group_by(locality_id) |>
  dplyr::summarise(k_total = sum(k), n_total = sum(n), n_cells = dplyr::n(), .groups = "drop")

quantile_sum <- hs_k |> dplyr::filter(k_total > 0) |> dplyr::pull(k_total) |> quantile(c(.1, .25, .5, .75))
state_sum <- hs_k |> dplyr::count(k_total == 0, k_total < 10, k_total < 25)


keep_hs <- hs_k |> dplyr::filter(k_total >= 10) |> dplyr::pull(locality_id)
WOST2 <- dplyr::filter(WOST, locality_id %in% keep_hs)

nrow(WOST2); dplyr::n_distinct(WOST2$locality_id)
WOST2 |> dplyr::distinct(locality_id, state_code) |> dplyr::count(state_code)

## Bayesian Model Framework
### 1) Build a model to fit parameter uncertainty for detecting
### birds at individual hotpot x species x month x year. This
### in effect is our eBird paraemter, that we can then use to build
### our predictive model on wading bird abundance in Everglades.
#### This only helps us build a model that asks:

#### Does eBird data track wading bird abundance changes in the colony sizes?


fit_1 <- brm(
  k | trials(n) ~ mean_log_dur + (1 | locality_id) +
    (1 | state_code:month) + (1 | state_code:month:year),
  data = WOST, family = binomial(), iter = 1000, chains = 4, cores = 2)

plot(fit_1)

fits <- c(list(fit_1),
          map(spp_dat[-1], \(d) update(fit_1, newdata = d, chains = 4, cores = 4)))
