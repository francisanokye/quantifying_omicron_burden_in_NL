library(dplyr)
library(tidyr)
library(lubridate)

# period 
periods_tbl <- tibble::tribble(
  ~period,            ~start,               ~end,
  "2020::Jul–Dec", as.Date("2020-07-01"), as.Date("2020-12-31"),
  "2021::Jan–Jun", as.Date("2021-01-01"), as.Date("2021-06-30"),
  "2021::Jul–Dec", as.Date("2021-07-01"), as.Date("2021-12-31"),
  "2022::Jan–Jun", as.Date("2022-01-01"), as.Date("2022-06-30")
)

prov_map <- c(AB="Alberta", BC="British Columbia", MB="Manitoba", NB="New Brunswick",
              NL="Newfoundland and Labrador", NT="Northwest Territories", NS="Nova Scotia",
              NU="Nunavut", ON="Ontario", PE="Prince Edward Island", QC="Quebec",
              SK="Saskatchewan", YT="Yukon")

# 2021 census populations for each province
pop_2021 <- tibble::tribble(
  ~prname, ~population,
  "Ontario", 14223942,
  "Quebec", 8501833,
  "British Columbia", 5000879,
  "Alberta", 4262635,
  "Manitoba", 1342153,
  "Saskatchewan", 1132505,
  "Nova Scotia", 969383,
  "New Brunswick", 775610,
  "Newfoundland and Labrador", 510550,
  "Prince Edward Island", 154331,
  "Yukon", 40232,
  "Northwest Territories", 41070,
  "Nunavut", 36858
)

# load reported cases
covid <- read.csv("../data/covid19-download.csv") |>
  dplyr::select(c("prname","date","totalcases")) |>
  mutate(date = as.Date(date, format = "%Y-%m-%d"),totalcases = as.numeric(totalcases)) |>
  dplyr::filter(prname %in% prov_map) |>
  arrange(prname, date)

grid <- covid |> distinct(prname) |> tidyr::crossing(periods_tbl)

# last cumulative on/before end (fallback when last inside missing)
cum_end_any <- grid |>
  left_join(covid, by="prname") |>
  filter(date <= end) |>
  group_by(prname, period, start, end) |>
  slice_max(date, n=1, with_ties=FALSE) |>
  transmute(prname, period, start, end, cum_end_any = totalcases)

# first & last within the interval
cum_within <- grid |>
  left_join(covid, by="prname") |>
  filter(date >= start, date <= end) |>
  group_by(prname, period, start, end) |>
  summarise(
    cum_start_in = first(totalcases),
    cum_end_in   = last(totalcases),
    .groups = "drop"
  )

# last cumulative before start (fallback when first inside missing)
cum_before <- grid |>
  left_join(covid, by="prname") |>
  filter(date < start) |>
  group_by(prname, period, start, end) |>
  slice_max(date, n=1, with_ties=FALSE) |>
  transmute(prname, period, start, end, cum_before = totalcases)

reported_bounds <- grid |>
  left_join(cum_within,  by=c("prname","period","start","end")) |>
  left_join(cum_before,  by=c("prname","period","start","end")) |>
  left_join(cum_end_any, by=c("prname","period","start","end")) |>
  left_join(pop_2021,    by="prname") |>
  mutate(
    province   = prname,
    cum_start  = dplyr::coalesce(cum_start_in, cum_before, 0),
    cum_end    = dplyr::coalesce(cum_end_in,   cum_end_any),
    rep_start_pct = 100 * (cum_start / population),      # %
    rep_end_pct   = 100 * (cum_end   / population),      # %
    cum_rep_percapita  = rep_end_pct - rep_start_pct          # percentage points
  ) |>
  select(province, period, start, end, population,
         cum_start, rep_start_pct, cum_end, rep_end_pct, cum_rep_percapita)

# serology (infection-induced): first/last inside interval ----
serology_inf <- read.csv("../data/seroprev_region.csv") |>
  filter(ab_target == "N") |>                                    # N = nucleocapsid
  mutate(
    samplingdate = mdy(samplingdate),
    period = case_when(
      year(samplingdate)==2020 & month(samplingdate)%in%7:12 ~ "2020::Jul–Dec",
      year(samplingdate)==2021 & month(samplingdate)%in%1:6  ~ "2021::Jan–Jun",
      year(samplingdate)==2021 & month(samplingdate)%in%7:12 ~ "2021::Jul–Dec",
      year(samplingdate)==2022 & month(samplingdate)%in%1:6  ~ "2022::Jan–Jun",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(period)) |>
  arrange(geo, samplingdate)

sero_bounds <- serology_inf |>
  group_by(geo, period) |>
  summarise(
    n_points       = sum(!is.na(seroprev_est)),
    sero_start_pct = first(seroprev_est[!is.na(seroprev_est)]) * 100,
    sero_end_pct   =  last(seroprev_est[!is.na(seroprev_est)]) * 100,
    sero_delta_pct  = ifelse(n_points >= 2, pmax(sero_end_pct - sero_start_pct, 0), NA_real_), # clamp negatives
    .groups = "drop"
  ) |>
  mutate(province = prov_map[geo]) |>
  select(province, period, sero_start_pct, sero_end_pct, sero_delta_pct) |>
  filter(!is.na(province))

# underreporting ratio: chnage in serology / cum. reported per capita pct ----
underreporting_df <- reported_bounds |>
  inner_join(sero_bounds, by=c("province","period")) |>
  mutate(
    underreporting_ratio = ifelse(cum_rep_percapita > 0, sero_delta_pct / cum_rep_percapita, NA_real_)
  ) |>
  select(province, period, , population, cum_start, cum_end, cum_rep_percapita, sero_delta_pct,underreporting_ratio) |>
  arrange(province)

underreporting_df