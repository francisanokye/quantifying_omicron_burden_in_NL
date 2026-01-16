

# library(dplyr)
# library(tidyr)
# library(lubridate)
# 
# # period
# periods_tbl <- tibble::tribble(
#   ~period,            ~start,               ~end,
#   "2020::Jul–Dec", as.Date("2020-07-01"), as.Date("2020-12-31"),
#   "2021::Jan–Jun", as.Date("2021-01-01"), as.Date("2021-06-30"),
#   "2021::Jul–Dec", as.Date("2021-07-01"), as.Date("2021-12-31"),
#   "2022::Jan–Jun", as.Date("2022-01-01"), as.Date("2022-06-30")
# )
# 
# prov_map <- c(AB="Alberta", BC="British Columbia", MB="Manitoba", NB="New Brunswick",
#               NL="Newfoundland and Labrador", NT="Northwest Territories", NS="Nova Scotia",
#               NU="Nunavut", ON="Ontario", PE="Prince Edward Island", QC="Quebec",
#               SK="Saskatchewan", YT="Yukon")
# 
# pop_2021 <- tibble::tribble(
#   ~prname, ~population,
#   "Ontario", 14223942,
#   "Quebec", 8501833,
#   "British Columbia", 5000879,
#   "Alberta", 4262635,
#   "Manitoba", 1342153,
#   "Saskatchewan", 1132505,
#   "Nova Scotia", 969383,
#   "New Brunswick", 775610,
#   "Newfoundland and Labrador", 510550,
#   "Prince Edward Island", 154331,
#   "Yukon", 40232,
#   "Northwest Territories", 41070,
#   "Nunavut", 36858
# )
# 
# # load reported cases
# covid <- read.csv("~/quantifying_omicron_burden_in_NL/data/covid19-download.csv") |>
#   select(prname, date, totalcases) |>
#   mutate(date = as.Date(date),
#          totalcases = as.numeric(totalcases)) |>
#   filter(prname %in% unname(prov_map)) |>
#   arrange(prname, date)
# 
# grid <- covid |> distinct(prname) |> crossing(periods_tbl)
# 
# # last cumulative on/before end
# cum_end_any <- grid |>
#   left_join(covid, by = "prname") |>
#   filter(date <= end) |>
#   group_by(prname, period, start, end) |>
#   slice_max(date, n = 1, with_ties = FALSE) |>
#   transmute(prname, period, start, end, cum_end_any = totalcases)
# 
# # first & last within interval (ensure date order)
# cum_within <- grid |>
#   left_join(covid, by="prname") |>
#   filter(date >= start, date <= end) |>
#   arrange(prname, period, date) |>
#   group_by(prname, period, start, end) |>
#   summarise(
#     cum_start_in = first(totalcases),
#     cum_end_in   = last(totalcases),
#     .groups = "drop"
#   )
# 
# # last cumulative before start
# cum_before <- grid |>
#   left_join(covid, by="prname") |>
#   filter(date < start) |>
#   group_by(prname, period, start, end) |>
#   slice_max(date, n = 1, with_ties = FALSE) |>
#   transmute(prname, period, start, end, cum_before = totalcases)
# 
# reported_bounds <- grid |>
#   left_join(cum_within,  by=c("prname","period","start","end")) |>
#   left_join(cum_before,  by=c("prname","period","start","end")) |>
#   left_join(cum_end_any, by=c("prname","period","start","end")) |>
#   left_join(pop_2021,    by="prname") |>
#   mutate(
#     province  = prname,
#     cum_start = coalesce(cum_start_in, cum_before, 0),
#     cum_end   = coalesce(cum_end_in,   cum_end_any),
#     delta_cases = cum_end - cum_start,
#     delta_cases_percapita = delta_cases / population,         # cases per person
#     delta_cases_percapita_pct = 100 * delta_cases_percapita   # optional (% of pop)
#   ) |>
#   select(province, period, start, end, population,
#          cum_start, cum_end, delta_cases,
#          delta_cases_percapita, delta_cases_percapita_pct)
# 
# # serology (infection-induced): first/last inside interval
# serology_inf <- read.csv("~/quantifying_omicron_burden_in_NL/data/seroprev_region.csv") |>
#   filter(ab_target == "N") |>
#   mutate(
#     samplingdate = mdy(samplingdate),
#     period = case_when(
#       year(samplingdate)==2020 & month(samplingdate) %in% 7:12 ~ "2020::Jul–Dec",
#       year(samplingdate)==2021 & month(samplingdate) %in% 1:6  ~ "2021::Jan–Jun",
#       year(samplingdate)==2021 & month(samplingdate) %in% 7:12 ~ "2021::Jul–Dec",
#       year(samplingdate)==2022 & month(samplingdate) %in% 1:6  ~ "2022::Jan–Jun",
#       TRUE ~ NA_character_
#     )
#   ) |>
#   filter(!is.na(period)) |>
#   arrange(geo, samplingdate)
# 
# sero_bounds <- serology_inf |>
#   group_by(geo, period) |>
#   summarise(
#     n_points = sum(!is.na(seroprev_est)),
#     sero_start = first(seroprev_est[!is.na(seroprev_est)]),   # proportion
#     sero_end   = last(seroprev_est[!is.na(seroprev_est)]),    # proportion
#     sero_delta = ifelse(n_points >= 2, pmax(sero_end - sero_start, 0), NA_real_),
#     .groups = "drop"
#   ) |>
#   mutate(
#     province = unname(prov_map[geo]),
#     sero_start_pct = 100 * sero_start,
#     sero_end_pct   = 100 * sero_end,
#     sero_delta_pct = 100 * sero_delta
#   ) |>
#   select(province, period, sero_start_pct, sero_end_pct, sero_delta_pct, sero_delta) |>
#   filter(!is.na(province))
# 
# # underreporting ratio: (change in seroprev proportion) / (change in reported cases per person)
# underreporting_df <- reported_bounds |>
#   inner_join(sero_bounds, by=c("province","period")) |>
#   mutate(
#     underreporting_ratio =
#       ifelse(delta_cases_percapita > 0, sero_delta / delta_cases_percapita, NA_real_)
#   ) |>
#   select(province, period, population,
#          cum_start, cum_end, delta_cases,
#          delta_cases_percapita, delta_cases_percapita_pct,
#          sero_delta_pct, underreporting_ratio) |>
#   arrange(province, period)
# 
# underreporting_df



# Correct CITF-style under-reporting computation
# Fixes ALL issues flagged:
#  (1) Denominator uses Δ cumulative reported cases per capita (% points): 100 * (ΔC / N)
#  (2) Adds explicit columns: ΔC and Δ cases per capita (% points)
#  (3) Avoids accidental zero ratios from early rounding or forced clamping
#  (4) Ensures first()/last() are taken in correct time order
#  (5) Fixes province filtering (names vs codes)
#  (6) Removes any stray/invalid pipe RHS

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(readr)
})

# --- periods ---------------------------------------------------------------
periods_tbl <- tibble::tribble(
  ~period,           ~start,               ~end,
  "2020::Jul–Dec",   as.Date("2020-07-01"), as.Date("2020-12-31"),
  "2021::Jan–Jun",   as.Date("2021-01-01"), as.Date("2021-06-30"),
  "2021::Jul–Dec",   as.Date("2021-07-01"), as.Date("2021-12-31"),
  "2022::Jan–Jun",   as.Date("2022-01-01"), as.Date("2022-06-30")
)

prov_map <- c(
  AB="Alberta", BC="British Columbia", MB="Manitoba", NB="New Brunswick",
  NL="Newfoundland and Labrador", NT="Northwest Territories", NS="Nova Scotia",
  NU="Nunavut", ON="Ontario", PE="Prince Edward Island", QC="Quebec",
  SK="Saskatchewan", YT="Yukon"
)

# 2021 census populations
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

# --- reported cumulative cases --------------------------------------------
covid <- readr::read_csv(
  "~/quantifying_omicron_burden_in_NL/data/covid19-download.csv",
  show_col_types = FALSE
) |>
  select(prname, date, totalcases) |>
  mutate(
    date = as.Date(date),
    totalcases = suppressWarnings(as.numeric(totalcases))
  ) |>
  filter(prname %in% unname(prov_map)) |>        # FIX: correct name matching
  arrange(prname, date)

grid <- covid |> distinct(prname) |> tidyr::crossing(periods_tbl)

# start cumulative: last value strictly BEFORE start
cum_before <- grid |>
  left_join(covid, by = "prname") |>
  filter(date < start) |>
  group_by(prname, period, start, end) |>
  slice_max(date, n = 1, with_ties = FALSE) |>
  transmute(prname, period, start, end, cum_before = totalcases)

# within interval: first/last value inside [start, end] (ordered by date)
cum_within <- grid |>
  left_join(covid, by = "prname") |>
  filter(date >= start, date <= end) |>
  arrange(prname, period, date) |>
  group_by(prname, period, start, end) |>
  summarise(
    cum_start_in = first(totalcases),
    cum_end_in   = last(totalcases),
    .groups = "drop"
  )

# end cumulative: last value on/before end
cum_end_any <- grid |>
  left_join(covid, by = "prname") |>
  filter(date <= end) |>
  group_by(prname, period, start, end) |>
  slice_max(date, n = 1, with_ties = FALSE) |>
  transmute(prname, period, start, end, cum_end_any = totalcases)

reported_bounds <- grid |>
  left_join(cum_within,  by = c("prname","period","start","end")) |>
  left_join(cum_before,  by = c("prname","period","start","end")) |>
  left_join(cum_end_any, by = c("prname","period","start","end")) |>
  left_join(pop_2021,    by = "prname") |>
  mutate(
    province  = prname,
    
    # FIX: use cum_before as start when possible; otherwise first inside; otherwise 0
    cum_start = coalesce(cum_before, cum_start_in, 0),
    
    # FIX: use last inside; otherwise last on/before end
    cum_end   = coalesce(cum_end_in, cum_end_any),
    
    # FIX: correct denominator components
    delta_cases = cum_end - cum_start,
    delta_cases_percapita = delta_cases / population,
    delta_cases_percapita_pct = 100 * delta_cases_percapita    # Δ%R (percentage points)
  ) |>
  select(province, period, start, end, population,
         cum_start, cum_end, delta_cases,
         delta_cases_percapita, delta_cases_percapita_pct)

# --- serology (infection-induced N antibody) -------------------------------
serology_inf <- readr::read_csv(
  "~/quantifying_omicron_burden_in_NL/data/seroprev_region.csv",
  show_col_types = FALSE
) |>
  filter(ab_target == "N") |>
  mutate(
    samplingdate = mdy(samplingdate),
    period = case_when(
      year(samplingdate)==2020 & month(samplingdate) %in% 7:12 ~ "2020::Jul–Dec",
      year(samplingdate)==2021 & month(samplingdate) %in% 1:6  ~ "2021::Jan–Jun",
      year(samplingdate)==2021 & month(samplingdate) %in% 7:12 ~ "2021::Jul–Dec",
      year(samplingdate)==2022 & month(samplingdate) %in% 1:6  ~ "2022::Jan–Jun",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(period)) |>
  arrange(geo, samplingdate)

serology_inf

sero_bounds <- serology_inf |>
  group_by(geo, period) |>
  summarise(
    n_points = sum(!is.na(seroprev_est)),
    sero_start = ifelse(n_points >= 1, first(seroprev_est[!is.na(seroprev_est)]), NA_real_),
    sero_end   = ifelse(n_points >= 1,  last(seroprev_est[!is.na(seroprev_est)]), NA_real_),
    
    # FIX: do NOT clamp to 0; allow negatives to show inconsistencies
    sero_delta = ifelse(n_points >= 2, sero_end - sero_start, NA_real_),
    
    .groups = "drop"
  ) |>
  mutate(
    province = unname(prov_map[geo]),
    sero_start_pct = 100 * sero_start,
    sero_end_pct   = 100 * sero_end,
    sero_delta_pct = 100 * sero_delta              # Δ%S (percentage points)
  ) |>
  select(province, period, sero_start_pct, sero_end_pct, sero_delta_pct) |>
  filter(!is.na(province))

# --- under-reporting ratio (CITF) -----------------------------------------
# FIX: UR = Δ%S / Δ%R  (percentage points / percentage points)
# Also avoid "0.0" artifacts by NOT rounding until display time.
underreporting_df <- reported_bounds |>
  inner_join(sero_bounds, by = c("province","period")) |>
  mutate(
    underreporting_ratio = ifelse(
      is.finite(delta_cases_percapita_pct) & delta_cases_percapita_pct > 0 &
        is.finite(sero_delta_pct),
      sero_delta_pct / delta_cases_percapita_pct,
      NA_real_
    )
  ) |>
  select(
    province, period, population,
    cum_start, cum_end, delta_cases,
    delta_cases_percapita_pct,   # this is the REQUIRED denominator column
    sero_delta_pct,
    underreporting_ratio
  ) |>
  arrange(province, period)

print(underreporting_df)



write.csv(underreporting_df, "~/quantifying_omicron_burden_in_NL/data/underreporting_df.csv", row.names = FALSE)
