source("lib/common.r")

map_iso3c <- function(iso3c) {
  dplyr::case_when(
    iso3c == "Canada" ~ "CAN",
    iso3c == "Newfoundland and Labrador" ~ "CAN-NL",
    iso3c == "Prince Edward Island" ~ "CAN-PE",
    iso3c == "Nova Scotia" ~ "CAN-NS",
    iso3c == "New Brunswick" ~ "CAN-NB",
    iso3c == "Quebec" ~ "CAN-QC",
    iso3c == "Ontario" ~ "CAN-ON",
    iso3c == "Manitoba" ~ "CAN-MB",
    iso3c == "Saskatchewan" ~ "CAN-SK",
    iso3c == "Alberta" ~ "CAN-AB",
    iso3c == "British Columbia" ~ "CAN-BC",
    iso3c == "Yukon" ~ "CAN-YT",
    iso3c == "Northwest Territories" ~ "CAN-NT",
    iso3c == "Nunavut" ~ "CAN-NU",
    iso3c == "Unknown province or territory of residence" ~ NA_character_,
    TRUE ~ NA_character_
  )
}

# yearly 5y age groups, 0-90+
deaths_year <- read_zip(
  url = "https://www150.statcan.gc.ca/n1/tbl/csv/13100709-eng.zip",
  file_name = "13100709.csv", delim = ",", extra = "-k"
) |>
  filter(Sex == "Both sexes") |>
  select(GEO, REF_DATE, `Age at time of death`, VALUE) |>
  setNames(c("iso3c", "date", "age_group", "deaths")) |>
  mutate(
    iso3c = str_replace(iso3c, ", place of residence", ""),
    iso3c = map_iso3c(iso3c),
    date = as.Date(sprintf("%d-01-01", date)),
    age_group = str_replace(age_group, "Age at time of death, ", ""),
    age_group = case_when(
      str_detect(age_group, "^(\\d+) to (\\d+) years$") ~ str_replace(
        age_group, "(\\d+) to (\\d+) years", "\\1-\\2"
      ),
      age_group %in% c("100 years and over") ~ "100+",
      age_group %in% c("all ages") ~ "all",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(iso3c), !is.na(date), !is.na(age_group), !is.na(deaths)) |>
  mutate(age_group = case_when(
    age_group %in% c("90-94", "95-99", "100+") ~ "90+",
    TRUE ~ age_group
  )) |>
  group_by(iso3c, date, age_group) |>
  summarize(deaths = sum(deaths), .groups = "drop")

# weekly deaths, 4 age groups
deaths_week <- read_zip(
  url = "https://www150.statcan.gc.ca/n1/tbl/csv/13100768-eng.zip",
  file_name = "13100768.csv", delim = ",", extra = "-k"
) |>
  filter(Sex == "Both sexes") |>
  select(GEO, REF_DATE, `Age at time of death`, VALUE) |>
  setNames(c("iso3c", "date", "age_group", "deaths")) |>
  mutate(
    iso3c = str_replace(iso3c, ", place of occurrence", ""),
    iso3c = map_iso3c(iso3c),
    age_group = str_replace(age_group, "Age at time of death, ", ""),
    age_group = case_when(
      age_group == "all ages" ~ "all",
      age_group == "0 to 44 years" ~ "0-44",
      age_group == "45 to 64 years" ~ "45-64",
      age_group == "65 to 84 years" ~ "65-84",
      age_group == "85 years and over" ~ "85+",
      TRUE ~ age_group
    )
  ) |>
  filter(!is.na(iso3c), !is.na(date), !is.na(age_group), !is.na(deaths))

pop_year <- read_zip(
  url = "https://www150.statcan.gc.ca/n1/tbl/csv/17100005-eng.zip",
  file_name = "17100005.csv", delim = ",", extra = "-k"
) |>
  filter(Gender == "Total - gender") |>
  select(GEO, REF_DATE, `Age group`, VALUE) |>
  setNames(c("iso3c", "date", "age_group", "population")) |>
  mutate(
    age_group = case_when(
      str_detect(age_group, "^(\\d+) to (\\d+) years$") ~ str_replace(
        age_group, "(\\d+) to (\\d+) years", "\\1-\\2"
      ),
      age_group %in% c("90 years and older") ~ "90+",
      age_group %in% c("All ages") ~ "all",
      TRUE ~ NA_character_
    )
  ) |>
  mutate(
    iso3c = map_iso3c(iso3c),
    date = as.Date(sprintf("%d-07-01", date))
  ) |>
  filter(!is.na(iso3c), !is.na(date), !is.na(age_group), !is.na(population)) |>
  group_by(iso3c) |>
  nest() |>
  mutate(
    data = map(data, ~ . |>
      group_by(age_group) |>
      nest() |>
      mutate(data = lapply(data, interpolate_population)) |>
      unnest(cols = "data"))
  ) |>
  unnest(cols = "data") |>
  select(all_of(c("iso3c", "date", "age_group", "population")))

pop_week <- pop_year |>
  mutate(
    age_group = case_when(
      age_group %in% c(
        "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
        "40-44"
      ) ~ "0-44",
      age_group %in% c("45-49", "50-54", "55-59", "60-64") ~ "45-64",
      age_group %in% c("65-69", "70-74", "75-79", "80-84") ~ "65-84",
      age_group %in% c("85-89", "90-94", "95-99") ~ "85+",
      TRUE ~ NA
    )
  ) |>
  filter(!is.na(age_group)) |>
  group_by(iso3c, date, age_group) |>
  summarize(population = sum(population), .groups = "drop")

weekly <- deaths_week |>
  inner_join(pop_week, by = join_by(iso3c, date, age_group)) |>
  filter(!is.na(iso3c), !is.na(date), !is.na(age_group), !is.na(deaths)) |>
  group_by(iso3c, date, age_group) |>
  summarize(
    deaths = sum(deaths), population = sum(population),
    .groups = "drop"
  )
weekly$n_age_groups <- length(unique((
  weekly |> filter(age_group != "all")
)$age_group))
weekly$type <- 3

yearly <- deaths_year |>
  inner_join(pop_year, by = join_by(iso3c, date, age_group)) |>
  mutate(
    age_group = case_when(
      age_group %in% c("0-4", "5-9") ~ "0-9",
      age_group %in% c("10-14", "15-19") ~ "10-19",
      age_group %in% c("20-24", "25-29") ~ "20-29",
      age_group %in% c("30-34", "35-39") ~ "30-39",
      age_group %in% c("40-44", "45-49") ~ "40-49",
      age_group %in% c("50-54", "55-59") ~ "50-59",
      age_group %in% c("60-64", "65-69") ~ "60-69",
      age_group %in% c("70-74", "75-79") ~ "70-79",
      age_group %in% c("80-84", "85-89") ~ "80-89",
      age_group %in% c("90-94", "95-99") ~ "90+",
      TRUE ~ age_group
    )
  ) |>
  filter(!is.na(iso3c), !is.na(date), !is.na(age_group), !is.na(deaths)) |>
  group_by(iso3c, date, age_group) |>
  summarize(
    deaths = sum(deaths), population = sum(population),
    .groups = "drop"
  )
yearly$n_age_groups <- length(unique((
  yearly |> filter(age_group != "all")
)$age_group))
yearly$type <- 1

can_mortality_states <- rbind(
  yearly,
  weekly
) |> arrange(iso3c, date, age_group, type)
can_mortality_states$source <- "statcan"

# source("./mortality/can/mortality_states.r")
