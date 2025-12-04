source("lib/common.r")

de_states <- as_tibble(read.csv("./data_static/deu_states_iso3c.csv"))

# Genesis 12411-0005: Bevölkerung: Deutschland, Stichtag, Altersjahre
pop_raw <- fetch_genesis_data(
  "https://apify.mortality.watch/destatis-genesis/12411-0005.csv.gz", 6, -4
)
pop <- pop_raw |>
  pivot_longer(
    cols = 2:ncol(pop_raw),
    names_to = "year",
    values_to = "population"
  ) |>
  setNames(c("age_group", "year", "population")) |>
  parse_age_groups() |>
  mutate(
    year = as.integer(right(year, 4)),
    population = as.integer(population)
  )
pop$jurisdiction <- "Deutschland"
pop <- pop |> relocate(jurisdiction, year, age_group, population)

# Genesis 12411-0012: Bevölkerung: Bundesländer, Stichtag, Altersjahre (flat)
pop_states <- read_delim(
  "https://apify.mortality.watch/destatis-genesis/12411-0012.csv?flat=1",
  delim = ";",
  locale = locale(encoding = "UTF-8"),
  col_types = cols(.default = "c")
) |>
  select(
    year = time,
    jurisdiction = `1_variable_attribute_label`,
    age_group = `2_variable_attribute_label`,
    population = value
  ) |>
  mutate(
    year = as.integer(substr(year, 1, 4)),
    population = suppress_warnings(
      as.integer(population),
      "NAs introduced by coercion"
    )
  ) |>
  parse_age_groups() |>
  relocate(jurisdiction, year, age_group, population) |>
  filter(!is.na(jurisdiction), !is.na(year), !is.na(age_group), !is.na(population))

pop2 <- pop |>
  mutate(
    age_group = case_when(
      age_group %in% 0:4 ~ "0-4",
      age_group %in% 5:9 ~ "5-9",
      age_group %in% 10:14 ~ "10-14",
      age_group %in% 15:19 ~ "15-19",
      age_group %in% 20:24 ~ "20-24",
      age_group %in% 25:29 ~ "25-29",
      age_group %in% 30:34 ~ "30-34",
      age_group %in% 35:39 ~ "35-39",
      age_group %in% 40:44 ~ "40-44",
      age_group %in% 45:49 ~ "45-49",
      age_group %in% 50:54 ~ "50-54",
      age_group %in% 55:59 ~ "55-59",
      age_group %in% 60:64 ~ "60-64",
      age_group %in% 65:69 ~ "65-69",
      age_group %in% 70:74 ~ "70-74",
      age_group %in% 75:79 ~ "75-79",
      age_group %in% 80:84 ~ "80-84",
      age_group == "85+" ~ "85+",
      age_group == "all" ~ "all"
    )
  ) |>
  group_by(jurisdiction, year, age_group) |>
  summarise(population = sum(population), .groups = "drop")

# 10y age groups
pop3 <- pop |>
  mutate(
    age_group = case_when(
      age_group %in% 0:9 ~ "0-9",
      age_group %in% 10:19 ~ "10-19",
      age_group %in% 20:29 ~ "20-29",
      age_group %in% 30:39 ~ "30-39",
      age_group %in% 40:49 ~ "40-49",
      age_group %in% 50:59 ~ "50-59",
      age_group %in% 60:69 ~ "60-69",
      age_group %in% 70:79 ~ "70-79",
      age_group %in% 80:84 ~ "80+",
      age_group == "85+" ~ "80+"
    )
  ) |>
  filter(!is.na(age_group)) |>
  group_by(jurisdiction, year, age_group) |>
  summarise(population = sum(population), .groups = "drop")

# Additional larger groups
pop4 <- pop |>
  mutate(
    age_group = case_when(
      age_group %in% 0:29 ~ "0-29",
      age_group %in% 30:44 ~ "30-44",
      age_group %in% 45:64 ~ "45-64",
      age_group %in% 65:74 ~ "65-74",
      age_group %in% 75:84 ~ "75-84"
    )
  ) |>
  filter(!is.na(age_group)) |>
  group_by(jurisdiction, year, age_group) |>
  summarise(population = sum(population), .groups = "drop")

rm(pop)

pop_states2 <- pop_states |>
  mutate(
    age_group = case_when(
      age_group %in% 0:64 ~ "0-64",
      age_group %in% 65:74 ~ "65-74",
      age_group %in% 75:84 ~ "75-84",
      age_group %in% 85:89 ~ "85+",
      age_group == "90+" ~ "85+",
      age_group == "all" ~ "all",
      .default = age_group
    )
  ) |>
  group_by(jurisdiction, year, age_group) |>
  summarise(population = sum(population), .groups = "drop")

rm(pop_states)

# Format: iso3c, jurisdiction, year, population, is_projection
de_population <- rbind(pop2, pop3, pop4, pop_states2) |>
  filter(!is.na(year), !is.na(population)) |>
  inner_join(de_states, by = "jurisdiction") |>
  nest(data = c("year", "population")) |>
  mutate(data = lapply(data, forecast_population)) |>
  unnest(cols = "data")

stopifnot(length(unique(de_population$iso3c)) == 17)

rm(de_states, pop2, pop_states2)

# source("./population/deu/deu.r")
