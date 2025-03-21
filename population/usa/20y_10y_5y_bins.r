source("lib/common.r")

us_states_iso3c <- as_tibble(read.csv("./data_static/usa_states_iso3c.csv"))

# 1999 from CDC Wonder
parse_data <- function(df, age_group, year) {
  if (age_group == "all") {
    df2 <- df |>
      select("State", "Population") |>
      setNames(c("jurisdiction", "population"))
    df2$age_group <- "all"
  } else {
    if ("State" %in% colnames(df)) {
      df2 <- df |>
        select("State", "Population", "Five-Year Age Groups Code") |>
        setNames(c("jurisdiction", "population", "age_group"))
    } else {
      df2 <- df |>
        select("Population", "Five-Year Age Groups Code") |>
        setNames(c("population", "age_group"))
      df2$jurisdiction <- "United States"
    }
  }
  df2$year <- year
  df2 |>
    rowwise() |>
    mutate(population = as_integer(population)) |>
    filter(!is.na(jurisdiction), !is.na(population))
}

summarize_age_groups <- function(df) {
  df |>
    mutate(
      age_group = case_when(
        age_group %in% c("1") ~ "0-4",
        age_group %in% c("1-4") ~ "0-4",
        age_group %in% c("85-89") ~ "85+",
        age_group %in% c("90-94") ~ "85+",
        age_group %in% c("95-99") ~ "85+",
        age_group %in% c("95+") ~ "85+",
        age_group %in% c("100+") ~ "85+",
        .default = age_group
      )
    ) |>
    group_by(across(!population)) |>
    summarise(population = sum(population), .groups = "drop")
}

summarize_age_groups_10y <- function(df) {
  df |>
    mutate(
      age_group = case_when(
        age_group %in% c("0-4") ~ "0-9",
        age_group %in% c("5-9") ~ "0-9",
        age_group %in% c("10-14") ~ "10-19",
        age_group %in% c("15-19") ~ "10-19",
        age_group %in% c("20-24") ~ "20-29",
        age_group %in% c("25-29") ~ "20-29",
        age_group %in% c("30-34") ~ "30-39",
        age_group %in% c("35-39") ~ "30-39",
        age_group %in% c("40-44") ~ "40-49",
        age_group %in% c("45-49") ~ "40-49",
        age_group %in% c("50-54") ~ "50-59",
        age_group %in% c("55-59") ~ "50-59",
        age_group %in% c("60-64") ~ "60-69",
        age_group %in% c("65-69") ~ "60-69",
        age_group %in% c("70-74") ~ "70-79",
        age_group %in% c("75-79") ~ "70-79",
        age_group %in% c("80-84") ~ "80+",
        age_group %in% c("85+") ~ "80+",
        .default = age_group
      )
    ) |>
    group_by(across(!population)) |>
    summarise(population = sum(population), .groups = "drop")
}

summarize_age_groups_20y <- function(df) {
  df |>
    mutate(
      age_group = case_when(
        age_group %in% c("0-9") ~ "0-19",
        age_group %in% c("10-19") ~ "0-19",
        age_group %in% c("20-29") ~ "20-39",
        age_group %in% c("30-39") ~ "20-39",
        age_group %in% c("40-49") ~ "40-59",
        age_group %in% c("50-59") ~ "40-59",
        age_group %in% c("60-69") ~ "60-79",
        age_group %in% c("70-79") ~ "60-79",
        .default = age_group
      )
    ) |>
    group_by(across(!population)) |>
    summarise(population = sum(population), .groups = "drop")
}

calc_85 <- function(data) {
  split_data <- split(data, data$age_group != "all")
  all <- split_data[["FALSE"]]
  ages <- split_data[["TRUE"]]

  rbind(
    data |> select(-jurisdiction),
    tibble(
      age_group = "85+",
      year = 1999,
      population = all$population - sum(ages$population)
    )
  )
}

# 1999 - only present in CDC Wonder data.
df <- read_csv("./data_wonder/population_usa_1999_age.csv") |> as_tibble()
## National
usa <- parse_data(df, age_group = "ag", year = 1999) |>
  mutate(age_group = ifelse(is.na(age_group), "all", age_group))

## States
df <- read_csv("./data_wonder/population_usa-states_1999.csv") |> as_tibble()
all <- parse_data(df, age_group = "all", year = 1999)

# By 5y ag
df <- read_csv("./data_wonder/population_usa-states_1999_age.csv") |>
  as_tibble()
age <- parse_data(df, age_group = "ag", year = 1999)

df_1999 <- df <- bind_rows(usa, all, age) |>
  summarize_age_groups() |>
  group_by(jurisdiction) |>
  group_modify(~ calc_85(.x), .keep = TRUE)

df_1999_10y <- df_1999 |> summarize_age_groups_10y()
df_1999_20y <- df_1999_10y |> summarize_age_groups_20y()

# 2000-2023
data0 <- as_tibble(read.csv("./data_static/population_usa_2000-2010.csv"))
data1 <- as_tibble(read.csv("./data_static/population_usa_2010-2020.csv"))
data2 <- as_tibble(read.csv(paste0(
  "https://www2.census.gov/programs-surveys/popest/datasets/",
  "2020-2023/state/asrh/sc-est2023-agesex-civ.csv"
)))

# Transform data
a <- data0 |>
  filter(SEX == 0) |>
  select(
    NAME, AGE,
    POPESTIMATE2000, POPESTIMATE2001, POPESTIMATE2002,
    POPESTIMATE2003, POPESTIMATE2004, POPESTIMATE2005,
    POPESTIMATE2006, POPESTIMATE2007, POPESTIMATE2008,
    POPESTIMATE2009
  ) |>
  pivot_longer(
    cols = starts_with("POPEST"), names_to = "year", values_to = "population"
  ) |>
  transform(year = right(year, 4)) |>
  setNames(c("jurisdiction", "age", "year", "population"))

b <- data1 |>
  filter(SEX == 0) |>
  select(
    NAME, AGE,
    POPEST2010_CIV, POPEST2011_CIV, POPEST2012_CIV, POPEST2013_CIV,
    POPEST2014_CIV, POPEST2015_CIV, POPEST2016_CIV, POPEST2017_CIV,
    POPEST2018_CIV, POPEST2019_CIV, POPEST2020_CIV
  ) |>
  pivot_longer(
    cols = starts_with("POPEST"), names_to = "year", values_to = "population"
  ) |>
  transform(year = str_sub(year, 7, 10)) |>
  setNames(c("jurisdiction", "age", "year", "population")) |>
  as_tibble()

c <- data2 |>
  filter(SEX == 0) |>
  select(
    NAME, AGE, POPEST2020_CIV, POPEST2021_CIV, POPEST2022_CIV, POPEST2023_CIV
  ) |>
  pivot_longer(
    cols = starts_with("POPEST"), names_to = "year", values_to = "population"
  ) |>
  transform(year = str_sub(year, 7, 10)) |>
  setNames(c("jurisdiction", "age", "year", "population")) |>
  as_tibble()

# Compute the ratio of 2020 population between datasets b and c
ratios <- b |>
  filter(year == "2020") |>
  inner_join(
    c |> filter(year == "2020"),
    by = c("jurisdiction", "age"),
    suffix = c("_old", "_new")
  ) |>
  mutate(ratio = population_new / population_old) |>
  select(jurisdiction, age, ratio)

# Adjust populations for 2011–2019 based on the ratio
b <- b |>
  filter(year != "2020") |>
  inner_join(ratios, by = c("jurisdiction", "age")) |>
  mutate(
    population = round(
      population * (1 - (((as.numeric(year) - 2010) / 10)) * (1 - ratio))
    )
  ) |>
  select(jurisdiction, age, year, population)

# Create 5y age bands
population_grouped <- tibble(bind_rows(list(a, b, c))) |>
  mutate(
    # Create categories
    age_group = case_when(
      age == "999" ~ "all",
      age >= 0 & age <= 4 ~ "0-4",
      age >= 5 & age <= 9 ~ "5-9",
      age >= 10 & age <= 14 ~ "10-14",
      age >= 15 & age <= 19 ~ "15-19",
      age >= 20 & age <= 24 ~ "20-24",
      age >= 25 & age <= 29 ~ "25-29",
      age >= 30 & age <= 34 ~ "30-34",
      age >= 35 & age <= 39 ~ "35-39",
      age >= 40 & age <= 44 ~ "40-44",
      age >= 45 & age <= 49 ~ "45-49",
      age >= 50 & age <= 54 ~ "50-54",
      age >= 55 & age <= 59 ~ "55-59",
      age >= 60 & age <= 64 ~ "60-64",
      age >= 65 & age <= 69 ~ "65-69",
      age >= 70 & age <= 74 ~ "70-74",
      age >= 75 & age <= 79 ~ "75-79",
      age >= 80 & age <= 84 ~ "80-84",
      age >= 85 ~ "85+"
    )
  ) |>
  group_by(jurisdiction, year, age_group) |>
  summarise(population = sum(population)) |>
  mutate(year = as.integer(year)) |>
  ungroup()

population_grouped <- rbind(df_1999, population_grouped) |>
  inner_join(us_states_iso3c, by = c("jurisdiction"))



# Update totals to Vintage 2024
vintage2024_totals <- read_csv(
  paste0(
    "https://www2.census.gov/programs-surveys/popest/datasets/2020-2024/",
    "state/totals/NST-EST2024-ALLDATA.csv"
  )
) |>
  filter(SUMLEV %in% c("010", "040")) |>
  select(
    STATE, POPESTIMATE2020, POPESTIMATE2021, POPESTIMATE2022,
    POPESTIMATE2023, POPESTIMATE2024
  ) |>
  pivot_longer(
    cols = starts_with("POPEST"), names_to = "year",
    names_prefix = "POPESTIMATE", values_to = "population"
  ) |>
  setNames(c("id", "year", "population2")) |>
  mutate(
    year = as.integer(year),
    id = ifelse(id == "00", "all", id)
  )
vintage2024_totals$age_group <- "all"

scale_population <- function(df) {
  # Copy 2023 to 2024
  population_grouped_2024 <- df |> filter(year == 2023)
  population_grouped_2024$year <- 2024

  population_grouped2 <- rbind(df, population_grouped_2024) |>
    arrange(jurisdiction, age_group, year) |>
    left_join(
      vintage2024_totals,
      by = join_by(age_group, id, year)
    ) |>
    mutate(population = ifelse(is.na(population2), population, population2)) |>
    select(-population2)

  # Scale 2020-2024 age grouped populations by totals
  population_grouped2 |>
    group_by(id, year) |>
    mutate(
      total_all = sum(population[age_group == "all"], na.rm = TRUE),
      total_not_all = sum(population[age_group != "all"], na.rm = TRUE),
      scaling_factor = total_all / total_not_all,
      population = round(ifelse(
        age_group != "all", population * scaling_factor, population
      ))
    ) |>
    ungroup() |>
    select(-total_all, -total_not_all, -scaling_factor)
}

population_grouped_forecasted <- scale_population(population_grouped) |>
  nest(data = c("year", "population")) |>
  mutate(data = lapply(data, forecast_population)) |>
  unnest(cols = "data") |>
  filter(!is.na(jurisdiction)) |>
  relocate(iso3c, jurisdiction, age_group) |>
  select(-id)

save_csv(population_grouped_forecasted, "population/usa/5y")

# Create 10y age bands
population_grouped <- bind_rows(list(a, b, c)) |>
  mutate(
    # Create categories
    age_group = case_when(
      age == "999" ~ "all",
      age >= 0 & age <= 9 ~ "0-9",
      age >= 10 & age <= 19 ~ "10-19",
      age >= 20 & age <= 29 ~ "20-29",
      age >= 30 & age <= 39 ~ "30-39",
      age >= 40 & age <= 49 ~ "40-49",
      age >= 50 & age <= 59 ~ "50-59",
      age >= 60 & age <= 69 ~ "60-69",
      age >= 70 & age <= 79 ~ "70-79",
      age >= 80 ~ "80+"
    )
  ) |>
  group_by(jurisdiction, year, age_group) |>
  summarise(population = sum(population)) |>
  mutate(year = as.integer(year)) |>
  ungroup()

population_grouped <- rbind(df_1999_10y, population_grouped) |>
  inner_join(us_states_iso3c, by = c("jurisdiction"))

population_grouped_forecasted <- scale_population(population_grouped) |>
  nest(data = c("year", "population")) |>
  mutate(data = lapply(data, forecast_population)) |>
  unnest(cols = "data") |>
  filter(!is.na(jurisdiction)) |>
  relocate(iso3c, jurisdiction, age_group) |>
  select(-id)

save_csv(population_grouped_forecasted, "population/usa/10y")

# Create 20y age bands
population_grouped <- bind_rows(list(a, b, c)) |>
  mutate(
    # Create categories
    age_group = case_when(
      age == "999" ~ "all",
      age >= 0 & age <= 19 ~ "0-19",
      age >= 20 & age <= 39 ~ "20-39",
      age >= 40 & age <= 59 ~ "40-59",
      age >= 60 & age <= 79 ~ "60-79",
      age >= 80 ~ "80+"
    )
  ) |>
  group_by(jurisdiction, year, age_group) |>
  summarise(population = sum(population)) |>
  mutate(year = as.integer(year)) |>
  ungroup()

population_grouped <- rbind(df_1999_20y, population_grouped) |>
  inner_join(us_states_iso3c, by = c("jurisdiction"))

population_grouped_forecasted <- scale_population(population_grouped) |>
  nest(data = c("year", "population")) |>
  mutate(data = lapply(data, forecast_population)) |>
  unnest(cols = "data") |>
  filter(!is.na(jurisdiction)) |>
  relocate(iso3c, jurisdiction, age_group) |>
  select(-id)

save_csv(population_grouped_forecasted, "population/usa/20y")

# source("./population/usa/20y_10y_5y_bins.r")
