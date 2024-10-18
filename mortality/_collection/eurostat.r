source("lib/common.r")
source("population/std_pop.r")

.data <- dplyr::.data

# Functions
custom_match <- c(EL = "GRC", UK = "GBR")

get_population_by_age_group <- function(df, age_groups) {
  result <- NULL
  for (ag in age_groups) {
    if (grepl("^[0-9]+$", ag)) {
      result <- rbind(result, data.frame(
        age_group = as.integer(ag),
        population = (
          df |> filter(.data$age_group == as.integer(ag))
        )$population
      ))
    } else if (grepl("\\-", ag)) {
      parts <- split_age_group(ag)
      df2 <- df |> filter(.data$age_group %in% parts[1]:parts[2])
      result <- rbind(
        result,
        data.frame(age_group = ag, population = sum(df2$population))
      )
    } else if (grepl("\\+", ag)) {
      start <- as.integer(substr(ag, 0, nchar(ag) - 1))
      df2 <- df |> filter(.data$age_group %in% start:99)
      result <- rbind(
        result,
        data.frame(age_group = ag, population = sum(df2$population))
      )
    }
  }
  result
}

# Deaths
deaths_raw <- as_tibble(read.csv(
  gzfile("./data/eurostat_weekly.tsv.gz"),
  sep = "\t"
))

deaths <- deaths_raw |>
  pivot_longer(
    cols = 2:ncol(deaths_raw),
    names_to = "date",
    values_to = "deaths"
  ) |>
  mutate(
    year = as.integer(right(left(date, 5), 4)),
    week = as.integer(right(date, 2))
  ) |>
  mutate(
    date = date_parse(paste(year, week, 1), format = "%G %V %u"),
    deaths = suppress_warnings(
      as.integer(str_replace_all(deaths, c(" p" = "", ": " = ""))),
      "NAs introduced by coercion"
    )
  ) |>
  filter(!is.na(deaths)) |>
  separate_wider_delim(
    freq.age.sex.unit.geo.TIME_PERIOD,
    delim = ",",
    names = c("freq", "age_group", "sex", "unit", "iso3c")
  ) |>
  filter(sex == "T", age_group != "Y_GE80") |>
  mutate(
    age_group = case_when(
      age_group == "TOTAL" ~ "all",
      age_group == "UNK" ~ "NS",
      age_group == "Y80-89" ~ "80+",
      age_group == "Y_GE80" ~ "80+",
      age_group == "Y_GE90" ~ "80+",
      age_group == "Y_LT10" ~ "0-9",
      .default = suppress_warnings(
        str_replace_all(age_group, c("Y" = "")),
        "NAs introduced by coercion"
      )
    )
  ) |>
  select(iso3c, age_group, date, deaths) |>
  group_by(iso3c, age_group, date) |>
  summarize(deaths = sum(deaths)) |>
  ungroup()
rm(deaths_raw)

# Population
population_raw <- as_tibble(read.csv(
  gzfile("./data/eurostat_population.tsv.gz"),
  sep = "\t"
))

population <- population_raw |>
  pivot_longer(
    cols = 2:ncol(population_raw),
    names_to = "date",
    values_to = "population"
  ) |>
  mutate(date = as.integer(right(date, 4))) |>
  mutate(
    population = suppress_warnings(
      as.integer(str_replace_all(population, c(": " = ""))),
      "NAs introduced by coercion"
    )
  ) |>
  filter(!is.na(population)) |>
  separate_wider_delim(
    freq.unit.sex.age.geo.TIME_PERIOD,
    delim = ",",
    names = c("frequencey", "unit", "sex", "age_group", "iso3c")
  ) |>
  filter(sex == "T") |>
  mutate(
    age_group = case_when(
      age_group == "TOTAL" ~ "all",
      age_group == "UNK" ~ "NS",
      age_group %in% c("Y_LT1", "Y_OPEN") ~ NA,
      .default = suppress_warnings(
        str_replace_all(age_group, c("Y" = "")),
        "NAs introduced by coercion"
      )
    )
  ) |>
  select(iso3c, age_group, date, population)
rm(population_raw)

# Summarize age groups.
population <- population |>
  filter(!age_group %in% c("NS", "_GE75", "_GE85", "80-84")) |>
  mutate(age_group = case_when(
    age_group == "_LT5" ~ "0-4",
    age_group == "_GE80" ~ "80+",
    .default = age_group
  )) |>
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
      .default = age_group
    )
  ) |>
  group_by(iso3c, date, age_group) |>
  summarise(population = sum(population), .groups = "drop")

# Interpolate to latest year, just filling from last avail. year.
# Step 1: Get the latest year from the deaths dataset
max_year <- max(year(deaths$date)) + 1
# Step 2: Get the max year from population data
pop_max_year <- max(population$date)
# Step 3: Add missing years (if any)
if (pop_max_year < max_year) {
  population <- rbind(population, tibble(
    iso3c = unique(population$iso3c)[1], # Use the first iso3c as a placeholder
    date = (pop_max_year + 1):max_year, # Generate the range of missing years
    age_group = "all", # Set a default age group
    population = NA # Placeholder for population
  ))
}

# Step 4: Complete, fill, and filter the population data
population_projected <- population |>
  complete(iso3c, date, age_group) |>
  arrange(iso3c, age_group, date) |>
  group_by(iso3c, age_group) |>
  fill(population, .direction = "down") |>
  filter(!is.na(population))
rm(population)

# Interpolate sub yearly population.
population_daily <- population_projected |>
  mutate(date = date(sprintf("%d-01-01", date))) |>
  nest(data = !c("iso3c")) |>
  mutate(
    data = map(data, ~ . |>
      group_by(age_group) |>
      nest() |>
      mutate(data = lapply(data, interpolate_population)) |>
      unnest(cols = "data"))
  ) |>
  unnest(cols = "data")
rm(population_projected)

eurostat <- deaths |>
  inner_join(population_daily, by = c("iso3c", "age_group", "date")) |>
  filter(nchar(iso3c) == 2) |>
  mutate(iso3c = countrycode(
    iso3c,
    origin = "iso2c",
    destination = "iso3c",
    custom_match = custom_match
  )) |>
  arrange(iso3c, date, age_group) |>
  distinct(iso3c, date, age_group, .keep_all = TRUE)

eurostat$type <- 3
eurostat$n_age_groups <- 9
eurostat$source <- "eurostat"
rm(deaths, population_daily)

# source("mortality/_collection/eurostat.r")
