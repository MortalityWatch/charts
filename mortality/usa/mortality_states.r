source("lib/common.r")

# Weekly 2015+
wd_usa <- read_remote("deaths/usa/deaths_weekly_25y_20y_10y.csv") |>
  mutate(date = date_parse(paste(year, week, 1), format = "%G %V %u")) |>
  filter(!is.na(deaths))

md_usa_10y <- read_remote_zip("deaths/usa/monthly_10y.csv") |>
  mutate(date = date_parse(paste(year, month, 1), format = "%Y %m %d")) |>
  aggregate_80_plus()

# CMR, Weekly
dd_us1 <- wd_usa |>
  filter(age_group == "all") |>
  select(iso3c, date, deaths)
dd_us1$age_group <- "all"
dd_us1$type <- 3

# CMR, Monthly
dd_us2 <- md_usa_10y |>
  filter(age_group == "all") |>
  select(iso3c, date, deaths)
dd_us2$age_group <- "all"
dd_us2$type <- 2

dd_us <- rbind(dd_us1, dd_us2) |>
  arrange(iso3c, date, age_group, desc(type)) |>
  distinct(iso3c, date, .keep_all = TRUE)
dd_us$n_age_groups <- 1

rm(dd_us1, dd_us2)

# ASMR
## Weekly
deaths_weekly <- wd_usa |> filter(age_group != "all", !is.na(date))
deaths_weekly$type <- 3
deaths_weekly$n_age_groups <- 6

## Monthly
deaths_monthly <- md_usa_10y |> filter(age_group != "all", !is.na(date))
deaths_monthly$type <- 2
deaths_monthly$n_age_groups <- 9

rm(md_usa_10y)

cols <- c("iso3c", "date", "age_group", "type", "n_age_groups", "deaths")
dd_us_age <- rbind(
  deaths_weekly |> select(all_of(cols)),
  deaths_monthly |> select(all_of(cols))
) |>
  arrange(iso3c, date, age_group, type, age_group)

rm(deaths_weekly, deaths_monthly)

# Population
population <- read_remote("population/usa/10y.csv") |>
  mutate(date = date(sprintf("%d-07-01", year)), .after = iso3c) |>
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

population2 <- read_remote("population/usa/5y.csv") |>
  mutate(
    age_group = case_when(
      age_group %in% c("0-4") ~ "0-24",
      age_group %in% c("5-9") ~ "0-24",
      age_group %in% c("10-14") ~ "0-24",
      age_group %in% c("15-19") ~ "0-24",
      age_group %in% c("20-24") ~ "0-24",
      age_group %in% c("25-29") ~ "25-44",
      age_group %in% c("30-34") ~ "25-44",
      age_group %in% c("35-39") ~ "25-44",
      age_group %in% c("40-44") ~ "25-44",
      age_group %in% c("45-49") ~ "45-64",
      age_group %in% c("50-54") ~ "45-64",
      age_group %in% c("55-59") ~ "45-64",
      age_group %in% c("60-64") ~ "45-64",
      age_group %in% c("65-69") ~ "65-74",
      age_group %in% c("70-74") ~ "65-74",
      age_group %in% c("75-79") ~ "75-84",
      age_group %in% c("80-84") ~ "75-84",
      age_group %in% c("85+") ~ "85+"
    )
  ) |>
  group_by(iso3c, year, age_group) |>
  summarise(population = sum(population)) |>
  ungroup() |>
  filter(!is.na(population)) |>
  mutate(date = date(sprintf("%d-01-01", year)), .after = iso3c) |>
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

usa_mortality_states <- rbind(dd_us, dd_us_age) |>
  left_join(
    rbind(population, population2),
    by = c("iso3c", "date", "age_group")
  ) |>
  select(all_of(c(
    "iso3c", "date", "age_group", "deaths", "population", "type", "n_age_groups"
  ))) |>
  arrange(iso3c, date, age_group) |>
  distinct(iso3c, date, age_group, type, .keep_all = TRUE) |>
  filter(age_group != "NS", !is.na(population))

usa_mortality_states$source <- "cdc"

rm(dd_us, dd_us_age, population, population2)

# source("mortality/usa/mortality_states.r")
