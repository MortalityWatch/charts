source("lib/common.r")

data <- as_tibble(read.csv("./data_static/usa_states_age_weekly.csv"))
us_states_iso3c <- as_tibble(read.csv("./data_static/usa_states_iso3c.csv"))

df <- data |>
  filter(Type == "Unweighted") |>
  select(Jurisdiction, Week.Ending.Date, Age.Group, Number.of.Deaths) |>
  setNames(c("jurisdiction", "date", "age_group", "deaths")) |>
  mutate(
    date = yearweek(mdy(date)),
    # Create categories
    age_group = case_when(
      age_group == "Under 25 years" ~ "0-24",
      age_group == "25-44 years" ~ "25-44",
      age_group == "45-64 years" ~ "45-64",
      age_group == "65-74 years" ~ "65-74",
      age_group == "75-84 years" ~ "75-84",
      age_group == "85 years and older" ~ "85+"
    )
  ) |>
  complete(jurisdiction, date, age_group)

# Combine NY/NYC
ny <- df |>
  filter(jurisdiction %in% c("New York", "New York City")) |>
  group_by(date, age_group) |>
  summarise(deaths = sum(deaths)) |>
  ungroup()
ny$jurisdiction <- "New York"
df <- rbind(df |> filter(jurisdiction != "New York"), ny)

result_1 <- df |>
  left_join(
    us_states_iso3c |> select(iso3c, jurisdiction),
    by = "jurisdiction"
  ) |>
  select(-"jurisdiction") |>
  mutate(year = year(date), week = isoweek(date)) |>
  arrange(iso3c, date, age_group)

# Jurisdictions with suppressed data pre 2018
suppressed <- result_1 |> filter(
  age_group != "all",
  date <= make_yearweek(2018, 1),
  is.na(deaths)
)
supressed_jur <- unique(suppressed$iso3c)

# Continue dataset after 10/2023 via CDC Wonder
df_weekly_5y <- read_remote_zip("deaths/usa/weekly_5y.csv")
result_2 <- df_weekly_5y |>
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
      age_group %in% c("85-89") ~ "85+",
      age_group %in% c("90-94") ~ "85+",
      age_group %in% c("95+") ~ "85+",
      .default = age_group
    )
  ) |>
  group_by(iso3c, date, age_group, year, week) |>
  summarise(deaths = sum(deaths), .groups = "drop") |>
  mutate(date = yearweek(date))

# Join Result & Save
date_2018 <- make_yearweek(year = 2018, week = 1)

result_1_all <- result_1 |>
  group_by(across(c(-age_group, -deaths))) |>
  summarise(deaths = sum(deaths, na.rm = TRUE)) |>
  ungroup()
result_1_all$age_group <- "all"

result <- rbind(
  # Weekly all ages 2017+
  result_1_all |> filter(date < date_2018),
  # Weekly, complete 2017+
  result_1 |> filter(!iso3c %in% supressed_jur, date < date_2018),
  # All from CDC Wonder weekly 2018+
  result_2 |> filter(date >= date_2018, age_group != "NS")
) |>
  filter(age_group != "NS") |>
  relocate(iso3c, date, year, week, age_group, deaths) |>
  arrange(iso3c, date, age_group) |>
  complete(iso3c, date, age_group) |>
  filter(!is.na(year), !is.na(week), !is.na(age_group))

save_csv_zip(result, "deaths/usa/deaths_weekly_25y_20y_10y")

# source("mortality/usa/deaths_weekly_25y_20y_10y.r")
