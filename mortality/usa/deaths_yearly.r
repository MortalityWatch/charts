source("lib/common.r")

df_monthly_5y <- read_remote("deaths/usa/monthly_5y.csv")
df_monthly_10y <- read_remote("deaths/usa/monthly_10y.csv")
df_monthly_20y <- read_remote("deaths/usa/monthly_20y.csv")

max_year <- max(df_monthly_5y$year)
years <- df_monthly_5y |>
  filter(
    year %in% c(max_year, max_year - 1)
  ) |>
  group_by(year) |>
  summarize(n = n()) |>
  filter(n == max(n)) |>
  arrange(desc(year))
last_complete_year <- years$year[1]

result_5y <- df_monthly_5y |>
  filter(year <= last_complete_year) |>
  group_by(iso3c, age_group, year) |>
  summarize(deaths = sum(deaths))

result_10y <- df_monthly_10y |>
  filter(year <= last_complete_year) |>
  group_by(iso3c, age_group, year) |>
  summarize(deaths = sum(deaths))

result_20y <- df_monthly_20y |>
  filter(year <= last_complete_year) |>
  group_by(iso3c, age_group, year) |>
  summarize(deaths = sum(deaths))

save_csv(result_5y, "deaths/usa/yearly_5y")
save_csv(result_10y, "deaths/usa/yearly_10y")
save_csv(result_20y, "deaths/usa/yearly_20y")

# source("mortality/usa/deaths_yearly.r")
