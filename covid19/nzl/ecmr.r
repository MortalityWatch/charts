source("./lib/common.r")
options(warn = 1)

df <- read.csv("./data_static/DPE404001_20240225_052806_44.csv", skip = 1) |>
  as_tibble() |>
  filter(!is.na(Total)) |>
  mutate(X = make_yearquarter(
    year = as.integer(left(X, 4)),
    quarter = as.integer(right(X, 1))
  )) |>
  setNames(c("date", "population"))

# Interpolate monthly values
df$date <- as.Date(df$date)
df_monthly <- data.frame(
  date = seq(from = min(df$date), to = max(df$date), by = "month")
) |> as_tibble()
df_monthly$population <- approx(
  x = df$date, y = df$population, xout = df_monthly$date, method = "linear"
)$y

df |>
  as_tsibble(index = date) |>
  autoplot(.vars = population) +
  labs(
    title = "Population [New Zealand]",
    subtitle = "Source: https://infoshare.stats.govt.nz",
    x = "Quarter of year",
    y = "Population"
  ) +
  scale_y_continuous(labels = comma) +
  watermark()

all_deaths <- read.csv("./data_static/VSD349801_20240217_112550_45.csv") |>
  as_tibble() |>
  setNames(c("date", "deaths")) |>
  mutate(
    date = make_yearmonth(year = left(date, 4), month = right(date, 2)),
    deaths = as.integer(deaths)
  ) |>
  filter(!is.na(date), date >= make_yearmonth(year = 2010, month = 1))

ts_plot <- df_monthly |>
  mutate(date = yearmonth(date)) |>
  inner_join(all_deaths) |>
  mutate(cmr = deaths / population * 1000000) |>
  as_tsibble(index = date)

fc <- ts_plot |>
  filter_index("2010 Jan" ~ "2019 Dec") |>
  model(TSLM(cmr ~ season() + trend())) |>
  forecast(h = 48)

fc |>
  autoplot(data = ts_plot) +
  labs(
    title = "Crude Mortality Rate [New Zealand]",
    subtitle = "Source: https://infoshare.stats.govt.nz",
    x = "Month of year",
    y = "Deaths/100k"
  ) + watermark()

data_2020_n <- ts_plot |> filter_index("2020 Jan" ~ "2023 Jun")

excess <- as_tibble(fc) |>
  select(date, .mean) |>
  inner_join(data_2020_n, by = join_by(date)) |>
  mutate(ecmr = cmr - .mean, ecmr_p = ecmr / .mean)

excess |>
  as_tsibble(index = date) |>
  autoplot(ecmr_p) +
  labs(
    title = "Excess Crude Mortality Rate (eCMR) [New Zealand]",
    subtitle = "Source: https://infoshare.stats.govt.nz",
    x = "Month of Year",
    y = "Excess Deaths/100k"
  ) + watermark() +
  scale_y_continuous(labels = percent)

cum_ecmr <- sum(excess$ecmr)
cum_bl <- sum(excess$.mean)
cum_ecmr / cum_bl
