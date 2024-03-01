source("./lib/common.r")
options(warn = 1)

df <- read.csv("https://pastebin.com/raw/RvG4gyve") |>
  as_tibble() |>
  mutate(date = make_yearweek(year, week)) |>
  filter(vaccinated_definition == "immediately after first dose") |>
  select(date, vaccinated_count, unvaccinated_count) |>
  group_by(date) |>
  summarize(
    vaccinated_count = sum(vaccinated_count),
    unvaccinated_count = sum(unvaccinated_count),
    count = vaccinated_count + unvaccinated_count
  )

all_deaths <-
  read.csv("https://s3.mortality.watch/data/mortality/NLD/weekly.csv") |>
  as_tibble() |>
  mutate(date = yearweek(date)) |>
  select(date, deaths, population)

# Plot
df2 <- df |>
  pivot_longer(
    cols = 2:3,
    names_to = "type",
    values_to = "deaths"
  ) |>
  select(-count)
df2$type <- sub("_count", "", df2$type)

fit <- all_deaths |>
  as_tsibble(index = date) |>
  filter_index("2010 W01" ~ "2019 W52") |>
  model(TSLM(deaths ~ season() + trend()))
fc <- forecast(fit, h = 3 * 52 + 1)

chart <- df2 |>
  ggplot(aes(x = date, y = deaths)) +
  geom_area(position = position_stack(reverse = TRUE), aes(fill = type)) +
  geom_line(
    data = fc |> filter(date >= make_yearweek(2021, 1)),
    aes(y = .mean, color = "Trend (2010-2019)"),
    linetype = "dashed", size = 0.5
  ) +
  scale_color_manual(values = "black") +
  labs(
    title = paste0(
      "All-Cause Deaths by COVID-19 Vaccination Status [Netherlands]"
    ),
    subtitle = "Source: cbs.nl, mortality.watch",
    x = "Week of Year",
    y = "Deaths",
    fill = "COVID-19 vaccinated"
  ) +
  scale_fill_manual(values = c("#44781d", "#de5075")) +
  theme_bw() +
  theme(axis.text.x = element_text(
    angle = 30, hjust = 0.5, vjust = 0.5
  )) +
  scale_y_continuous(labels = comma) +
  scale_x_yearmonth(breaks = seq(as.Date("2019-01-01"),
    as.Date("2023-12-01"),
    by = "6 months"
  )) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank()) +
  watermark()
save_chart(chart, "nld/all-cause-vaxx-status", upload = FALSE)

# Vaccinated people
data <- read.csv("./data/owid.csv") |> as_tibble()

owid <- data |>
  filter(iso_code == "NLD") |>
  select(date, people_vaccinated) |>
  mutate(date = date(date)) |>
  mutate(
    date = yearweek(date),
    people_vaccinated = as.numeric(people_vaccinated)
  ) |>
  group_by(date) |>
  summarize(people_vaccinated = sum(people_vaccinated, na.rm = TRUE)) |>
  filter(people_vaccinated > 0)

pop <- owid |>
  inner_join(all_deaths) |>
  mutate(
    population_vaccinated = people_vaccinated,
    population_unvaccinated = population - people_vaccinated,
  ) |>
  select(-population, -people_vaccinated, -deaths) |>
  pivot_longer(
    cols = 2:3,
    names_to = "type",
    names_prefix = "population_",
    values_to = "population"
  )

df3 <- df2 |>
  inner_join(pop, by = join_by(date, type)) |>
  mutate(cmr = deaths / population * 100000)

chart <- df3 |>
  ggplot(aes(x = date, y = cmr, color = type)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c("#44781d", "#de5075")) +
  labs(
    title = paste0(
      "All-Cause Mortality by COVID-19 Vaccination Status [Netherlands]"
    ),
    subtitle = "Source: cbs.nl, mortality.watch",
    x = "Week of Year",
    y = "Deaths/100k population",
    fill = "COVID-19 vaccinated"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(
    angle = 30, hjust = 0.5, vjust = 0.5
  )) +
  scale_y_continuous(labels = comma) +
  scale_x_yearmonth(breaks = seq(as.Date("2019-01-01"),
    as.Date("2023-12-01"),
    by = "6 months"
  )) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank()) +
  watermark()

save_chart(chart, "nld/all-cause-mortality-vaxx-status", upload = FALSE)

# Also show vaccination rate
vaccinted_pct <- owid |>
  inner_join(all_deaths) |>
  mutate(population_vaccinated_pct = people_vaccinated / population) |>
  select(date, population_vaccinated_pct)

scale_factor <- max(df3$cmr)

chart <- df3 |>
  ggplot(aes(x = date, y = cmr)) +
  geom_line(aes(color = type), size = 1.5) +
  geom_line(
    data = vaccinted_pct |> filter(date <= max(df3$date)),
    aes(y = population_vaccinated_pct * scale_factor)
  ) +
  scale_color_manual(values = c("#44781d", "#de5075")) +
  labs(
    title = paste0(
      "All-Cause Mortality by COVID-19 Vaccination Status [Netherlands]"
    ),
    subtitle = "Source: cbs.nl, mortality.watch",
    x = "Week of Year",
    y = "Deaths/100k population",
    fill = "COVID-19 vaccinated"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(
    angle = 30, hjust = 0.5, vjust = 0.5
  )) +
  scale_y_continuous(
    labels = comma,
    name = "Deaths/100k population",
    sec.axis = sec_axis(~ . / scale_factor,
      name = "Population vaccinated",
      breaks = scales::pretty_breaks(n = 10),
      labels = scales::percent_format()
    )
  ) +
  scale_x_yearmonth(breaks = seq(as.Date("2019-01-01"),
    as.Date("2023-12-01"),
    by = "6 months"
  )) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank()) +
  watermark()

save_chart(chart, "nld/all-cause-mortality-vaxx-status-rate", upload = FALSE)

# Match levels frmo July 2021
ref <- df3 |> filter(
  date >= make_yearweek(year = 2021, week = 26),
  date <= make_yearweek(year = 2021, week = 26 + 13)
)
ratio <- sum(ref$cmr[ref$type == "vaccinated"]) /
  sum(ref$cmr[ref$type == "unvaccinated"])

chart <- df3 |>
  filter(date >= make_yearweek(year = 2021, week = 26)) |>
  mutate(cmr = ifelse(type == "vaccinated", cmr / ratio, cmr)) |>
  ggplot(aes(x = date, y = cmr, color = type)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c("#44781d", "#de5075")) +
  labs(
    title = paste0(
      "All-Cause Mortality by COVID-19 Vaccination Status [Netherlands]"
    ),
    subtitle = paste0(
      "Adjusted to match levels 2021 W26 - 2021 W39 (very low C19 prevalence) ",
      " · ",
      "Source: cbs.nl; mortality.watch"
    ),
    x = "Week of Year",
    y = "Deaths/100k population",
    fill = "COVID-19 vaccinated"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(
    angle = 30, hjust = 0.5, vjust = 0.5
  )) +
  scale_y_continuous(labels = comma) +
  scale_x_yearmonth(breaks = seq(as.Date("2019-01-01"),
    as.Date("2023-12-01"),
    by = "6 months"
  )) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank()) +
  watermark()

save_chart(chart, "nld/all-cause-mortality-vaxx-status-adj", upload = FALSE)
