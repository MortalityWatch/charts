# Load the necessary library
source("./lib/common.r")

# Wastewater SARS-CoV-2
wastewater <- read.csv("./data_static/gemiddelde-virusvracht-s.csv", sep = ";") |>
  as_tibble() |>
  pivot_longer(2:6, names_to = "date", values_to = "incidence") |>
  mutate(
    year1 = left(right(date, 9), 4),
    year2 = right(right(date, 9), 4),
    incidence = incidence / 100
  ) |>
  mutate(year = ifelse(Week >= 40, year1, year2)) |>
  mutate(date = make_yearweek(as.integer(year), Week)) |>
  select(date, incidence) |>
  as_tsibble(index = date) |>
  filter(!is.na(incidence))

# ACM
deaths <- read_csv("https://s3.mortality.watch/data/mortality/NLD/weekly.csv") |>
  mutate(date = yearweek(date)) |>
  select(date, asmr_country_excess)

df <- wastewater |> inner_join(deaths)

ggplot(df, aes(x = asmr_country_excess, y = incidence)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_cor(method = "pearson", p.accuracy = 0.001, color = "red") +
  labs(
    title = paste0(
      "Correlation between Weekly Excess Mortality & ",
      "SARS-CoV-2 Wasterwater Incidence"
    ),
    subtitle = paste(
      "Source: rivm.nl, mortality.watch",
      "95% CI",
      sep = " · "
    ),
    x = "Excess ASMR",
    y = "SARS-CoV-2 Incidence per 100k"
  ) +
  theme_bw() +
  theme(legend.position = "top") +
  theme(legend.title = element_blank())

ggplot(df, aes(x = date)) +
  geom_line(aes(y = incidence), color = "blue") +
  geom_line(aes(y = asmr_country_excess), color = "red") +
  labs(
    title = paste0(
      "Weekly Excess Mortality (red) & ",
      "SARS-CoV-2 Wasterwater Incidence (blue)"
    ),
    subtitle = "Source: rivm.nl, mortality.watch",
    x = "Week of Year",
    y = "Excess ASMR / SARS-CoV-2 Incidence per 1k"
  ) +
  theme_bw() +
  theme(legend.position = "top") +
  theme(legend.title = element_blank())
