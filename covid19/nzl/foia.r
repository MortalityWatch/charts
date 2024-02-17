source("./lib/common.r")
options(warn = 1)

# DL FOIA data
url <- paste0(
  "https://fyi.org.nz/request/25021/response/96520/attach/5/",
  "/Data%20Attachment%201.xlsx"
)
file <- "./data_static/nzl_deaths_vaccination_status.xlsx"
retry_download(url, file)
data <- read_excel(file, sheet = "Data", range = "A1:E99999") |> as_tibble()

# Tidy up data.
order <- c("0-20", "21-40", "41-60", "61-80", "81-99", "100+")
df <- data |>
  setNames(c("date", "age_group", "doses", "days_since_last", "deaths")) |>
  mutate(
    date = make_yearmonth(year = left(date, 4), month = right(date, 2)),
    age_group = case_when(
      age_group == "0 to 20" ~ "0-20",
      age_group == "21 to 40" ~ "21-40",
      age_group == "41 to 60" ~ "41-60",
      age_group == "61 to 80" ~ "61-80",
      age_group == "81 to 100" ~ "81-99",
      age_group == "100+" ~ "100+",
      .default = NA
    ),
    doses = as.integer(doses),
    deaths = as.integer(deaths),
    vaccinated = doses > 0
  ) |>
  filter(!is.na(date), !is.na(age_group))

# Plot
df2 <- df |>
  group_by(date, vaccinated) |>
  summarize(deaths = sum(deaths, na.rm = TRUE)) |>
  mutate(vaccinated = ifelse(vaccinated, "vaccinated", "unvaccinated"))

chart <- df2 |>
  filter(date <= make_yearmonth(year = 2023, month = 11)) |>
  ggplot(aes(x = date, y = deaths)) +
  geom_area(position = position_stack(reverse = TRUE), aes(fill = vaccinated)) +
  labs(
    title = paste0(
      "All-Cause Deaths by COVID-19 Vaccination Status [New Zealand]"
    ),
    subtitle = "Source: FYI.org.nz; infoshare.stats.govt.nz",
    x = "Month of Year",
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
save_chart(chart, "nzl/all-cause-vaxx-status-1", upload = FALSE)

# Plot
df2 <- df |>
  group_by(date, vaccinated) |>
  summarize(deaths = sum(deaths, na.rm = TRUE)) |>
  mutate(vaccinated = ifelse(vaccinated, "vaccinated", "unvaccinated"))

owid <- as_tibble(read.csv("./data/owid.csv")) |>
  select(iso_code, date, people_vaccinated_per_hundred) |>
  setNames(c("iso3c", "date", "dose_pct")) |>
  filter(iso3c == "NZL") |>
  mutate(
    dose_pct = dose_pct / 100,
    date = as.Date(date)
  ) |>
  filter(!is.na(dose_pct)) |>
  mutate(date = yearmonth(date)) |>
  group_by(date) |>
  summarize(dose_pct = mean(dose_pct))

a <- df2 |> inner_join(owid, by = c("date"))
a$population <- 5123000
mr <- a |>
  mutate(population = ifelse(
    vaccinated == "unvaccinated",
    (1 - dose_pct) * population,
    dose_pct * population
  )) |>
  mutate(cmr = deaths / population * 100000)

chart <-
  mr |>
  filter(date <= make_yearmonth(year = 2023, month = 11)) |>
  ggplot(aes(x = date, y = cmr)) +
  geom_line(aes(color = vaccinated)) +
  scale_color_manual(values = c("#44781d", "#de5075")) +
  labs(
    title = paste0(
      "All-Cause Mortality by COVID-19 Vaccination Status [New Zealand]"
    ),
    subtitle = "Source: FYI.org.nz; OWID",
    x = "Month of Year",
    y = "Deaths/100k population",
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
save_chart(chart, "nzl/all-cause-mr-vaxx-status-1", upload = FALSE)

# Stacked Chart
df2 <- df |>
  group_by(date, vaccinated, age_group) |>
  summarize(deaths = sum(deaths, na.rm = TRUE)) |>
  mutate(vaccinated = ifelse(vaccinated, "vaccinated", "unvaccinated"))

df2$age_group <- factor(df2$age_group, levels = order)
chart <- df2 |>
  filter(date <= make_yearmonth(year = 2023, month = 11)) |>
  ggplot(aes(x = date, y = deaths)) +
  geom_area(position = position_stack(reverse = TRUE), aes(fill = vaccinated)) +
  labs(
    title = paste0(
      "All-Cause Deaths by COVID-19 Vaccination Status & ",
      "Age Group [New Zealand]"
    ),
    subtitle = "Source: FYI.org.nz",
    x = "Month of Year",
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
    by = "12 months"
  )) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank()) +
  watermark() +
  facet_wrap(~age_group, scales = "free")
save_chart(chart, "nzl/all-cause-vaxx-status-2", upload = FALSE)

# All Deaths
df2 <- df |>
  group_by(date, vaccinated) |>
  summarize(deaths = sum(deaths, na.rm = TRUE)) |>
  mutate(vaccinated = ifelse(vaccinated, "vaccinated", "unvaccinated"))

all_deaths <- read.csv("./data_static/VSD349801_20240217_112550_45.csv") |>
  as_tibble() |>
  setNames(c("date", "deaths")) |>
  mutate(
    date = make_yearmonth(year = left(date, 4), month = right(date, 2)),
    deaths = as.integer(deaths)
  ) |>
  filter(!is.na(date), date >= make_yearmonth(year = 2019, month = 1))

chart <- df2 |>
  filter(date <= make_yearmonth(year = 2023, month = 11)) |>
  ggplot(aes(x = date, y = deaths)) +
  geom_area(position = position_stack(reverse = TRUE), aes(fill = vaccinated)) +
  geom_line(data = all_deaths, aes(x = date, y = deaths), color = "black") +
  labs(
    title = paste0(
      "Deaths by COVID-19 Vaccination Status vs All Deaths [New Zealand]"
    ),
    subtitle = "Source: FYI.org.nz; infoshare.stats.govt.nz",
    x = "Month of Year",
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

save_chart(chart, "nzl/all-cause-vaxx-status-3", upload = FALSE)
