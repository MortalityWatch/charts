source("./lib/common.r")
options(warn = 1)

# Monthly totals
all_deaths <- read.csv("./data_static/VSD349801_20240217_112550_45.csv") |>
  as_tibble() |>
  setNames(c("date", "deaths")) |>
  mutate(
    date = make_yearmonth(year = left(date, 4), month = right(date, 2)),
    deaths = as.integer(deaths)
  ) |>
  filter(!is.na(date), date >= make_yearmonth(year = 2010, month = 1))

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
    vaccinated = doses > 0,
    type = case_when(
      !is.na(deaths) & doses == 0 ~ "unvaccinated",
      is.na(deaths) & doses == 0 ~ "unvaccinated_suppressed",
      !is.na(deaths) & doses > 0 ~ "vaccinated",
      is.na(deaths) & doses > 0 ~ "vaccinated_suppressed"
    )
  ) |>
  mutate(date = make_yearmonth(year = left(date, 4), month = right(date, 2))) |>
  filter(age_group == "0-20")

# Replace with upper limit
df$deaths[is.na(df$deaths)] <- 4

df2 <- df |>
  group_by(date, vaccinated, age_group) |>
  summarize(deaths = sum(deaths, na.rm = TRUE)) |>
  mutate(vaccinated = ifelse(vaccinated, "vaccinated", "unvaccinated"))

df2$age_group <- factor(df2$age_group, levels = order)
chart <- df2 |>
  filter(date <= make_yearmonth(year = 2023, month = 11)) |>
  ggplot(aes(x = date)) +
  geom_area(
    position = position_stack(reverse = TRUE),
    aes(y = deaths, fill = vaccinated)
  ) +
  labs(
    title = paste0(
      "UPPER LIMIT / All-Cause Deaths by COVID-19 Vaccination Status & ",
      "Age Group [New Zealand]"
    ),
    subtitle = "Suppressed (1-4) set to 4; Source: FYI.org.nz",
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

save_chart(chart, "nzl/all-cause-vaxx-status-0-20-upper-limit", upload = FALSE)
