source("lib/common.r")
options(warn = 1)

# National
df1 <- read_excel(
  "./data_static/sonderauswertung-sterbefaelle-endgueltige-daten.xlsx",
  sheet = "csv-12613-02",
  range = "D1:G99999"
)
df2 <- read_excel(
  "./data/sonderauswertung-sterbefaelle.xlsx",
  sheet = "csv-12613-02",
  range = "D1:G99999"
)

result1 <- rbind(df1, df2) |>
  setNames(c("year", "age_group", "week", "deaths")) |>
  mutate(jurisdiction = "Deutschland") |>
  select(jurisdiction, year, week, age_group, deaths)

# States
df1 <- read_excel(
  "./data_static/sonderauswertung-sterbefaelle-endgueltige-daten.xlsx",
  sheet = "csv-12613-09",
  range = "B1:G99999"
)
df2 <- read_excel(
  "./data/sonderauswertung-sterbefaelle.xlsx",
  sheet = "csv-12613-09",
  range = "B1:G99999"
)
result2 <- rbind(df1, df2) |>
  setNames(c("jurisdiction", "sex", "year", "age_group", "week", "deaths")) |>
  select(jurisdiction, year, week, age_group, deaths)

df <- rbind(result1, result2) |>
  mutate(
    year = as.integer(year),
    week = as.integer(week),
    deaths = as.integer(deaths)
  ) |>
  filter(!is.na(deaths)) |>
  arrange(year, jurisdiction, age_group, week) |>
  distinct(jurisdiction, year, week, age_group, .keep_all = TRUE)

date <- now() %m-% weeks(2)
y <- year(date)
w <- week(date)

len <- nrow(df |> filter(
  jurisdiction == "Deutschland",
  year == y,
  week == w
))

save_csv(df, paste0("deaths/deu/Tote_", y, "_", sprintf("%02d", w)))
save_csv(df, paste0("deaths/deu/deaths"))

# source("./mortality/deu/deaths.r")
