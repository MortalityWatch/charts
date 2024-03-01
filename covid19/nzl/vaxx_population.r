source("./lib/common.r")
options(warn = 1)

file_list <- list.files(
  path = "./covid19/nzl/nzvax",
  pattern = "\\.csv$", full.names = TRUE
)

data <- tibble()
for (file in file_list) {
  date <- str_extract(file, "\\d{4}-\\d{2}-\\d{2}")
  content <- read_csv(file, col_types = cols(.default = "c"))
  content$date <- date
  data <- bind_rows(data, content)
}

df <- data |>
  select(date, `Age group`, `First dose administered`) |>
  setNames(c("date", "age_group", "vaccinated")) |>
  mutate(
    date = as.Date(date),
    vaccinated = as.integer(str_replace_all(vaccinated, ",", ""))
  ) |>
  group_by(date, age_group) |>
  filter(!is.na(vaccinated), age_group != "Various") |>
  summarize(vaccinated = sum(vaccinated, na.rm = TRUE)) |>
  filter(date > as.Date("2021-08-31"))

df |>
  ggplot(aes(
    x = date, y = vaccinated,
    fill = age_group
  )) +
  geom_area() +
  labs(
    title = paste0(
      "COVID-19 Vaccinations by Age Group [New Zealand]"
    ),
    subtitle = "Source: github.com/minhealthnz/nz-covid-data",
    x = "Week of Year",
    y = "Vaccinations"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(
    angle = 30, hjust = 0.5, vjust = 0.5
  )) +
  scale_x_yearweek() +
  scale_y_continuous(labels = comma) +
  # theme(legend.position = "top") +
  theme(legend.title = element_blank()) +
  watermark()

df <- df |> filter(date == as.Date("2021-09-07"))

# Disaggregate the value column by age_group for single years
disaggregated_df <- df %>%
  mutate(age_group = ifelse(age_group == "90+", "90-100", age_group)) |>
  separate(age_group, into = c("start_age", "end_age"), sep = "-") %>%
  mutate(across(c(start_age, end_age), as.integer)) %>%
  rowwise() %>%
  mutate(age = list(start_age:end_age)) %>%
  unnest(age) %>%
  mutate(value = vaccinated / (end_age - start_age + 1)) %>%
  select(age, value) |>
  ungroup() |>
  # Use small number for tail
  add_row(age = 101:120, value = min(df$vaccinated) / 200)

loess_fit <- loess(value ~ age, data = disaggregated_df, span = .25)

loess_predicted_values <- tibble(
  age = disaggregated_df$age,
  loess_fit = predict(loess_fit, na.action = na.exclude)
) |>
  filter(age <= 110) |>
  mutate(loess_fit = ifelse(loess_fit > 0, loess_fit, 0))

# Make sure totals stay stable.
ratio <- sum(loess_predicted_values$loess_fit) / sum(disaggregated_df$value)
adjusted <- loess_predicted_values |> mutate(loess_fit = loess_fit / ratio)

adjusted |>
  ggplot(aes(x = age, y = loess_fit)) +
  geom_point(data = disaggregated_df, aes(x = age, y = value)) +
  geom_line(color = "red")
