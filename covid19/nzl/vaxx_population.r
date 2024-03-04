source("./lib/common.r")
options(warn = 1)

plot <- function(df, st = c()) {
  df |> ggplot(aes(x = date, y = population_vaccinated, fill = age_group)) +
    geom_area(position = position_stack(reverse = TRUE)) +
    labs(
      title = paste0(
        "COVID-19 Vaccinations by Age Group [New Zealand]"
      ),
      subtitle = paste(c(st, "Source: github.com/minhealthnz/nz-covid-data"),
        collapse = " · "
      ),
      x = "Week of Year",
      y = "Vaccinations"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(
      angle = 30, hjust = 0.5, vjust = 0.5
    )) +
    scale_x_yearweek() +
    scale_y_continuous(labels = comma) +
    theme(legend.position = "top") +
    theme(legend.title = element_blank()) +
    watermark()
}

# Vaxx data from Wayback machine
df1 <- read_csv(paste0(
  "https://drive.usercontent.google.com/download?id=",
  "1J6TQAFAGhxPaDAzGDHNe2Qnugv3-1Kfk&export=download&authuser=0"
), col_names = FALSE, col_types = "cDcii") |>
  setNames(c(
    "source", "date", "age_group", "population_vaccinated",
    "population_vaccinated_2nd"
  )) |>
  filter(age_group != "Total", date <= as.Date("2021-08-18")) |>
  mutate(
    age_group = str_replace_all(age_group, " ", ""),
    age_group = str_replace_all(age_group, "years", ""),
    age_group = str_replace_all(age_group, "/Unknown", "")
  ) |>
  select(date, age_group, population_vaccinated)

# Vaxx data from MOH Github
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

df2 <- data |>
  select(
    date,
    `Age group`,
    `First dose administered`,
    `At least partially vaccinated`
  ) |>
  setNames(c("date", "age_group", "vaccinated", "vaccinated2")) |>
  mutate(
    date = as.Date(date),
    vaccinated = ifelse(is.na(vaccinated), vaccinated2, vaccinated),
    vaccinated = as.integer(str_replace_all(vaccinated, ",", ""))
  ) |>
  group_by(date, age_group) |>
  filter(!is.na(vaccinated), age_group != "Various") |>
  summarize(vaccinated = sum(vaccinated, na.rm = TRUE)) |>
  rename(population_vaccinated = vaccinated) |>
  filter(date > as.Date("2021-08-31"))

df <- rbind(df1, df2) |> arrange(date, age_group)

save_chart(plot(df), "nzl/vaccinated_by_age_original", upload = FALSE)

fit_loess <- function(x, span) {
  loess_fit <- loess(value ~ age, data = x, span = span)

  loess_predicted_values <- tibble(
    date = x$date[1],
    age = x$age,
    loess_fit = predict(loess_fit, na.action = na.exclude)
  )

  # Make sure totals stay stable.
  # ratio <- sum(loess_predicted_values$loess_fit) / sum(x$value)
  # result <- loess_predicted_values |> mutate(value = loess_fit / ratio)
  x$value2 <- loess_predicted_values$loess_fit
  x
}

# x <- df |> filter(date == as.Date("2022-05-11"))
get_disaggregated_vaccinated <- function(x) {
  # Disaggregate the value column by age_group for single years
  disaggregated_df <- x |>
    mutate(age_group = ifelse(age_group == "80+", "80-100", age_group)) |>
    mutate(age_group = ifelse(age_group == "90+", "90-100", age_group)) |>
    separate(age_group, into = c("start_age", "end_age"), sep = "-") |>
    mutate(across(c(start_age, end_age), as.integer)) |>
    rowwise() |>
    mutate(age = list(start_age:end_age)) |>
    unnest(age) |>
    mutate(value = population_vaccinated / (end_age - start_age + 1)) |>
    ungroup()
  age_groups <- length(unique(x$age_group))
  ages <- max(disaggregated_df$end_age) - min(disaggregated_df$start_age)
  span <- 0.2

  result <- disaggregated_df
  for (i in 1:100) {
    result <- fit_loess(x = result, span) |>
      group_by(start_age) |>
      mutate(value = value2 * sum(value) / sum(value2)) |>
      select(-value2)
  }
  result

  # Sample Chart
  # disaggregated_df |>
  #   ggplot(aes(x = age, y = value)) +
  #   geom_point() +
  #   geom_point(data = result, aes(x = age, y = value), color = "red")
}

reaggregated_df <- df |>
  group_by(date) |>
  group_map(~ {
    print(unique(.x$date))
    get_disaggregated_vaccinated(.x)
  }, .keep = TRUE) |>
  bind_rows() |>
  mutate(
    age_group = case_when(
      age %in% 0:20 ~ "0-20",
      age %in% 21:40 ~ "21-40",
      age %in% 41:60 ~ "41-60",
      age %in% 61:80 ~ "61-80",
      age %in% 81:120 ~ "81+",
      .default = NA
    )
  ) |>
  group_by(date, age_group) |>
  summarize(population_vaccinated = round(sum(value, na.rm = TRUE)))

order <- c("0-20", "21-40", "41-60", "61-80", "81+")
reaggregated_df$age_group <- factor(reaggregated_df$age_group, levels = order)

save_chart(
  plot(reaggregated_df, "Re-binned age groups"),
  "nzl/vaccinated_by_age_rebinned",
  upload = FALSE
)

# Ensure that values are not decreasing by using later mins, if necessary
ensure_increasing <- function(a) {
  last_row <- nrow(a)
  for (i in 1:(last_row - 1)) {
    current_val <- a[i, "population_vaccinated"]$population_vaccinated
    next_min <- min(a[(i + 1):last_row, "population_vaccinated"])
    if (current_val > next_min) {
      a[i, "population_vaccinated"] <- next_min
    }
  }
  return(a)
}

reaggregated_df2 <- reaggregated_df |>
  group_by(age_group) |>
  group_map(~ ensure_increasing(.x), .keep = TRUE) |>
  bind_rows() |>
  arrange(date, age_group)
reaggregated_df2$age_group <- factor(reaggregated_df2$age_group, levels = order)

get_data_by_time_resolution <- function(fun, week_offset = 0) {
  x <- reaggregated_df2
  x$date <- x$date + week_offset * 7
  x |>
    arrange(age_group, date) |>
    group_by(age_group) |>
    group_modify(~ {
      .x |>
        as_tsibble(index = date) |>
        fill_gaps() |>
        mutate(population_vaccinated = round(na.approx(
          population_vaccinated,
          na.rm = FALSE
        ))) |>
        as_tibble()
    }) |>
    mutate(date = fun(date)) |>
    group_by(date, age_group) |>
    summarize(population_vaccinated = as.integer(
      mean(population_vaccinated, na.rm = TRUE)
    ), .groups = "drop")
}

# The underlying data is given at semi-random dates, let's align them to months
weekly_pop_vaxx <- get_data_by_time_resolution(yearweek)
save_chart(
  plot(weekly_pop_vaxx, c("Re-binned age groups", "Adjusted for consitency")),
  "nzl/vaccinated_by_age_rebinned_adjusted",
  upload = FALSE
)

# Total population by age groups
# Table: Estimated Resident Population by Age and Sex (1991+) (Qrtly)
pop <- read_csv("./data_static/DPE403901_20240303_071022_7.csv", skip = 3)
quarterly_pop <- pop |>
  pivot_longer(
    cols = 2:ncol(pop),
    names_to = "age",
    values_to = "population"
  ) |>
  filter(!is.na(population)) |>
  setNames(c("date", "age", "population")) |>
  mutate(
    date = make_yearquarter(
      year = as.integer(left(date, 4)),
      quarter = as.integer(right(date, 1))
    ),
    age = sub(" Years", "", age),
    age = case_when(
      age == "95 and Over" ~ "95+",
      age == "Total All Ages" ~ "all",
      .default = age
    )
  )

# Convert quarterly to weekly
get_monthly <- function(df, n) {
  df |>
    uncount(n, .id = "month") |>
    mutate(date = yearmonth(date(.data$date))) |>
    mutate(date = .data$date + month) |>
    mutate(population = ifelse(month > 1, NA, population)) |>
    select(-last_col()) |>
    mutate(population = round(na.approx(population, na.rm = FALSE)))
}

monthly_pop <- quarterly_pop |>
  group_by(age) |>
  get_monthly(3) |>
  mutate(age_group = case_when(
    age %in% 0:20 ~ "0-20",
    age %in% 21:40 ~ "21-40",
    age %in% 41:60 ~ "41-60",
    age %in% 61:80 ~ "61-80",
    age %in% 81:94 ~ "81+",
    age %in% "95+" ~ "81+",
    .default = NA
  )) |>
  group_by(date, age_group) |>
  summarize(population = as.integer(sum(population))) |>
  filter(!is.na(age_group))

for (offset in -4:4) {
  monthly_pop_vaxx <- get_data_by_time_resolution(yearmonth, offset)
  monthly_pop_vaxx_status <- monthly_pop |>
    inner_join(monthly_pop_vaxx, by = join_by(date, age_group)) |>
    mutate(population_unvaccinated = population - population_vaccinated)

  save_csv(
    monthly_pop_vaxx_status,
    paste0("nzl/population_vaccinated_month_age_offset_", offset),
    upload = FALSE
  )
}

# source("./covid19/nzl/vaxx_population.r")
