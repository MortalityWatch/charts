source("./lib/common.r")
options(warn = 1)

deaths <- read_csv("./out/nzl/deaths_vaccinated.csv", col_types = "ccci") |>
  mutate(date = yearmonth(date)) |>
  rename(type = vaccinated) |>
  group_by(date, age_group, type) |>
  mutate(age_group = case_when(
    age_group %in% c("81-99", "100+") ~ "81+",
    .default = age_group
  )) |>
  summarize(deaths = as.integer(sum(deaths, na.rm = TRUE))) |>
  arrange(date)

population <- tibble()
for (offset in -4:4) {
  x <- read_csv(
    paste0("./out/nzl/population_vaccinated_month_age_offset_", offset, ".csv"),
    col_types = "cciii"
  ) |>
    mutate(date = yearmonth(date)) |>
    select(-population) |>
    pivot_longer(
      cols = 3:4,
      names_to = "type",
      names_prefix = "population_",
      values_to = "population"
    ) |>
    rename(!!paste0("population_", offset) := "population")
  if (nrow(population) == 0) {
    population <- x
  } else {
    population <- population |> left_join(x)
  }
}

df <- tibble()
for (offset in -4:4) {
  x <- deaths |>
    inner_join(population) |>
    mutate(!!paste0("cmr_", offset) := deaths / get(paste0("population_", offset)) * 100000)
  if (nrow(df) == 0) {
    df <- x
  } else {
    df <- df |> left_join(x)
  }
}

# date type deaths population cmr
chart <-
  df |>
  ggplot(aes(x = date, y = cmr_0, color = type)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("#44781d", "#de5075")) +
  labs(
    title = paste0(
      "All-Cause Mortality by COVID-19 Vaccination Status [New Zealand]"
    ),
    subtitle = "Source: infoshare.nl, mortality.watch",
    x = "Month of Year",
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
  watermark() +
  facet_wrap(~age_group, scales = "free")

save_chart(chart, "nzl/acm_age_vaxx", upload = FALSE)

# ASMR, use 2022, Jan as ref
std_pop <- df |>
  filter(date == make_yearmonth(year = 2022, month = 1)) |>
  group_by(age_group) |>
  summarize(population_0 = sum(population_0), .groups = "drop") |>
  mutate(weight = population_0 / sum(population_0)) |>
  select(age_group, weight)

df <- tibble()
for (offset in -4:4) {
  x <- deaths |>
    inner_join(population) |>
    mutate(!!paste0("cmr_", offset) := deaths / get(paste0("population_", offset)) * 100000)
  if (nrow(df) == 0) {
    df <- x
  } else {
    df <- df |> left_join(x)
  }
}

df2 <- df |>
  # Exclude that age group, b/c incomplete
  # filter(age_group != "0-20") |>
  inner_join(std_pop, by = join_by(age_group)) |>
  mutate_at(vars(starts_with("cmr_")), ~ . * weight) |>
  group_by(date, type) |>
  summarise_at(vars(starts_with("cmr_")), list(sum = ~ sum(., na.rm = TRUE)))

df3 <- df2 |>
  pivot_longer(
    cols = 3:11,
    names_to = "offset",
    values_to = "asmr"
  ) |>
  mutate(offset = str_extract(offset, "-?\\d+"))

chart <- df3 |>
  filter(offset == 0, date >= make_yearmonth(2022, 1)) |>
  ggplot(aes(x = date, y = asmr, fill = type)) +
  geom_col(position = "dodge") +
  # geom_line(linewidth = 1) +
  scale_fill_manual(values = c("#44781d", "#de5075")) +
  labs(
    title = paste0(
      "All-Cause ASMR by COVID-19 Vaccination Status [New Zealand]"
    ),
    subtitle = "Source: cbs.nl, mortality.watch",
    x = "Month of Year",
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
  watermark() +
  coord_cartesian(ylim = c(0, 100))

save_chart(chart, "nzl/asmr_vaxx", upload = FALSE)

# ASMR with week offset
chart <- df3 |>
  filter(offset %in% c(-4, -2, -1, 0, 1, 2, 4)) |>
  ggplot(aes(x = date, y = asmr, color = type, linetype = offset)) +
  geom_line(aes(alpha = ifelse(offset == "0", "solid", "dashed")),
    linewidth = 1
  ) +
  scale_color_manual(values = c("#44781d", "#de5075")) +
  scale_alpha_manual(values = c(0.5, 1), guide = "none") +
  scale_linetype_manual(values = c(3, 4, 5, 1, 3, 4, 5), guide = "none") +
  labs(
    title = paste0(
      "All-Cause ASMR by COVID-19 Vaccination Status [New Zealand]"
    ),
    subtitle = "Source: cbs.nl, mortality.watch",
    x = "Month of Year",
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
  watermark() +
  coord_cartesian(ylim = c(0, 200))

save_chart(chart, "nzl/asmr_vaxx_offset", upload = FALSE)

# Deaths By year/age_group
deaths |>
  mutate(date = year(date)) |>
  group_by(date, age_group) |>
  summarize(deaths = sum(deaths)) |>
  mutate(deaths_pct = deaths / sum(deaths)) |>
  filter(date == 2022)

# ASMR Mean/CI
mean_ci_by_type <- df3 %>%
  filter(offset == 0, date >= make_yearmonth(2022, 1)) %>%
  group_by(type) %>%
  do(tidy(t.test(.$asmr, conf.level = 0.95))) %>%
  # filter(term == "estimate") %>%
  select(type, estimate, conf.low, conf.high) %>%
  rename(mean = estimate, ci_lower = conf.low, ci_upper = conf.high)

t_test_result <- t.test(
  asmr ~ type,
  data = df3, subset = offset == 0 & date >= make_yearmonth(2022, 1)
)
p_value <- t_test_result$p.value
p_value_annotation <- sprintf("p-value: %.4f", p_value)
ggplot(mean_ci_by_type, aes(x = type, y = mean, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.2, position = position_dodge(0.9)
  ) +
  annotate("text",
    x = 1.5,
    y = max(mean_ci_by_type$mean),
    label = p_value_annotation,
    vjust = -1.5, size = 3.5, color = "black"
  ) +
  labs(
    title = paste0(
      "All-Cause ASMR by COVID-19 Vaccination Status [New Zealand]"
    ),
    subtitle = "Jan 2022 - Apr 2023 · Source: cbs.nl",
    x = "Vaccination Status",
    y = "Deaths/100k population",
    fill = "COVID-19 vaccinated"
  ) +
  scale_fill_manual(values = c("#44781d", "#de5075")) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_text(
    aes(label = round(mean, 1)),
    position = position_stack(vjust = 0.5), color = "black"
  )

# Cumulatively
df3 %>%
  filter(offset == 0, date >= make_yearmonth(2022, 1)) |>
  group_by(type) |>
  mutate(asmr = cumsum(asmr)) |>
  ggplot(aes(x = date, y = asmr, color = type)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("#44781d", "#de5075")) +
  labs(
    title = paste0(
      "All-Cause Mortality by COVID-19 Vaccination Status [New Zealand]"
    ),
    subtitle = "Source: infoshare.nl, mortality.watch",
    x = "Month of Year",
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
