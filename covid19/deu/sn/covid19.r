source("./population/deu/deu.r")

pop <- de_population |>
  filter(age_group == "all", year == 2022) |>
  select(jurisdiction, population)

df <- read_csv(paste0(
  "https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Todesfaelle",
  "_in_Deutschland/main/COVID-19-Todesfaelle_Bundeslaender.csv"
)) |>
  setNames(
    c("date", "jurisdiction", "jurisdiction_id", "deaths", "deaths_total")
  ) |>
  filter(date == "2023-W04") |>
  select("jurisdiction", "deaths_total")

df_plot <- df |>
  inner_join(pop, by = c("jurisdiction")) |>
  mutate(c19_cmr = deaths_total / population * 100000)

chart <- ggplot(df_plot, aes(x = reorder(jurisdiction, c19_cmr), y = c19_cmr)) +
  geom_col(fill = "#bb4664") +
  geom_text(aes(
    y = c19_cmr,
    label = sprintf("%.1f", c19_cmr),
  ), vjust = +1.5) +
  labs(
    title = paste0(
      "COVID-19 Mortality Rate 2020-2022, German states"
    ),
    subtitle = "Source: rki.de · 2020-2023 W04 (accounting reporting delay)",
    x = "Calendar Year",
    y = "Excess Deaths/1,000 population"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
  theme(legend.position = "top", legend.title = element_blank())

save_chart(chart, "deu/states_covid19_2020-2022", upload = FALSE)
