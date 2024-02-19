source("lib/common.r")

summarize_85plus <- function(x) {
  x |>
    mutate(
      age_group = case_when(
        age_group == "85-89" ~ "85+",
        age_group == "90-94" ~ "85+",
        age_group == "95+" ~ "85+",
        .default = age_group
      )
    ) |>
    group_by_at(vars(-deaths)) |>
    summarise(deaths = sum(deaths)) |>
    ungroup()
}

de_states <- as_tibble(read.csv("./data_static/deu_states_iso3c.csv"))
df <- read_remote("deaths/deu/deaths.csv") |>
  inner_join(de_states, by = "jurisdiction")

rm(de_states)

# All Ages
df_all <- df |>
  filter(age_group == "Insgesamt") |>
  mutate(
    date = date_parse(paste(year, week, 1), format = "%G %V %u"),
    age_group = "all"
  ) |>
  select(iso3c, date, age_group, deaths)
df_all$n_age_groups <- 1

# By age group, national
df_age_d_13 <- df |>
  filter(jurisdiction == "Deutschland" & age_group != "Insgesamt") |>
  mutate(date = date_parse(paste(year, week, 1), format = "%G %V %u")) |>
  summarize_85plus() |>
  select(iso3c, date, age_group, deaths)
df_age_d_13$n_age_groups <- 13

# By age group, states
df_age_states <- df |>
  filter(jurisdiction != "Deutschland" & age_group != "Insgesamt") |>
  mutate(date = date_parse(paste(year, week, 1), format = "%G %V %u")) |>
  group_by(iso3c, date, age_group) |>
  summarise(deaths = sum(deaths)) |>
  ungroup()
df_age_states$n_age_groups <- 4

rm(df)

# Population
source("population/deu/deu.r")

population <- de_population |>
  mutate(date = date(sprintf("%d-12-31", year)), .after = iso3c) |>
  group_by(iso3c) |>
  nest() |>
  mutate(
    data = map(data, ~ . |>
      group_by(age_group) |>
      nest() |>
      mutate(data = lapply(data, interpolate_population)) |>
      unnest(cols = "data"))
  ) |>
  unnest(cols = "data") |>
  select(iso3c, date, age_group, population)
rm(de_population)

deu_mortality_states <- rbind(df_all, df_age_d_13, df_age_states) |>
  inner_join(population, by = c("iso3c", "date", "age_group")) |>
  arrange(iso3c, date, age_group) |>
  distinct(iso3c, date, age_group, n_age_groups, .keep_all = TRUE)

deu_mortality_states$type <- 3
deu_mortality_states$source <- "destatis"

rm(df_all, df_age_d_13, df_age_states, population)

# source("./mortality/deu/mortality_states.r")
