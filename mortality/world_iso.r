source("lib/common.r")

population <- read_remote("population/world.csv")

country_codes <- population |>
  select(iso3c, jurisdiction) |>
  unique()

deu_states <- as_tibble(read.csv("./data_static/deu_states_iso3c.csv")) |>
  mutate(jurisdiction = ifelse(
    jurisdiction == "Deutschland",
    "Germany",
    paste0("DEU - ", jurisdiction)
  ))
can_states <- as_tibble(read.csv("./data_static/can_states_iso3c.csv")) |>
  mutate(jurisdiction = ifelse(
    jurisdiction == "Canada",
    jurisdiction,
    paste0("CAN - ", jurisdiction)
  ))
usa_states <- as_tibble(read.csv("./data_static/usa_states_iso3c.csv")) |>
  select(iso3c, jurisdiction) |>
  mutate(jurisdiction = ifelse(
    jurisdiction == "United States",
    jurisdiction,
    paste0("USA - ", jurisdiction)
  ))

uk_states <- rbind(
  data.frame(iso3c = "GBRTENW", jurisdiction = "England & Wales"),
  data.frame(iso3c = "GBR_NIR", jurisdiction = "Northern Ireland"),
  data.frame(iso3c = "GBR_SCO", jurisdiction = "Scotland")
)

iso3c_jurisdiction <- rbind(
  deu_states, can_states, usa_states, uk_states, country_codes
) |>
  distinct(iso3c, .keep_all = TRUE) |>
  arrange(iso3c)
