source("./lib/common.r")

iso3c <- "NZL"

df <- read.csv(
  paste0("https://s3.mortality.watch/data/mortality/", iso3c, "/yearly.csv")
)
nz <- df |>
  as_tibble() |>
  select(date, asmr_esp) |>
  mutate(year = as.integer(left(date, 4))) |>
  as_tsibble(index = date) |>
  filter(!is.na(asmr_esp))

# Split data into training and test
df_train <- nz |> filter(year < 2020)
df_test <- nz |> filter(year >= 2020)

# Model
model <- df_train |> model(
  ETS(asmr_esp ~ error("A") + trend("Ad") + season("N"))
)

# Baseline
bl <- model |>
  augment() |>
  as_tibble() |>
  select(date, .fitted) |>
  filter(!is.na(.fitted))

model |>
  forecast(h = 30) |>
  autoplot(bind_rows(df_train, df_test)) +
  geom_line(aes(y = .fitted), data = bl, linetype = 2, col = "blue") +
  labs(title = paste0("ASMR [", iso3c, "]"))
