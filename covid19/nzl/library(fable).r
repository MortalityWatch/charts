library(fable)
library(tidyverse)

df <- as_tsibble(USAccDeaths) |>
  as_tibble() |>
  mutate(index = year(index)) |>
  group_by(index) |>
  summarize(value = sum(value)) |>
  as_tsibble(index = index)

df_train <- df |> filter(index < 1977)
df_test <- df |> filter(index >= 1977)

model <- df_train |> model(lm = TSLM(value ~ trend()))
bl <- model |>
  forecast(h = 2) |>
  hilo(95)

excess <- sum(df_test$value - bl$.mean)
print(paste0("Cumulative Excess 1977-1978: ", excess))
