source("./lib/common.r")

x <- seq(0, 2 * pi, length.out = 52 * 2)
y <- sin(x * 2 + 9)
x <- 1:104

df <- tibble(
  week = x,
  deaths = 21000 + 3000 * (y + runif(length(y), min = -0.4, max = 0.4)),
  pop = 84000000
) |> mutate(cmr = deaths / pop * 100000)

df |> ggplot(aes(x = week, y = cmr)) +
  geom_point(color = "black") +
  labs(
    title = "Simulated Mortality per Week via Sine Wave",
    x = "Week",
    y = "Deaths per 100k"
  )

# Split by Vaccination Status
logistic_growth_fun <- function(t, K, A, r) {
  K / (1 + A * exp(-r * t))
}
df$vaxx_p <- logistic_growth_fun(1:104, K = 0.75, A = 99, r = 0.15)

# No difference
df2 <- df
df2$deaths_vaxx <- df2$deaths * df2$vaxx_p
df2$pop_vaxx <- df2$pop * df2$vaxx_p
df2$deaths_unvaxx <- df2$deaths * (1 - df2$vaxx_p)
df2$pop_unvaxx <- df2$pop * (1 - df2$vaxx_p)
df2$cmr_vaxx <- df2$deaths_vaxx / df2$pop_vaxx * 100000
df2$cmr_unvaxx <- df2$deaths_unvaxx / df2$pop_unvaxx * 100000

scale_factor <- max(df2$cmr) * 2
df2 |> ggplot(aes(x = week)) +
  geom_line(aes(y = vaxx_p * scale_factor), color = "black", linetype = "dashed") +
  geom_point(aes(y = cmr_vaxx), color = "#d45c02", shape = 3) +
  geom_point(aes(y = cmr_unvaxx), color = "#239b78", shape = 4) +
  labs(
    title = "Simulated Mortality per Week via Sine Wave",
    subtitle = "Placebo · 0) Base Case",
    x = "Week",
    y = "Deaths per 100k"
  ) +
  scale_y_continuous(
    labels = comma,
    name = "Deaths/100k population",
    sec.axis = sec_axis(~ . / scale_factor,
      name = "Population vaccinated",
      breaks = scales::pretty_breaks(n = 10),
      labels = scales::percent_format()
    )
  )

# 1. Unknown as Unvaxx
df2 <- df
df2$deaths_vaxx <- .5 * df2$deaths * df2$vaxx_p
df2$deaths_unvaxx <- .5 * df2$deaths * (1 - df2$vaxx_p)
df2$deaths_unknown <- .5 * df2$deaths * df2$vaxx_p
df2$pop_vaxx <- df2$pop * df2$vaxx_p
df2$pop_unvaxx <- df2$pop * (1 - df2$vaxx_p)
df2$cmr_vaxx <- df2$deaths_vaxx / df2$pop_vaxx * 100000
df2$cmr_unvaxx <- (df2$deaths_unvaxx + df2$deaths_unknown) / df2$pop_unvaxx * 100000

df2 |> ggplot(aes(x = week)) +
  geom_line(aes(y = vaxx_p * scale_factor), color = "black", linetype = "dashed") +
  geom_line(aes(y = cmr_vaxx), color = "#239b78") +
  geom_line(aes(y = cmr_unvaxx), color = "#d45c02") +
  labs(
    title = "Simulated Mortality per Week via Sine Wave",
    subtitle = "Placebo · 50% Unknown Vaxx Status attributed to Unvaxxed",
    x = "Week",
    y = "Deaths per 100k"
  ) +
  scale_y_continuous(
    labels = comma,
    name = "Deaths/100k population",
    sec.axis = sec_axis(~ . / scale_factor,
      name = "Population vaccinated",
      breaks = scales::pretty_breaks(n = 10),
      labels = scales::percent_format()
    )
  )

# 4. Reporting delay of vaccination status
df2 <- df
df2$deaths_vaxx <- df2$deaths * df2$vaxx_p
df2$deaths_unvaxx <- df2$deaths * (1 - df2$vaxx_p)
df2$pop_vaxx <- df2$pop * df2$vaxx_p
df2$pop_unvaxx <- df2$pop * (1 - df2$vaxx_p)

# Delay by 4 weeks
df2$pop_vaxx <- lead(df2$pop_vaxx, 4)
df2$pop_unvaxx <- lead(df2$pop_unvaxx, 4)

df2$cmr_vaxx <- df2$deaths_vaxx / df2$pop_vaxx * 100000
df2$cmr_unvaxx <- df2$deaths_unvaxx / df2$pop_unvaxx * 100000

df2 |>
  filter(!is.na(cmr_vaxx)) |>
  ggplot(aes(x = week)) +
  geom_line(aes(y = vaxx_p * scale_factor), color = "black", linetype = "dashed") +
  geom_line(aes(y = cmr_vaxx), color = "#239b78") +
  geom_line(aes(y = cmr_unvaxx), color = "#d45c02") +
  labs(
    title = "Simulated Mortality per Week via Sine Wave",
    subtitle = "Placebo · 14d Reporting delay",
    x = "Week",
    y = "Deaths per 100k"
  ) +
  scale_y_continuous(
    labels = comma,
    name = "Deaths/100k population",
    sec.axis = sec_axis(~ . / scale_factor,
      name = "Population vaccinated",
      breaks = scales::pretty_breaks(n = 10),
      labels = scales::percent_format()
    )
  )
