# Plot County Map
make_chart <- function(y, cum = FALSE) {
  lims <- get_limits(ts |> filter(level == 5, date >= 2020))

  df <- if (cum) {
    ts |> filter(is_cumulative == TRUE)
  } else {
    ts |> filter(date %in% y)
  }

  ts_plot <- germany |>
    select(CC_2, geom) |>
    inner_join(df, by = join_by(CC_2 == id)) |>
    as_tibble()

  # Mean Excess
  ggplot(ts_plot) +
    geom_sf(aes(geometry = geom, fill = cmr_excess_p)) +
    scale_fill_viridis_c(
      option = "D",
      na.value = "#bbbbbb",
      name = "eCMR",
      label = scales::percent,
      limits = lims
    ) +
    labs(
      title = paste0(
        "Relative Excess Crude Mortality Rate (eCMR) - ",
        "Counties [Sachsen, Germany]"
      ),
      subtitle = paste0(
        "Source: statistik.sachsen.de/genonline ·",
        " Year: ", paste(y, collapse = ", "),
        " · 2010-2019 linear trend baseline"
      ),
      x = "",
      y = ""
    ) +
    theme(plot.background = element_rect(fill = "white")) +
    theme_bw()
}

for (y in 2020:2022) {
  save_chart(
    make_chart(y),
    paste0("deu/sn/county/map/ecmr_mean_", y),
    upload = FALSE
  )
}
save_chart(
  make_chart(c(2020:2022), TRUE),
  "deu/sn/county/map/ecmr_mean_2020_2022",
  upload = FALSE
)

# Significant Excess
make_chart <- function(y, cum = FALSE) {
  df <- if (cum) {
    ts |> filter(is_cumulative == TRUE)
  } else {
    ts |> filter(date %in% y)
  }
  df <- df |> mutate(cmr_excess_sign = as.integer(cmr_excess_sign))

  ts_plot <- germany |>
    select(CC_2, geom) |>
    inner_join(df, by = join_by(CC_2 == id)) |>
    as_tibble()

  ggplot(ts_plot) +
    geom_sf(aes(geometry = geom, fill = cmr_excess_sign)) +
    scale_fill_viridis_c(
      option = "D",
      na.value = "#bbbbbb",
      name = "eCMR",
      label = scales::percent,
      limits = c(0, 1)
    ) +
    labs(
      title = paste0(
        "Jurisdictions with Stat. Sign. Excess Mortality (eCMR) - ",
        "Counties [Sachsen, Germany]"
      ),
      subtitle = paste0(
        "Source: statistik.sachsen.de/genonline ·",
        " Year: ", paste(y, collapse = ", "),
        " · 2010-2019 linear trend baseline · ",
        "yellow = TRUE"
      ),
      x = "",
      y = ""
    ) +
    theme(plot.background = element_rect(fill = "white")) +
    theme_bw() +
    theme(legend.position = "none")
}

for (y in 2020:2022) {
  save_chart(
    make_chart(y),
    paste0("deu/sn/county/map/ecmr_sign_", y),
    upload = FALSE
  )
}
save_chart(
  make_chart(y = c(2020:2022), TRUE),
  "deu/sn/county/map/ecmr_sign_2020_2022",
  upload = FALSE
)

# Plot Muni Maps
make_chart <- function(y, cum = FALSE) {
  df <- if (cum) {
    ts |> filter(is_cumulative == TRUE, level == 8)
  } else {
    ts |> filter(date %in% y, level == 8)
  }

  ts_plot <- germany_4 |>
    inner_join(df) |>
    as_tibble() |>
    mutate(cmr_excess_p = pmax(0, pmin(1, cmr_excess_p)))

  # Mean Excess
  ggplot(ts_plot) +
    geom_sf(aes(geometry = geom, fill = cmr_excess_p)) +
    scale_fill_viridis_c(
      option = "D",
      na.value = "#bbbbbb",
      name = "eCMR",
      label = scales::percent,
      limits = c(0, 1)
    ) +
    labs(
      title = paste0(
        "Relative Excess Crude Mortality Rate (eCMR) - ",
        "Municipalities [Sachsen, Germany]"
      ),
      subtitle = paste0(
        "Source: statistik.sachsen.de/genonline ·",
        " Year: ", paste(y, collapse = ", "),
        " · 2010-2019 linear trend baseline"
      ),
      x = "",
      y = ""
    ) +
    theme(plot.background = element_rect(fill = "white")) +
    theme_bw()
}

for (y in 2020:2022) {
  save_chart(
    make_chart(y),
    paste0("deu/sn/muni/map/ecmr_mean_", y),
    upload = FALSE
  )
}
save_chart(
  make_chart(y = c(2020:2022), TRUE),
  paste0("deu/sn/muni/map/ecmr_mean_2020_2022"),
  upload = FALSE
)

# Significant Excess
make_chart <- function(y, cum = FALSE) {
  df <- if (cum) {
    ts |> filter(is_cumulative == TRUE, level == 8)
  } else {
    ts |> filter(date %in% y, level == 8)
  }
  df <- df |> mutate(cmr_excess_sign = as.integer(cmr_excess_sign))

  ts_plot <- germany_4 |>
    inner_join(df) |>
    as_tibble() |>
    mutate(cmr_excess_p = pmax(0, pmin(1, cmr_excess_p)))

  ggplot(ts_plot) +
    geom_sf(aes(geometry = geom, fill = cmr_excess_sign)) +
    scale_fill_viridis_c(
      option = "D",
      na.value = "#bbbbbb",
      name = "eCMR",
      label = scales::percent,
      limits = c(0, 1)
    ) +
    labs(
      title = paste0(
        "Jurisdictions with Stat. Sign. Excess Mortality (eCMR) - ",
        "Counties [Sachsen, Germany]"
      ),
      subtitle = paste0(
        "Source: statistik.sachsen.de/genonline ·",
        " Year: ", paste(y, collapse = ", "),
        " · 2010-2019 linear trend baseline · ",
        "yellow = TRUE"
      ),
      x = "",
      y = ""
    ) +
    theme(plot.background = element_rect(fill = "white")) +
    theme_bw() +
    theme(legend.position = "none")
}

for (y in 2020:2022) {
  save_chart(
    make_chart(y),
    paste0("deu/sn/muni/map/ecmr_sign_", y),
    upload = FALSE
  )
}
save_chart(
  make_chart(y = c(2020:2022), TRUE),
  "deu/sn/muni/map/ecmr_sign_2020_2022",
  upload = FALSE
)

# source('./covid19/deu/sn/plot_map_ecmr.r')
