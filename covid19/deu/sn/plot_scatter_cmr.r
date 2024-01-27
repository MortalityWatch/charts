# Scatter Plots
chart <- make_scatter_plot(ts |> filter(date >= 2010))
save_chart(chart, "deu/sn/scatter_cmr_all", upload = FALSE)

## All regions
chart <- make_rel_scatter_plot(ts |> filter(date >= 2010))
save_chart(chart, "deu/sn/scatter_ecmr_all", upload = FALSE)

## All munis w/ sign. excess
ts_plot <- ts |> filter(
  level == 8,
  date >= 2010,
  any(cmr_excess_sign, na.rm = TRUE)
)
chart <- make_rel_scatter_plot(
  ts_plot,
  paste(
    "Jurisdictions with any sign. excess ('20-'22)",
    paste0("n=", length(unique(ts_plot$id))),
    sep = SEP
  )
)
save_chart(chart, "deu/sn/muni/scatter/ecmr_sign", upload = FALSE)

## All munis w/o sign. excess
ts_plot <- ts |> filter(
  level == 8,
  date >= 2010,
  !any(cmr_excess_sign, na.rm = TRUE)
)
chart <- make_rel_scatter_plot(
  ts_plot,
  paste(
    "Jurisdictions without any sign. excess ('20-'22)",
    paste0("n=", length(unique(ts_plot$id))),
    sep = SEP
  )
)
save_chart(chart, "deu/sn/muni/scatter/ecmr_no_sign", upload = FALSE)

## All munis w sign. excess in all years
all_excess <- ts |> filter(level == 8, cmr_excess_sign_all == TRUE)
ts_plot <- ts |> filter(date >= 2010, id %in% all_excess$id)
chart <- make_rel_scatter_plot(
  ts_plot,
  paste(
    "Jurisdictions with all years sign. excess ('20-'22)",
    paste0("n=", length(unique(ts_plot$id))),
    sep = SEP
  )
)

save_chart(chart, "deu/sn/muni/scatter/ecmr_sign_all", upload = FALSE)

# Excess vs population size
make_chart <- function(df, split_by_sign_excess) {
  if (split_by_sign_excess) {
    chart <- ggplot(df, aes(
      x = population, y = cmr_excess_p,
      color = cmr_excess_sign
    ))
  } else {
    chart <- ggplot(df, aes(x = population, y = cmr_excess_p))
  }
  chart +
    geom_point() +
    labs(
      title = paste0(
        "Excess Crude Mortality Rate (eCMR) vs. Population by Municipality ",
        "[Saxony, Germany]"
      ),
      subtitle = "2020-2022; Population 2020",
      x = "Population (Log)",
      y = "Excess Deaths/100k population"
    ) +
    theme_bw() +
    coord_trans(x = "log2") +
    scale_y_continuous(labels = scales::percent_format()) +
    theme(legend.position = "top") +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5)) +
    geom_hline(yintercept = 0, linewidth = 0.5) +
    scale_color_manual(values = c("#44781d", "#de5075")) +
    stat_correlation(use_label(c("R", "P", "n", "method"))) +
    stat_poly_line()
}

ts_plot <- ts |> filter(
  is_cumulative == TRUE, level == 8,
  !is.na(population), !is.na(cmr_excess_p)
)

chart <- make_chart(df = ts_plot, FALSE)
save_chart(chart, "deu/sn/muni/scatter/cmr_population", upload = FALSE)
chart <- make_chart(ts_plot, TRUE)
save_chart(chart, "deu/sn/muni/scatter/cmr_population_sign", upload = FALSE)

# Excess vs population density
make_chart <- function(df, split_by_sign_excess) {
  if (split_by_sign_excess) {
    chart <- ggplot(df, aes(
      x = density, y = cmr_excess_p,
      color = cmr_excess_sign
    ))
  } else {
    chart <- ggplot(df, aes(x = density, y = cmr_excess_p))
  }
  chart +
    geom_point() +
    labs(
      title = paste0(
        "Excess Crude Mortality Rate (eCMR) vs. ",
        "Population Density by Municipality ",
        "[Saxony, Germany]"
      ),
      subtitle = "2020-2022; Population Density 2020",
      x = "People/km^2 (Log)",
      y = "Excess Deaths/100k population"
    ) +
    theme_bw() +
    coord_trans(x = "log2") +
    scale_y_continuous(labels = scales::percent_format()) +
    theme(legend.position = "top") +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5)) +
    geom_hline(yintercept = 0, linewidth = 0.5) +
    scale_color_manual(values = c("#44781d", "#de5075")) +
    stat_correlation(use_label(c("R", "P", "n", "method"))) +
    stat_poly_line()
}

ts_plot <- ts |>
  filter(
    is_cumulative == TRUE, level == 8, !is.na(population), !is.na(cmr_excess_p)
  ) |>
  inner_join(germany_4 |> select(id, area), by = join_by(id)) |>
  mutate(density = population / (as.integer(area) / 1000000)) |>
  select(id, density, cmr_excess_p, cmr_excess_sign)

chart <- make_chart(df = ts_plot, FALSE)
save_chart(chart, "deu/sn/muni/scatter/cmr_population_density", upload = FALSE)
chart <- make_chart(ts_plot, TRUE)
save_chart(
  chart,
  "deu/sn/muni/scatter/cmr_population_density_sign",
  upload = FALSE
)
