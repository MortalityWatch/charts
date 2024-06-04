source("lib/common.r")

calculate_excess_ <- function(data, col_name) {
  col <- sym(col_name)
  data |> mutate(
    "{col_name}_ex" := !!col - !!sym(paste0(col_name, "_bl")),
    "{col_name}_ex_lo" :=
      !!col - !!sym(paste0(col_name, "_bl_hi")),
    "{col_name}_ex_hi" :=
      !!col - !!sym(paste0(col_name, "_bl_lo"))
  )
}

calculate_baseline_excess <- function(df,
                                      metric_column,
                                      period_type,
                                      bl_years,
                                      bl_model,
                                      fc_years,
                                      year_period_multiplier = 1,
                                      date_col = "date") {
  ts <- df |> as_tsibble(index = date_col)

  if ((has_gaps(ts))$.gaps[1]) {
    return(df)
  }

  forecast_interval <- round(fc_years * year_period_multiplier)

  if (period_type %in% c("yearly", "fluseason", "midyear")) {
    bl_data <- ts |> filter(!!sym(date_col) %in% bl_years)
  } else {
    bl_data <- ts |> filter(lubridate::year(!!sym(date_col)) %in% bl_years)
  }

  model <- bl_data |> fabletools::model(bl_model)
  fc <- model |> fabletools::forecast(h = forecast_interval)
  fc_hl <- fabletools::hilo(fc, 95) |> fabletools::unpack_hilo(cols = "95%")
  bl <- model |> augment()

  result <- tibble("{date_col}" := c(bl[[date_col]], fc[[date_col]])) |>
    dplyr::mutate(
      "{metric_column}_bl" := c(bl$.fitted, fc$.mean),
      "{metric_column}_bl_lo" := c(
        rep(NA, length(bl$.fitted)),
        fc_hl$`95%_lower`
      ),
      "{metric_column}_bl_hi" := c(
        rep(NA, length(bl$.fitted)),
        fc_hl$`95%_upper`
      )
    )

  ts |>
    left_join(result, by = date_col) |>
    relocate(
      paste0(metric_column, "_bl"),
      paste0(metric_column, "_bl_lo"),
      paste0(metric_column, "_bl_hi"),
      .after = all_of(metric_column)
    ) |>
    calculate_excess_(metric_column) |>
    mutate(
      "{metric_column}_ex_p" := !!sym(paste0(metric_column, "_ex")) /
        !!sym(paste0(metric_column, "_bl")),
      "{metric_column}_ex_p_lo" :=
        !!sym(paste0(metric_column, "_ex_lo")) /
          !!sym(paste0(metric_column, "_bl")),
      "{metric_column}_ex_p_hi" :=
        !!sym(paste0(metric_column, "_ex_hi")) /
          !!sym(paste0(metric_column, "_bl")),
      "{metric_column}_ex_sign" :=
        ifelse(!!sym(paste0(metric_column, "_bl")) < 1, FALSE,
          !!sym(metric_column) > !!sym(paste0(metric_column, "_bl_hi"))
        )
    ) |>
    as_tibble()
}

create_models <- function(col, min_data_year) {
  adjust_years <- function(start_year) {
    if (start_year < min_data_year) {
      return(min_data_year:2019)
    } else {
      return(start_year:2019)
    }
  }

  models <- list(
    naive = list(
      title = "naive",
      model = NAIVE(!!sym(col)),
      years = adjust_years(2015)
    ),
    mean3 = list(
      title = "mean",
      model = TSLM(!!sym(col)),
      years = adjust_years(2017)
    ),
    mean5 = list(
      title = "mean",
      model = TSLM(!!sym(col)),
      years = adjust_years(2015)
    ),
    lin_reg = list(
      title = "lin. regr.",
      model = TSLM(!!sym(col) ~ trend()),
      years = adjust_years(2010)
    ),
    ets15 = list(
      title = "ETS",
      model = ETS(!!sym(col) ~ error("A") + trend("Ad")),
      years = adjust_years(2005)
    ),
    ets21 = list(
      title = "ETS",
      model = ETS(!!sym(col) ~ error("A") + trend("Ad")),
      years = adjust_years(1999)
    )
  )

  return(models)
}

custom_match <- c(EL = "GRC", UK = "GBR")

# Vaccinated
data <- read.csv("./data/owid.csv") |> as_tibble()

df <- data |>
  select(
    iso_code,
    date,
    people_vaccinated_per_hundred,
    people_fully_vaccinated_per_hundred,
    total_boosters_per_hundred,
    total_vaccinations_per_hundred
  ) |>
  mutate(date = date(date)) |>
  setNames(c("iso3c", "date", "dose_1", "dose_2", "dose_3", "dose_all"))

vaccinated_max <- df |>
  mutate(date = year(date)) |>
  group_by(iso3c, date) |>
  summarise(across(1:4, \(x) suppress_warnings(
    max(x, na.rm = TRUE) / 100,
    "no non-missing arguments to max; returning -Inf"
  ))) |>
  pivot_longer(
    cols = starts_with("dose_"),
    names_to = "dose",
    values_to = "vaccinated_pct",
    names_prefix = "dose_"
  ) |>
  filter(!is.infinite(vaccinated_pct))

vaccinated_mean <- df |>
  mutate(date = year(date)) |>
  group_by(iso3c, date) |>
  summarise(across(1:4, \(x) suppress_warnings(
    mean(x, na.rm = TRUE) / 100,
    "no non-missing arguments to max; returning -Inf"
  ))) |>
  pivot_longer(
    cols = starts_with("dose_"),
    names_to = "dose",
    values_to = "vaccinated_pct",
    names_prefix = "dose_"
  ) |>
  filter(!is.infinite(vaccinated_pct))

save_csv(vaccinated_max, "covid19/vaccinated_year_max", upload = FALSE)
save_csv(vaccinated_mean, "covid19/vaccinated_year_mean", upload = FALSE)

vaccinated <- vaccinated_max

# Fertility
fertility <- as_tibble(read.csv(
  gzfile("./data/eurostat_fertility.tsv.gz"),
  sep = "\t"
))

fertility <- fertility |>
  pivot_longer(
    cols = 2:ncol(fertility),
    names_to = "date",
    values_to = "rate"
  ) |>
  mutate(date = as.integer(right(date, 4))) |>
  mutate(
    rate = suppress_warnings(
      as.double(str_replace_all(rate, c(": " = ""))),
      "NAs introduced by coercion"
    )
  ) |>
  # filter(!is.na(rate)) |>
  separate_wider_delim(
    freq.indic_de.geo.TIME_PERIOD,
    delim = ",",
    names = c("freq", "indic_de", "iso3c")
  ) |>
  select(iso3c, date, rate) |>
  filter(nchar(iso3c) == 2, !iso3c %in% c("FX", "XK")) |>
  mutate(iso3c = countrycode(
    iso3c,
    origin = "iso2c",
    destination = "iso3c",
    custom_match = custom_match
  ))

models <- create_models(col = "rate", 2011)
df_all <- tibble()
for (mdl_key in names(models)) {
  mdl <- models[[mdl_key]]
  mdl$key <- mdl_key

  years <- c(mdl$years, (max(mdl$years) + 1):(max(mdl$years) + 4))
  df <- fertility |>
    filter(date %in% years, !is.na(rate)) |>
    group_by(iso3c) |>
    filter(length(unique(date)) >= length(mdl$years)) |>
    group_modify(~ calculate_baseline_excess(
      .x,
      metric_column = "rate",
      period_type = "yearly",
      bl_years = mdl$years,
      bl_model = mdl$model,
      fc_years = 4
    ))
  df$model <- mdl_key
  df_all <- rbind(df_all, df)
}

# By year
by_year <- fertility |>
  filter(date > 2020) |>
  inner_join(fertility_bl, by = join_by(iso3c)) |>
  mutate(fertility_ex_p = rate.x / rate.y - 1) |>
  inner_join(vaccinated, by = join_by(iso3c, date))

# Cumulative 2021-2022
all <- fertility |>
  filter(date > 2020) |>
  inner_join(fertility_bl, by = join_by(iso3c)) |>
  group_by(iso3c) |>
  summarize(fertility = sum(rate.x), bl = sum(rate.y)) |>
  mutate(fertility_ex_p = fertility / bl - 1) |>
  inner_join(
    vaccinated |>
      filter(date > 2020) |>
      group_by(iso3c, dose) |>
      summarize(vaccinated_pct = mean(vaccinated_pct, na.rm = TRUE))
  )
all$date <- "2021-2022"

df_plot <- rbind(
  all |> select(-fertility, -bl),
  by_year |> select(-rate.x, -rate.y)
) |> arrange(iso3c, date, dose)

df_plot$date <- factor(df_plot$date, levels = c("2021", "2022", "2021-2022"))

df_plot <- df_all |>
  filter(date >= 2020, !is.na(rate_ex)) |>
  select(iso3c, date, rate_ex_p, model) |>
  inner_join(
    vaccinated |>
      filter(date > 2020) |>
      group_by(iso3c, date, dose) |>
      summarize(vaccinated_pct = mean(vaccinated_pct, na.rm = TRUE)),
    by = join_by(iso3c, date), relationship = "many-to-many"
  )

make_chart <- function(df) {
  ggplot(
    df,
    aes(x = vaccinated_pct, y = rate_ex_p)
  ) +
    geom_point() +
    geom_smooth(method = "lm") +
    stat_cor(method = "pearson", p.accuracy = 0.001) +
    labs(
      title = paste0(
        "Change in Fertility Rate vs COVID-19 Vaccination Rate [Europe]"
      ),
      subtitle = paste(
        "Baseline: 2015-'19",
        "Source: Eurostat, OWID",
        "95% CI",
        sep = " · "
      ),
      x = "COVID-19 Vaccination Rate",
      y = "Excess Fertility Rate"
    ) +
    theme_bw() +
    scale_x_continuous(labels = scales::percent_format()) +
    scale_y_continuous(labels = scales::percent_format()) +
    theme(legend.position = "top") +
    theme(legend.title = element_blank()) +
    facet_wrap(vars(date, dose, model),
      scales = "free_x",
      labeller = labeller(date = label_both, dose = label_value)
    )
}

save_chart(
  make_chart(df_plot |> filter(
    model %in% c("naive", "lin_reg", "mean3", "mean5")
  )),
  "covid19/fertility_v_vaxx",
  8,
  upload = FALSE
)
