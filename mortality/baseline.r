source("lib/common.r")
source("lib/parallel.r")

min_bl_len <- 4
max_bl_len <- 10
forecast_len <- 4
min_data_len <- min_bl_len + forecast_len

get_optimal_size <- function(df, type) {
  min <- Inf
  optimal_size <- min(max_bl_len, nrow(df))
  type <- sym(type)
  if (nrow(na.omit(df[type])) < min_bl_len + forecast_len) {
    return(NA)
  }

  for (size in min_bl_len:min(nrow(df) - forecast_len, max_bl_len)) {
    acc <- df |>
      head(-forecast_len) |>
      tsibble::slide_tsibble(.size = size) |>
      model(fable::TSLM(!!type ~ trend())) |>
      forecast(h = forecast_len) |>
      fabletools::accuracy(df)
    if (!is.nan(acc$RMSE) && acc$RMSE < min) {
      min <- acc$RMSE
      print(paste("New min:", min, "Size:", size))
      optimal_size <- size
    }
  }

  return(optimal_size)
}

filter_complete_latest <- function(df) {
  gaps <- df |> tsibble::scan_gaps()
  if (nrow(gaps) == 0) {
    return(df |> select(-iso3c))
  }
  df |>
    filter(date > max(gaps)) |>
    select(-iso3c)
}

process_country <- function(iso3c) {
  print(paste0("Processing, ", iso3c))
  result <- tibble()

  tryCatch(
    {
      # Yearly data
      data <- read_remote(paste0("mortality/", iso3c, "/yearly.csv"))
      yearly <- data |>
        filter(source != "un") |>
        mutate(chart_type = "yearly", .after = "iso3c")

      # Flu season data
      data <- read_remote(paste0("mortality/", iso3c, "/fluseason.csv"))
      fluseason <- data |>
        filter(source != "un") |>
        mutate(date = as.integer(left(date, 4))) |>
        mutate(chart_type = "fluseason", .after = "iso3c")

      # Midyear data
      data <- read_remote(paste0("mortality/", iso3c, "/midyear.csv"))
      midyear <- data |>
        filter(source != "un") |>
        mutate(date = as.integer(left(date, 4))) |>
        mutate(chart_type = "midyear", .after = "iso3c")

      rbind(result, rbind(yearly, fluseason, midyear))
    },
    error = function(e) {
      message(paste("Error processing", iso3c, ":", e$message))
      return(tibble(iso3c = iso3c, error = e$message))
    }
  )
}

meta <- read_remote("mortality/world_meta.csv") |> filter(source != "un")
codes <- unique(meta$iso3c)
with_progress({
  p <- progressor(steps = length(codes))
  result <- codes |>
    future_map(~ {
      p()
      process_country(.x)
    }) |>
    bind_rows()
})

save_csv(result, "mortality/world_baseline")

# source("mortality/baseline.r")
