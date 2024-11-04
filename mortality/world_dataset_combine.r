source("lib/common.r")

# Load the unique iso3c values
countries <- read_csv("out/mortality/world_meta.csv") |>
  select(iso3c) |>
  distinct()

load_country_data <- function(iso3c, type) {
  url <- paste0("out/mortality/", iso3c, "/", type, ".csv")
  df <- tryCatch(
    {
      read_csv(url) |> mutate(type = as.character(type))
    },
    error = function(e) {
      print(e)
      message(paste("Failed to load", type, "data for", iso3c))
      NULL # Return NULL on error
    }
  )
  return(df)
}

# Types of data to load
data_types <- c("yearly", "fluseason", "quarterly", "monthly", "weekly")

# Load data for each type and save separate CSVs
map(data_types, function(type) {
  all_data <- countries$iso3c |> map_dfr(~ load_country_data(.x, type))
  if (!is.null(all_data)) {
    save_csv(all_data, paste0("mortality/world_", type))
  }
})
