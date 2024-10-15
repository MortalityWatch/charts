source("lib/common.r")

# Validation
iso3c <- c(
  "DEU", "DEU-BW", "USA", "USA-CA", "GBRTENW", "FRA", "SWE", "AUS", "NZL"
)
age_groups <- c("", "_85+", "_65-74")
result <- tibble()
for (code in iso3c) {
  for (age in age_groups) {
    input_file <- paste0("./out/mortality/", code, "/yearly", age, ".csv")

    if (file.exists(input_file)) {
      df <- read_csv(input_file, col_types = cols(.default = "c"))
      filtered_df <- df |> filter(date %in% c(2010, 2020))
      result <- bind_rows(result, filtered_df)
    } else {
      cat("File not found:", input_file, "\n")
    }
  }
}
# write.csv(
#   result, "./data_static/validation.csv",
#   row.names = FALSE, quote = TRUE
# )
verified <- read_csv(
  "./data_static/validation.csv",
  col_types = cols(.default = "c")
)
verified <- as_tibble(verified)
stopifnot(all.equal(result, verified))
