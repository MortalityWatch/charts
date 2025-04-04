source("lib/common.r")
source("lib/asmr.r")
source("lib/parallel.r")

source("mortality/world_dataset_functions.r")

source("mortality/_collection/un.r")
source("mortality/_collection/mortality_org.r")
source("mortality/_collection/world_mortality.r")
source("mortality/_collection/eurostat.r")
source("mortality/usa/mortality_states.r")
source("mortality/can/mortality_states.r")
source("mortality/deu/mortality_states.r")

# Load Data
asmr_types <- c("asmr_who", "asmr_esp", "asmr_usa", "asmr_country")

data <- rbind(
  deu_mortality_states,
  can_mortality_states,
  usa_mortality_states,
  eurostat,
  world_mortality,
  mortality_org,
  un
) |>
  arrange(iso3c, date, desc(type), source)

rm(
  deu_mortality_states,
  can_mortality_states,
  usa_mortality_states,
  eurostat,
  world_mortality,
  mortality_org,
  un
)

# Define type resolution priorities
priority_weekly <- c(3, 2, 1) # Prefer 3, then 2, then 1
priority_monthly <- c(2, 3, 1) # Prefer 2, then 3, then 1
priority_yearly <- c(1, 2, 3) # Prefer 1, then 2, then 3
priority_dataset <- c(
  "cdc", "statcan", "destatis", "world_mortality", "mortality_org", "un"
)

if (Sys.getenv("STAGE") != "") {
  data <- data |> filter(iso3c %in% c("USA", "SWE", "JPN", "DEU", "AFG"))
}
if (Sys.getenv("ISO3C") != "") {
  data <- data |> filter(str_detect(iso3c, Sys.getenv("ISO3C")))
}

# Country names are saved in meta data.
source("mortality/world_iso.r")
save_info(
  df = data |> inner_join(iso3c_jurisdiction, by = c("iso3c")),
  upload = FALSE
)

rm(iso3c_jurisdiction)

# Function to select the correct row per date based on priority order
filter_by_priority <- function(data, priority_order) {
  data |>
    group_by(date, age_group) |>
    # Arrange by the given priority order
    arrange(match(source, priority_dataset), match(type, priority_order)) |>
    slice(1) |> # Select the first row per date
    ungroup()
}

process_country <- function(df) {
  iso3c <- df[1, ]$iso3c
  print(paste0("ISO: ", iso3c))
  # Make furr pull in these functions
  fluseason <- fluseason
  midyear <- midyear
  yearweek <- yearweek
  dd <- df |>
    expand_daily() |>
    mutate(cmr = deaths / population * 100000)

  dd_all <- dd |> filter(age_group == "all")
  dd_age <- dd |> filter(age_group != "all")
  dd_asmr <- dd_age
  if (nrow(dd_age)) {
    dd_asmr <- dd_age |>
      group_by(iso3c, type, n_age_groups, source) |>
      group_modify(~ calculate_asmr_variants(.x), .keep = TRUE) |>
      ungroup()
    dd_asmr$age_group <- "all"
  }

  # Apply filtering to both datasets
  dd_all_weekly <- filter_by_priority(dd_all, priority_weekly)
  dd_all_monthly <- filter_by_priority(dd_all, priority_monthly)
  dd_all_yearly <- filter_by_priority(dd_all, priority_yearly)

  dd_asmr_weekly <- filter_by_priority(dd_asmr, priority_weekly)
  dd_asmr_monthly <- filter_by_priority(dd_asmr, priority_monthly)
  dd_asmr_yearly <- filter_by_priority(dd_asmr, priority_yearly)

  dd_age_weekly <- filter_by_priority(dd_age, priority_weekly)
  dd_age_monthly <- filter_by_priority(dd_age, priority_monthly)
  dd_age_yearly <- filter_by_priority(dd_age, priority_yearly)

  for (ag in unique(dd$age_group)) {
    print(paste0("Age Group: ", ag))
    if (ag == "all") {
      write_dataset(
        iso3c, ag,
        weekly = summarize_data_all(dd_all_weekly, dd_asmr_weekly,
          type = "yearweek"
        ),
        monthly = summarize_data_all(dd_all_monthly, dd_asmr_monthly,
          type = "yearmonth"
        ),
        quarterly = summarize_data_all(dd_all_monthly, dd_asmr_monthly,
          type = "yearquarter"
        ),
        yearly = summarize_data_all(dd_all_yearly, dd_asmr_yearly,
          type = "year"
        ),
        by_fluseason = summarize_data_all(dd_all_monthly, dd_asmr_monthly,
          type = "fluseason"
        ),
        by_midyear = summarize_data_all(dd_all_monthly, dd_asmr_monthly,
          type = "midyear"
        )
      )
    } else {
      dd_ag_f_weekly <- dd_age_weekly |>
        filter(age_group == ag) |>
        distinct(iso3c, date, age_group, .keep_all = TRUE)
      dd_ag_f_monthly <- dd_age_monthly |>
        filter(age_group == ag) |>
        distinct(iso3c, date, age_group, .keep_all = TRUE)
      dd_ag_f_yearly <- dd_age_yearly |>
        filter(age_group == ag) |>
        distinct(iso3c, date, age_group, .keep_all = TRUE)
      write_dataset(
        iso3c, ag,
        weekly = summarize_data_by_time(dd_ag_f_weekly,
          type = "yearweek"
        ),
        monthly = summarize_data_by_time(dd_ag_f_monthly,
          type = "yearmonth"
        ),
        quarterly = summarize_data_by_time(dd_ag_f_monthly,
          type = "yearquarter"
        ),
        yearly = summarize_data_by_time(dd_ag_f_yearly,
          type = "year"
        ),
        by_fluseason = summarize_data_by_time(dd_ag_f_monthly,
          type = "fluseason"
        ),
        by_midyear = summarize_data_by_time(dd_ag_f_monthly,
          type = "midyear"
        )
      )
    }
  }
}

if (Sys.getenv("CI") != 1) {
  countries <- unique(data$iso3c)
  with_progress({
    p <- progressor(steps = length(countries))
    data |>
      group_split(iso3c) |>
      future_walk(~ {
        process_country(.x)
        p()
      })
  })
} else {
  df <- data |>
    group_split(iso3c) |>
    walk(process_country)
}

print("Finished.")

# source("mortality/world_dataset.r")
