source("./lib/common.r")
options(timeout = 600)

urls <- list(
  c(
    paste0(
      "https://raw.githubusercontent.com/robert-koch-institut/",
      "COVID-19-Impfungen_in_Deutschland/main/Archiv/",
      "2022-09-05_Deutschland_Landkreise_COVID-19-Impfungen.csv"
    ),
    "covid19_deu_vaccinations.csv"
  ),
  c(
    paste0(
      "https://www2.census.gov/programs-surveys/popest/datasets/2000-2010/",
      "intercensal/state/st-est00int-agesex.csv"
    ),
    "population_usa_2000-2010.csv"
  ),
  c(
    paste0(
      "https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/",
      "state/asrh/SC-EST2020-AGESEX-CIV.csv"
    ),
    "population_usa_2010-2020.csv"
  ),
  c(
    paste0(
      "https://www2.census.gov/programs-surveys/popest/datasets/2000-2010/",
      "intercensal/county/co-est00int-agesex-5yr.csv"
    ),
    "population_usa_county_2000-2010.csv"
  ),
  c(
    paste0(
      "https://www2.census.gov/programs-surveys/popest/datasets/",
      "2010-2020/counties/asrh/CC-EST2020-ALLDATA-36.csv"
    ),
    "population_usa_county_2010-2020.csv"
  ),
  c(
    paste0(
      "https://data.cdc.gov/api/views/3yf8-kanr/rows.csv",
      "?accessType=DOWNLOAD&bom=true&format=true"
    ),
    "usa_deaths_causes_2014_2019.csv"
  ),
  c(
    paste0(
      "https://raw.githubusercontent.com/dr5hn/countries-states-cities",
      "-database/master/csv/countries.csv"
    ),
    "countries.csv"
  ),
  c(
    "https://www.destatis.de/static/DE/dokumente/5126108209005_SB.xlsx",
    "sonderauswertung-sterbefaelle-endgueltige-daten.xlsx"
  ),
  c(
    paste0(
      "https://data.cdc.gov/api/views/muzy-jte6/rows.csv?accessType=DOWNLOAD",
      "&bom=true&format=true"
    ),
    "usa_deaths_causes_2020_n.csv"
  ),
  c(
    "https://data.cdc.gov/api/views/y5bj-9g5w/rows.csv?accessType=DOWNLOAD",
    "usa_states_age_weekly.csv"
  ),
  c(
    "https://data.cdc.gov/api/views/xkkf-xrst/rows.csv?accessType=DOWNLOAD",
    "usa_states_excess_weekly.csv"
  ),
  c(
    "https://data.cdc.gov/api/views/unsk-b7fc/rows.csv?accessType=DOWNLOAD",
    "usa_states_vaccination.csv"
  )
)

dir <- "data_static/"
for (url in urls) {
  print(paste0(dir, url[2]))
  retry_download(url[1], paste0(dir, url[2]))
}
