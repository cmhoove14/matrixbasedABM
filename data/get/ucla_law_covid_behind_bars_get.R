# Get data from UCLA Covid Behind Bars github project https://github.com/uclalawcovid19behindbars

uclabb <- readr::read_csv(url("https://raw.githubusercontent.com/uclalawcovid19behindbars/historical-data/main/data/CA_adult_facility_covid_counts_historical.csv"))

saveRDS(uclabb, "data/raw/ucla_law_covid_behind_bars_github_data_1-11-21.rds")
