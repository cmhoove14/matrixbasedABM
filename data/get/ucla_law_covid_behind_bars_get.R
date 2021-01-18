# Get data from UCLA Covid Behind Bars github project https://github.com/uclalawcovid19behindbars

uclabb <- readr::read_csv(url("https://raw.githubusercontent.com/uclalawcovid19behindbars/historical-data/main/data/CA-historical-data.csv"))

saveRDS(uclabb, paste0(here::here("data","raw"), 
                       "/ucla_law_covid_behind_bars_github_data_", 
                       Sys.Date(),
                       ".rds"))
