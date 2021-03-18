# Get data from UCLA Covid Behind Bars github project https://github.com/uclalawcovid19behindbars
system(paste0("curl -o ", here::here("data", "raw", "ucla_law_covid_behind_bars"), Sys.Date(), ".csv " ,
              "https://raw.githubusercontent.com/uclalawcovid19behindbars/historical-data/main/data/CA-historical-data.csv"))

