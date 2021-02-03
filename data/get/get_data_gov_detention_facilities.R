

system(paste0("wget -O ", here::here("data", "raw", "data_gov_detention_facilities"), Sys.Date(), ".csv " ,
              "https://data.ca.gov/dataset/46648eff-26bc-4b8b-bd34-6b022bf5c84e/resource/dadeb689-08e3-4fbd-ac09-c101bcb2f2b2/download/adult-facilies-data-covid-19_1.22.21.csv")) 
