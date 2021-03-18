library(data.table)
library(tidyverse)
  
# Get case data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---------------------------
  system(paste0("curl -o ", here::here("data", "raw", "CA_cases"), Sys.Date(), ".csv " ,
                "https://data.chhs.ca.gov/dataset/f333528b-4d38-4814-bebb-12db1f10f535/resource/046cdd2b-31e5-4d34-9ed3-b48cdbc4be7a/download/covid19cases_test.csv")) 
  
  CA_cases <- read.csv(paste0(here::here("data", "raw", "CA_cases"), Sys.Date(), ".csv")) %>% 
    filter(area_type == "County")
  
  CA_cases_dt <- as.data.table(CA_cases)
  CA_cases_dt[, County := paste0(area, " County")]

# Save ------------------
  saveRDS(CA_cases_dt, here::here("data", "derived", paste0("CA_Cases_Pops", Sys.Date(), ".rds")))  
    