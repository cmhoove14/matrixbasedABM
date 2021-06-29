library(data.table)
library(tidyverse)

# Get county fips codes ~~~~~~~~~~~~~~~~~~~~~~~~~---------------------------
temp.file <- paste(tempfile(),".xlsx",sep = "")
download.file("https://www2.census.gov/programs-surveys/popest/geographies/2018/all-geocodes-v2018.xlsx", 
              temp.file, mode = "wb")

fips <- readxl::read_excel(temp.file, skip = 4)
  ca_cnty_fips <- fips %>% 
    filter(`State Code (FIPS)` == "06" &
             `County Code (FIPS)` != "000") %>% 
    mutate(County = `Area Name (including legal/statistical area description)`,
           fips = paste0(`State Code (FIPS)`, `County Code (FIPS)`)) %>% 
    dplyr::select(County, fips)
  ca_cnty_fips_dt <- as.data.table(ca_cnty_fips)
  
# Get case data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---------------------------
  system(paste0("curl -o ", here::here("data", "raw", "CA_cases"), Sys.Date(), ".csv " ,
                "https://data.chhs.ca.gov/dataset/f333528b-4d38-4814-bebb-12db1f10f535/resource/046cdd2b-31e5-4d34-9ed3-b48cdbc4be7a/download/covid19cases_test.csv")) 
  
  CA_cases <- read.csv(paste0(here::here("data", "raw", "CA_cases"), Sys.Date(), ".csv")) %>% 
    filter(area_type == "County")
  
  CA_cases_dt <- as.data.table(CA_cases)
  CA_cases_dt[, County := paste0(area, " County")]

  CA_cases_dt2 <- merge(CA_cases_dt, ca_cnty_fips_dt, by = "County")
    CA_cases_dt2$date <- as.Date(CA_cases_dt2$date)
  CA_cases_dt2 <- CA_cases_dt2[!is.na(date),]
    
# Save ------------------
  saveRDS(CA_cases_dt2, here::here("data", "derived", paste0("CA_Cases_Pops", Sys.Date(), ".rds")))  
    