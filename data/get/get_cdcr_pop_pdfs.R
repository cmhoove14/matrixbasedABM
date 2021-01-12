library(lubridate)
library(tidyverse)

#------------------
# Download all pdfs for conversion
# -----------------
base_url <- "https://www.cdcr.ca.gov/research/wp-content/uploads/sites/174/" 

start_date <- as.Date("2020-01-01")
end_date   <- as.Date("2020-12-31")
interval   <- 7
get_dates  <- seq.Date(start_date, end_date, interval)



# Function to get pdf from url for target date. Saves downloaded pdf with same name into target_folder
  get_pdf <- function(target_date, target_folder) {
    print(target_date)
    
    
    get_month <- month(target_date)
    get_month <- ifelse(str_length(get_month) == 1, 
                        paste0("0", get_month),
                        get_month)
    
    get_day   <- day(target_date)
    get_day   <- ifelse(str_length(get_day) == 1, 
                        paste0("0", get_day),
                        get_day)
    
    # Website has sept 30 file in october folder (10) for some reason
    get_month_file <- ifelse(target_date == as.Date("2020-09-30"),
                             "10",
                             get_month)
    
    get_url <- paste0(base_url, 
                      "2020/", 
                      get_month_file,
                      "/Tpop1d20",
                      get_month,
                      get_day,
                      ".pdf")
    
    download.file(get_url,
                  destfile = paste0(target_folder, "/", basename(get_url)),
                  mode = "wb")
  }

# Apply function to all target dates
  for(d in 1:length(get_dates)){
    get_pdf(get_dates[d], here::here("data", "raw"))
  }
  