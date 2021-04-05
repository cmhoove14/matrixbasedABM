library(pdftools)
library(tidyverse)

pdf_files <- list.files(here::here("data", "raw"), 
                        pattern = ".pdf")

pdf_to_table <- function(pdf_file){
  got_year <- if_else(substr(pdf_file,7,8) == "21", "2021", "2020")
  
  got_date <- as.Date(paste0(got_year, "-",
                             substr(pdf_file,9,10),
                             "-",
                             substr(pdf_file,11,12)))
  
  print(got_date)
  
  got_txt <- pdf_text(here::here("data", "raw", pdf_file)) %>% 
    readr::read_lines()
  
  # Lines where data starts and ends
  start_line1 <- which(grepl("Male Institutions", got_txt)) + 1
  end_line1 <- which(grepl("Male Total", got_txt)) - 1
  
  dat_txt1 <- got_txt[start_line1:end_line1] %>% 
    str_replace_all(",","")

  # Same for female institutions
  start_line2 <- which(grepl("Female Institutions", got_txt)) + 1
  end_line2 <- which(grepl("Female Total", got_txt)) - 1
  
  dat_txt2 <- got_txt[start_line2:end_line2] %>% 
    str_replace_all(",","")

  # Convert text to tibbles
    # Male institutions
    list1 <- str_split(dat_txt1, "  ")
    dat1 <- plyr::ldply(list1, function(l){
      chars <- unlist(l)
      chars2 <- chars[which(nchar(chars) > 0)]
      return(chars2)
    })
    
    colnames(dat1) <- c("Facility", "Capacity", "Design_Capacity", "Percent_Occupied", "Staffed_Capacity")
    dat1 <- dat1 %>% 
      mutate(
        Facility_Type = "Male Institution",
        Report_Date = got_date
      )
 
    #Female institutions
    list2 <- str_split(dat_txt2, "  ")
    dat2 <- plyr::ldply(list2, function(l){
      chars <- unlist(l)
      chars2 <- chars[which(nchar(chars) > 0)]
      return(chars2)
    })
    
    colnames(dat2) <- c("Facility", "Capacity", "Design_Capacity", "Percent_Occupied", "Staffed_Capacity")
    dat2 <- dat2 %>% 
      mutate(
        Facility_Type = "Female Institution",
        Report_Date = got_date
      )
    
    return(bind_rows(dat1, dat2))
       
}


fin_pop_dat <- bind_rows(lapply(pdf_files, pdf_to_table)) %>% 
  mutate(Fac_Code = gsub(".*\\((.*)\\).*", "\\1", Facility))
                
saveRDS(fin_pop_dat, 
        paste0(here::here("data", "derived"), "/cdcr_population_ts_", Sys.Date(),".rds"))
     