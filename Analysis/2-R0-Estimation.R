#-------------------------------------
# Estimate R0 for CA state prison facilities using exponential growth method
# Chris Hoover, Jan 2020
# ------------------------------------

library(R0) # https://bmcmedinformdecismak.biomedcentral.com/articles/10.1186/1472-6947-12-147#Sec17
library(tidyverse)

# Load data
dat <- readRDS(here::here("data", "derived", "state_prisons_pop_cases_fin.rds")) %>% 
  filter(!is.na(Facility) & 
           !grepl("CHCF", Facility) & 
           !grepl("SATF", Facility)) %>%  # Both of these facilities had 0 cases. Both seem to be specilized for heatlhcare/treatment, so makes sense
  mutate(
    Facility2 = str_replace(Facility, " State Prison", "")
  )

I_curves <- dat %>% 
  ggplot() +
  geom_line(aes(x = Date, y = New_Residents_Confirmed_7day)) +
  facet_wrap(facets = "Facility2",
             nrow = 4, ncol = 8,
             labeller = label_wrap_gen()) +
  theme_bw() +
  theme(strip.text = element_text(size = 6.5)) +
  labs(x = "Date",
       y = "Resident incident cases (weekly average)")

# create generation time
GT_pars  <- EpiNow2::get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
GT_sars2 <-  generation.time("gamma", c(GT_pars$mean,GT_pars$sd)) 

# function to get data for exponential growth phase for facility
get_growth_phase <- function(facility, DF){
  data <- DF %>% 
    filter(Facility == facility)
  
  peak_day <- which.max(data %>% pull(New_Residents_Confirmed_7day))
  data_before_peak <- data[1:peak_day,]
  inc_til_peak <- round(data_before_peak$New_Residents_Confirmed_7day)
  
  day <- peak_day
  inc_day <- inc_til_peak[day]

  while(inc_day > 0 & day > 7){ # Stop once day gets to 7 due to 7 day smoothing wndow right align means NAs for first 7 observations
    day <- day-1
    inc_day <- inc_til_peak[day]
  }
  
  
  start_day <- day
  
  peak_date <- data$Date[peak_day]
  start_date <- data$Date[start_day]
  
  return(c(start_date, peak_date))
}

# Get data frame of expnential phase for all facilities
dat_exp_phase <- bind_rows(lapply(unique(dat$Facility),
                                  function(f){
                                    start_to_peak <- get_growth_phase(f, dat)
                                    out <- dat %>% 
                                      filter(Facility == f) %>% 
                                      filter(Date >= start_to_peak[1] & Date <= start_to_peak[2])
                                    return(out)
                                  }))

# Add expnential phase to facilities plot
I_curves <- I_curves +
  geom_line(data = dat_exp_phase,
            aes(x = Date, y = New_Residents_Confirmed_7day),
            col = "red")

# Get R0 for all facilities based o exponential phase
fac_R0s <- bind_rows(lapply(unique(dat$Facility),
                            function(f){
                              inc_dat <- dat_exp_phase %>% 
                                filter(Facility == f) %>% 
                                pull(New_Residents_Confirmed_7day) %>% 
                                round()
                              
                              R_est <- est.R0.EG(inc_dat, GT_sars2)
                              
                              return(c("Facility" = f,
                                       "R0" = round(R_est$R,2),
                                       "R0_lo" = round(R_est$conf.int[1],2),
                                       "R0_hi" = round(R_est$conf.int[2],2)))
                            }))

fac_R0s <- fac_R0s %>% 
  mutate(
    label     = paste0(R0," (",
                       R0_lo, " - ",
                       R0_hi, ")"),
    Facility2 = str_replace(Facility, " State Prison", "")
  )

I_curves_label <- I_curves +
  geom_text(
    data    = fac_R0s,
    mapping = aes(x = as.Date("2020-10-20", format = "%Y-%m-%d"), 
                  y = 120, 
                  label = label),
    size = 2,
    col = "red"
  )

ggsave(plot = I_curves_label,
       filename = here::here("Plots", "R0_estimates_exponential_growth_incident_cases.jpg"),
       height = 6, 
       width = 9,
       units = "in",
       dpi = 300)
