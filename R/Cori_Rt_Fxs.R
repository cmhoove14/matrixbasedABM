# Function to estiamte Rt using Cori et al method assuming residents are local cases and staff cases are imported
cori_rt <- function(facil, dat){
  print(facil)
  
  inc_dat <- dat %>% 
    filter(Facility == facil) %>% 
    mutate(
      res_new = round(Residents_Confirmed2 - lag(Residents_Confirmed2)),
      res_new2 = if_else(res_new < 0, 0, res_new),
      stf_new = round(Staff_Confirmed2 - lag(Staff_Confirmed2)),
      stf_new2 = if_else(stf_new < 0, 0, stf_new)
    ) %>% 
    ungroup() %>% 
    dplyr::select(Date, res_new2, stf_new2) %>% 
    rename(date = Date,
           local = res_new2,
           imported = stf_new2)
  
  # Restrict to beginning of outbreak
  t_1st <- min(inc_dat$date[which(inc_dat$local + inc_dat$imported != 0)])
  inc_dat <- inc_dat %>% 
    filter(date >= t_1st)
  
  # Sliding weeklywindows in which to estimate Rt
  window = 7
  ts <- 2:(nrow(inc_dat)-(window+1))
  te <- 2:(nrow(inc_dat)-(window+1))+window
  
  R_config <- EpiEstim::make_config(t_start = ts,
                                    t_end = te,
                                    mean_si = 5.2, # From https://dx.doi.org/10.1016%2Fj.cegh.2020.08.007
                                    std_si = 1,
                                    seed = 430)
  
  Cori_R <- EpiEstim::estimate_R(incid = inc_dat,
                                 method = "parametric_si",
                                 config = R_config)
  
  out <- Cori_R$R %>% 
    mutate(
      Facility = facil
    )
  
  return(out)
}
