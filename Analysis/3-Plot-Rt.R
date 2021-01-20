# ---------------------------------------
# PLots of Rt through time
# Chris Hoover Jan 2021
# ---------------------------------------

library(ggplot2)
library(tidyverse)

# Cori et al method among residents ------------------
cori_rts_dat <- readRDS(here::here("data", "derived", "Cori_etal_Rts_residents.rds"))

cori_rts_plot <- cori_rts_dat %>% 
  mutate(Facility2 = str_replace(Facility, " State Prison", "")) %>% 
  ggplot() +
    geom_line(aes(x = Date, y = `Median(R)`)) +
    geom_ribbon(aes(x = Date, ymin = `Quantile.0.25(R)`, ymax = `Quantile.0.75(R)`),
                col = "grey50", alpha = 0.5) +
    facet_wrap(facets = "Facility2",
               nrow = 4, ncol = 8,
               labeller = label_wrap_gen()) +
    theme_bw() +
    theme(strip.text = element_text(size = 6.5)) +
    labs(x = "Date",
         y = "Resident Active Cases")

# Cori et al method assuming staff are imported and residents are local
cori_rts_stf_res_dat <- readRDS(here::here("data", "derived", "Cori_Rts_residents_staff.rds"))

cori_rts_stf_res_plot <- cori_rts_stf_res_dat %>% 
  mutate(Facility2 = str_replace(Facility, " State Prison", "")) %>% 
  ggplot() +
    geom_line(aes(x = Date, y = `Median(R)`)) +
    geom_ribbon(aes(x = Date, ymin = `Quantile.0.25(R)`, ymax = `Quantile.0.75(R)`),
                col = "grey50", alpha = 0.5) +
    facet_wrap(facets = "Facility2",
               nrow = 5, ncol = 5,
               labeller = label_wrap_gen()) +
    theme_bw() +
    theme(strip.text = element_text(size = 6.5)) +
    labs(x = "Date",
         y = expression(R[t]~estimate))

ggsave(plot = cori_rts_stf_res_plot,
       filename = here::here("Plots", "Cori_Rts_stf_imported_res_local.jpg"),
       height = 6, 
       width = 8,
       units = "in",
       dpi = 300)


# EpiNow2 Rt estimates -----------------------
Rts <- readRDS(here::here("data", "derived", "Rt_estimates_df.rds")) %>%   
  mutate(
    Facility = str_replace(region, " State Prison", "")
  )


rt_plots <- Rts %>% 
  ggplot() +
    geom_line(aes(x = date, y = median)) +
    geom_ribbon(aes(ymax = upper_90, ymin = lower_90, x = date),
                col = "grey50", alpha = 0.5) +
    geom_hline(yintercept = 1, lty = 3) +
    facet_wrap(facets = "Facility",
               nrow = 4, ncol = 8,
               labeller = label_wrap_gen()) +
    theme_bw() +
    theme(strip.text = element_text(size = 6)) +
    labs(x = "Date",
         y = expression(R[t]~Estimate))

ggsave(plot = rt_plots,
       filename = here::here("Plots", "Rt_by_date_faceted.jpg"),
       height = 6, 
       width = 9,
       units = "in",
       dpi = 300)
              