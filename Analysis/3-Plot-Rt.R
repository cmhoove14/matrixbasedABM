# ---------------------------------------
# PLots of Rt through time
# Chris Hoover Jan 2021
# ---------------------------------------

library(ggplot2)
library(tidyverse)

Rts <- readRDS(here::here("data", "derived", "Rt_estimates_df.rds")) %>%   
  mutate(
    Facility = str_replace(region, " State Prison", "")
  )


rt_plots <- Rts %>% 
ggplot() +
  geom_line(aes(x = date, y = median)) +
  geom_ribbon(aes(ymax = upper_90, ymin = lower_90, x = date),
              col = "grey50", alpha = 0.5) +
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


              ggplot(rt_dat) +
                geom_col(data = inc_dat,
                         aes(x = date, y = confirm/1e2),
                         fill = "grey50", alpha = 0.5) +
                geom_col(data = sf_test,
                         aes(x = Date, y = pct),
                         fill = "darkred", alpha = 0.5) +
                geom_line(aes(x = date,y = mean), col = "darkblue", size = 1.2) +
                geom_ribbon(aes(x = date, ymin = mean-sd, ymax = mean+sd),
                            fill = "blue", alpha = 0.5) +
                theme_classic() +
                geom_hline(lty = 2, yintercept = 1) +
                labs(x = "Date",
                     y = expression(R[t]),
                     title = "SF County Confirmed Cases and Rt Estimate")
              