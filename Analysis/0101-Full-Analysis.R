

library(zoo)
library(tidyverse)
library(gridExtra)

# Data -----------------
dat           <- readRDS(here::here("data/derived/state_prisons_pop_cases_fin2021-02-02.rds"))
CA_cases_pops <- readRDS(here::here("data/derived/CA_Cases_Pops2021-02-18.rds"))

dat_case_pop_merge <- dat %>% 
  left_join(CA_cases_pops %>% mutate(County.FIPS = as.numeric(fips)),
            by = c("County.FIPS" = "County.FIPS",
                   "Date" = "date"))

# Functions -----------------------
#Simulate clusters that occur after an introduction
#Outputs is a table theat lists the number of clusters of an individual size, and the numeber that lead to full blow outbreaks (last row of output)
#Inputs are:
#   the reproduciton number (R)
#   the dispersion parameter (k)
#   the size at which point an outbreak is deemed to occur (thresh)
#   the number of introductions to simulate (number)

sim_cluster <-function(R,k,thresh,number) {
  all_sizes <- tibble(i = 1:number) %>% group_by(i) %>% do({
    num_inf <- 1
    cluster_size <- 1
    while (num_inf > 0) {
      num_inf <- sum(rnbinom(num_inf,size = k, mu = R))
      cluster_size <- cluster_size + num_inf
      if(cluster_size > thresh) {
        cluster_size <- thresh + 1
        num_inf <- 0
      }
    }
    tibble(cluster_size = cluster_size)
  })
  all_sizes %>% group_by(cluster_size, .groups = "drop") %>% summarize(n = n())
}


# prob that i cases cause j cases
calc_r_ij <- function (i,j,r,k) {
  log_r_ij <- lgamma(j +k*i) - lgamma(j+1) - lgamma(k*i) + 
    k * i * log(k/(r+k)) +
    j * log(r/(r+k))
  r_ij<- exp(log_r_ij)
}

# Likelihood of data
#
# thresh denotes the size cutoff that determines when a cluster becomes an outbreak.
# c_j_arr = probability of having a cluster of size j
calc_cluster_logL <- function(par, data, thresh) {
  R <- exp(par[1])
  k <- exp(par[2])
  
  cluster_size <- 1:thresh
  c_j_arr <- calc_r_ij(i=cluster_size,j=cluster_size-1,R,k)/cluster_size
  cluster_size[thresh+1] <- thresh + 1
  c_j_arr[thresh+1] <- 1-sum(c_j_arr)
  prob_arr <- tibble(cluster_size = cluster_size, prob = c_j_arr)
  
  # Truncate right tail
  data$cluster_size <- ifelse(data$cluster_size > thresh,thresh+1, data$cluster_size)
  
  #Do log L
  data <- left_join(data,prob_arr,by = 'cluster_size')
  nLL <- -sum(data$n * log(data$prob))
  
  return(nLL)
}

# Optimizer to find best fit Reff and k
reff_k_find <- function(INIT, DATA, THRESH){
  out_fit <- optim(par = INIT, 
                   fn = calc_cluster_logL, 
                   # Additional functions to cal_cluster_logL
                   data = DATA,
                   thresh = THRESH,
                   #method = "L-BFGS-B",
                   #control = list("trace" = 1)
                   hessian = TRUE)
  
  if(out_fit$convergence == 0){
    return(list("logPars" = out_fit$par,
                "SEs" = sqrt(diag(solve(out_fit$hessian)))))
  } else {
    warning("No convergence")
    return(list("logPars" = NA,
                "SEs" = NA))
  }
  
}

# Likelihood of data with fixed k
#
# thresh denotes the size cutoff that determines when a cluster becomes an outbreak.
# c_j_arr = probability of having a cluster of size j
calc_cluster_logL_fix_k <- function(R, k, data, thresh) {
  
  cluster_size <- 1:thresh
  c_j_arr <- calc_r_ij(i=cluster_size,j=cluster_size-1,R,k)/cluster_size
  cluster_size[thresh+1] <- thresh + 1
  c_j_arr[thresh+1] <- 1-sum(c_j_arr)
  prob_arr <- tibble(cluster_size = cluster_size, prob = c_j_arr)
  
  # Truncate right tail
  data$cluster_size <- ifelse(data$cluster_size > thresh,thresh+1, data$cluster_size)
  
  #Do log L
  data <- left_join(data,prob_arr,by = 'cluster_size')
  nLL <- -sum(data$n * log(data$prob))
  
  return(nLL)
}
# Optimizer to find best fit Reff with fixed k
reff_find <- function(R_INIT, K_est, DATA, THRESH){
  out_fit <- optim(par = R_INIT, 
                   fn = calc_cluster_logL_fix_k,
                   method = "Brent",
                   #control = list("trace" = 1)
                   hessian = TRUE, 
                   lower = 0, upper = 1e10,
                   # Additional functions to calc_cluster_logL
                   k = K_est,
                   data = DATA,
                   thresh = THRESH)
  
  if(out_fit$convergence == 0){
    return(list("logPars" = out_fit$par,
                "SEs" = sqrt(diag(solve(out_fit$hessian)))))
  } else {
    warning("No convergence")
    return(list("logPars" = NA,
                "SEs" = NA))
  }
  
}

# define_outbreaks -----------------
# 7 day washout period 
outbreaks_df7day <- dat %>% 
  dplyr::select(c(Facility:Residents_Active)) %>% 
  group_by(Facility) %>% 
  mutate( # Identify outbreaks as new cases emerging following 7 days with no cases
    new_cases_7day = zoo::rollsum(New_Residents_Confirmed_rmv_neg, k = 7, 
                                  na.pad = T, align = "right"),
    new_cases_7day_lead1 = lead(new_cases_7day),
    outbreak_start = if_else(new_cases_7day == 0 & new_cases_7day_lead1 > 0, 1, 0),
    # Give each outbreak a unique identifier
    outbreak_num = cumsum(if_else(is.na(outbreak_start), 0, outbreak_start)) + outbreak_start*0,
    Facility_Outbreak = paste0(Facility, " Outbreak ", outbreak_num),
    # For plotting outbreaks 
    plot7day_cases = if_else(new_cases_7day == 0, NA_real_, New_Residents_Confirmed_7day)
  ) %>% 
  ungroup() %>% 
  filter(!is.na(outbreak_num) & outbreak_num > 0)



# 10 day washout period 
outbreaks_df10day <- dat %>% 
  dplyr::select(c(Facility:Residents_Active)) %>% 
  group_by(Facility) %>% 
  mutate( # Identify outbreaks as new cases emerging following 10 days with no cases
    new_cases_10day = zoo::rollsum(New_Residents_Confirmed_rmv_neg, k = 10, 
                                   na.pad = T, align = "right"),
    new_cases_10day_lead1 = lead(new_cases_10day),
    outbreak_start = if_else(new_cases_10day == 0 & new_cases_10day_lead1 > 0, 1, 0),
    # Give each outbreak a unique identifier
    outbreak_num = cumsum(if_else(is.na(outbreak_start), 0, outbreak_start)) + outbreak_start*0,
    Facility_Outbreak = paste0(Facility, " Outbreak ", outbreak_num),
    plot7day_cases = if_else(new_cases_10day == 0, NA_real_, New_Residents_Confirmed_7day)
  ) %>% 
  ungroup() %>% 
  filter(!is.na(outbreak_num) & outbreak_num > 0)



# 14 day washout period      
outbreaks_df14day <- dat %>% 
  dplyr::select(c(Facility:Residents_Active)) %>% 
  group_by(Facility) %>% 
  mutate( # Identify outbreaks as new cases emerging following 14 days with no cases
    new_cases_14day = zoo::rollsum(New_Residents_Confirmed_rmv_neg, k = 14, 
                                   na.pad = T, align = "right"),
    new_cases_14day_lead1 = lead(new_cases_14day),
    outbreak_start = if_else(new_cases_14day == 0 & new_cases_14day_lead1 > 0, 1, 0),
    # Give each outbreak a unique identifier
    outbreak_num = cumsum(if_else(is.na(outbreak_start), 0, outbreak_start)) + outbreak_start*0,
    Facility_Outbreak = paste0(Facility, " Outbreak ", outbreak_num),
    plot7day_cases = if_else(new_cases_14day == 0, NA_real_, New_Residents_Confirmed_7day)
  ) %>% 
  ungroup() %>% 
  filter(!is.na(outbreak_num) & outbreak_num > 0)


# Plot time series with outbreaks delineated -----------------------
outbreaks_10d_wash_plot <- outbreaks_df10day %>% 
  mutate(Facility2 = str_remove(unlist(str_split(Facility, pattern = "\\("))[seq(2,nrow(.)*2, by=2)], pattern = "\\)")) %>% 
  ggplot() +
  geom_line(aes(x = Date, y = plot7day_cases, col = as.factor(outbreak_num))) +
  theme_classic() +
  theme(axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 16),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        #plot.title = element_text(size = 18),
        plot.tag = element_text(size = 20)) +
  scale_y_continuous(trans = "log1p",
                     breaks = c(0,10,100,1000)) +
  facet_wrap(facets = "Facility2",
             nrow = 5, ncol = 7) +
  labs(y = "7-day average of incident resident cases",
       col = "Facility\nOutbreak\nNumber",
      # title = "Outbreaks in all facilities",
       tag = "A.")

outbreaks_10d_wash_plot

# Plot raw outbreak size distribution -------------------
outbreak_size7day <- outbreaks_df7day %>% 
  group_by(Facility_Outbreak) %>% 
  summarise(outbreak_size = sum(New_Residents_Confirmed_rmv_neg))


outbreak_size10day <- outbreaks_df10day %>% 
  group_by(Facility_Outbreak) %>% 
  summarise(outbreak_size = sum(New_Residents_Confirmed_rmv_neg))


outbreak_size14day <- outbreaks_df14day %>% 
  group_by(Facility_Outbreak) %>% 
  summarise(outbreak_size = sum(New_Residents_Confirmed_rmv_neg))

# Define upper cutoff for outbreak
upper_cutoff1 <- 10

outbreak_size7day_10uppercut <- outbreak_size7day %>% 
  mutate(outbreak_size_uppercut = if_else(outbreak_size > upper_cutoff1, upper_cutoff1+1, outbreak_size)) %>% 
  group_by(outbreak_size_uppercut) %>% 
  summarise(n_ob = n())

outbreak_size10day_10uppercut <- outbreak_size10day  %>% 
  mutate(outbreak_size_uppercut = if_else(outbreak_size > upper_cutoff1, upper_cutoff1+1, outbreak_size)) %>% 
  group_by(outbreak_size_uppercut) %>% 
  summarise(n_ob = n())

outbreak_size14day_10uppercut <- outbreak_size14day %>% 
  mutate(outbreak_size_uppercut = if_else(outbreak_size > upper_cutoff1, upper_cutoff1+1, outbreak_size)) %>% 
  group_by(outbreak_size_uppercut) %>% 
  summarise(n_ob = n())

# Outbreak distributon characteristics
# Number of outbreaks
outbreak_size10day_10uppercut %>% pull(n_ob) %>% sum()

# Number with no onward transmission
outbreak_size10day_10uppercut %>% filter(outbreak_size_uppercut == 1) %>% pull(n_ob)

# Number not resultng in outbreak above cutoff threshold
outbreak_size10day_10uppercut %>% filter(outbreak_size_uppercut < upper_cutoff1+1) %>% pull(n_ob) %>% sum()

# Number resulting in outbreak above cutoff threshold
outbreak_size10day_10uppercut %>% filter(outbreak_size_uppercut == upper_cutoff1+1) %>% pull(n_ob) %>% sum()

# Plot outbreak size distribution with upper cutoff
outbreak_size_distn <- outbreak_size10day_10uppercut %>% 
  ggplot(aes(x = outbreak_size_uppercut, y = n_ob)) +
  geom_col() +
  geom_col(data = outbreak_size10day_10uppercut %>% filter(outbreak_size_uppercut == (upper_cutoff1+1)),
           fill = "darkred") +
  theme_classic() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        #plot.title = element_text(size = 18),
        plot.tag = element_text(size = 20)) +
  scale_x_continuous(breaks = c(1:(upper_cutoff1+1)),
                     labels = c(as.character(c(1:(upper_cutoff1))),
                                paste0(">", upper_cutoff1))) +
  labs(x = "Outbreak size",
       y = "Frequency",
       #title = "Outbreak size distribution with 10-day washout",
       tag = "B.")

outbreak_size_distn

# Estimate Reff and k from outbreak size distributions --------------------------------
wash_thresh_k <- expand.grid("Wash" = c(7,10,14),
                             "thresh" = c(3,5,10),
                             "k" = seq(0.1,1.0,by = 0.1))

Reff_ests <- t(apply(wash_thresh_k,1,function(x){
  wash <- as.numeric(x[1])
  thresh = as.numeric(x[2])
  k = as.numeric(x[3])
  
  if(wash == 7){
    df_use = outbreak_size7day
  } else if(wash == 10){
    df_use = outbreak_size10day
  } else if(wash == 14){
    df_use = outbreak_size14day
  } else {
    NULL
  }
  
  outbreak_uppercut <- df_use %>% 
    mutate(cluster_size = if_else(outbreak_size > thresh, thresh+1, outbreak_size)) %>% 
    group_by(cluster_size) %>% 
    summarise(n = n(), .groups = 'drop')
  
  Reff_est <- reff_find(R_INIT = log(2),
                        K_est = k,
                        DATA = outbreak_uppercut, 
                        THRESH = thresh)
  
  return(c("Reff" = exp(Reff_est$logPars[1]),
           "Reff_SE" = exp(Reff_est$SEs[1])))
  
}))

Reff_results <- cbind(wash_thresh_k, Reff_ests)


#Plot Reff by k with different washouts ---------
Reff_results %>% 
  filter(thresh == 10, k > 0.1) %>% 
  ggplot(aes(x = k, y = Reff, col = as.factor(Wash))) +
    geom_point() +
    geom_errorbar(aes(ymin = Reff-Reff_SE,
                      ymax = Reff+Reff_SE),
                  width = 0.01) +
    scale_y_continuous(trans = "log",
                       breaks = c(1,5,10,50,100,500),
                       limits = c(0.9,501)) +
    scale_x_continuous(breaks = seq(0.1,1.0, by = 0.1)) +
    theme_classic() +
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          #plot.title = element_text(size = 18),
          plot.tag = element_text(size = 20)) +
    labs(y = expression(R[eff]),
         x = "k",
         col = "Washout\nPeriod")
         #title = expression(Bootstrapped~R[eff]~estimates),
         #tag = "C.")

ggsave(filename = here::here("Plots/Reff_by_k_by_washout.jpg"),
       height = 5, width = 7)

# Reff and k estimation accounting for immunity ------------
# Because many facilities have large outbreaks that occur prior to smaller outbreaks, need to censor outbreaks that may have been restricted by effects of immunity


# NP Bootstrap of Reff ---------------------------
set.seed(430)

Reff_boots <- sapply(1:10000, function(...){
  outbreak_uppercut <- outbreak_size10day %>%
    mutate(cluster_size = if_else(outbreak_size > 10, 10+1, outbreak_size)) %>%
    slice(sample(1:nrow(.), size = nrow(.), replace = T)) %>%
    group_by(cluster_size) %>%
    summarise(n = n(), .groups = 'drop')
  
  
  Reff_est <- reff_find(R_INIT = log(2),
                        K_est = 1.0,
                        DATA = outbreak_uppercut,
                        THRESH = 10)
  
  return(exp(Reff_est$logPars))
  
}, simplify = T)



# hist(Reff_boots, breaks = 30, main = "", xlab = expression(R[eff]), cex.lab = 1.2)

reff_boots_df <- as.data.frame(Reff_boots)

reff_boots_plot <- ggplot(reff_boots_df, aes(x = Reff_boots)) +
  geom_histogram() +
  theme_classic() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        #plot.title = element_text(size = 18),
        plot.tag = element_text(size = 20)) +
  labs(x = expression(R[eff]),
       y = "Frequency",
       #title = expression(Bootstrapped~R[eff]~estimates),
       tag = "C.")

reff_boots_plot

c(quantile(Reff_boots, 0.25),
  median(Reff_boots),
  quantile(Reff_boots, 0.75))

fin <- grid.arrange(grobs = list(outbreaks_10d_wash_plot, outbreak_size_distn, reff_boots_plot),
  widths = c(2, 1),
  heights = c(1,0.1,1),
  layout_matrix = rbind(c(1, 2),
                        c(1, NA),
                        c(1, 3))
)
fin

ggsave(filename = here::here("Plots/Prisons_Fin_Fig.jpg"),
       plot = fin,
       height = 9, width = 14)
