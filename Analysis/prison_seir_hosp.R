# ASSUMPTIONS
# Discrete, synchronized generations with deterministic rate of infection
# Within each block, transmission events are independent and identical
# Prisoners do not move from one block to another

setwd("/Users/sblumberg/Google Drive/Research/COVID-19/Prison/")
library(tidyverse)
library (gridExtra)

rm(list = ls())

UNIT_SIZE = 1000
NUM_INDEX = 1
PROP_HOSP = 0.2
TIME_STEP = .1
INC_TIME = 4
INF_TIME_MIN = 4
INF_TIME_MAJ = 7
HOSP_DUR = 14
TOT_TIME = 250

gen_time_series <- function(prop_capacity, UNIT_SIZE = 1000, R_max = 2.5) {

  current_time <-  tibble(time = 0, num_exp = NUM_INDEX,  num_inf_min = 0, num_inf_maj = 0, num_susc = UNIT_SIZE*prop_capacity - NUM_INDEX, num_hosp = 0, num_rec = 0) #stores results for current generation
  time_series <- current_time #Will store all data
  
  R <- R_max * prop_capacity
  N <- UNIT_SIZE * prop_capacity
  

  while (current_time$time <= TOT_TIME) {
    last_time <- current_time
    new_exp <- R*(TIME_STEP)*(last_time$num_inf_min/INF_TIME_MIN + last_time$num_inf_maj/INF_TIME_MAJ) * last_time$num_susc / N
    new_inf <- last_time$num_exp * (TIME_STEP/INC_TIME)
    new_inf_min <- new_inf * (1-PROP_HOSP)
    new_inf_maj <- new_inf * PROP_HOSP
    new_hosp <- last_time$num_inf_maj *(TIME_STEP/INF_TIME_MAJ)
    
    num_susc <- last_time$num_susc - new_exp # Susceptibles are decreased by the # who are now infectious
    num_exp <-last_time$num_exp + new_exp - new_inf_min - new_inf_maj
    num_inf_min <-last_time$num_inf_min + new_inf_min - last_time$num_inf_min * (TIME_STEP/INF_TIME_MIN)
    num_inf_maj <-last_time$num_inf_maj + new_inf_maj - new_hosp
    num_hosp <- last_time$num_hosp + new_hosp - last_time$num_hosp * (TIME_STEP/HOSP_DUR)
    num_rec <- last_time$num_rec + last_time$num_inf_min * (TIME_STEP/INF_TIME_MIN) + last_time$num_hosp * (TIME_STEP/HOSP_DUR)

    current_time <- tibble(time = last_time$time + TIME_STEP, num_susc = num_susc, num_exp = num_exp,  
                           num_inf_min = num_inf_min, num_inf_maj = num_inf_maj, num_hosp = num_hosp, num_rec = num_rec)
    time_series <- bind_rows(time_series,current_time)
  }
  time_series
}

r2p5 <- gen_time_series(prop_capacity = 1)
r1p5 <- gen_time_series(prop_capacity = 1, R_max = 1.5)
r2p5_long <- r2p5 %>% pivot_longer(c(names(r2p5)[-1]), names_to = 'Var') %>% mutate (R = 2.5)
r1p5_long <- r1p5 %>% pivot_longer(c(names(r2p5)[-1]), names_to = 'Var') %>% mutate (R = 1.5)

ggplot(data = bind_rows(r2p5_long,r1p5_long) %>% filter(Var %in% c('num_hosp','num_inf_min','num_inf_maj'))) +
  geom_line(aes(x=time, y = value, col = Var)) +
  facet_wrap(~R)
ggsave('Figs/seir_example.jpg')

explore_decarceration <- function(prop_capacity_arr, R_max_arr) {
  setup_arr <-tibble(prop_capacity = rep(prop_capacity_arr, time = length(R_max_arr)), R_max = rep(R_max_arr, each = length(prop_capacity_arr))) 
  results <- setup_arr %>% group_by(prop_capacity,R_max) %>% do({
    time_series <- gen_time_series(.$prop_capacity, R_max = .$R_max)
    tot_inf <- last(time_series$num_rec)
    prop_inf <- tot_inf / UNIT_SIZE / .$prop_capacity
    time_inf_peak <- time_series$time[which.max(time_series$num_inf_min + time_series$num_inf_maj)]
    max_num_inf <- max(time_series$num_inf_min + time_series$num_inf_maj)
    
    tot_hosp <- tot_inf*PROP_HOSP # Tot # of hospitalizatin
    prop_hosp <- tot_hosp / UNIT_SIZE / .$prop_capacity
    time_hosp_peak <- time_series$time[which.max(time_series$num_hosp)]
    max_num_hosp <- max(time_series$num_hosp)

    tibble(tot_inf = tot_inf, prop_inf = prop_inf, time_inf_peak = time_inf_peak, max_num_inf = max_num_inf,
           tot_hosp = tot_hosp, prop_hosp = prop_hosp, time_hosp_peak = time_hosp_peak, max_num_hosp = max_num_hosp)
  })
}

#decarceration_results <- explore_decarceration(prop_capacity_arr = 1, R_max_arr = seq(1.5,2.5,1))
decarceration_results <- explore_decarceration(prop_capacity_arr = seq(1,.5, by = -.01), R_max_arr = seq(1.5,3.5,0.5))
write.csv(decarceration_results,"decarceration_results.csv",row.names = FALSE)

# Tot # of hospitalizatin
# Peak of bed use
# Timing of peak
# Peak # of beds

p1 <- ggplot(data = decarceration_results) +
  geom_line(aes(x=1-prop_capacity, y = tot_inf, col = as.factor(R_max))) +
  ylab ("Total infected") +
  xlab ("Proportion decarcerated")
(p1)

p2 <- ggplot(data = decarceration_results) +
  geom_line(aes(x=1-prop_capacity, y = prop_inf, col = as.factor(R_max))) +
  ylab ("Proportion infected") +
  xlab ("Proportion decarcerated")
(p2)

p3 <- ggplot(data = decarceration_results) +
  geom_line(aes(x=1-prop_capacity, y = time_inf_peak, col = as.factor(R_max))) +
  ylab ("Time of peak infections (days)") +
  xlab ("Proportion decarcerated")
(p3)

p4 <- ggplot(data = decarceration_results) +
  geom_line(aes(x=1-prop_capacity, y = max_num_inf, col = as.factor(R_max))) +
  ylab ("Maximum infected at once") +
  xlab ("Proportion decarcerated")
(p4)

p_all <- grid.arrange(p1,p2,p3,p4)
ggsave("Figs/seir091420.jpg",plot = p_all)

q1 <- ggplot(data = decarceration_results) +
  geom_line(aes(x=1-prop_capacity, y = tot_hosp, col = as.factor(R_max))) +
  ylab ("Total hospitalized") +
  xlab ("Proportion decarcerated")
(q1)

q2 <- ggplot(data = decarceration_results) +
  geom_line(aes(x=1-prop_capacity, y = prop_hosp, col = as.factor(R_max))) +
  ylab ("Proportion hospitalized") +
  xlab ("Proportion decarcerated")
(q2)

q3 <- ggplot(data = decarceration_results) +
  geom_line(aes(x=1-prop_capacity, y = time_hosp_peak, col = as.factor(R_max))) +
  ylab ("Time of peak hospitalization (days)") +
  xlab ("Proportion decarcerated")
(q3)

q4 <- ggplot(data = decarceration_results) +
  geom_line(aes(x=1-prop_capacity, y = max_num_hosp, col = as.factor(R_max))) +
  ylab ("Maximum hospitalized at once") +
  xlab ("Proportion decarcerated")
(q4)

q_all <- grid.arrange(q1,q2,q3,q4)
ggsave("Figs/seir_hosp091420.jpg",plot = q_all)
