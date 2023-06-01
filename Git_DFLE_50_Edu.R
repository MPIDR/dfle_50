# Author: Donata Stonkute
###############################################################################
# --------------------------------
# This script includes discrete-time regression models, life table simulation  
# and the Sullivan method for a single country.
# This produces gender- and education-specific life, disability and disability-free  
# expectancies at age 50 that are saved in data files.
# Please check compatibility of your computer  for running parallel computing
# --------------------------------


Sys.setenv(LANG = "en")

# libraries
library(tidyverse)
library(future)
library(furrr)
library(data.table)
library(tictoc)

# Set working directory
# setwd("your_directory")

# get necessary functions
source('functions_P1.R')

# If you followed by code in "Analytical_sample_prep.R", you should have a folder 
# "Data" with subdata files of individual countries.
# Here I will provide an example using Austria.
dat <- fread("Data/Austria.csv") 

dtm <- dat %>% filter(gender=="man")
dtf <- dat %>% filter(gender=="woman")

age <- seq(50, 110, 2) # panel structure

bs_it = 1000 # number of bootstraps

## Men -------------------------------------------------------------------

# create empty vectors to store the results
men_start <- c()

tic() # for time monitoring
plan(multisession, workers = 10) # if more cores chosen, process becomes slower
# plan(sequential) # if run locally, instead of servers


men_start <- 1:bs_it %>% furrr::future_map(~ { # ~ everything on the left
  age <- seq(50, 110, 2) # panel structure
  
  resampled_data <- long_resample(dat=dtm, id= "mergeid", time= "wave")
  
  ###- prevalence vector preparation -###
  labels <- c(seq(50, 90, 2))
  
  prevalence <-  resampled_data %>%
    mutate(
      age = cut(
        age,
        breaks = c(seq(50, 90, by = 2), 120),
        labels = labels,
        right = FALSE
      ),
      edu = factor(edu, levels = c("low", "medium", "high")),
      gender = factor(gender, levels = c("woman", "man"))
    ) %>%
    filter(gali != 99 # we estimate prevalence among alive only
    )
  
  ## By GALI
  wx <- prevalence %>%
    group_by(country, gender, edu, age) %>% 
    mutate(wx = mean(gali)) %>%
    ungroup() %>%
    select(country, gender, edu, age, wx) %>%
    distinct() %>% 
    arrange(country,gender, edu, age) 
  
  
  # discrete-time models - probabilities of death / survival
  fit_age <-
    glm(dead ~ age + I(age ^ 2) + low + high ,
        data = resampled_data,
        family = binomial(link = "cloglog"))
  
  # predict px given the model 
  surv_low_m <- 1-predict(fit_age,data.frame(age=seq(50,110,2), low=1, high=0),type = "response")
  surv_mid_m <- 1- predict(fit_age,data.frame(age=seq(50,110,2), low=0, high=0),type = "response")
  surv_high_m <- 1- predict(fit_age,data.frame(age=seq(50,110,2), low=0, high=1),type = "response")
  
  # last age group - no immortality 
  surv_low_m[length(surv_low_m)] <- 0
  surv_mid_m[length(surv_mid_m)] <- 0
  surv_high_m[length(surv_high_m)] <- 0
  
  qx_low_m <- 1 - surv_low_m
  qx_mid_m <- 1 - surv_mid_m
  qx_high_m <- 1 - surv_high_m
  
  ### low edu -----------------------------------------------------------------
  
  # simulate lifetimes
  LT_low_m <- sim_lt(eqx=qx_low_m, eta=age, n=2)
  # truncate LT to 90+
  LT_low_m <- LT_low_m %>% 
    filter(age<=90) %>% 
    mutate(Lx=ifelse(Lx==tail(Lx, 1), yes=Tx, no=Lx))
  
  LE_low_m <- LT_low_m$ex[LT_low_m$age == 50]
  
  wx_low_m <- wx %>%
    filter(country == "Austria",
           gender == "man",
           edu == "low") %>%
    pull(wx)
  
  DFLE_low_m <- exDF(lx = LT_low_m$lx, wx = wx_low_m, Lx = LT_low_m$Lx)
  DLE_low_m <- LE_low_m - DFLE_low_m

  ## mid edu -----------------------------------------------------------------
  
  LT_mid_m <- sim_lt(eqx=qx_mid_m, eta=age, n=2)
  
  LT_mid_m <- LT_mid_m %>% 
    filter(age<=90) %>% 
    mutate(Lx=ifelse(Lx==tail(Lx, 1), yes=Tx, no=Lx))
  
  LE_mid_m <- LT_mid_m$ex[LT_mid_m$age == 50]
  
  wx_mid_m <- wx %>%
    filter(country == "Austria",
           gender == "man",
           edu == "medium") %>%
    pull(wx)
  
  DFLE_mid_m <- exDF(lx = LT_mid_m$lx, wx = wx_mid_m, Lx = LT_mid_m$Lx)
  DLE_mid_m <- LE_mid_m - DFLE_mid_m

  
  ## high edu -----------------------------------------------------------------
  
  LT_high_m <- sim_lt(eqx=qx_high_m, eta=age, n=2)
  LT_high_m <- LT_high_m %>% 
    filter(age<=90) %>% 
    mutate(Lx=ifelse(Lx==tail(Lx, 1), yes=Tx, no=Lx))
  
  LE_high_m <- LT_high_m$ex[LT_high_m$age == 50]
  
  wx_high_m <- wx %>%
    filter(country == "Austria",
           gender == "man",
           edu == "high") %>%
    pull(wx)
  
  DFLE_high_m <- exDF(lx = LT_high_m$lx, wx = wx_high_m, Lx = LT_high_m$Lx)
  DLE_high_m <- LE_high_m - DFLE_high_m
  
  # bind the results of each iteration in the object 
  results <- data.table(LE_low_m, DFLE_low_m, DLE_low_m, 
                        LE_mid_m, DFLE_mid_m, DLE_mid_m,
                        LE_high_m, DFLE_high_m, DLE_high_m)
  results 
  
}
# keep results fixed between different runs, but different between iterations
, .options = furrr_options(seed = 1234)
)

men_dt <- rbindlist(men_start)
head(men_dt)
toc()

plan(sequential) # closing cores

### Conf Interv
# low
LE_low_m <- confidence_interval(men_dt$LE_low_m)
DFLE_low_m <- confidence_interval(men_dt$DFLE_low_m)
DLE_low_m <- confidence_interval(men_dt$DLE_low_m)

low_m <- as.data.frame(rbind(LE_low_m, DFLE_low_m, DLE_low_m)) %>% 
  mutate(
    estimate = V1,
    lower = `2.5%`,
    upper = `97.5%`,
    country = "Austria", 
    gender = "Men",
    edu = "low",
    Expectancy = c("Total", "Disability-Free", "Disability")
  ) %>% 
  select(country, gender, edu, Expectancy, estimate, lower, upper)

# mid
LE_mid_m <- confidence_interval(men_dt$LE_mid_m)
DFLE_mid_m <- confidence_interval(men_dt$DFLE_mid_m)
DLE_mid_m <- confidence_interval(men_dt$DLE_mid_m)

mid_m <- as.data.frame(rbind(LE_mid_m, DFLE_mid_m, DLE_mid_m)) %>% 
  mutate(
    estimate = V1,
    lower = `2.5%`,
    upper = `97.5%`,
    country = "Austria", 
    gender = "Men",
    edu = "medium",
    Expectancy = c("Total", "Disability-Free", "Disability")) %>% 
  select(country, gender, edu, Expectancy, estimate, lower, upper)

# high
LE_high_m <- confidence_interval(men_dt$LE_high_m)
DFLE_high_m <- confidence_interval(men_dt$DFLE_high_m)
DLE_high_m <- confidence_interval(men_dt$DLE_high_m)

high_m <- as.data.frame(rbind(LE_high_m, DFLE_high_m, DLE_high_m)) %>% 
  mutate(
    estimate = V1,
    lower = `2.5%`,
    upper = `97.5%`,
    country = "Austria", 
    gender = "Men",
    edu = "high",
    Expectancy = c("Total", "Disability-Free", "Disability")) %>% 
  select(country, gender, edu, Expectancy, estimate, lower, upper)

# combine
men_ext <- as.data.frame(rbind(low_m, mid_m, high_m))
head(men_ext)



# Women -------------------------------------------------------------------

women_start <- c()

tic() # for time monitoring
plan(multisession, workers = 10) # if more cores chosen, process becomes slower
# plan(sequential) # if run locally, instead of servers


women_start <- 1:bs_it %>% furrr::future_map(~ { # ~ everything on the left
  age <- seq(50, 110, 2) # panel structure
  
  resampled_data <- long_resample(dat=dtf, id= "mergeid", time= "wave")
  
  ###- prevalence vector preparation -###
  labels <- c(seq(50, 90, 2))
  
  prevalence <-  resampled_data %>%
    mutate(
      age = cut(
        age,
        breaks = c(seq(50, 90, by = 2), 120),
        labels = labels,
        right = FALSE
      ),
      edu = factor(edu, levels = c("low", "medium", "high")),
      gender = factor(gender, levels = c("woman", "man"))
    ) %>%
    filter(gali != 99 # we estimate prevalence among alive only
    )
  
  ## By GALI
  wx <- prevalence %>%
    group_by(country, gender, edu, age) %>% 
    mutate(wx = mean(gali)) %>%
    ungroup() %>%
    select(country, gender, edu, age, wx) %>%
    distinct() %>% 
    arrange(country,gender, edu, age) 
  
  # discrete-time models - probabilities of death / survival
  fit_age <-
    glm(dead ~ age + I(age ^ 2) + low + high ,
        data = resampled_data,
        family = binomial(link = "cloglog"))
  
  # predict px given the model 
  surv_low_f <- 1-predict(fit_age,data.frame(age=seq(50,110,2), low=1, high=0),type = "response")
  surv_mid_f <- 1- predict(fit_age,data.frame(age=seq(50,110,2), low=0, high=0),type = "response")
  surv_high_f <- 1- predict(fit_age,data.frame(age=seq(50,110,2), low=0, high=1),type = "response")
  
  # last age group - no immortality 
  surv_low_f[length(surv_low_f)] <- 0
  surv_mid_f[length(surv_mid_f)] <- 0
  surv_high_f[length(surv_high_f)] <- 0
  
  qx_low_f <- 1 - surv_low_f
  qx_mid_f <- 1 - surv_mid_f
  qx_high_f <- 1 - surv_high_f
  
  ### low edu -----------------------------------------------------------------
  
  # simulate lifetimes
  LT_low_f <- sim_lt(eqx=qx_low_f, eta=age, n=2)
  # truncate LT to 90+
  LT_low_f <- LT_low_f %>% 
    filter(age<=90) %>% 
    mutate(Lx=ifelse(Lx==tail(Lx, 1), yes=Tx, no=Lx))
  
  LE_low_f <- LT_low_f$ex[LT_low_f$age == 50]
  
  wx_low_f <- wx %>%
    filter(country == "Austria",
           gender == "woman",
           edu == "low") %>%
    pull(wx)
  
  DFLE_low_f <- exDF(lx = LT_low_f$lx, wx = wx_low_f, Lx = LT_low_f$Lx)
  DLE_low_f <- LE_low_f - DFLE_low_f
  
  ## mid edu -----------------------------------------------------------------
  
  LT_mid_f <- sim_lt(eqx=qx_mid_f, eta=age, n=2)
  
  LT_mid_f <- LT_mid_f %>% 
    filter(age<=90) %>% 
    mutate(Lx=ifelse(Lx==tail(Lx, 1), yes=Tx, no=Lx))
  
  LE_mid_f <- LT_mid_f$ex[LT_mid_f$age == 50]
  
  wx_mid_f <- wx %>%
    filter(country == "Austria",
           gender == "woman",
           edu == "medium") %>%
    pull(wx)
  
  DFLE_mid_f <- exDF(lx = LT_mid_f$lx, wx = wx_mid_f, Lx = LT_mid_f$Lx)
  DLE_mid_f <- LE_mid_f - DFLE_mid_f
  
  
  ## high edu -----------------------------------------------------------------
  
  LT_high_f <- sim_lt(eqx=qx_high_f, eta=age, n=2)
  LT_high_f <- LT_high_f %>% 
    filter(age<=90) %>% 
    mutate(Lx=ifelse(Lx==tail(Lx, 1), yes=Tx, no=Lx))
  
  LE_high_f <- LT_high_f$ex[LT_high_f$age == 50]
  
  wx_high_f <- wx %>%
    filter(country == "Austria",
           gender == "woman",
           edu == "high") %>%
    pull(wx)
  
  DFLE_high_f <- exDF(lx = LT_high_f$lx, wx = wx_high_f, Lx = LT_high_f$Lx)
  DLE_high_f <- LE_high_f - DFLE_high_f
  
  # bind the results of each iteration in the object 
  results <- data.table(LE_low_f, DFLE_low_f, DLE_low_f, 
                        LE_mid_f, DFLE_mid_f, DLE_mid_f,
                        LE_high_f, DFLE_high_f, DLE_high_f)
  results 
  
}
# keep results fixed between different runs, but different between iterations
, .options = furrr_options(seed = 1234)
)

women_dt <- rbindlist(women_start)
head(women_dt)
toc()

plan(sequential) # closing cores

### Confidence Intervals
# low
LE_low_f <- confidence_interval(women_dt$LE_low_f)
DFLE_low_f <- confidence_interval(women_dt$DFLE_low_f)
DLE_low_f <- confidence_interval(women_dt$DLE_low_f)

low_f <- as.data.frame(rbind(LE_low_f, DFLE_low_f, DLE_low_f)) %>% 
  mutate(
    estimate = V1,
    lower = `2.5%`,
    upper = `97.5%`,
    country = "Austria", 
    gender = "Women",
    edu = "low",
    Expectancy = c("Total", "Disability-Free", "Disability")
  ) %>% 
  select(country, gender, edu, Expectancy, estimate, lower, upper)

# mid
LE_mid_f <- confidence_interval(women_dt$LE_mid_f)
DFLE_mid_f <- confidence_interval(women_dt$DFLE_mid_f)
DLE_mid_f <- confidence_interval(women_dt$DLE_mid_f)

mid_f <- as.data.frame(rbind(LE_mid_f, DFLE_mid_f, DLE_mid_f)) %>% 
  mutate(
    estimate = V1,
    lower = `2.5%`,
    upper = `97.5%`,
    country = "Austria", 
    gender = "Women",
    edu = "medium",
    Expectancy = c("Total", "Disability-Free", "Disability")) %>% 
  select(country, gender, edu, Expectancy, estimate, lower, upper)

# high
LE_high_f <- confidence_interval(women_dt$LE_high_f)
DFLE_high_f <- confidence_interval(women_dt$DFLE_high_f)
DLE_high_f <- confidence_interval(women_dt$DLE_high_f)

high_f <- as.data.frame(rbind(LE_high_f, DFLE_high_f, DLE_high_f)) %>% 
  mutate(
    estimate = V1,
    lower = `2.5%`,
    upper = `97.5%`,
    country = "Austria", 
    gender = "Women",
    edu = "high",
    Expectancy = c("Total", "Disability-Free", "Disability")) %>% 
  select(country, gender, edu, Expectancy, estimate, lower, upper)

# combine
women_ext <- as.data.frame(rbind(low_f, mid_f, high_f))
head(women_ext)

# both genders
AUT <- rbind(men_ext, women_ext)
# save it in "Results" folder
write.csv(AUT, "directory_name")






