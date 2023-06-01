# Author: Donata Stonkute
#########################
# This analysis uses data or information from the Harmonized Survey of Health, 
# Ageing and Retirement in Europe dataset and Codebook, Version F as of February 
# 2023 developed by the Gateway to Global Aging Data. The development of the 
# Harmonized [Study] was funded by the National Institute on Aging (R01 AG030153, 
# RC2 AG036619, 1R03AG043052). For more information, please refer to www.g2aging.org
#########################

Sys.setenv(LANG = "en")
library(tidyverse)
library(data.table)

# Set working directory
# setwd("your_directory")

# Read the CSV file 
# dt_full <- read.csv("g2ageing harmonized data file")

# Recode country codes to country names
dt_full$country <- recode(
  dt_full$country,
  "11" = "Austria",
  "12" = "Germany",
  "13" = "Sweden",
  "14" = "Netherlands",
  "15" = "Spain",
  "16" = "Italy",
  "17" = "France",
  "18" = "Denmark",
  "19" = "Greece",
  "20" = "Switzerland",
  "23" = "Belgium",
  "25" = "Israel",
  "28" = "Czechia",
  "29" = "Poland",
  "30" = "Ireland",
  "31" = "Luxembourg",
  "32" = "Hungary",
  "33" = "Portugal",
  "34" = "Slovenia",
  "35" = "Estonia",
  "47" = "Croatia",
  "48" = "Lithuania",
  "51" = "Bulgaria",
  "53" = "Cyprus",
  "55" = "Finland",
  "57" = "Latvia",
  "59" = "Malta",
  "61" = "Romania",
  "63" = "Slovakia"
)

# Interview status --------------------------------------------------------

# excluding not yet identified (=0), alive but did not respond (=4), 
# died before last week (=6), not known (=9)
dt_present <- dt_full %>% 
  filter(iwstat %in% c(1, 5)) 


# Alive -------------------------------------------------------------------

# Filter respondents with interview status 1 (alive)
dt_alive <- dt_present %>% 
  filter(iwstat == 1) 


# Age eligibility -------------------------------------------------------

# Filter out respondents aged less than 50
dt_complete <- dt_alive %>% 
  filter(age >= 50) 


# Adding death indicator for survival analysis ----------------------------

# Set 'dead' column to 0 for all records
data.table::setDT(dt_complete)[, dead := 0]

# Update 'dead' column to 1 for respondents with non-NA 'radyear' values
dt_complete[.(dt_complete[!is.na(radyear), unique(mergeid)]),
            on = .(mergeid),
            mult = "last", dead := 1]

# Baseline indicator ------------------------------------------------------

# Set 'basel' column to 0 for all records
data.table::setDT(dt_complete)[, basel := 0]

# Update 'basel' column to 1 for respondents with unique 'mergeid' values
dt_complete[.(dt_complete[, unique(mergeid)]),
            on = .(mergeid),
            mult = "first", basel := 1]


# Data frame for analysis, renaming variables -----------------------------

# Rename and transform variables in dt_complete using dplyr
dt_complete <- dt_complete %>%
  mutate(
    gender = ifelse(ragender == 1, "man", "woman"),
    edu = case_when(
      raeducl == 1 ~ "low",
      raeducl == 2 ~ "medium",
      raeducl == 3 ~ "high"
    ),
    gali = hlthlma,
    adl_bi = ifelse(adl == 0, yes = 0, no = 1)
  ) %>% 
  select(mergeid, country, wave, gender, edu, age, adl_bi, gali, radyear, dead, basel)

# Analytical sample formatting --------------------------------------------

# Transform variables and create additional columns in dt_complete using dplyr
dt <- dt_complete %>%
  mutate(
    low = ifelse(edu == "low", 1, 0),
    medium = ifelse(edu == "medium", 1, 0),
    high = ifelse(edu == "high", 1, 0),
    edu = factor(edu, levels = c("low", "medium", "high")),
    year = case_when(
      wave == 1 ~ 2004,
      wave == 2 ~ 2007,
      wave == 3 ~ 2009,
      wave == 4 ~ 2011,
      wave == 5 ~ 2013,
      wave == 6 ~ 2015,
      wave == 7 ~ 2017,
      wave == 8 ~ 2019
    )
  )

## COVID-19 period deaths ----------------------------------------------------

# Right censor deaths after 2019 (=going back in time)
dtt <- dt %>%
  mutate(
    covid_dd = ifelse(radyear %in% c(2020, 2021), yes = 1, no = 0),
    dead = ifelse(covid_dd == 1, yes = 0, no = dead),
    radyear = ifelse(covid_dd == 1, yes = NA, no = radyear)
  )


# Add deaths --------------------------------------------------------------

# Arrange dtt by mergeid and descending wave, and add deaths to the dataset
dd <- dtt %>% arrange(mergeid, desc(wave)) %>%
  filter(!is.na(radyear)) %>%
  group_by(mergeid) %>%
  slice_head() %>%
  mutate(wave = wave + 1, age = age + 2, gali = 99) %>%
  ungroup()

dat_dd <- rbind(dtt, dd)

# Clean death records
duomenys <- dat_dd %>% 
  arrange(mergeid, wave) %>%
  mutate(dead = ifelse(gali == 99, 1, 0))

dtw <- duomenys %>%
  group_by(mergeid) %>%
  mutate(wave_lead = lead(wave),
         wave_diff = wave_lead - wave) %>%
  ungroup()


## Non-consecutive waves ---------------------------------------------------

dtf <- dtw  # Create a temporary dataframe for non-consecutive waves

# Create a 'grp' column indicating if difference between waves is more than 1 (non-consecutive)
setDT(dtf)[, grp := cumsum(c(0, diff(wave)) > 1), by = mergeid] 

dtf[, ID := .GRP, by = .(mergeid, grp)]
dtf[, time := .N, by = ID]


# Remove both non-consecutive observations and those that participated only in one wave
dat <- as.data.frame(dtf) %>% 
  filter(time > 1)


# Filter out last wave and wave 9 for analysis
dati <- dat %>% 
  mutate(lastw = ifelse(wave > 7 & dead == 0, 1, 0)) %>% 
  filter(lastw == 0, wave != 9)
dati <- dati[, -c(15:22)] # unnecessary columns


# Omit missing values -------------------------------------------------

dt_full <- dati %>% 
  filter(
    !is.na(gender) &
      !is.na(edu) &
      !is.na(age) &
      !is.na(adl_bi) &
      !is.na(gali)
  )


## Country selection -------------------------------------------------------

final_dta <- dt_full %>% 
  filter(
    country %in% c(
      "Austria",
      "Belgium",
      "Czechia",
      "Denmark",
      "Estonia",
      "Spain",
      "France",
      "Italy",
      "Sweden",
      "Slovenia"
    )
  )


sample_info <- as.data.frame(table(final_dta$country))
colnames(sample_info) <- c("Country", "Observations")

# write.csv(final_dta, "file directory, name.csv", row.names = FALSE)


# Saving individual country files -----------------------------------------

# setwd("data folder")

austria <- final_dta %>% filter(country=="Austria")
belgium <- final_dta %>% filter(country=="Belgium")
czechia <- final_dta %>% filter(country=="Czechia")
denmark <- final_dta %>% filter(country=="Denmark")
estonia <- final_dta %>% filter(country=="Estonia")
france <- final_dta %>% filter(country=="France")
italy <- final_dta %>% filter(country=="Italy")
slovenia <- final_dta %>% filter(country=="Slovenia")
spain <- final_dta %>% filter(country=="Spain")
sweden <- final_dta %>% filter(country=="Sweden")

write.csv(austria, "Austria.csv", row.names=FALSE)
write.csv(belgium, "Belgium.csv", row.names=FALSE)
write.csv(czechia, "Czechia.csv", row.names=FALSE)
write.csv(denmark, "Denmark.csv", row.names=FALSE)
write.csv(estonia, "Estonia.csv", row.names=FALSE)
write.csv(france, "France.csv", row.names=FALSE)
write.csv(italy, "Italy.csv", row.names=FALSE)
write.csv(slovenia, "Slovenia.csv", row.names=FALSE)
write.csv(spain, "Spain.csv", row.names=FALSE)
write.csv(sweden, "Sweden.csv", row.names=FALSE)
