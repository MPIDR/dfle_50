# This code combines country-specific results into one data frame

# libraries
library(tidyverse)

# Main estimates ----------------------------------------------------------

AUT <- read.csv("directory_name") # for Austria
BEL <- read.csv("directory_name") # for Belgium
CZE <- read.csv("directory_name") # for Czechia
DNK <- read.csv("directory_name") # for Denmark
EST <- read.csv("directory_name") # for Estonia
ESP <- read.csv("directory_name") # for Spain
ITA <- read.csv("directory_name") # for Italy
FRA <- read.csv("directory_name") # for France
SVN <- read.csv("directory_name") # for Slovenia
SWE <- read.csv("directory_name") # for Sweden

# combining data into global
all <- do.call("rbind", list(AUT, BEL, CZE, DNK, EST, ESP, ITA, FRA, SVN, SWE))
# write.csv(all, "directory_name", row.names = FALSE)