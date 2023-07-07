# load csv file of all countries combined
dat <- read.csv("directory_name")

## libraries
library(tidyverse)
library(tidyr)
library(viridis)

## ggplot parameters
theme_graph <- function (base_size = 15, base_family = "sans") {
  theme(plot.title = element_text(size = 16, face = "bold", hjust=0.5, margin = margin(20, 0, 5, 0)),
        plot.subtitle = element_text(colour = "#000000", size = 14,hjust=0.5, margin = margin(0, 0, 10, 0)),
        plot.caption = element_text(colour = "#000000", size = 12, hjust=1, margin = margin(10, 0, 20, 0)),
        plot.background = element_rect(fill = "#FFFFFF"), 
        panel.background = element_rect(fill = "#FFFFFF", colour = "#000000", linetype = "solid"), 
        panel.grid.major.x = element_line(colour = "#C9C9C9", linetype = "dotted"),
        panel.grid.major.y = element_line(colour = "#C9C9C9", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 12, colour = "#000000", hjust=0.5, face = "bold", margin = margin(10, 0, 10, 0)), 
        axis.title.y = element_blank(), axis.text = element_text(size = 12, colour = "#000000"),
        axis.line.y = element_line(colour = "#000000"),
        axis.line.x = element_line(colour = "#000000"),
        axis.ticks = element_line(colour = "#000000", size = 1),   
        strip.text.x = element_text(size = 12),
        legend.text = element_text(size = 10, colour = "#000000"),
        legend.background = element_rect(fill = "#FFFFFF", colour = "white", size = 0.3, linetype = "blank"), 
        legend.key = element_rect(fill = NA, linetype = "blank"), 
        legend.position = "bottom",
        legend.direction = "horizontal")
}

## order data
dat <- dat %>% 
  mutate(country = factor(country, levels = c("Estonia", "Czechia", "Slovenia", 
                                              "Austria", "Denmark", "Belgium",
                                              "Sweden", "France", "Spain", "Italy")),
         Education = factor(edu, levels = c("low", "medium", "high")), 
         Gender = gender)

DFLE <- dat %>% filter(Expectancy=="Disability-Free")
LE <- dat %>%   filter(Expectancy=="Total") 
DLE <- dat %>%   filter(Expectancy=="Disability") 



###---- DFLE at 50 by edu 
min <- min(DFLE$lower)
max <- max(DFLE$upper)

dfle_plot <- ggplot(DFLE,aes(x = estimate, y = country, color=Education)) +
  geom_point(size=3)+
  geom_errorbar(aes(xmin=lower, xmax=upper), width=0.3, size = 0.5) +
  scale_y_discrete(limits=rev,name= "Country")+
  labs(x="Years")+
  scale_x_continuous(
    limits = c(min, max),
    breaks = c(seq(10, 32, 2))) +
  facet_grid(.~Gender) +
  theme_graph() +
  theme(strip.placement = "outside",
        legend.box.margin=margin(-15,0,0,0),
        legend.background = element_rect(linetype = "blank")) +
  scale_color_viridis(discrete = T, option = "C", end = 0.8)  

dfle_plot

# ggsave("directory_name")

# you can replicate the plot for DLE or LE.
