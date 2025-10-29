# Clear environment
rm(list=ls()); graphics.off(); cat("\f")

# Set working directory
setwd("~/Desktop/Papers/Sediment Paper/Code/")

# Load packages
library(vegan)
library(tidyverse)
library(goeveg)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(FSA)

# Read dataset
df <- read_csv('Intertidal_Sed_MPmass_SHP_v4.csv')

# Transform 1st Order Concentration data into long format for ggplot
df_1stOrder_Conc <- df %>%
  select(Sample, Site, Location, Depth, Av_Conc_NonFiber, Av_Conc_Fiber) %>%
  pivot_longer(cols = c( Av_Conc_NonFiber, Av_Conc_Fiber), 
               names_to = "Particle_Type", values_to = "Concentration")

#df_1stOrder_Conc <- df_1stOrder_Conc %>%
#  mutate(Concentration = Concentration * 1000)  # convert g/g → mg/g


#Add * to the UNB-2 samples that did not process below 500um
df_1stOrder_Conc <- df_1stOrder_Conc %>%
  mutate(mark_star = ifelse(
    Location == "UNB-2" & Depth %in% c('10-15cm', '15-20cm', '20-25cm', '25-30cm', '30-35cm', '35-40cm', '40-45cm', '45-50cm', '50-55cm') & Particle_Type == "Av_Conc_Fiber",
    "*", ""
  ))


#################################################################################

# Plot stacked bar chart for 1st Order Concentration

# Define label replacements for Particle_Type
conc_labels <- c(
  Av_Conc_NonFiber = "Non-Fiber",
  Av_Conc_Fiber = "Fiber"
)

#Plot
p <- ggplot(df_1stOrder_Conc, aes(x = Depth, y = Concentration, fill = Particle_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    aes(label = mark_star),
    position = position_stack(vjust = 1.05),
    size = 5
  )+
  facet_wrap(~ Location, scales = "free_y") +  # Create one panel per Location
  #geom_vline(xintercept = vline_positions, color = "black", linetype = "solid", size = 0.5) +
  theme_bw() +
  labs(title = "Mass-based MP Concentration: Intertidal Sediments",
       x = NULL, y = expression(Concentration~"("*mg/g~"dw sediment"*")"),
       fill = "Particle Type") +
  scale_fill_manual(
    values = c("steelblue", "darkorange"),
    labels = conc_labels
  ) +
  theme(
    text = element_text(family = "Times New Roman"),# global font
    strip.text = element_text(family = "Times New Roman",     # facet labels
                              size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text.y = element_text(size = 12),
    plot.title  = element_text(size = 12, face = "bold")
  )
p

p <- p + labs(
  caption = "*indicates the 250-500 um sieved size class was not completed"
) +
  theme(
    plot.caption = element_text(hjust = 0.5, size = 10, face = "italic")
  )

p

#################################################################################
################################################################################

#Plot stacked bar charts for Morphological Composition


# Transform All MP Composition data into long format for ggplot
df_All_Comp <- df %>%
  select(Sample, Site, Location, Depth, Perc_fiber, Perc_fiberbun, Perc_film, Perc_sphere, Perc_foam, Perc_fragment, Perc_pellet, Perc_TRWP) %>%
  pivot_longer(cols = c(Perc_fiber, Perc_fiberbun, Perc_film, Perc_sphere, Perc_foam, Perc_fragment, Perc_pellet, Perc_TRWP), 
               names_to = "Particle_Type", values_to = "Composition")

#Add * to the UNB-2 samples that did not process below 500um
df_All_Comp<- df_All_Comp %>%
  mutate(mark_star = ifelse(
    Location == "UNB-2" & Depth %in% c('10-15cm', '15-20cm', '20-25cm', '25-30cm', '30-35cm', '35-40cm', '40-45cm', '45-50cm', '50-55cm') & Particle_Type == "Perc_fiber",
    "*", ""
  ))

# Define label replacements for Particle_Type
comp_labels <- c(
  Perc_fiber = "fiber",
  Perc_fiberbun = "fiber bundle",
  Perc_film = "film",
  Perc_sphere = "sphere",
  Perc_pellet = "pellet",
  Perc_foam = "foam",
  Perc_fragment = "fragment",
  Perc_TRWP = "TRWP"
)

df_All_Comp <- df_All_Comp %>%
  mutate(Composition = ifelse(is.na(Composition), 0, Composition))


# Plot stacked bar chart for 1st Order Concentration
p<- ggplot(df_All_Comp, aes(x = Depth, y = Composition * 100, fill = Particle_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    aes(label = mark_star),
    position = position_stack(vjust = 1.0),
    size = 5
  )+
  facet_wrap(~ Location) +  # Create one panel per Location
  theme_bw() +
  labs(title = "Mass-based Morphological MP Composition",
       x = "Site", y = "Morphological Mass Composition [%]",
       fill = "Morphology") +
  scale_fill_manual(
    values = RColorBrewer::brewer.pal(n = 8, name = "Set2"),
    labels = comp_labels
  ) +
  theme(
    text = element_text(family = "Times New Roman"),# global font
    strip.text = element_text(family = "Times New Roman",     # facet labels
                              size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text.y = element_text(size = 12),
    plot.title  = element_text(size = 12, face = "bold")
  )

p

p <- p + labs(
  caption = "*indicates the 250-500 um sieved size class was not completed"
) +
  theme(
    plot.caption = element_text(hjust = 0.5, size = 10, face = "italic")
  )

p
