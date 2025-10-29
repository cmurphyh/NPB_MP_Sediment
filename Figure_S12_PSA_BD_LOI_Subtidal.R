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
library(viridis)
library(patchwork)

# Read dataset
df <- read_csv('Surface_Sed_MP_SHP_v4.csv')

df <- df %>%
  filter(Sample != 'SUB-4-10-S1') %>%
  filter(Sample !=  'SUB-4-7-S1') %>%
  filter(Sample !=  'SUB-4-9-S1') %>%
  filter(Sample != 'UNB-1-00-05') %>%
  filter(Sample != 'UNB-2-00-05') %>%
  filter(Sample != 'UNB-3-00-05')

################################################################################
#Transform data into long-format for plotting

# Transform All Textures into long format for ggplot
df_texture <- df %>%
  select(Sample, Site, Perc_VCOARSESAND, Perc_COARSESAND, Perc_MSAND, Perc_FSAND, Perc_VFSAND, Perc_VCOARSESILT, Perc_COARSESILT, Perc_MEDIUMSILT, Perc_FSILT, Perc_VFSILT, Perc_CLAY ) %>%
  pivot_longer(cols = c(Perc_VCOARSESAND, Perc_COARSESAND, Perc_MSAND, Perc_FSAND, Perc_VFSAND, Perc_VCOARSESILT, Perc_COARSESILT, Perc_MEDIUMSILT, Perc_FSILT, Perc_VFSILT, Perc_CLAY), 
               names_to = "Sediment_Type", values_to = "Texture")

# Transform SAND vs MUD data into long format for ggplot
df_Sand_Mud <- df %>%
  select(Sample, Site,  SAND_Perc, MUD_Perc) %>%
  pivot_longer(cols = c( SAND_Perc, MUD_Perc), 
               names_to = "Sediment_Type", values_to = "Texture")

# Filter and rename for clarity
df_BDLOI <- df %>%
  select(Sample, `Av_ DryBD_gcc-1`, `StdDev_DryBD_gcc-1`, `Av_Perc_OM`, `StdDev_Perc_OM`) %>%
  rename(
    Av_DryBD = `Av_ DryBD_gcc-1`,
    SD_DryBD = `StdDev_DryBD_gcc-1`,
    Mean_Percent_OM = `Av_Perc_OM`,
    SD_Percent_OM = `StdDev_Perc_OM`)


#Figures for Illustration
################################################################################
# (1) Stacked bar chart for Morphological Composition

# Define label replacements for Texture
comp_labels <- c(
  Perc_VCOARSESAND = "Very Coarse Sand",
  Perc_COARSESAND= "Coarse Sand",
  Perc_MSAND = "Medium Sand", 
  Perc_FSAND= "Fine Sand",
  Perc_VFSAND= "Very Fine Sand",
  Perc_VCOARSESILT= "Very Coarse Silt",
  Perc_COARSESILT = "Coarse Silt",
  Perc_MEDIUMSILT = "Medium Silt",
  Perc_FSILT = "Fine Silt",
  Perc_VFSILT = "Very Fine Silt",
  Perc_CLAY = "Clay" )


df_texture <- df_texture %>%
  mutate(Texture = ifelse(is.na(Texture), 0, Texture))

sediment_order <- c(
  "Perc_VCOARSESAND", "Perc_COARSESAND", "Perc_MSAND", "Perc_FSAND", "Perc_VFSAND",
  "Perc_VCOARSESILT", "Perc_COARSESILT", "Perc_MEDIUMSILT", "Perc_FSILT", "Perc_VFSILT",
  "Perc_CLAY"
)


df_texture <- df_texture %>%
  mutate(
    Sediment_Type = factor(Sediment_Type, levels = sediment_order),
    Texture = ifelse(is.na(Texture), 0, Texture)
  )


#Map sediment type to grain size index (1 = coarsest, 11 = finest)
grain_size_index <- setNames(1:length(sediment_order), sediment_order)

df_texture <- df_texture %>%
  mutate(
    Grain_Size_Index = grain_size_index[Sediment_Type],
    Sediment_Type = factor(Sediment_Type, levels = sediment_order)
  )

# Plot
p1 <- ggplot(df_texture, aes(x = Sample, y = Texture * 100, fill = Grain_Size_Index)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(
    #title = "Surface Sediment Grain Size",
    x = NULL, y = "Grain Size [%]",
    fill = NULL) +
  scale_fill_gradientn(
    colours = viridis::viridis(11),
    breaks = 1:11,
    labels = comp_labels[sediment_order]) +
  theme(axis.text.x = element_blank(),  # Hide x-axis labels
        axis.ticks.x = element_blank(),
        legend.key.width = unit(0.75, "cm"),       # Wider legend keys
        legend.key.height = unit(1.5, "cm"))


################################################################################
# (2) Stacked bar chart for Sand vs Mud

# Define label replacements for Particle_Type
conc_labels <- c(
  SAND_Perc = "Sand",
  MUD_Perc = "Mud")

#Plot
p2<- ggplot(df_Sand_Mud, aes(x = Sample, y = Texture * 100, fill = Sediment_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(#title = "Surface Sediment Texture",
       x = NULL, 
       y = "Sediment Type [%]",
       fill = "Sediment Class") +
  scale_fill_manual(values = c("lightgreen", "darkgreen"),
    labels = conc_labels) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

################################################################################
# (3) Bar chart for Dry Bulk Density (g/cc) 

# Plot
p3 <- ggplot(df_BDLOI, aes(x = Sample, y = Av_DryBD)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = Av_DryBD - SD_DryBD, ymax = Av_DryBD + SD_DryBD),
                width = 0.2) +
  theme_minimal() +
  labs(#title = "Dry Bulk Density by Sample",
    x = NULL,
    y = expression(atop("Dry Bulk", paste(Density~"(g"^{1}~cc^{-1}*")"))))  +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


################################################################################
# (4) Bar chart for Organic Matter Content (%) 

# Plot
p4 <- ggplot(df_BDLOI, aes(x = Sample, y = Mean_Percent_OM)) +
  geom_bar(stat = "identity", fill = "gold") +
  geom_errorbar(aes(ymin = Mean_Percent_OM - SD_Percent_OM, ymax = Mean_Percent_OM + SD_Percent_OM),
                width = 0.2) +
  theme_minimal() +
  labs(#title = "Organic Matter Content (%)",
       x = NULL,
       y=expression(atop("Organic Matter", paste("Content (%)")))) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1))


################################################################################
p1 <- p1 +
  scale_x_discrete(position = "top") +
  theme(
    axis.title.x = element_text(),
    axis.text.x = element_text(angle = 90, hjust = 0)
  )

add_vlines <- function(plot, positions = c(6.5, 12.5, 16.5)) {
  plot + geom_vline(xintercept = positions, color = "black", linetype = "solid", size = 0.5)
}

p1 <- add_vlines(p1)
p2 <- add_vlines(p2)
p3 <- add_vlines(p3)
p4 <- add_vlines(p4)


# Combine with patchwork
combined_plot <- (p1 / p2 / p3/ p4) + plot_layout(heights = c(2, 0.75, 1, 1)) & 
  theme(legend.position = "right")


# Display the plot
combined_plot



################################################################################



