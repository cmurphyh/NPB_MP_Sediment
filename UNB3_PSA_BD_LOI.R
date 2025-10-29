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

# Load and tidy the data
df <- read.csv("UNB3_plot.csv") #replicates removed and gaps extrapolated

#df <- df %>%
#  filter(Sample != 'SUB-4-10-S1') %>%


#Figures for Illustration
################################################################################
# (1) Stacked bar chart for Sediment Grain Size

df_texture <- df %>%
  select(Top, Bottom, width, plot, Perc_VCOARSESAND, Perc_COARSESAND, Perc_MSAND, Perc_FSAND, Perc_VFSAND, Perc_VCOARSESILT, Perc_COARSESILT, Perc_MEDIUMSILT, Perc_FSILT, Perc_VFSILT, Perc_CLAY) %>%
  rename(Depth = plot) %>%
  pivot_longer(cols = c(Perc_VCOARSESAND, Perc_COARSESAND, Perc_MSAND, Perc_FSAND, Perc_VFSAND, Perc_VCOARSESILT, Perc_COARSESILT, Perc_MEDIUMSILT, Perc_FSILT, Perc_VFSILT, Perc_CLAY), 
               names_to = "Sediment_Type", values_to = "Texture")


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


# Plot: Stacked horizontal bar chart of grain size vs. depth
p1 <- ggplot(df_texture, aes(x = Texture * 100, y = Depth, fill = Grain_Size_Index)) +
  geom_bar(stat = "identity", position = "stack", orientation = "y") +  # Horizontal bar stack
  coord_cartesian(ylim = c(50, 0)) +
  scale_y_reverse(expand = c(0, 0)) +  # Depth increases downward
  scale_x_continuous(position = "top") + 
  theme_minimal() +
  labs(
    x = "Grain Size (%)",
    y = "Depth (cm bgs)",
    title = "UNB-3",
    fill = NULL
  ) +
  scale_fill_gradientn(
    colours = viridis::viridis(11),
    breaks = 1:11,
    labels = comp_labels[sediment_order]
  )+ 
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.text = element_text(size = 10, angle = 90, hjust = 1),
    legend.key.width = unit(1.5, "cm"),       # Wider legend keys
    legend.key.height = unit(0.5, "cm"),      # Taller keys if needed
    legend.title = element_text(size = 12),
    legend.box.just = "center")

p1

################################################################################
# (2) Stacked bar chart for Sand vs Mud

# Transform SAND vs MUD data into long format for ggplot
df_Sand_Mud <- df %>%
  select(Top, Bottom, width, plot, SAND_Perc, MUD_Perc) %>%
  rename(Depth = plot) %>%
  pivot_longer(cols = c( SAND_Perc, MUD_Perc), 
               names_to = "Sediment_Type", values_to = "Texture")

# Define label replacements for Particle_Type
conc_labels <- c(
  SAND_Perc = "Sand",
  MUD_Perc = "Mud")

#Plot
p2<- ggplot(df_Sand_Mud, aes(x = Texture * 100, y = Depth, fill = Sediment_Type)) +
  geom_bar(stat = "identity", position = "stack", orientation = "y") +
  coord_cartesian(ylim = c(50, 0)) +
  scale_y_reverse(expand = c(0, 0)) +  # Depth increases downward
  scale_x_continuous(position = "top") + 
  theme_minimal() +
  labs(#title = "UNB-3",
    y = NULL, 
    x = "Sediment Type [%]",
    fill = NULL) +
  scale_fill_manual(values = c("lightgreen", "darkgreen"),
                    labels = conc_labels) +
  theme(  legend.position = "bottom",
          legend.direction = "vertical",
          legend.box = "vertical",
          legend.text = element_text(size = 10, angle = 0, hjust = 0),
          legend.key.width = unit(1, "cm"),       # Wider legend keys
          legend.key.height = unit(0.5, "cm"),      # Taller keys if needed
          legend.title = element_text(size = 12),
          legend.box.just = "center")

p2

################################################################################
# (3) Bar chart for Dry Bulk Density (g/cc) 

# Load and tidy the data
df_BDLOI <- read.csv("UNB3_BDLOI_Results.csv") #replicates removed and gaps extrapolated


# Filter and rename for clarity
df_BDLOI <- df_BDLOI %>%
  select(plot, Dry.BD..g.mL., X..OM.LOI) %>%
  rename(
    DryBD = Dry.BD..g.mL.,
    Percent_OM = X..OM.LOI)


# Plot
p3 <- ggplot(df_BDLOI, aes(y = plot, x = DryBD)) +
  geom_bar(stat = "identity", fill = "steelblue", orientation = "y") +
  coord_cartesian(ylim = c(50, 0)) +
  scale_y_reverse(expand = c(0, 0)) +  # Depth increases downward
  scale_x_continuous(position = "top") + 
  theme_minimal() +
  labs(#title = "Dry Bulk Density by Sample",
    y = NULL,
    x = expression(Dry~Bulk~Density~"(g"^{1}~cc^{-1}*")"))

p3

################################################################################
# (4) Bar chart for Organic Matter Content (%) 

# Plot
p4 <- ggplot(df_BDLOI, aes(y = plot, x = Percent_OM)) +
  geom_bar(stat = "identity", fill = "gold", orientation = "y") +
  coord_cartesian(ylim = c(50, 0)) +
  scale_y_reverse(expand = c(0, 0)) +  # Depth increases downward
  scale_x_continuous(position = "top") + 
  scale_y_continuous(position = "right") +
  theme_minimal() +
  labs(#title = "Dry Bulk Density by Sample",
    y = "Depth (cm bgs)",
    x = "Organic Matter Content (%)" )  

p4

################################################################################

################################################################################
#Combine plots
# Helper function to add horizontal lines
add_hlines <- function(plot, positions = seq(0, 50, by = 5)) {
  plot + geom_hline(yintercept = positions, color = "black", linetype = "solid", size = 0.5)
}

# Apply to plots
p1 <- add_hlines(p1)
p2 <- add_hlines(p2)
p3 <- add_hlines(p3)
p4 <- add_hlines(p4)

y_limits <- c(0, 50)

p1 <- p1 + scale_y_continuous(limits = y_limits, expand = c(0, 0))
p2 <- p2 + scale_y_continuous(limits = y_limits, expand = c(0, 0))
p3 <- p3 + scale_y_continuous(limits = y_limits, expand = c(0, 0))
p4 <- p4 + scale_y_continuous(limits = y_limits, expand = c(0, 0))

blank_y_axis <- theme(
  axis.title.y = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank()
)

p2 <- p2 + blank_y_axis
p3 <- p3 + blank_y_axis
p4 <- p4 + blank_y_axis

# Combine with patchwork
combined_plot <- (p1 | p2 | p3|p4) + 
  plot_layout(widths = c(2, 0.75, 1, 1)) &   # Adjust relative widths as needed
  theme(legend.position = "bottom")



# Display the plot
combined_plot