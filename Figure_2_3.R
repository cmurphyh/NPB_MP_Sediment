#Figure 2

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
library(patchwork)

# Read dataset
df <- read_csv('Surface_Sed_MP_SHP_v4.csv')

df <- df %>%
  filter(Sample != 'SUB-4-10-S1') %>%
  filter(Sample !=  'SUB-4-7-S1') %>%
  filter(Sample !=  'SUB-4-9-S1')

# Read dataset
df2 <- read_csv('Surface_Sed_MPmass_SHP_v4.csv')

df2 <- df2 %>%
  filter(Sample != 'SUB-4-10-S1') %>%
  filter(Sample !=  'SUB-4-7-S1') %>%
  filter(Sample !=  'SUB-4-9-S1')

site_colors <- c("Upper Bay Saltmarsh"= "#80B1D3",
                 "Upper Bay Basin I/III"= "#BEBADA",
                 "Upper Bay Basin II"= "#FB8072",
                 "Middle Newport Bay" = "#FFFFB3",
                 "Lower Harbor"= "#8DD3C7")




################################################################################

#Transform Concentration data into long format for ggplot
df_long_Con <- df %>%
  select(Sample, Site, Conc_Total, Conc_NonFiber, Conc_TRWP) %>%
  pivot_longer(cols = c(Conc_Total, Conc_NonFiber,  Conc_TRWP), 
               names_to = "Particle_Type", values_to = "Concentration")

df_long_Con <- df_long_Con %>%
  mutate(Site = factor(Site, levels = c("Upper Bay Saltmarsh", "Upper Bay Basin I/III", "Upper Bay Basin II", "Middle Newport Bay", "Lower Harbor")))
################################################################################

#Transform Concentration data into long format for ggplot
df_long_Con_mass <- df2 %>%
  select(Sample, Site, Av_Conc_Total, Av_Conc_NonFiber, Av_Conc_TRWP) %>%
  pivot_longer(cols = c(Av_Conc_Total, Av_Conc_NonFiber,  Av_Conc_TRWP), 
               names_to = "Particle_Type", values_to = "Concentration")

df_long_Con_mass <- df_long_Con_mass %>%
  mutate(Site = factor(Site, levels = c("Upper Bay Saltmarsh", "Upper Bay Basin I/III", "Upper Bay Basin II", "Middle Newport Bay", "Lower Harbor")),
Concentration = Concentration * 1000)  # convert g/g → mg/g
################################################################################
# () Boxplots for Total, Non-Fiber and TRWP Concentration: Count

facet_labels <- c(
  Conc_NonFiber = "Non-Fiber",
  Conc_Total = "Total",
  Conc_TRWP = "TRWP"
)

#set order of panels
df_long_Con <- df_long_Con %>%
  mutate(Particle_Type = factor(Particle_Type, levels = c("Conc_Total", "Conc_NonFiber", "Conc_TRWP")))

# Create boxplot
p1 <- ggplot(df_long_Con, aes(x = Site, y = Concentration, fill = Site)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) + # Outliers hidden for clarity
  geom_jitter(aes(color = Site), width = 0.2, alpha = 0.5) +  # Add jittered points
  facet_wrap(~Particle_Type, scales = "free_y", labeller = labeller(Particle_Type = facet_labels)) +  # Separate plots for each type
  theme_minimal() +
  labs(title = "Count-based MP Concentration",
       x = NULL, y = expression(""*n^1~g^{-1}~"dw sediment"*"")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = site_colors) +
  scale_color_manual(values = site_colors)+
  theme(
    text = element_text(family = "Times New Roman"),# global font
    strip.text = element_text(family = "Times New Roman",     # facet labels
                              size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 12),
    plot.title  = element_text(size = 12, face = "bold")
  )
# Use colorblind-friendly palette

#p1

################################################################################
# () Boxplots for Total, Non-Fiber and TRWP Concentration: Mass

facet_labels2 <- c(
  Av_Conc_NonFiber = "Non-Fiber",
  Av_Conc_Total = "Total",
  Av_Conc_TRWP = "TRWP"
)

#set order of panels
df_long_Con_mass <- df_long_Con_mass %>%
  mutate(Particle_Type = factor(Particle_Type, levels = c("Av_Conc_Total", "Av_Conc_NonFiber", "Av_Conc_TRWP")))


# Create boxplot
p2 <- ggplot(df_long_Con_mass, aes(x = Site, y = Concentration, fill = Site)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) + # Outliers hidden for clarity
  geom_jitter(aes(color = Site), width = 0.2, alpha = 0.5) +  # Add jittered points
  facet_wrap(~Particle_Type, scales = "free_y", labeller = labeller(Particle_Type = facet_labels2)) +  # Separate plots for each type
  theme_minimal() +
  labs(title = "Mass-based MP Concentration",
       x = NULL, y = expression(""*mg^1~g^{-1}~"dw sediment"*"")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = site_colors) +
  scale_color_manual(values = site_colors)+
  theme(
    text = element_text(family = "Times New Roman"),# global font
    strip.text = element_text(family = "Times New Roman",     # facet labels
                              size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 12),
    plot.title  = element_text(size = 12, face = "bold")
  )
# Use colorblind-friendly palette

#p2

################################################################################
################################################################################
# Combine with patchwork
combined_plot <- (p1 / p2 ) + 
  plot_layout(widths = c(1, 1)) &   # Adjust relative widths as needed
  theme(legend.position = "bottom")

combined_plot

################################################################################
################################################################################
################################################################################
################################################################################
#Figure3

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
library(patchwork)

# Read dataset
df <- read_csv('Surface_Sed_MP_SHP_v4.csv')

df <- df %>%
  filter(Sample != 'SUB-4-10-S1') %>%
  filter(Sample !=  'SUB-4-7-S1') %>%
  filter(Sample !=  'SUB-4-9-S1')

# Read dataset
df2 <- read_csv('Surface_Sed_MPmass_SHP_v4.csv')

df2 <- df2 %>%
  filter(Sample != 'SUB-4-10-S1') %>%
  filter(Sample !=  'SUB-4-7-S1') %>%
  filter(Sample !=  'SUB-4-9-S1')


################################################################################

#Transform data into long-format for plotting

# Transform All MP Composition data into long format for ggplot
df_All_Comp <- df %>%
  select(Sample, Site, Perc_fiber, Perc_fiberbun, Perc_film, Perc_sphere, Perc_foam, Perc_fragment, Perc_pellet, Perc_TRWP) %>%
  pivot_longer(cols = c(Perc_fiber, Perc_fiberbun, Perc_film, Perc_sphere, Perc_foam, Perc_fragment, Perc_pellet, Perc_TRWP), 
               names_to = "Particle_Type", values_to = "Composition")


# Transform All MP Composition data into long format for ggplot
df_All_Comp_mass <- df2 %>%
  select(Sample, Site, Perc_fiber, Perc_fiberbun, Perc_film, Perc_sphere, Perc_foam, Perc_fragment, Perc_pellet, Perc_TRWP) %>%
  pivot_longer(cols = c(Perc_fiber, Perc_fiberbun, Perc_film, Perc_sphere, Perc_foam, Perc_fragment, Perc_pellet, Perc_TRWP), 
               names_to = "Particle_Type", values_to = "Composition")


# Order sites (reuse the same across both datasets)
site_levels <- c("Upper Bay Saltmarsh","Upper Bay Basin I/III",
                 "Upper Bay Basin II","Middle Newport Bay","Lower Harbor")

################################################################################
# (1) Stacked bar chart for Morphological Composition

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

df_All_Comp <- df_All_Comp %>%
  mutate(Site = factor(Site, levels = site_levels)) %>%  # set site order
  arrange(Site, Sample) %>%                               # sort rows by Site, then Sample
  mutate(Sample = factor(Sample, levels = unique(Sample)))# lock bar order on x-axis


vline_positions <- df_All_Comp %>%
  distinct(Sample, Site) %>%
  count(Site, name = "n") %>%
  mutate(cum = cumsum(n), x = cum + 0.5) %>%
  pull(x)
vline_positions <- vline_positions[-length(vline_positions)]



# Plot
p1 <- ggplot(df_All_Comp, aes(x = Sample, y = Composition * 100, fill = Particle_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_vline(xintercept = vline_positions, color = "black", linetype = "solid", size = 0.5) +
  theme_minimal() +
  labs(title = "Count-based Morphological MP Composition",
       x = NULL, y = "Count Composition [%]",
       fill = "Morphology") +
  scale_fill_manual(
    values = RColorBrewer::brewer.pal(n = 8, name = "Set2"),
    labels = comp_labels
  ) +
  theme(
    text = element_text(family = "Times New Roman"),       # global font
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text.y = element_text(size = 12),
    plot.title  = element_text(size = 12, face = "bold")
  )

#p1
################################################################################
################################################################################
# (1) Stacked bar chart for Morphological Composition

# Define label replacements for Particle_Type
comp_labels2 <- c(
  Perc_fiber = "fiber",
  Perc_fiberbun = "fiber bundle",
  Perc_film = "film",
  Perc_sphere = "sphere",
  Perc_pellet = "pellet",
  Perc_foam = "foam",
  Perc_fragment = "fragment",
  Perc_TRWP = "TRWP"
)

df_All_Comp_mass <- df_All_Comp_mass %>%
  mutate(Composition = ifelse(is.na(Composition), 0, Composition))

df_All_Comp_mass <- df_All_Comp_mass %>%
  mutate(Site = factor(Site, levels = site_levels)) %>%
  arrange(Site, Sample) %>%
  mutate(Sample = factor(Sample, levels = unique(Sample)))

vline_positions <- df_All_Comp_mass %>%
  distinct(Sample, Site) %>%
  count(Site, name = "n") %>%
  mutate(cum = cumsum(n), x = cum + 0.5) %>%
  pull(x)
vline_positions <- vline_positions[-length(vline_positions)]


# Plot
p2 <- ggplot(df_All_Comp_mass, aes(x = Sample, y = Composition * 100, fill = Particle_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_vline(xintercept = vline_positions, color = "black", linetype = "solid", size = 0.5) +
  theme_minimal() +
  labs(title = " Mass-based Morphological MP Composition",
       x = NULL, y = "Mass Composition [%]",
       fill = "Morphology") +
  scale_fill_manual(
    values = RColorBrewer::brewer.pal(n = 8, name = "Set2"),
    labels = comp_labels2
  ) +
theme(
  text = element_text(family = "Times New Roman"),       # global font
  legend.text = element_text(size = 10),
  legend.title = element_text(size = 10),
  axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
  axis.text.y = element_text(size = 12),
  plot.title  = element_text(size = 12, face = "bold")
)

#p2

################################################################################
# Combine with patchwork
combined_plot <- (p1 / p2 ) + 
  plot_layout(widths = c(1, 1)) &   # Adjust relative widths as needed
  theme(legend.position = "bottom")

combined_plot

################################################################################
################################################################################