# Clear environment
rm(list=ls()); graphics.off(); cat("\f")

# Set working directory
setwd("~/Desktop/Papers/Sediment Paper/Code/")

# Load packages
library(vegan)
library(tidyverse)
library(goeveg)

# Read dataset
df <- read_csv('Surface_Sed_MP_SHP_v4.csv')


# Order sites (reuse the same across both datasets)
site_levels <- c("Upper Bay Saltmarsh","Upper Bay Basin I/III",
                 "Upper Bay Basin II","Middle Newport Bay","Lower Harbor")

# Order sites (reuse the same across both datasets)
#site_levels <- c("Lower Harbor", "Middle Newport Bay",
#                 "Upper Bay Basin II", "Upper Bay Basin I/III","Upper Bay Saltmarsh")

# Palette that matches your site order
site_levels <- c("Lower Harbor", "Middle Newport Bay",
                 "Upper Bay Basin II", "Upper Bay Basin I/III", "Upper Bay Saltmarsh")

site_colors <- c(
  "Lower Harbor"           = "#8DD3C7",
  "Middle Newport Bay"     = "#FFFFB3",
  "Upper Bay Basin II"     = "#FB8072",
  "Upper Bay Basin I/III"  = "#BEBADA",
  "Upper Bay Saltmarsh"    = "#80B1D3"
)


# Prepare data for NMDS
conc_data <- df %>%  
  mutate(Site = factor(Site, levels = site_levels)) %>%  # set site order
  select(Sample, Perc_fiber, Perc_fiberbun, Perc_film, Perc_foam, Perc_sphere, Perc_fragment, Perc_TRWP) %>%  
  column_to_rownames(var = "Sample")


# Check if data is numeric
str(conc_data)

# Compute dissimilarity matrix
dist_conc <- vegdist(conc_data, method = "bray")

# Run NMDS
set.seed(123)
nmds_conc <- metaMDS(conc_data, distance = "bray", k = 2)

# Check stress
print(nmds_conc$stress)

# Scree plot for dimensionality check
dimcheck_out <- dimcheckMDS(conc_data, distance = "bray", k = 6)
print(dimcheck_out)

# Stress plot
stressplot(nmds_conc)

# Extract NMDS scores for samples (sites)
nmds_SiteScores <- as.data.frame(scores(nmds_conc)$sites) %>%
  rownames_to_column(var = "Sample") %>%
  left_join(df %>% select(Sample, Site), by = "Sample")  # Include site info

# Extract NMDS scores for variables (species equivalents)
nmds_SpeciesScores <- as.data.frame(scores(nmds_conc, "species"))
nmds_SpeciesScores$Variable <- rownames(nmds_SpeciesScores)

# Compute centroids for Site groups
Site_Centroid <- nmds_SiteScores %>%
  group_by(Site) %>%
  summarise(NMDS1 = mean(NMDS1), NMDS2 = mean(NMDS2))

# Compute convex hull for grouping
site_hull <- nmds_SiteScores %>%
  group_by(Site) %>%
  slice(chull(NMDS1, NMDS2))


# ... your code up to ggplot() ...

ggplot() +
  geom_polygon(data = site_hull,
               aes(x = NMDS1, y = NMDS2, fill = Site, group = Site),
               alpha = 0.3) +
  geom_point(data = nmds_SiteScores,
             aes(x = NMDS1, y = NMDS2, colour = Site), size = 2) +
  geom_text(data = nmds_SiteScores,
            aes(x = NMDS1, y = NMDS2, label = Sample),
            hjust = 0.5, vjust = -0.8, size = 3) +
  geom_text(data = nmds_SpeciesScores,
            aes(x = NMDS1, y = NMDS2, label = Variable),
            hjust = 0, vjust = -1, size = 4, fontface = "bold") +
  geom_point(data = Site_Centroid,
             aes(x = NMDS1, y = NMDS2, color = Site), size = 5, shape = 17) +
  
  # >>> Add these two lines <<<
  scale_color_manual(name = "Site", values = site_colors, limits = site_levels) +
  scale_fill_manual(name = "Site",  values = site_colors, limits = site_levels) +
  
  annotate("text",
           x = max(nmds_SiteScores$NMDS1) * 0.75,
           y = max(nmds_SiteScores$NMDS2) * 1.65,
           label = paste("2D stress =", round(nmds_conc$stress, 3))) +
  labs(x = "NMDS1", y = "NMDS2") +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),# global font
    axis.text = element_blank(), axis.ticks = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_line(color = "black"),
    legend.position = c(0.15, 0.85),
    legend.background = element_rect(fill = "white", colour = "black"),
    legend.key.size = unit(0.4, "cm")
  )

