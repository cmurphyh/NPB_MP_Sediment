# Clear environment
rm(list=ls()); graphics.off(); cat("\f")

# Set working directory
setwd("~/Desktop/Papers/Sediment Paper/Code")

# Load packages
library(vegan)
library(tidyverse)
library(goeveg)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(FSA)
library(plotly)
library(geometry) 

df <- read_csv("surface_paired_merged_data.csv")


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




df <- df %>%
  filter(plastic_or_not != 'NA') %>%
  filter(Sample != 'SUB-4-10-S1') %>%
  filter(Sample !=  'SUB-4-7-S1') %>%
  filter(Sample !=  'SUB-4-9-S1') %>%
  filter(Depth == '0-5cm') 


df <- df %>%
  filter(`1st Order Morph`== 'Non-Fiber') %>%
  filter(plastic_or_not == 'plastic') %>%
  filter(good_match_vals ==  'TRUE') 

###############################################################################
#NMDS

df <- df %>%
  group_by(Site, Sample, material_class) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Site) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  spread(key = material_class, value = percentage, fill = 0)

df

write.csv(df, "surf_perct_poly.csv")


names <- colnames(df)
names
names <- names[4:17]
names

df_collapsed <- df %>%
  group_by(Sample, Site) %>%
  summarise(across(, sum))

# Prepare data for NMDS
mat_data <- df_collapsed  %>%
  select(Sample,  c(names)) %>%  
  column_to_rownames(var = 'Sample')


# Calculate dissimilarity matrix (e.g., using Bray-Curtis)
diss_matrix <- vegdist(mat_data, method = "bray")

# Run NMDS
set.seed(123)
nmds_conc <- metaMDS(mat_data, distance = "bray", k = 2)

# Check stress
print(nmds_conc$stress)

# Scree plot for dimensionality check
dimcheck_out <- dimcheckMDS(mat_data, distance = "bray", k = 6)
print(dimcheck_out)

# Stress plot
stressplot(nmds_conc)

# Extract NMDS scores for samples (sites)
nmds_SiteScores <- as.data.frame(scores(nmds_conc)$sites) %>%
  rownames_to_column(var = "Sample") %>%
  left_join(df_collapsed %>% select(Sample, Site), by = "Sample")  # Include site info

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

# Final NMDS plot with sample labels
ggplot() +
  
  # Add convex hulls
  geom_polygon(data = site_hull, aes(x = NMDS1, y = NMDS2, fill = Site, group = Site), alpha = 0.3) +
  
  # Add site points
  geom_point(data = nmds_SiteScores, aes(x = NMDS1, y = NMDS2, colour = Site), size = 2) +
  
  # Add sample labels
  geom_text(data = nmds_SiteScores, aes(x = NMDS1, y = NMDS2, label = Sample),
            hjust = 0.5, vjust = -0.8, size = 3) +  # Offset sample labels slightly
  
  # Add species scores with offset labels
  geom_text(data = nmds_SpeciesScores, aes(x = NMDS1, y = NMDS2, label = Variable),
            hjust = 0, vjust = -1, size = 4, fontface = "bold") +  # Offset species labels
  
  # Add centroid points
  geom_point(data = Site_Centroid, aes(x = NMDS1, y = NMDS2, color = Site), size = 5, shape = 17) +
  
  
  # >>> Add these two lines <<<
  scale_color_manual(name = "Site", values = site_colors, limits = site_levels) +
  scale_fill_manual(name = "Site",  values = site_colors, limits = site_levels) +
  
  # Annotate stress value
  annotate("text", x = max(nmds_SiteScores$NMDS1) * 0.75, 
           y = max(nmds_SiteScores$NMDS2) * 0.85, 
           label = paste("2D stress =", round(nmds_conc$stress, 3))) +
  
  # Adjust theme
  labs(x = "NMDS1", y = "NMDS2") + 
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),# global font
    axis.text = element_blank(), axis.ticks = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        axis.line = element_line(color = "black"))
################################################################################
# Run NMDS k=3

set.seed(123)
nmds_conc <- metaMDS(mat_data, distance = "bray", k = 3)

# Check stress
print(nmds_conc$stress)

# Extract NMDS scores for sites (samples)
nmds_SiteScores <- as.data.frame(scores(nmds_conc)$sites) %>%
  rownames_to_column(var = "Sample") %>%
  left_join(df_collapsed %>% select(Sample, Site), by = "Sample")

# Extract NMDS scores for species (variables)
nmds_SpeciesScores <- as.data.frame(scores(nmds_conc, "species"))
nmds_SpeciesScores$Variable <- rownames(nmds_SpeciesScores)

# Compute centroids in 3D
Site_Centroid <- nmds_SiteScores %>%
  group_by(Site) %>%
  summarise(NMDS1 = mean(NMDS1), NMDS2 = mean(NMDS2), NMDS3 = mean(NMDS3))

# Function to compute 3D convex hull
compute_3D_hull <- function(data) {
  if (nrow(data) < 4) {
    return(NULL)  # Skip sites with fewer than 4 points
  }
  hull_indices <- convhulln(data[, c("NMDS1", "NMDS2", "NMDS3")], "Fx")
  hull_points <- data[unique(as.vector(hull_indices)), ]  # Flatten and subset points
  return(hull_points)
}

# Compute convex hull for each site
site_hull <- nmds_SiteScores %>%
  group_by(Site) %>%
  group_split() %>%
  map_dfr(compute_3D_hull)


################################################################################


par(mfrow = c(1, 3))  # Create 3 plots in a row

plot(nmds_SiteScores$NMDS1, nmds_SiteScores$NMDS2, col = as.factor(nmds_SiteScores$Site),
     pch = 16, xlab = "NMDS1", ylab = "NMDS2", main = "NMDS1 vs NMDS2")

plot(nmds_SiteScores$NMDS1, nmds_SiteScores$NMDS3, col = as.factor(nmds_SiteScores$Site),
     pch = 16, xlab = "NMDS1", ylab = "NMDS3", main = "NMDS1 vs NMDS3")

plot(nmds_SiteScores$NMDS2, nmds_SiteScores$NMDS3, col = as.factor(nmds_SiteScores$Site),
     pch = 16, xlab = "NMDS2", ylab = "NMDS3", main = "NMDS2 vs NMDS3")

################################################################################

# Plot NMDS in 3D using plotly
plot_ly() %>%
  # Add sample points (sites)
  add_trace(data = nmds_SiteScores, 
            x = ~NMDS1, y = ~NMDS2, z = ~NMDS3, 
            color = ~Site, text = ~Sample, 
            type = "scatter3d", mode = "markers", 
            marker = list(size = 6, opacity = 0.8, line = list(width = 0.5))) %>%
  
  # Add centroids (larger points for sites)
  add_trace(data = Site_Centroid, 
            x = ~NMDS1, y = ~NMDS2, z = ~NMDS3, 
            color = ~Site, 
            type = "scatter3d", mode = "markers", 
            marker = list(size = 6,  symbol = "x")) %>%

  
  # Layout adjustments
  layout(title = "Interactive 3D NMDS Plot",
         scene = list(xaxis = list(title = "NMDS1"),
                      yaxis = list(title = "NMDS2"),
                      zaxis = list(title = "NMDS3")),
         showlegend = TRUE)  # Hide legend if desired

