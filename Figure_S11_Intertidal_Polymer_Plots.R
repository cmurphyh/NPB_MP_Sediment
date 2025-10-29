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

# Set working directory
setwd("~/Desktop/Papers/Sediment Paper/Code/")


#########
#By Sample
#########



df <- read_csv("intertidal_paired_merged_data.csv")


df <- df %>%
  filter(plastic_or_not != 'NA') 


df <- df %>%
  filter(`1st Order Morph`== 'Non-Fiber') %>%
  filter(plastic_or_not == 'plastic') %>%
  filter(good_match_vals ==  'TRUE') 


# Summarize data: Count occurrences of material_class per site
df_summary <- df %>%
  group_by(Location, Depth, material_class) %>%
  summarise(count = n(), .groups = "drop")

# Sum counts across Depth and material_class for each Location
df_location_totals <- df_summary %>%
  group_by(Location) %>%
  summarise(total_count = sum(count), .groups = "drop")

print(df_location_totals)


# Compute totals per bar (Depth x Location)
df_totals <- df_summary %>%
  group_by(Location, Depth) %>%
  summarise(total = sum(count), .groups = "drop")

# Plot stacked bar chart + totals on top
ggplot(df_summary, aes(x = Depth, y = count, fill = material_class)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  facet_wrap(~ Location, scales = "free_y") +
  # Add total labels
  geom_text(data = df_totals,
            aes(x = Depth, y = total, label = total),
            vjust = -0.3,            # nudge above the bar
            #fontface = "bold",
            inherit.aes = FALSE) +   # don't inherit fill/color from main plot
  labs(x = "Site", y = "Count", fill = "Material Class",
       title = "Distribution of Material Class by Site: Non-Fibers only (excluding TRWP) (n = 633)") +
  theme_minimal() +
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


# View result
print(df_summary)

# Summarize data: Count occurrences of material_class per site and calculate percentage
df_summary <- df %>%
  group_by(Location, Depth, material_class) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Location, Depth) %>%
  mutate(percentage = count / sum(count) * 100)  # Convert counts to percentages

write.csv2(df_summary,"material_class_intertidal_sample_NonFibersOnly.csv")

print(df_summary)


################################################################################
#Fibers

# Set working directory
setwd("~/Desktop/Papers/Sediment Paper/Code/")

df <- read_csv("intertidal_paired_merged_data.csv")


df <- df %>%
  filter(plastic_or_not != 'NA') 


df <- df %>%
  filter(`1st Order Morph`== 'Fiber') %>%
  filter(plastic_or_not == 'plastic') %>%
  filter(good_match_vals ==  'TRUE') 


# Summarize data: Count occurrences of material_class per site
df_summary <- df %>%
  group_by(Location, Depth, material_class) %>%
  summarise(count = n(), .groups = "drop")

# Sum counts across Depth and material_class for each Location
df_location_totals <- df_summary %>%
  group_by(Location) %>%
  summarise(total_count = sum(count), .groups = "drop")

print(df_location_totals)

# Compute totals per bar (Depth x Location)
df_totals <- df_summary %>%
  group_by(Location, Depth) %>%
  summarise(total = sum(count), .groups = "drop")

# Plot stacked bar chart + totals on top
ggplot(df_summary, aes(x = Depth, y = count, fill = material_class)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  facet_wrap(~ Location, scales = "free_y") +
  # Add total labels
  geom_text(data = df_totals,
            aes(x = Depth, y = total, label = total),
            vjust = -0.3,            # nudge above the bar
            #fontface = "bold",
            inherit.aes = FALSE) +   # don't inherit fill/color from main plot
  labs(x = "Site", y = "Count", fill = "Material Class",
       title = "Distribution of Material Class by Site: Fibers only (n = 301)") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),# global font
    strip.text = element_text(family = "Times New Roman",     # facet labels
                              size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text.y = element_text(size = 12),
    plot.title  = element_text(size = 12, face = "bold")
  )+
scale_fill_viridis_d(option = "plasma")

# View result
print(df_summary)


#############
#By site
#############

df <- read_csv("intertidal_paired_merged_data.csv")


df <- df %>%
  filter(plastic_or_not != 'NA') 


df <- df %>%
  filter(`1st Order Morph`== 'Non-Fiber') %>%
  filter(plastic_or_not == 'plastic') %>%
  filter(good_match_vals ==  'TRUE') 


# Summarize data: Count occurrences of material_class per site
df_summary <- df %>%
  group_by(Location, material_class) %>%
  summarise(count = n(), .groups = "drop")

# View result
print(df_summary)

# Plot stacked bar chart
ggplot(df_summary, aes(x = Location, y = count, fill = material_class)) +
  geom_bar(stat = "identity", position = "stack",color = "black") +
  labs(x = "Site", y = "Count", fill = "Material Class",
       title = "Distribution of Material Class by Site: Non-Fibers only (excluding TRWP) (N = 633)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability


# Summarize data: Count occurrences of material_class per site and calculate percentage
df_summary <- df %>%
  group_by(Location, material_class) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Location) %>%
  mutate(percentage = count / sum(count) * 100)  # Convert counts to percentages

write.csv2(df_summary,"material_class_intertidal_site_NonFibersOnly.csv")

print(df_summary)

# Plot stacked bar chart with percentages
ggplot(df_summary, aes(x = Location, y = percentage, fill = material_class)) +
  geom_bar(stat = "identity", position = "stack",color = "black") +
  labs(x = "Site", y = "Percentage", fill = "Material Class",
       title = "Percentage Distribution of Material Class by Site: Non-Fibers only (excluding TRWP) (N = 633)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  scale_y_continuous(labels = scales::percent_format(scale = 1)) # Format y-axis as percent



#Plot pie charts
ggplot(df_summary, aes(x = "", y = percentage, fill = material_class)) +
  geom_bar(stat = "identity", width = 1, color = "black") +  # Black borders
  coord_polar(theta = "y") +
  labs(title = "Material Class Distribution by Site: Non-Fibers only (excluding TRWP)") +
  theme_void() +  # Removes unnecessary axes
  theme(legend.position = "right") +
  facet_wrap(~ Location)  # Creates a separate pie chart for each site

################################################################################
#Fibers

# Set working directory
setwd("~/Desktop/Papers/Sediment Paper/Code/")

df <- read_csv("intertidal_paired_merged_data.csv")


df <- df %>%
  filter(plastic_or_not != 'NA') 


df <- df %>%
  filter(`1st Order Morph`== 'Fiber') %>%
  filter(plastic_or_not == 'plastic') %>%
  filter(good_match_vals ==  'TRUE') 


# Summarize data: Count occurrences of material_class per site
df_summary <- df %>%
  group_by(Location, material_class) %>%
  summarise(count = n(), .groups = "drop")

# Sum counts across Depth and material_class for each Location
df_location_totals <- df_summary %>%
  group_by(Location) %>%
  summarise(total_count = sum(count), .groups = "drop")

print(df_location_totals)

# View result
print(df_summary)

# Plot stacked bar chart
ggplot(df_summary, aes(x = Location, y = count, fill = material_class)) +
  geom_bar(stat = "identity", position = "stack",color = "black") +
  labs(x = "Site", y = "Count", fill = "Material Class",
       title = "Distribution of Material Class by Site: Fibers only (N = 301)") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),# global font
    strip.text = element_text(family = "Times New Roman",     # facet labels
                              size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text.y = element_text(size = 12),
    plot.title  = element_text(size = 12, face = "bold")
  )+ # Rotate x-axis labels for readability
  scale_fill_viridis_d(option = "plasma")

# Summarize data: Count occurrences of material_class per site and calculate percentage
df_summary <- df %>%
  group_by(Location, material_class) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Location) %>%
  mutate(percentage = count / sum(count) * 100)  # Convert counts to percentages

write.csv2(df_summary,"material_class_surface_site_FibersOnly.csv")

print(df_summary)

# Plot stacked bar chart with percentages
ggplot(df_summary, aes(x = Location, y = percentage, fill = material_class)) +
  geom_bar(stat = "identity", position = "stack",color = "black") +
  labs(x = "Site", y = "Percentage", fill = "Material Class",
       title = "Percentage Distribution of Material Class by Site: Fibers only (N = 301)") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),# global font
    strip.text = element_text(family = "Times New Roman",     # facet labels
                              size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text.y = element_text(size = 12),
    plot.title  = element_text(size = 12, face = "bold")
  ) +  # Rotate x-axis labels
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Format y-axis as percent
scale_fill_viridis_d(option = "plasma")

#Plot pie charts
ggplot(df_summary, aes(x = "", y = percentage, fill = material_class)) +
  geom_bar(stat = "identity", width = 1, color = "black") +  # Black borders
  coord_polar(theta = "y") +
  labs(title = "Material Class Distribution by Site: Fibers only") +
  theme_void() +  # Removes unnecessary axes
theme(legend.position = "right",
  text = element_text(family = "Times New Roman"),# global font
  strip.text = element_text(family = "Times New Roman",     # facet labels
                            size = 12, face = "bold"),
  legend.text = element_text(size = 10),
  legend.title = element_text(size = 10),
  plot.title  = element_text(size = 12, face = "bold")
)+
  facet_wrap(~ Location) +  # Creates a separate pie chart for each site
scale_fill_viridis_d(option = "plasma")

# Summarize data: Count occurrences per site
df_summary <- df %>%
  group_by(Location) %>%
  summarise(count = n(), .groups = "drop")


################################################################################
#All Microplastics



#########
#By Sample
#########



df <- read_csv("intertidal_paired_merged_data.csv")


df <- df %>%
  filter(plastic_or_not != 'NA') 


df <- df %>%
  filter(plastic_or_not == 'plastic') %>%
  filter(good_match_vals ==  'TRUE') 


# Summarize data: Count occurrences of material_class per site
df_summary <- df %>%
  group_by(Location, Depth, material_class) %>%
  summarise(count = n(), .groups = "drop")

# Sum counts across Depth and material_class for each Location
df_location_totals <- df_summary %>%
  group_by(Location) %>%
  summarise(total_count = sum(count), .groups = "drop")

print(df_location_totals)


# Compute totals per bar (Depth x Location)
df_totals <- df_summary %>%
  group_by(Location, Depth) %>%
  summarise(total = sum(count), .groups = "drop")

# Plot stacked bar chart + totals on top
ggplot(df_summary, aes(x = Depth, y = count, fill = material_class)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  facet_wrap(~ Location, scales = "free_y") +
  # Add total labels
  geom_text(data = df_totals,
            aes(x = Depth, y = total, label = total),
            vjust = -0.3,            # nudge above the bar
            #fontface = "bold",
            inherit.aes = FALSE) +   # don't inherit fill/color from main plot
  labs(x = "Site", y = "Count", fill = "Material Class",
       title = "Distribution of Material Class by Site: All Miroplastics (excluding TRWP) (N = 934)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# View result
print(df_summary)

# Summarize data: Count occurrences of material_class per site and calculate percentage
df_summary <- df %>%
  group_by(Location, Depth, material_class) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Location, Depth) %>%
  mutate(percentage = count / sum(count) * 100)  # Convert counts to percentages

write.csv2(df_summary,"material_class_intertidal_sample_AllMPs.csv")

print(df_summary)



#############
#By site
#############

df <- read_csv("intertidal_paired_merged_data.csv")


df <- df %>%
  filter(plastic_or_not != 'NA') 


df <- df %>%
  filter(plastic_or_not == 'plastic') %>%
  filter(good_match_vals ==  'TRUE') 


# Summarize data: Count occurrences of material_class per site
df_summary <- df %>%
  group_by(Location, material_class) %>%
  summarise(count = n(), .groups = "drop")

# View result
print(df_summary)

# Plot stacked bar chart
ggplot(df_summary, aes(x = Location, y = count, fill = material_class)) +
  geom_bar(stat = "identity", position = "stack",color = "black") +
  labs(x = "Site", y = "Count", fill = "Material Class",
       title = "Distribution of Material Class by Site: All Microplastics (excluding TRWP) (N = 934)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability


# Summarize data: Count occurrences of material_class per site and calculate percentage
df_summary <- df %>%
  group_by(Location, material_class) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Location) %>%
  mutate(percentage = count / sum(count) * 100)  # Convert counts to percentages

write.csv2(df_summary,"material_class_intertidal_site_AllMPs.csv")

print(df_summary)


# Plot stacked bar chart with percentages
ggplot(df_summary, aes(x = Location, y = percentage, fill = material_class)) +
  geom_bar(stat = "identity", position = "stack",color = "black") +
  labs(x = "Site", y = "Percentage", fill = "Material Class",
       title = "Percentage Distribution of Material Class by Site: All Microplastics (excluding TRWP) (N = 934)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  scale_y_continuous(labels = scales::percent_format(scale = 1)) # Format y-axis as percent



#Plot pie charts
ggplot(df_summary, aes(x = "", y = percentage, fill = material_class)) +
  geom_bar(stat = "identity", width = 1, color = "black") +  # Black borders
  coord_polar(theta = "y") +
  labs(title = "Material Class Distribution by Site: All Microplastics (excluding TRWP) (N = 934)") +
  theme_void() +  # Removes unnecessary axes
  theme(legend.position = "right") +
  facet_wrap(~ Location)  # Creates a separate pie chart for each site



