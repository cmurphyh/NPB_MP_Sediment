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

df <- read_csv("surface_paired_merged_data.csv",)


df <- df %>%
  filter(plastic_or_not != 'NA') %>%
  filter(Sample != 'SUB-4-10-S1') %>%
  filter(Sample !=  'SUB-4-7-S1') %>%
  filter(Sample !=  'SUB-4-9-S1') #%>%
  #filter(Depth == '0-5cm') 

# ---- Set site order ----
site_levels <- c("Upper Bay Saltmarsh",
                 "Upper Bay Basin I/III",
                 "Upper Bay Basin II",
                 "Middle Newport Bay",
                 "Lower Harbor")

df <- df %>%
  mutate(Site = factor(Site, levels = site_levels))



df <- df %>%
  filter(`1st Order Morph`== 'Non-Fiber') %>%
  filter(plastic_or_not == 'plastic') %>%
  filter(good_match_vals ==  'TRUE') 


# Summarize data: Count occurrences of material_class per site
df_summary <- df %>%
  group_by(Site, material_class) %>%
  summarise(count = n(), .groups = "drop")

df_summary <- df_summary %>%
  mutate(Site = factor(Site, levels = site_levels))  # ensure same order here too


# View result
print(df_summary)

# Plot stacked bar chart
ggplot(df_summary, aes(x = Site, y = count, fill = material_class)) +
  geom_bar(stat = "identity", position = "stack",color = "black") +
  labs(x = "Site", y = "Count", fill = "Material Class",
       title = "Distribution of Material Class by Site: Non-Fibers only") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability


# Summarize data: Count occurrences of material_class per site and calculate percentage
df_summary <- df %>%
  group_by(Site, material_class) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Site) %>%
  mutate(percentage = count / sum(count) * 100)  # Convert counts to percentages

write.csv2(df_summary,"material_class_surface_site_NonFibersOnly.csv")

print(df_summary)

# Plot stacked bar chart with percentages

# ---- Compute sample sizes per site ----
site_counts <- df %>%
  group_by(Site) %>%
  summarise(n = n(), .groups = "drop")

# Build labels in correct order
site_label_levels <- site_counts %>%
  arrange(Site) %>%  # follow factor order
  mutate(lbl = paste0(Site, "\n(n = ", n, ")")) %>%
  pull(lbl)

df_plot <- df_summary %>%
  left_join(site_counts, by = "Site") %>%
  mutate(Site_label = paste0(Site, "\n(n = ", n, ")"),
         Site_label = factor(Site_label, levels = site_label_levels))


ggplot(df_plot, aes(x = Site_label, y = percentage, fill = material_class)) +
  geom_bar(stat = "identity", position = "stack",color = "black") +
  labs(x = "Site", y = "Non-Fiber Spectra Dataset: Non-Fibers", fill = "Material Class (Open Specy)",
       title = "Percentage Distribution of Material Class by Site: Non-Fibers only") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),       # global font
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 12),
    plot.title  = element_text(size = 12, face = "bold")
  )  # Rotate x-axis labels
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + # Format y-axis as percent



#Plot pie charts
ggplot(df_summary, aes(x = "", y = percentage, fill = material_class)) +
  geom_bar(stat = "identity", width = 1, color = "black") +  # Black borders
  coord_polar(theta = "y") +
  labs(title = "Material Class Distribution by Site: Non-Fibers only") +
  theme_void() +  # Removes unnecessary axes
    theme(legend.position = "right",
      text = element_text(family = "Times New Roman"),       # global font
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10),
      axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
      axis.text.y = element_text(size = 12),
      plot.title  = element_text(size = 12, face = "bold")
    )+
  facet_wrap(~ Site)+  # Creates a separate pie chart for each site

#################################################################################

df <- read_csv("surface_paired_merged_data.csv",)


df <- df %>%
  filter(plastic_or_not != 'NA') %>%
  filter(Sample != 'SUB-4-10-S1') %>%
  filter(Sample !=  'SUB-4-7-S1') %>%
  filter(Sample !=  'SUB-4-9-S1') #%>%
#filter(Depth == '0-5cm') 


df <- df %>%
  filter(`1st Order Morph`== 'Non-Fiber') %>%
  filter(plastic_or_not == 'plastic') %>%
  filter(good_match_vals ==  'TRUE') 

site_levels <- c("Upper Bay Saltmarsh",
                 "Upper Bay Basin I/III",
                 "Upper Bay Basin II",
                 "Middle Newport Bay",
                 "Lower Harbor")

df <- df %>%
  mutate(Site = factor(Site, levels = site_levels))

vline_positions <- df_summary %>%
  distinct(Sample, Site) %>%
  count(Site, name = "n") %>%
  mutate(cum = cumsum(n), x = cum + 0.5) %>%
  pull(x)
vline_positions <- vline_positions[-length(vline_positions)]


# Summarize data: Count occurrences of material_class per site
df_summary <- df %>%
  group_by(Site, Sample, material_class) %>%
  summarise(count = n(), .groups = "drop")

sample_order <- df %>%
  distinct(Sample, Site) %>%
  arrange(Site, Sample) %>%
  pull(Sample)

df_summary <- df_summary %>%
  mutate(Sample = factor(Sample, levels = sample_order))

vline_positions <- df_summary %>%
  distinct(Sample, Site) %>%
  count(Site, name = "n") %>%
  mutate(cum = cumsum(n), x = cum + 0.5) %>%
  pull(x)
vline_positions <- vline_positions[-length(vline_positions)]


# View result
print(df_summary)

# Plot stacked bar chart
ggplot(df_summary, aes(x = Sample, y = count, fill = material_class)) +
  geom_bar(stat = "identity", position = "stack",color = "black") +
  geom_vline(xintercept = vline_positions, color = "black", linetype = "solid", size = 0.5) +
  labs(x = "Samples", y = "Count", fill = "Material Class",
       title = "Distribution of Material Class by Site: Non-Fibers only") +
  theme_minimal() +
  theme(legend.position = "right",
        text = element_text(family = "Times New Roman"),       # global font
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
        axis.text.y = element_text(size = 12),
        plot.title  = element_text(size = 12, face = "bold")
  )




################################################################################
# Set working directory
setwd("~/Desktop/Papers/Sediment Paper/Code/")


df <- read_csv("surface_paired_merged_data.csv",)


df <- df %>%
  filter(plastic_or_not != 'NA') %>%
  filter(Sample != 'SUB-4-10-S1') %>%
  filter(Sample !=  'SUB-4-7-S1') %>%
  filter(Sample !=  'SUB-4-9-S1') #%>%
#filter(Depth == '0-5cm') 


df <- df %>%
  filter(`1st Order Morph`== 'Fiber') %>%
  filter(plastic_or_not == 'plastic') %>%
  filter(good_match_vals ==  'TRUE') 

site_levels <- c("Upper Bay Saltmarsh",
                 "Upper Bay Basin I/III",
                 "Upper Bay Basin II",
                 "Middle Newport Bay",
                 "Lower Harbor")

df <- df %>%
  mutate(Site = factor(Site, levels = site_levels))


# Summarize data: Count occurrences of material_class per site
df_summary <- df %>%
  group_by(Site, material_class) %>%
  summarise(count = n(), .groups = "drop")

# View result
print(df_summary)

# sample size per Site (do NOT build labels here yet)
site_counts <- df %>%
  group_by(Site) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(Site = factor(Site, levels = site_levels))

# make the final label vector in correct order
site_label_levels <- site_counts %>%
  arrange(Site) %>%
  mutate(lbl = paste0(Site, "\n(n = ", n, ")")) %>%
  pull(lbl)

# join + create factor for plotting on x
df_plot <- df_summary %>%
  left_join(site_counts, by = "Site") %>%
  mutate(Site_label = paste0(Site, "\n(n = ", n, ")"),
         Site_label = factor(Site_label, levels = site_label_levels))



# Plot stacked bar chart
ggplot(df_plot, aes(x = Site_label, y = count, fill = material_class)) +
  geom_bar(stat = "identity", position = "stack",color = "black") +
  labs(x = "Site", y = "Composition within Fiber Spectra Dataset", fill = "Material Class (Open Specy)",
       title = "Percentage Distribution of Material Class by Site: Fibers only") +
  theme_minimal() +
  theme(legend.position = "right",
        text = element_text(family = "Times New Roman"),       # global font
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 12),
        plot.title  = element_text(size = 12, face = "bold")
  )+
  scale_fill_viridis_d(option = "plasma")

# Summarize data: Count occurrences of material_class per site and calculate percentage
df_summary <- df %>%
  group_by( material_class) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = count / sum(count) * 100)  # Convert counts to percentages

write.csv2(df_summary,"material_class_surface_FibersOnly.csv")

print(df_summary)



# Compute sample size per Site
site_counts <- df %>%
  summarise(n = n()) %>%
  mutate(Site_label = paste0(Site, "\n(N = ", n, ")"))

df_plot <- df_summary %>%
  left_join(site_counts, by = "Site")



# Plot stacked bar chart with percentages
ggplot(df_summary, aes(x = "", y = percentage, fill = material_class)) +
  geom_bar(stat = "identity", position = "stack",color = "white") +
  labs(x = "", y = "Percentage", fill = "Material Class (Open Specy)",
       title = "Composition within Fiber Spectra Dataset (n = 197)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  scale_y_continuous(labels = scales::percent_format(scale = 1))+  # Format y-axis as percent
  scale_fill_viridis_d(option = "plasma")+
theme(legend.position = "right",
      text = element_text(family = "Times New Roman"),       # global font
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10),
      axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
      axis.text.y = element_text(size = 12),
      plot.title  = element_text(size = 12, face = "bold")
)

#Plot pie charts
ggplot(df_summary, aes(x = "", y = percentage, fill = material_class)) +
  geom_bar(stat = "identity", width = 1, color = "black") +  # Black borders
  coord_polar(theta = "y") +
  labs(x = "Site", title =  "Composition within Fiber Spectra Dataset (n=197)",
       fill = "Material Class (Open Specy)") +
  theme_void() +  # Removes unnecessary axes
  theme(legend.position = "right") + 
  scale_fill_viridis_d(option = "plasma")+
theme(legend.position = "right",
      text = element_text(family = "Times New Roman"),       # global font
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10),
      plot.title  = element_text(size = 12, face = "bold")
)
###
#
#

df <- read_csv("surface_paired_merged_data.csv",)


df <- df %>%
  filter(plastic_or_not != 'NA') %>%
  filter(Sample != 'SUB-4-10-S1') %>%
  filter(Sample !=  'SUB-4-7-S1') %>%
  filter(Sample !=  'SUB-4-9-S1') #%>%
#filter(Depth == '0-5cm') 


df <- df %>%
  filter(`1st Order Morph`== 'Fiber') %>%
  filter(plastic_or_not == 'plastic') %>%
  filter(good_match_vals ==  'TRUE') 


# Summarize data: Count occurrences of material_class per site
df_summary <- df %>%
  group_by(Sample, material_class) %>%
  summarise(count = n(), .groups = "drop")

# View result
print(df_summary)

# Plot stacked bar chart
ggplot(df_summary, aes(x = Sample, y = count, fill = material_class)) +
  geom_bar(stat = "identity", position = "stack",color = "black") +
  labs(x = "Samples", y = "Count", fill = "Material Class",
       title = "Distribution of Material Class by Site: Fibers only") +
  theme_minimal() +
  theme(legend.position = "right",
        text = element_text(family = "Times New Roman"),       # global font
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 12),
        plot.title  = element_text(size = 12, face = "bold")
  )  # Rotate x-axis labels for readability

