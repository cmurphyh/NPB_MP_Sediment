# Clear environment
rm(list=ls()); graphics.off(); cat("\f")

# Load packages
library(vegan)
library(tidyverse)
library(goeveg)
library(ggplot2)
library(dplyr)
#library(ggpubr)
#library(FSA)

# Set working directory
setwd("~/Desktop/Papers/Sediment Paper/Code/")

df <- read_csv("intertidal_paired_merged_data.csv")

df <- df %>%
  filter(`1st Order Morph`!= 'NA') %>%
  filter(`1st Order Morph`!= 'Spike')

# Summarize data: Count occurrences of 1st order per site
df_count <- df %>%
  group_by(Location,Sample, Depth, Size) %>%
  summarise(count = n(), .groups = "drop")

df_count <- df_count %>%
  mutate(mark_star = ifelse(
    Location == "UNB-2" & Depth %in% c('10-15cm', '15-20cm', '20-25cm', '25-30cm', '30-35cm', '35-40cm', '40-45cm', '45-50cm', '50-55cm') & Size == "500um",
    "*", ""
  ))


# Plot stacked bar chart
p<- ggplot(df_count, aes(x = Depth, y = count, fill = factor(Size, levels = c( "250um", "500um","1000um")), group_by(Location))) +
  geom_bar(stat = "identity", position = "stack",color = "black") +
  geom_text(
    aes(label = mark_star),
    position = position_stack(vjust = 1.05),
    size = 5
  )+
  facet_wrap(~ Location, scales = "free_y") +  # Add free_y scales here
  theme_bw() +
  labs(x = NULL, y = "Count", fill = "Sieve Size",
       title = "All Microplastics at 5 cm Intervals") +
  theme(text = element_text(family = "Times New Roman"),# global font
        axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for readability

#p <- p + labs(
#  caption = "*indicates the 250-500 um sieved size class was not completed"
#) +
#  theme(
#    plot.caption = element_text(hjust = 0.5, size = 10, face = "italic")
#  )


p1 <- p +
  geom_hline(yintercept = 50, linetype = "dashed", color = "black", linewidth = 0.8)

p1

# Summarize data: Count occurrences of 1st order per site
df_count_NF <- df %>%
  filter(`1st Order Morph`== 'Non-Fiber') %>%
  group_by(Location,Sample, Depth, Size) %>%
  summarise(count = n(), .groups = "drop")

df_count_NF <- df_count_NF %>%
  mutate(mark_star = ifelse(
    Location == "UNB-2" & Depth %in% c('10-15cm', '15-20cm', '20-25cm', '25-30cm', '30-35cm', '35-40cm', '40-45cm', '45-50cm', '50-55cm') & Size == "500um",
    "*", ""
  ))

# Plot stacked bar chart
p<- ggplot(df_count_NF, aes(x = Depth, y = count, fill = factor(Size, levels = c( "250um", "500um","1000um")), group_by(Location))) +
  geom_bar(stat = "identity", position = "stack",color = "black") +
  geom_text(
    aes(label = mark_star),
    position = position_stack(vjust = 1.05),
    size = 5
  )+
  facet_wrap(~ Location, scales = "free_y") +  # Add free_y scales here
  theme_bw() +
  labs(x = c('UNB-1', 'UNB-2', 'UNB-3'), y = "Count", fill = "Sieve Size",
       title = "Non-Fibers at 5 cm Intervals") +
  theme(text = element_text(family = "Times New Roman"),# global font
        axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

p2<- p+geom_hline(yintercept = 12, linetype = "dashed", color = "orange", linewidth = 0.8)


#p2 <- p + labs(
#  caption = "*indicates the 250-500 um sieved size class was not completed"
#) +
#  theme(
#    plot.caption = element_text(hjust = 0.5, size = 10, face = "italic")
#  )



# Summarize data: Count occurrences of 1st order per site
df_count_F <- df %>%
  filter(`1st Order Morph`== 'Fiber') %>%
  group_by(Location,Sample, Depth, Size) %>%
  summarise(count = n(), .groups = "drop")

df_count_F <- df_count_F %>%
  mutate(mark_star = ifelse(
    Location == "UNB-2" & Depth %in% c('10-15cm', '15-20cm', '20-25cm', '25-30cm', '30-35cm', '35-40cm', '40-45cm', '45-50cm', '50-55cm') & Size == "500um",
    "*", ""
  ))

# Plot stacked bar chart
p<- ggplot(df_count_F, aes(x = Depth, y = count, fill = factor(Size, levels = c( "250um", "500um","1000um")), group_by(Location))) +
  geom_bar(stat = "identity", position = "stack",color = "black") +
  geom_text(
    aes(label = mark_star),
    position = position_stack(vjust = 1.05),
    size = 5
  )+
  facet_wrap(~ Location, scales = "free_y") +  # Add free_y scales here
  theme_bw() +
  labs( y = "Count", fill = "Sieve Size",
       title = "Fibers at 5 cm Intervals") +
  theme(text = element_text(family = "Times New Roman"),# global font
        axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for readability

p3 <- p+geom_hline(yintercept = 48, linetype = "dashed", color = "blue", linewidth = 0.8)


#p3 <- p + labs(
#  caption = "*indicates the 250-500 um sieved size class was not completed"
#) +
#  theme(
#    plot.caption = element_text(hjust = 0.5, size = 10, face = "italic")
#  )


# Suppress x-axes for top two plots
p1 <- p1 + theme(text = element_text(family = "Times New Roman"),# global font
  axis.title.x = element_blank(),
  axis.text.x  = element_blank(),
  axis.ticks.x = element_blank()
)

p2 <- p2 + theme(text = element_text(family = "Times New Roman"),# global font
  axis.title.x = element_blank(),
  axis.text.x  = element_blank(),
  axis.ticks.x = element_blank()
)

# Match y-axis appearance of p1
p2 <- p2 + theme(text = element_text(family = "Times New Roman"),# global font
  axis.title.y = element_text(size = 12),
  axis.text.y  = element_text(size = 10)
)

p3 <- p3 + theme(text = element_text(family = "Times New Roman"),# global font
  axis.title.y = element_text(size = 12),
  axis.text.y  = element_text(size = 10)
)



library(patchwork)

# Combine plots with one shared legend
combined_plot <- (p1 / p2 / p3) +
  plot_layout(guides = "collect") &               
  theme(legend.position = "bottom")                

# Add a shared caption (note) at the bottom
combined_plot <- combined_plot +
  plot_annotation(
    caption = "* indicates the 250–500 µm sieved size class was not completed",
    theme = theme(text = element_text(family = "Times New Roman"),# global font,
      plot.caption = element_text(
        hjust = 0.5,
        size = 12,
        face = "italic"
      )
    )
  )

# Display final combined figure
combined_plot



# Summarize data: Count occurrences of morphology per site
df_morph <- df %>%
  group_by(Location,Sample, Depth, Morphology) %>%
  summarise(count = n(), .groups = "drop")

# View result
print(df_morph)

# Plot stacked bar chart
p<- ggplot(df_morph, aes(x = Depth, y = count, fill = Morphology), group_by(Location)) +
  geom_bar(stat = "identity", position = "stack",color = "black") +
  facet_wrap(~ Location) +  # Create one panel per Location
  theme_bw() +
  labs(x = c('UNB-1', 'UNB-2', 'UNB-3'), y = "Count", fill = "Morphology",
       title = "Distribution of Morphologies by Site: All Microplastics") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+  # Rotate x-axis labels for readability
  labs(x = "Depth", y = "Count", fill = "Morphology") +
  scale_fill_manual(values = c(
    "fiber" = "#F8766D",      # Orange
    "fiber bundle" = "#CD9600", 
    "film" = "#7CAE00",
    "foam" = "#00BE67",
    "fragment" = "#00BFC4",   # Blue# Yellow
    "pellet" = "#00A9FF",
    "sphere" = "#C77CFF",      # Red
    "TRWP" = "#FF61CC"
  ))

p





df_morph_NF <- df %>%
  filter(`1st Order Morph`== 'Non-Fiber') %>%
  group_by(Location,Sample, Depth, Morphology) %>%
  summarise(count = n(), .groups = "drop")

# Plot stacked bar chart
p<- ggplot(df_morph_NF, aes(x = Depth, y = count, fill = Morphology), group_by(Location)) +
  geom_bar(stat = "identity", position = "stack",color = "black") +
  facet_wrap(~ Location) +  # Create one panel per Location
  theme_bw() +
  labs(x = c('UNB-1', 'UNB-2', 'UNB-3'), y = "Count", fill = "Morphology",
       title = "Distribution of Morphologies by Site: Non-Fibers Only") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+  # Rotate x-axis labels for readability
  labs(x = "Depth", y = "Count", fill = "Morphology") +
  scale_fill_manual(values = c(
    "fiber" = "#F8766D",      # Orange
    "fiber bundle" = "#CD9600", 
    "film" = "#7CAE00",
    "foam" = "#00BE67",
    "fragment" = "#00BFC4",   # Blue# Yellow
    "pellet" = "#00A9FF",
    "sphere" = "#C77CFF",      # Red
    "TRWP" = "#FF61CC"
  ))
  
p

scales::hue_pal()(8)  # Generate default ggplot colors for 6 categories



