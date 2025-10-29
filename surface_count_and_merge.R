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

df <- read_csv("surface_paired_merged_data_v2.csv")

df <- df %>%
  filter(`1st Order Morph`!= 'NA') %>%
  filter(`1st Order Morph`!= 'Spike')

# Summarize data: Count occurrences of MP
df_count <- df %>%
  group_by(Location,Sample, Depth) %>%
  summarise(Total_Cnt = n(), .groups = "drop")

# Summarize data: Count occurrences of 1st order 
df_count_NF <- df %>%
  filter(`1st Order Morph`== 'Non-Fiber') %>%
  group_by(Location,Sample, Depth) %>%
  summarise(Total_NF = n(), .groups = "drop")

# Summarize data: Count occurrences of 1st order 
df_count_F <- df %>%
  filter(`1st Order Morph`== 'Fiber') %>%
  group_by(Location,Sample, Depth) %>%
  summarise(Total_F = n(), .groups = "drop")

# Summarize data: Count occurrences of morpholoy
df_count_fiber <- df %>%
  filter(`Morphology`== 'fiber') %>%
  group_by(Location,Sample, Depth) %>%
  summarise(Total_fiber = n(), .groups = "drop")

# Summarize data: Count occurrences of morpholoy
df_count_fiberbun <- df %>%
  filter(`Morphology`== 'fiber bundle') %>%
  group_by(Location,Sample, Depth) %>%
  summarise(Total_fiberbun = n(), .groups = "drop")

# Summarize data: Count occurrences of morpholoy
df_count_film <- df %>%
  filter(`Morphology`== 'film') %>%
  group_by(Location,Sample, Depth) %>%
  summarise(Total_film = n(), .groups = "drop")

# Summarize data: Count occurrences of morpholoy
df_count_foam <- df %>%
  filter(`Morphology`== 'foam') %>%
  group_by(Location,Sample, Depth) %>%
  summarise(Total_foam = n(), .groups = "drop")

# Summarize data: Count occurrences of morpholoy
df_count_fragment <- df %>%
  filter(`Morphology`== 'fragment') %>%
  group_by(Location,Sample, Depth) %>%
  summarise(Total_fragment = n(), .groups = "drop")

# Summarize data: Count occurrences of morpholoy
df_count_pellet <- df %>%
  filter(`Morphology`== 'pellet') %>%
  group_by(Location,Sample, Depth) %>%
  summarise(Total_pellet = n(), .groups = "drop")

# Summarize data: Count occurrences of morpholoy
df_count_sphere <- df %>%
  filter(`Morphology`== 'sphere') %>%
  group_by(Location,Sample, Depth) %>%
  summarise(Total_sphere = n(), .groups = "drop")

# Summarize data: Count occurrences of morpholoy
df_count_TRWP <- df %>%
  filter(`Morphology`== 'TRWP') %>%
  group_by(Location,Sample, Depth) %>%
  summarise(Total_TRWP = n(), .groups = "drop")

#merge results together
merged_df <- df_count %>%
  full_join(df_count_NF,by = "Sample") %>%
  full_join(df_count_F, by = "Sample") %>%
  full_join(df_count_fiber, by = "Sample") %>%
  full_join(df_count_fiberbun, by = "Sample") %>%
  full_join(df_count_film, by = "Sample") %>%
  full_join(df_count_foam, by = "Sample") %>%
  full_join(df_count_fragment, by = "Sample") %>%
  full_join(df_count_pellet, by = "Sample") %>%
  full_join(df_count_sphere, by = "Sample") %>%
  full_join(df_count_TRWP, by = "Sample")

write.csv(merged_df,"summary_surface_counts_v2.csv")
