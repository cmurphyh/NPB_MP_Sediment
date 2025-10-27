# Clear environment
rm(list=ls()); graphics.off(); cat("\f")

# Set working directory
setwd("~/Desktop/Papers/Sediment Paper/Code")

# Load packages
library(tidyverse)
library(patchwork)


df_UNB1 <- read_csv("UNB1_age_depth_conc_v3.csv")
df_UNB1 <- rbind.data.frame(df_UNB1[c(1:53,55),]) #v2

df_UNB3 <- read_csv("UNB3_age_depth_conc_v3.csv")
df_UNB3 <- rbind.data.frame(df_UNB3[1:66,]) #v2

df_UNB1$median <- as.numeric(df_UNB1$median)
df_UNB3$median <- as.numeric(df_UNB3$median)

# Plot 1: UNB-1 Conc_Total [n/kg dw]
p1 <- 
  ggplot(df_UNB1, aes(x = 1000*`Median_n/g/yr` , y = `median`, group=group)) +
  geom_line(color = "#007200", size = 1.5) +
  scale_x_continuous(
    position = "top", limits = c(0,100)) +
  scale_y_continuous(
    position = "left", limits = c(1950, 2021.7),  breaks = seq(1950, 2020, by = 10)) +
  labs(
    title = "UNB-3 ",
    x = " MP Accretion (n/kg/yr)",
    y = "Year"
  ) +
  theme_minimal(base_family = "Times New Roman", base_size = 12) +
  theme(
    legend.text  = element_text(size = 12),
    legend.title = element_text(size = 12),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    plot.title   = element_text(size = 12, face = "bold")
  )

p1



# Plot 1: UNB-1 Conc_Total [n/kg dw]
p2 <- 
  ggplot(df_UNB3, aes(x = 1000*`Median_n/g/yr` , y = `median`, group=group)) +
  geom_line(color = "#007200", size = 1.5) +
  scale_x_continuous(
    position = "top", limits = c(0,400)) +
  scale_y_continuous(
    position = "left", limits = c(1950, 2021.7),  breaks = seq(1950, 2020, by = 10)) +
  labs(
    title = "UNB-3 ",
    x = " MP Accretion (n/kg/yr)",
    y = "Year"
  ) +
  theme_minimal(base_family = "Times New Roman", base_size = 12) +
  theme(
    legend.text  = element_text(size = 12),
    legend.title = element_text(size = 12),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    plot.title   = element_text(size = 12, face = "bold")
  )

p2



# Display side-by-side using patchwork
library(patchwork)
p1 + p2

