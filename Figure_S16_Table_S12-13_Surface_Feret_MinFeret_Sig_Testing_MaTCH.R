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
library(patchwork)

df <- read_csv("Surface_MaTCH_v4_Ws_d.csv")


df <- df %>%
  filter( X1st.Order.Morph != 'NA') %>%
  filter( Morphology != 'spike') %>%
  filter(Sample != 'SUB-4-10-S1') %>%
  filter(Sample !=  'SUB-4-7-S1') %>%
  filter(Sample !=  'SUB-4-9-S1') 

df <- df %>%
  filter(Feret <= 5) %>%
  filter(Feret >= 0.250)

#df <- df %>%
#  filter(is.na(OS_Plastic_Not) | OS_Plastic_Not != 'Not')


df_NF <- df %>%
  filter(X1st.Order.Morph== 'Non-Fiber') 

NF_Feret <- df_NF$Feret
summary(NF_Feret)


NF_Ferets <- df %>%
  filter(X1st.Order.Morph == 'Non-Fiber') %>%
  group_by(Site) %>%
  summarise(
    n = n(),
    mean = mean(Feret, na.rm = TRUE),
    median = median(Feret, na.rm = TRUE),
    sd = sd(Feret, na.rm = TRUE),
    min = min(Feret, na.rm = TRUE),
    max = max(Feret, na.rm = TRUE)
  )

NF_Feret_samp <- df %>%
  filter(X1st.Order.Morph == 'Non-Fiber') %>%
  group_by(Sample) %>%
  summarise(
    n = n(),
    mean = mean(Feret, na.rm = TRUE),
    median = median(Feret, na.rm = TRUE),
    sd = sd(Feret, na.rm = TRUE),
    min = min(Feret, na.rm = TRUE),
    max = max(Feret, na.rm = TRUE)
  )

###############################################################################
###############################################################################

# Transform Non-Fiber data into long format for ggplot
df_NF <- df_NF %>%
  select(Site, Sample, X1st.Order.Morph, Feret, MinFeret) %>%
  pivot_longer(cols = c(Feret), 
               names_to = "dimension", values_to = "Size_mm")

df_NF$Size_mm <-as.numeric(df_NF$Size_mm)

# Create boxplots

# Compute sample size per Site
site_counts2 <- df_NF %>%
  filter(dimension == "Feret") %>%
  group_by(Site) %>%
  summarise(n = n()) %>%
  mutate(Site_label = paste0(Site, "\n(N = ", n, ")"))

df_plot2 <- df_NF %>%
  filter(dimension == "Feret") %>%
  left_join(site_counts2, by = "Site")

# Compute sample size per Sample
site_counts1 <- df_NF %>%
  filter(dimension == "Feret") %>%
  group_by(Sample) %>%
  summarise(n = n()) %>%
  mutate(Sample_label = paste0(Sample, "\n(N = ", n, ")"))

df_plot1 <- df_NF %>%
  filter(dimension == "Feret") %>%
  left_join(site_counts1, by = "Sample")


#Plot by Sample
p1 <- ggplot(df_plot1, aes(x = Sample_label, y = Size_mm*1000 , fill = Site)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA)+
  geom_jitter(aes(color = Site), width = 0.2, alpha = 0.5) +
  coord_cartesian(ylim = c(0, 5000)) +
  facet_wrap(~ dimension, scales = "free_y" ) +
  labs(title = "Surface Sediments (0-5 cm): Particle Size of Non-Fibers only",
       x = NULL, y = 'Feret Diameter [µm]') +
  theme(
    text = element_text(family = "Times New Roman"),# global font
    strip.text = element_text(family = "Times New Roman",     # facet labels
                              size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, vjust = 0.5, size = 10),
    axis.text.y = element_text(size = 12),
    plot.title  = element_text(size = 12, face = "bold")
  )

#Plot by Site
p2 <- ggplot(df_plot2, aes(x = Site_label, y = Size_mm * 1000 , fill = Site)) + 
  geom_boxplot(alpha = 0.7, outlier.shape = NA)+
  geom_jitter(aes(color = Site), width = 0.2, alpha = 0.5) +
  coord_cartesian(ylim = c(0, 5000)) +
  facet_wrap(~ dimension, scales = "free_y" ) +
  labs(
    title = "Surface Sediments (0-5 cm): Particle Size of Non-Fibers only",
    x = NULL, y = 'Feret Diameter [µm]') +
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

# Combine with patchwork
combined_plot <- (p1 / p2 ) + 
  plot_layout(widths = c(1.5, 1)) &   # Adjust relative widths as needed
  theme(legend.position = "bottom")

combined_plot
###############################################################################
###############################################################################
#Significance Testing

# Test normality of Feret and MinFeret for each Site
#library(rstatix)

#df_NF %>%
#  group_by(Site, dimension) %>%
#  shapiro_test(Size_mm)

# Test homogeneity of variance (Levene's Test)
#library(car)

#leveneTest(Size_mm ~ Site, data = df_NF)

#ALL DATASETS WERE NOT NORMAL SO GOING WITH NONPARAMETRIC OPTION

df_NF_Site <- df_NF

df_NF_Sample <- df_NF

# Kruskal-Wallis by Site
df_NF_Site %>%
  group_by(dimension) %>%
  summarise(kw_test = list(kruskal.test(Size_mm ~ Site, data = .))) %>%
  pull(kw_test)

# Kruskal-Wallis by Sample
df_NF_Sample %>%
  group_by(dimension) %>%
  summarise(kw_test = list(kruskal.test(Size_mm ~ Sample, data = .))) %>%
  pull(kw_test)

library(FSA)

# Dunn's test by Site for Feret and MinFeret
df_NF_Site %>%
  group_by(dimension) %>%
  group_map(~ dunnTest(Size_mm ~ Site, data = .x, method = "holm"))
dunn_result <- dunnTest(Size_mm ~ Site, data = df_NF, method = "holm")
# View the full table
View(dunn_result$res)


df_NF_Sample %>%
  group_by(dimension) %>%
  group_map(~ dunnTest(Size_mm ~ Sample, data = .x, method = "holm"))
dunn_result <- dunnTest(Size_mm ~ Sample, data = df_NF, method = "holm")
View(dunn_result$res)
# View the full table
View(dunn_result$res)
###############################################################
###############################################################
