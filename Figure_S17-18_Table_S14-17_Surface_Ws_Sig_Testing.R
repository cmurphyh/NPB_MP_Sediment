
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
library(rstatix)

df <- read_csv("Surface_MaTCH_v4_Ws_d.csv")


df <- df %>%
  filter(!is.na(`X1st.Order.Morph`)) %>%
  filter(Morphology != 'spike') %>%
  filter(!Sample %in% c('SUB-4-10-S1', 'SUB-4-7-S1', 'SUB-4-9-S1')) %>%
  filter(Feret <= 5)  %>%
  filter(Feret >= 0.250)

#df <- df %>%
#  filter(is.na(OS_Plastic_Not) | OS_Plastic_Not != 'Not')


df_spectra_only_NF_noTRWP <- df %>%
  filter(Material..Raw.Data. != 'NA') %>%
  filter(X1st.Order.Morph == 'Non-Fiber') %>%
filter( Morphology != 'rubbery fragment') 

df_All_NF_noTRWP <- df %>%
  filter(X1st.Order.Morph == 'Non-Fiber') %>%
filter( Morphology != 'rubbery fragment') 

df_All_NF <- df %>%
  filter(X1st.Order.Morph == 'Non-Fiber') 

df1 <- df_spectra_only_NF_noTRWP

df2 <- df_All_NF_noTRWP

df3 <- df_All_NF

df_spectra_only_site_NF_noTRWP <- df1 %>%
  group_by(Site) %>%
  summarise(
    n = n(),
    mean = mean( W_adj, na.rm = TRUE),
    median = median(W_adj, na.rm = TRUE),
    sd = sd(W_adj, na.rm = TRUE),
    min = min(W_adj, na.rm = TRUE),
    max = max(W_adj, na.rm = TRUE)
  )

df_All_site_NF_noTRWP <- df2 %>%
  group_by(Site) %>%
  summarise(
    n = n(),
    mean = mean( W_adj, na.rm = TRUE),
    median = median(W_adj, na.rm = TRUE),
    sd = sd(W_adj, na.rm = TRUE),
    min = min(W_adj, na.rm = TRUE),
    max = max(W_adj, na.rm = TRUE)
  )

df_All_site_NF <- df3 %>%
  group_by(Site) %>%
  summarise(
    n = n(),
    mean = mean( W_adj, na.rm = TRUE),
    median = median(W_adj, na.rm = TRUE),
    sd = sd(W_adj, na.rm = TRUE),
    min = min(W_adj, na.rm = TRUE),
    max = max(W_adj, na.rm = TRUE)
  )

###############################################################################
###############################################################################

df1$W_adj <-as.numeric(df1$W_adj)

df2$W_adj <-as.numeric(df2$W_adj)

df3$W_adj <-as.numeric(df3$W_adj)


# Transform Non-Fiber data into long format for ggplot
df1 <- df1 %>%
  select(Site, Sample, X1st.Order.Morph, W_adj) %>%
  pivot_longer(cols = c(W_adj), 
               names_to = "particle_Ws", values_to = "Ws_m_s")

df2 <- df2 %>%
  select(Site, Sample, X1st.Order.Morph, W_adj) %>%
  pivot_longer(cols = c(W_adj), 
               names_to = "particle_Ws", values_to = "Ws_m_s")

df3 <- df3 %>%
  select(Site, Sample, X1st.Order.Morph, W_adj) %>%
  pivot_longer(cols = c(W_adj), 
               names_to = "particle_Ws", values_to = "Ws_m_s")

###############################################################################
###############################################################################

# Compute sample size per Site
#df1
site_counts1 <- df1 %>%
  filter(Ws_m_s != "NA")  %>%
  group_by(Site) %>%
  summarise(n = n()) %>%
  mutate(Site_label = paste0(Site, "\n(n = ", n, ")"))

df_plot1 <- df1 %>%
  filter(Ws_m_s != "NA")  %>%
  left_join(site_counts1, by = "Site")

#df2
site_counts2 <- df2 %>%
  filter(Ws_m_s != "NA")  %>%
  group_by(Site) %>%
  summarise(n = n()) %>%
  mutate(Site_label = paste0(Site, "\n(n = ", n, ")"))

df_plot2 <- df2 %>%
  filter(Ws_m_s != "NA")  %>%
  left_join(site_counts2, by = "Site")

#df3
site_counts3 <- df3 %>%
  filter(Ws_m_s != "NA")  %>%
  group_by(Site) %>%
  summarise(n = n()) %>%
  mutate(Site_label = paste0(Site, "\n(n = ", n, ")"))

df_plot3 <- df3 %>%
  filter(Ws_m_s != "NA")  %>%
  left_join(site_counts3, by = "Site")

###############################################################################
###############################################################################


# Create boxplots

#Plot by Site
#df1
p1 <- ggplot(df_plot1, aes(x = Site_label, y = Ws_m_s , fill = Site)) + 
  geom_boxplot(alpha = 0.7, outlier.shape = NA)+
  geom_jitter(aes(color = Site), width = 0.2, alpha = 0.5) +
  coord_cartesian(ylim = c(-0.02, 0.02)) +
  facet_wrap(~ particle_Ws, scales = "free_y" ) +
  labs(
    title = "FTIR dataset of Non-Fibers only (No TRWPs)",
    x = NULL, y = 'Particle settling velocity [m/s]') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p1

#df2
p2 <- ggplot(df_plot2, aes(x = Site_label, y = Ws_m_s , fill = Site)) + 
  geom_boxplot(alpha = 0.7, outlier.shape = NA)+
  geom_jitter(aes(color = Site), width = 0.2, alpha = 0.5) +
  coord_cartesian(ylim = c(-0.1, 0.1)) +
  facet_wrap(~ particle_Ws, scales = "free_y" ) +
  labs(
    title = "Surface Sediments (0-5 cm): Ws Non-Fibers only (without TRWPs)",
    x = NULL, y = 'Particle settling velocity [m/s]') +
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

p2

#df3
p3 <- ggplot(df_plot3, aes(x = Site_label, y = Ws_m_s , fill = Site)) + 
  geom_boxplot(alpha = 0.7, outlier.shape = NA)+
  geom_jitter(aes(color = Site), width = 0.2, alpha = 0.5) +
  coord_cartesian(ylim = c(-0.1, 0.1)) +
  facet_wrap(~ particle_Ws, scales = "free_y" ) +
  labs(
    title = "Surface Sediments (0-5 cm): Ws Non-Fibers only (including TRWPs)",
    x = NULL, y = 'Particle Settling Velocity [m/s]') +
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

p3


###############################################################################
###############################################################################
#Significance Testing

# Test normality of Particle Mass for each Site

df1 %>%
  group_by(Site, particle_Ws) %>%
  shapiro_test(Ws_m_s)

df2 %>%
  group_by(Site, particle_Ws) %>%
  shapiro_test(Ws_m_s)

df3 %>%
  group_by(Site, particle_Ws) %>%
  shapiro_test(Ws_m_s)

###############################################################################
#If normal then:
# Test homogeneity of variance (Levene's Test)
#library(car)
#leveneTest(Mass_mg ~ Site, data = df_NF)
###############################################################################

#ALL DATASETS WERE NOT NORMAL SO GOING WITH NONPARAMETRIC OPTION

df1_Sig <- df1

df2_Sig <- df2

df3_Sig <- df3

# Kruskal-Wallis by Site
df1_Sig %>%
  group_by(particle_Ws) %>%
  summarise(kw_test = list(kruskal.test(Ws_m_s ~ Site, data = .))) %>%
  pull(kw_test)


df2_Sig %>%
  group_by(particle_Ws) %>%
  summarise(kw_test = list(kruskal.test(Ws_m_s ~ Site, data = .))) %>%
  pull(kw_test)

df3_Sig %>%
  group_by(particle_Ws) %>%
  summarise(kw_test = list(kruskal.test(Ws_m_s ~ Site, data = .))) %>%
  pull(kw_test)

# Dunn's test by Site for Ws
df1_Sig %>%
  group_by(particle_Ws) %>%
  group_map(~ dunnTest(Ws_m_s ~ Site, data = .x, method = "holm"))
dunn_result <- dunnTest(Ws_m_s ~ Site, data = df1_Sig, method = "holm")
# View the full table
View(dunn_result$res)

df2_Sig %>%
  group_by(particle_Ws) %>%
  group_map(~ dunnTest(Ws_m_s ~ Site, data = .x, method = "holm"))
dunn_result <- dunnTest(Ws_m_s ~ Site, data = df2_Sig, method = "holm")
# View the full table
View(dunn_result$res)

df3_Sig %>%
  group_by(particle_Ws) %>%
  group_map(~ dunnTest(Ws_m_s ~ Site, data = .x, method = "holm"))
dunn_result <- dunnTest(Ws_m_s ~ Site, data = df3_Sig, method = "holm")
# View the full table
View(dunn_result$res)

###############################################################################
###############################################################################
###############################################################################
###############################################################################

df_plot3 %>%
  filter(X1st.Order.Morph == 'Non-Fiber') %>%
  group_by(Site) %>%
  summarise(
    n = n(),
    mean = mean(`Ws_m_s`, na.rm = TRUE),
    median = median(`Ws_m_s`, na.rm = TRUE),
    sd = sd(`Ws_m_s`, na.rm = TRUE),
    min = min(`Ws_m_s`, na.rm = TRUE),
    max = max(`Ws_m_s`, na.rm = TRUE)
  )

df_plot3 %>%
  filter(X1st.Order.Morph == 'Non-Fiber') %>%
  group_by(Sample) %>%
  summarise(
    n = n(),
    mean = mean(`Ws_m_s`, na.rm = TRUE),
    median = median(`Ws_m_s`, na.rm = TRUE),
    sd = sd(`Ws_m_s`, na.rm = TRUE),
    min = min(`Ws_m_s`, na.rm = TRUE),
    max = max(`Ws_m_s`, na.rm = TRUE)
  )

###############################################################################
###############################################################################


# Transform Non-Fiber data into long format for ggplot
df_NF <- df_plot3 %>%
  select(Site, Sample, X1st.Order.Morph, `Ws_m_s`) %>%
  pivot_longer(cols = c(`Ws_m_s`), 
               names_to = "particle_Ws", values_to = "Ws_m_s")

df_NF$Ws_m_s <-as.numeric(df_NF$Ws_m_s)

# Create boxplots

# Compute sample size per Site

# Compute sample size per Sample
site_counts4 <- df_NF %>%
  filter(particle_Ws == "Ws_m_s") %>%
  group_by(Sample) %>%
  summarise(n = n()) %>%
  mutate(Sample_label = paste0(Sample, "\n(n = ", n, ")"))

df_plot4 <- df_NF %>%
  filter(particle_Ws == "Ws_m_s") %>%
  left_join(site_counts4, by = "Sample")

df_All_sample_NF <- df_plot4 %>%
  filter(X1st.Order.Morph == 'Non-Fiber') %>%
  group_by(Sample) %>%
  summarise(
    n = n(),
    mean = mean(`Ws_m_s`, na.rm = TRUE),
    median = median(`Ws_m_s`, na.rm = TRUE),
    sd = sd(`Ws_m_s`, na.rm = TRUE),
    min = min(`Ws_m_s`, na.rm = TRUE),
    max = max(`Ws_m_s`, na.rm = TRUE)
  )


#Plot by Sample
p4 <- ggplot(df_plot4, aes(x = Sample_label, y = Ws_m_s , fill = Site)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA)+
  geom_jitter(aes(color = Site), width = 0.2, alpha = 0.5) +
  coord_cartesian(ylim = c(-0.1, 0.1)) +
  facet_wrap(~ particle_Ws, scales = "free_y" ) +
  labs(title = "Surface Sediments (0-5 cm): Ws Non-Fibers only (including TRWPs)",
       x = NULL, y = 'Particle Settling Velocity [m/s]') +
 # scale_y_log10() +  # <--- log scale here
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

p4

# Combine with patchwork
combined_plot <- (p4 / p3 ) + 
  plot_layout(widths = c(1.5, 1)) &   # Adjust relative widths as needed
  theme(legend.position = "bottom")

combined_plot


# Compute sample size per Sample
#df2
site_counts5 <- df2 %>%
  filter(Ws_m_s != "NA")  %>%
  group_by(Sample) %>%
  summarise(n = n()) %>%
  mutate(Sample_label = paste0(Sample, "\n(n = ", n, ")"))

df_plot5 <- df2 %>%
  filter(Ws_m_s != "NA")  %>%
  left_join(site_counts5, by = "Sample")

df_All_sample_NF <- df_plot5 %>%
  filter(X1st.Order.Morph == 'Non-Fiber') %>%
  group_by(Sample) %>%
  summarise(
    n = n(),
    mean = mean(`Ws_m_s`, na.rm = TRUE),
    median = median(`Ws_m_s`, na.rm = TRUE),
    sd = sd(`Ws_m_s`, na.rm = TRUE),
    min = min(`Ws_m_s`, na.rm = TRUE),
    max = max(`Ws_m_s`, na.rm = TRUE)
  )

#Plot by Sample
p5 <- ggplot(df_plot5, aes(x = Sample_label, y = Ws_m_s , fill = Site)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA)+
  geom_jitter(aes(color = Site), width = 0.2, alpha = 0.5) +
  coord_cartesian(ylim = c(-0.1, 0.1)) +
  facet_wrap(~ particle_Ws, scales = "free_y" ) +
  labs(title = "Surface Sediments (0-5 cm): Ws Non-Fibers only (without TRWPs)",
       x = NULL, y = 'Particle Settling Velocity [m/s]') +
  # scale_y_log10() +  # <--- log scale here
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

p5

# Combine with patchwork
combined_plot <- (p5 / p2 ) + 
  plot_layout(widths = c(1.5, 1)) &   # Adjust relative widths as needed
  theme(legend.position = "bottom")

combined_plot