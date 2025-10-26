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

# Read dataset
df <- read_csv('Surface_Sed_MPmass_SHP_v4.csv')

df <- df %>%
  filter(Sample != 'SUB-4-10-S1') %>%
  filter(Sample !=  'SUB-4-7-S1') %>%
  filter(Sample !=  'SUB-4-9-S1')

write.csv(df, "Surface_Sample_Mass_Results_no_reps.csv")



# Order sites (reuse the same across both datasets)
site_levels <- c("Upper Bay Saltmarsh","Upper Bay Basin I/III",
                 "Upper Bay Basin II","Middle Newport Bay","Lower Harbor")


################################################################################

#Transform data into long-format for plotting

# Transform All MP Composition data into long format for ggplot
df_All_Comp <- df %>%
  select(Sample, Site, Perc_fiber, Perc_fiberbun, Perc_film, Perc_sphere, Perc_foam, Perc_fragment, Perc_pellet, Perc_TRWP) %>%
  pivot_longer(cols = c(Perc_fiber, Perc_fiberbun, Perc_film, Perc_sphere, Perc_foam, Perc_fragment, Perc_pellet, Perc_TRWP), 
               names_to = "Particle_Type", values_to = "Composition")


# Transform 1st Order Concentration data into long format for ggplot
df_1stOrder_Conc <- df %>%
  select(Sample, Site,  Av_Conc_NonFiber, Av_Conc_Fiber) %>%
  pivot_longer(cols = c( Av_Conc_NonFiber, Av_Conc_Fiber), 
               names_to = "Particle_Type", values_to = "Concentration")

df_1stOrder_Conc <- df_1stOrder_Conc %>%
  mutate(Concentration = Concentration * 1000)  # convert g/g → mg/g


#Transform Concentration data into long format for ggplot
df_long_Con <- df %>%
  select(Sample, Site, Av_Conc_Total, Av_Conc_NonFiber, Av_Conc_TRWP) %>%
  pivot_longer(cols = c(Av_Conc_Total, Av_Conc_NonFiber,  Av_Conc_TRWP), 
               names_to = "Particle_Type", values_to = "Concentration")

# Transform Concentration data into long format for ggplot
df_long <- df %>%
  select(Sample, Site, Av_Conc_fiber, Av_Conc_fiberbun, Av_Conc_film, Av_Conc_foam, Av_Conc_sphere, Av_Conc_fragment, Av_Conc_pellet, Av_Conc_TRWP) %>%
  pivot_longer(cols = c(Av_Conc_fiber, Av_Conc_fiberbun, Av_Conc_film, Av_Conc_foam, Av_Conc_sphere, Av_Conc_fragment, Av_Conc_pellet, Av_Conc_TRWP), 
               names_to = "Particle_Type", values_to = "Concentration")

# Transform 1st Order Composition data into long format for ggplot
df_1stOrder_Perc <- df %>%
  select(Sample, Site,  Perc_NF, Perc_F) %>%
  pivot_longer(cols = c( Perc_NF, Perc_F), 
               names_to = "Particle_Type", values_to = "Composition")

################################################################################
#Figures for Illustration
################################################################################
# (1) Stacked bar chart for 1st Order Concentration


df_1stOrder_Conc <- df_1stOrder_Conc %>%
  mutate(Site = factor(Site, levels = site_levels)) %>%  # set site order
  arrange(Site, Sample) %>%                               # sort rows by Site, then Sample
  mutate(Sample = factor(Sample, levels = unique(Sample)))# lock bar order on x-axis

vline_positions <- df_1stOrder_Conc %>%
  distinct(Sample, Site) %>%
  count(Site, name = "n") %>%
  mutate(cum = cumsum(n), x = cum + 0.5) %>%
  pull(x)
vline_positions <- vline_positions[-length(vline_positions)]



# Define label replacements for Particle_Type
conc_labels <- c(
  Av_Conc_NonFiber = "Non-Fiber",
  Av_Conc_Fiber = "Fiber"
)

#Plot
ggplot(df_1stOrder_Conc, aes(x = Sample, y = Concentration, fill = Particle_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_vline(xintercept = vline_positions, color = "black", linetype = "solid", size = 0.5) +
  theme_minimal() +
  labs(title = "Mass-based MP Concentration: Surface Sediments (0-5 cm)",
       x = NULL, y = expression(Concentration~"("*mg^1~g^{-1}~"dw sediment"*")"),
       fill = "Particle Type") +
  scale_fill_manual(
    values = c("steelblue", "darkorange"),
    labels = conc_labels
  ) +
  #scale_y_continuous(limits = c(0, 20), expand = expansion(mult = c(0, 0.05))) +
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

################################################################################

#################################################################################
####Significance Testing between Sites: Concentration Total, Non-Fiber, TRWP
#################################################################################
# Check normality for each Particle Type
df_long_Con %>%
  group_by(Particle_Type) %>%
  summarise(p_value = shapiro.test(Concentration)$p.value)


#If p < 0.05, the data is not normally distributed → Use Kruskal-Wallis
#If p > 0.05, the data is normally distributed → Use ANOVA

#Anova
#anova_result <- aov(Concentration ~ Site * Particle_Type, data = df_long_Con)
#summary(anova_result)
#TukeyHSD(anova_result)


#Kruskal-Wallis
kruskal_test <- df_long_Con %>%
  group_by(Particle_Type) %>%
  summarise(p_value = kruskal.test(Concentration ~ Site)$p.value)
print(kruskal_test)

#if significant than compare pairs with Dunns
# Perform Dunn's test for each Particle_Type
dunn_results <- df_long_Con %>%
  group_by(Particle_Type) %>%
  do({
    test_result <- dunnTest(Concentration ~ Site, data = ., method = "bonferroni")
    test_result$res  # Extract the results data frame from the dunnTest object
  })

# View the full table
View(dunn_results)

# View results
print(dunn_results)

write.csv(dunn_results,"surf_sig_massconc_kw_dunn_results.csv")

#################################################################################
####Signifigance Testing between Sites: Composition and Morphology
#################################################################################

# Check normality for each Particle Type

df_All_Comp <- df_All_Comp %>%
  filter(Particle_Type != 'Perc_pellet')

df_All_Comp %>%
  group_by(Particle_Type) %>%
  summarise(p_value = shapiro.test(Composition)$p.value)


#If p < 0.05, the data is not normally distributed → Use Kruskal-Wallis
#If p > 0.05, the data is normally distributed → Use ANOVA

#Anova
#anova_result <- aov(Composition ~ Site * Particle_Type, data = df_All_Comp)
#summary(anova_result)

#Tukey_result <-TukeyHSD(anova_result)

# Extract Tukey HSD results
#tukey_results <- TukeyHSD(anova_result)

# Convert to a data frame for easier filtering
#tukey_df <- as.data.frame(tukey_results$`Site:Particle_Type`)

# Add comparison column
#tukey_df$Comparison <- rownames(tukey_df)

# Filter significant results (p < 0.05)
#significant_results <- tukey_df %>%
#  filter(`p adj` < 0.05)

#print(tukey_df)

#write.csv(tukey_df,"surf_sig_comp_anova_tukey_sigresults.csv")

#Kruskal-Wallis
kruskal_test <- df_All_Comp %>%
  group_by(Particle_Type) %>%
  summarise(p_value = kruskal.test(Composition ~ Site)$p.value)
print(kruskal_test)

#if significant than compare pairs with Dunns
# Perform Dunn's test for each Particle_Type
dunn_results <- df_All_Comp %>%
  group_by(Particle_Type) %>%
  do({
    test_result <- dunnTest(Composition ~ Site, data = ., method = "bonferroni")
    test_result$res  # Extract the results data frame from the dunnTest object
  })

# View results
print(dunn_results, n=70)

write.csv(dunn_results,"surf_sig_comp_kw_dunn_results.csv")

#####################################################################
####Signifigance Testing between Sites: Concentration and Morphology
#####################################################################


df_long <- df_long %>%
  filter(Particle_Type != 'Conc_pellet')

# Check normality for each Particle Type
df_long %>%
  group_by(Particle_Type) %>%
  summarise(p_value = shapiro.test(Concentration)$p.value)


#If p < 0.05, the data is not normally distributed → Use Kruskal-Wallis
#If p > 0.05, the data is normally distributed → Use ANOVA

#Anova
#anova_result <- aov(Concentration ~ Site * Particle_Type, data = df_long)
#summary(anova_result)

#Tukey_result <-TukeyHSD(anova_result)

# Extract Tukey HSD results
#tukey_results <- TukeyHSD(anova_result)

# Convert to a data frame for easier filtering
#tukey_df <- as.data.frame(tukey_results$`Site:Particle_Type`)

# Add comparison column
#tukey_df$Comparison <- rownames(tukey_df)

# Filter significant results (p < 0.05)
#significant_results <- tukey_df %>%
#  filter(`p adj` < 0.05)

#print(tukey_df)

#write.csv(tukey_df,"surf_sig_morphconc_anova_tukey_sigresults.csv")

#Kruskal-Wallis
kruskal_test <- df_long %>%
  group_by(Particle_Type) %>%
  summarise(p_value = kruskal.test(Concentration ~ Site)$p.value)
print(kruskal_test)

#if significant than compare pairs with Dunns
# Perform Dunn's test for each Particle_Type
dunn_results <- df_long %>%
  group_by(Particle_Type) %>%
  do({
    test_result <- dunnTest(Concentration ~ Site, data = ., method = "bonferroni")
    test_result$res  # Extract the results data frame from the dunnTest object
  })

# View results
print(dunn_results, n=70)

write.csv(dunn_results,"surf_sig_morphconc_kw_dunn_results.csv")
