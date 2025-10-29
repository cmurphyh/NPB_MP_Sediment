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
library(patchwork)

# Load and tidy the data
df <- read.csv("UNB3_plot.csv") #replicates removed and gaps extrapolated

y_limits <- c(0, 50)

#Figures for Illustration
################################################################################
# (1) Stacked bar chart for Sediment Grain Size

df_texture <- df %>%
  select(Top, Bottom, width, plot, Perc_VCOARSESAND, Perc_COARSESAND, Perc_MSAND, Perc_FSAND, Perc_VFSAND, Perc_VCOARSESILT, Perc_COARSESILT, Perc_MEDIUMSILT, Perc_FSILT, Perc_VFSILT, Perc_CLAY) %>%
  rename(Depth = plot) %>%
  pivot_longer(cols = c(Perc_VCOARSESAND, Perc_COARSESAND, Perc_MSAND, Perc_FSAND, Perc_VFSAND, Perc_VCOARSESILT, Perc_COARSESILT, Perc_MEDIUMSILT, Perc_FSILT, Perc_VFSILT, Perc_CLAY), 
               names_to = "Sediment_Type", values_to = "Texture")


comp_labels <- c(
  Perc_VCOARSESAND = "Very Coarse Sand",
  Perc_COARSESAND= "Coarse Sand",
  Perc_MSAND = "Medium Sand", 
  Perc_FSAND= "Fine Sand",
  Perc_VFSAND= "Very Fine Sand",
  Perc_VCOARSESILT= "Very Coarse Silt",
  Perc_COARSESILT = "Coarse Silt",
  Perc_MEDIUMSILT = "Medium Silt",
  Perc_FSILT = "Fine Silt",
  Perc_VFSILT = "Very Fine Silt",
  Perc_CLAY = "Clay" )


df_texture <- df_texture %>%
  mutate(Texture = ifelse(is.na(Texture), 0, Texture))

sediment_order <- c(
  "Perc_VCOARSESAND", "Perc_COARSESAND", "Perc_MSAND", "Perc_FSAND", "Perc_VFSAND",
  "Perc_VCOARSESILT", "Perc_COARSESILT", "Perc_MEDIUMSILT", "Perc_FSILT", "Perc_VFSILT",
  "Perc_CLAY"
)


df_texture <- df_texture %>%
  mutate(
    Sediment_Type = factor(Sediment_Type, levels = sediment_order),
    Texture = ifelse(is.na(Texture), 0, Texture)
  )


#Map sediment type to grain size index (1 = coarsest, 11 = finest)
grain_size_index <- setNames(1:length(sediment_order), sediment_order)

df_texture <- df_texture %>%
  mutate(
    Grain_Size_Index = grain_size_index[Sediment_Type],
    Sediment_Type = factor(Sediment_Type, levels = sediment_order)
  )


# Plot: Stacked horizontal bar chart of grain size vs. depth
p1 <- ggplot(df_texture, aes(x = Texture * 100, y = Depth, fill = Grain_Size_Index)) +
  geom_bar(stat = "identity", position = "stack", orientation = "y") +  # Horizontal bar stack
  coord_cartesian(ylim = c(50, 0)) +
  scale_y_reverse(expand = c(0, 0)) +  # Depth increases downward
  scale_x_continuous(position = "top") + 
  theme_minimal() +
  labs(
    x = "Grain Size (%)",
    y = "Depth (cm bgs)",
    title = "UNB-3",
    fill = NULL
  ) +
  scale_fill_gradientn(
    colours = viridis::viridis(11),
    breaks = 1:11,
    labels = comp_labels[sediment_order]
  )+ 
  theme(text = element_text(family = "Times New Roman"),# global font
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.text = element_text(size = 10, angle = 90, hjust = 1),
        legend.key.width = unit(1.5, "cm"),       # Wider legend keys
        legend.key.height = unit(0.5, "cm"),      # Taller keys if needed
        legend.title = element_text(size = 12),
        legend.box.just = "center")

p1

################################################################################
# (2) Stacked bar chart for Sand vs Mud

# Transform SAND vs MUD data into long format for ggplot
df_Sand_Mud <- df %>%
  select(Top, Bottom, width, plot, SAND_Perc, MUD_Perc) %>%
  rename(Depth = plot) %>%
  pivot_longer(cols = c( SAND_Perc, MUD_Perc), 
               names_to = "Sediment_Type", values_to = "Texture")

# Define label replacements for Particle_Type
conc_labels <- c(
  SAND_Perc = "Sand",
  MUD_Perc = "Mud")

#Plot
p2<- ggplot(df_Sand_Mud, aes(x = Texture * 100, y = Depth, fill = Sediment_Type)) +
  geom_bar(stat = "identity", position = "stack", orientation = "y") +
  coord_cartesian(ylim = c(50, 0)) +
  scale_y_reverse(expand = c(0, 0)) +  # Depth increases downward
  scale_x_continuous(position = "top") + 
  theme_minimal() +
  labs(#title = "UNB-3",
    y = NULL, 
    x = "Sediment Type (%)",
    fill = NULL) +
  scale_fill_manual(values = c("lightgreen", "darkgreen"),
                    labels = conc_labels) +
  theme(text = element_text(family = "Times New Roman"),# global font
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.box = "vertical",
        legend.text = element_text(size = 10, angle = 0, hjust = 0),
        legend.key.width = unit(1, "cm"),       # Wider legend keys
        legend.key.height = unit(0.5, "cm"),      # Taller keys if needed
        legend.title = element_text(size = 12),
        legend.box.just = "center")

#p2

################################################################################
# (3) Bar chart for Dry Bulk Density (g/cc) 

# Load and tidy the data
df_BDLOI <- read.csv("UNB3_BDLOI_Results.csv") #replicates removed and gaps extrapolated


# Filter and rename for clarity
df_BDLOI <- df_BDLOI %>%
  select(plot, Dry.BD..g.mL., X..OM.LOI) %>%
  rename(
    DryBD = Dry.BD..g.mL.,
    Percent_OM = X..OM.LOI)


# Plot
p3 <- ggplot(df_BDLOI, aes(y = plot, x = DryBD)) +
  geom_bar(stat = "identity", fill = "steelblue", orientation = "y") +
  coord_cartesian(ylim = c(50, 0)) +
  scale_y_reverse(expand = c(0, 0)) +  # Depth increases downward
  scale_x_continuous(position = "top", limits = c(0, 2)) + 
  theme_minimal() +
  labs(#title = "Dry Bulk Density by Sample",
    y = NULL,
    x = expression(Dry~Bulk~Density~"(g/"*cm^{3}*")"))+
  theme(text = element_text(family = "Times New Roman"),# global font
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.box = "vertical",
        legend.text = element_text(size = 10, angle = 0, hjust = 0),
        legend.key.width = unit(1, "cm"),       # Wider legend keys
        legend.key.height = unit(0.5, "cm"),      # Taller keys if needed
        legend.title = element_text(size = 12),
        legend.box.just = "center")

#p3

################################################################################
# (4) Bar chart for Organic Matter Content (%) 

# Plot
p4 <- ggplot(df_BDLOI, aes(y = plot, x = Percent_OM)) +
  geom_bar(stat = "identity", fill = "gold", orientation = "y") +
  coord_cartesian(ylim = c(50, 0)) +
  scale_y_reverse(expand = c(0, 0)) +  # Depth increases downward
  scale_x_continuous(position = "top", limits = c(0, 60)) + 
  scale_y_continuous(position = "right") +
  theme_minimal() +
  labs(#title = "Dry Bulk Density by Sample",
    y = "Depth (cm bgs)",
    x = "Organic Matter Content (%)" )  +
  theme(text = element_text(family = "Times New Roman"),# global font
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.box = "vertical",
        legend.text = element_text(size = 10, angle = 0, hjust = 0),
        legend.key.width = unit(1, "cm"),       # Wider legend keys
        legend.key.height = unit(0.5, "cm"),      # Taller keys if needed
        legend.title = element_text(size = 12),
        legend.box.just = "center")

#p4


################################################################################
#Plot UNB3
################################################################################

# Plot 5a: UNB-3 Accretion  [n/kg dw/yr]
#p5a <- 
#  ggplot(df, aes(x = 1000*`Median_n/g/yr` , y = `depth`, group=group)) +
#  geom_line(color = "#007200", size = 1.5) +
#  geom_line(aes(x = 1000*`Lower_n/g/yr`), color = "gray", linetype = "dashed", size = 1) +
#  geom_line(aes(x = 1000*`Upper_n/g/yr`), color = "gray", linetype = "dashed", size = 1) +
#  coord_cartesian(ylim = c(50, 0)) +
#  scale_y_reverse(expand = c(0, 0)) +
#  scale_x_continuous(
#    position = "top"
#  ) +
#  labs(
#    title = "UNB-1 Microplastic Accretion",
#    x = " number per kg dw sediment per year [n/kg/yr]]",
#    y = "depth [cm bgs]"
#  ) +
#  theme_minimal()

#p5a


#(5)

df <- read_csv("UNB3_age_depth_conc_v3.csv")

df$mean <- as.numeric(df$mean)

#df <- rbind.data.frame(df[1:56,]) #v1

df <- rbind.data.frame(df[1:66,]) #v2

# Plot 1: Conc_Total [n/kg dw]
p5 <- ggplot(df, aes(x = `Conc_Total [n/g dw]` , y = depth, group=group)) +
  geom_line(color = "#0072B2", size = 1.5) +
  coord_cartesian(ylim = c(50, 0)) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(position = "top") + 
  #scale_y_continuous(position = "right") +# Depth increases downward
  #scale_x_continuous( limits = c(0, 500),
  #  position = "top") +
  # scale_y_continuous(limits = c(1952.2, 2021.7), 
  #                  breaks = c(1952.2, 1962.3, 1969.8, 1976.6, 1983.3, 1990.1, 1995.6, 2002, 2010, 2021.7),
  #                  sec.axis = dup_axis(
  #                   labels = c("45", "40", "35", "30", "25", "20", "15", "10", "5", "0"), # Custom labels
  #                  name = "depth [cm bgs] " )) +                # Right-side axis title
  labs(
    x = "Concentration (n/g dw)",
    y = "Year"
  ) +
  theme_minimal()+
  theme(text = element_text(family = "Times New Roman"),# global font
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.box = "vertical",
        legend.text = element_text(size = 10, angle = 0, hjust = 0),
        legend.key.width = unit(1, "cm"),       # Wider legend keys
        legend.key.height = unit(0.5, "cm"),      # Taller keys if needed
        legend.title = element_text(size = 12),
        legend.box.just = "center")

#p5



# Plot 6: Av_Total [g/kg dw]
p6 <-ggplot(df, aes(x = `Av_Conc_Total [mg/g dw]`, y = depth, group = group)) +
  geom_line(color = "#D55E00", size = 1.5) +
  coord_cartesian(ylim = c(50, 0)) +
  scale_y_reverse(expand = c(0, 0)) +  # Primary y-axis: Depth
  scale_x_continuous(position = "top") +
  geom_line(aes(x = `Min_Conc_Total [mg/g dw]`), color = "gray", linetype = "dashed", size = 1) +
  geom_line(aes(x = `Max_Conc_Total [mg/g dw]`), color = "gray", linetype = "dashed", size = 1) +
  scale_y_continuous(
    limits = y_limits,
    expand = c(0, 0),
    breaks = c(50, 45, 40, 35, 30, 25, 20, 15, 10, 5, 0),
    sec.axis = dup_axis(
      breaks = c(50, 45, 40, 35, 30, 25, 20, 15, 10, 5, 0),
      labels = c( "1973.3", "1976.8", "1981.0", "1985.2", "1989.2", "1993.8", "1998.6", "2003.5", "2008.2", "2014", "2021.7"),
      name = "Year")) +
  labs(
    x = "Concentration (mg/g dw)"
    #y = "Depth (cm bgs)"
  ) +
  theme_minimal()+
  theme(text = element_text(family = "Times New Roman"),# global font
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.box = "vertical",
        legend.text = element_text(size = 10, angle = 0, hjust = 0),
        legend.key.width = unit(1, "cm"),       # Wider legend keys
        legend.key.height = unit(0.5, "cm"),      # Taller keys if needed
        legend.title = element_text(size = 12),
        legend.box.just = "center")

#p6


################################################################################
################################################################################
#Combine plots
# Helper function to add horizontal lines
add_hlines <- function(plot, positions = seq(0, 50, by = 5)) {
  plot + geom_hline(yintercept = positions, color = "black", linetype = "solid", size = 0.5)
}

# Apply to plots
p1 <- add_hlines(p1)
p2 <- add_hlines(p2)
p3 <- add_hlines(p3)
p4 <- add_hlines(p4)
p5 <- add_hlines(p5)
p6 <- add_hlines(p6)
y_limits <- c(0, 50)

#p1 <- p1 + scale_y_continuous(limits = y_limits, expand = c(0, 0))
p2 <- p2 + scale_y_continuous(limits = y_limits, expand = c(0, 0))
p3 <- p3 + scale_y_continuous(limits = y_limits, expand = c(0, 0))
p4 <- p4 + scale_y_continuous(limits = y_limits, expand = c(0, 0))
p5 <- p5 + scale_y_continuous(limits = y_limits, expand = c(0, 0))
#p6 <- p6 + scale_y_continuous(limits = y_limits, expand = c(0, 0))

p6

blank_y_axis <- theme(
  axis.title.y = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank()
)

p2 <- p2 + blank_y_axis
p3 <- p3 + blank_y_axis
p4 <- p4 + blank_y_axis
p5 <- p5 + blank_y_axis
p6 <- p6 + 
  theme(
    axis.title.y.left = element_blank(),
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_blank(),
    axis.line.y.left = element_blank()
  )
# Set your x position (e.g., center of the x-axis or a specific value)
x_pos <- 100  # replace with the correct value from your plot


text_plot <- ggplot() +
 # annotate("text", x = x_pos, y = 6, label = "[3.4 yrs]", size = 3.5) +
  annotate("text", x = x_pos, y = 16, label = "[3.5 yrs]", size = 3.5) +
  annotate("text", x = x_pos, y = 25, label = "[4.2 yrs]", size = 3.5) +
  annotate("text", x = x_pos, y = 34, label = "[4.2 yrs]", size = 3.5) +
  annotate("text", x = x_pos, y = 43, label = "[4.0 yrs]", size = 3.5) +
  annotate("text", x = x_pos, y = 53, label = "[4.6 yrs]", size = 3.5) +
  annotate("text", x = x_pos, y = 62, label = "[4.8 yrs]", size = 3.5) +
  annotate("text", x = x_pos, y = 72, label = "[4.9 yrs]", size = 3.5) +
  annotate("text", x = x_pos, y = 81, label = "[4.7 yrs]", size = 3.5) +
  annotate("text", x = x_pos, y = 91, label = "[5.7 yrs]", size = 3.5) +
  annotate("text", x = x_pos, y = 100, label = "[7.7 yrs]", size = 3.5) +
  labs(x = "Years/Sample") +  # use x-axis title
  theme_void() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title.x.top = element_text(size = 12, hjust = 0.5,  margin = margin(t = 2, b = -8)), # top label
    axis.text.x.top  = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.title.x     = element_blank()  # remove bottom label
  ) +
  scale_x_continuous(position = "top")  # force x-axis at top


p1 <- p1 & theme(
  axis.title.y = element_text(size = 14, face = "bold"),
  axis.text.y  = element_text(size = 12, face = "bold")
)

p6 <- p6  & theme(
  axis.title.y = element_text(size = 14, face = "bold"),
  axis.text.y  = element_text(size = 12, face = "bold")
)


# Combine with patchwork
combined_plot <- (p1 | p2 | p3 | p4 | p5 | p6 |  text_plot  ) + 
  plot_layout(widths = c(1.75, 0.75, 1, 1, 1, 1 ,0.5)) &   # Adjust relative widths as needed
  theme(legend.position = "bottom")



# Display the plot
combined_plot


