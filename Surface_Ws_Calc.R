#Script to Calculate Rising and Settling Velocities of Particles
#Cmurp025@ucr.edu
#version: 7/31/2025
#Updates from previous: Edits to run Subtidal and Intertidal MP Ws

#####Set up##############################################################

###Clear the R environment
rm(list=ls());graphics.off();cat("\f");

#load packages
library(dplyr)
library(tibble)

###############################################################################
#Notes: 
#(1) Each row of data is a separate particle with at least one dimension measured,
#polymer identified, and morphology listed. 
#(2) The data file was run through the MaTCH tool (Hapich et al., 2024) to add
#particle densities and any missing dimensions (projected width and height).
#(3) The tool can also harmonize morphology classifications (if needed)
#(4) Equations for Fiber and Non-Fiber Settling/Rising Velocities are from 
#Waldschlager et al., 2019 (equations 10-13).
#(6a) constants: 9.81 m/s2 (gravity), 1.05 m2/s (Kinematic viscosity of water at 18 degree C), and 1024 kg/m3 (salt water density)
#(6b) constants: 9.81 m/s2 (gravity), 1.19 m2/s (Kinematic viscosity), and 1026 kg/m3 (salt water density) (for saltwater water with 15 C )
#(6c) constants: 9.81 m/s2 (gravity), 1.16 m2/s (Kinematic viscosity), and 1026 kg/m3 (salt water density) (for saltwater with 16 C )
#(6d) constants: 9.81 m/s2 (gravity), 1.23 m2/s (Kinematic viscosity), and 1026 kg/m3 (salt water density) (for saltwater with 13-14 C )
###############################################################################
#####Start here with the MaTCHED dataset#######################################

#Set working directory
setwd("~/Desktop/Papers/Sediment Paper/Code")

#Load data into R
data <- read.csv("Surface_MaTCH_v3.csv") #CHANGE TO YOUR INPUT FILE

####data clean up #############################################################
#Make sure dimensional data is in a numeric format

#Length
data$Length..microns. <- as.numeric(data$Length..microns.)

#Width
#Here we measured the particle width and therefore we use our 
#measured value over the projected values from MaTCH
data$Width..microns. <- as.numeric(data$Width..microns.)

#Height
#Assuming value is from MaTCH: assign which projected Height to use
#options from outputs in our data (min=10%, max=90%, mean)
data$Height..microns. <- as.numeric(data$Height..microns.) #mean

#check formats are numeric
class(data$Length..microns.)
class (data$Width..microns.)
class (data$Height..microns.)

#######################################################################

#convert dimensions [um] to [m] and assign as a, b and c axis
data$a <-data$Length..microns./1E6
data$b <-data$Width..microns./1E6
data$c <-data$Height..microns./1E6

###functions needed to add variables to data table for calculations

# Function for calculating the Corey Shape Factor (CSF) [unitless]
assign_CSF <- function(a, b, c) {
  c / sqrt(a*b)
}

#Add CSF results to data table
data$CSF <- mapply(assign_CSF, data$a, data$b, data$c)

#Function for calculating the Equivalent Diameter (Deqi) [m]
#(Note that Non-Fibers and Fibers are treated differently; fibers Deqi = c) 
#Equations 7 & 8 of Waldschlager et al., 2019
assign_Deqi <- function(X1st.Order.Morph, a, b, c) {
  if (X1st.Order.Morph== "Non-Fiber") {
    return((a*b*c)^(1/3))
  } else if (X1st.Order.Morph == "Fiber") {
    return(c)
  } else {
    return(NA)
  }
}
#Add Deqi result to data table
data$Deqi<- mapply(assign_Deqi, data$X1st.Order.Morph, data$a, data$b, data$c)


## Load Constants for calculations
g = 9.81 # m/s2 gravity
v = 1.23E-6 #m2/s kinematic viscosity
data$water.density = 1026 #kg/m3

#Selection and Conversion of Particle Density

##Convert particle density from [mg/um3] to [kg/m3] by mutiplying by 1e+12

#ACTIVATE WHICH DENSITY TO USE from MaTCH (mean, 5th, 10th, 90th, 95th)

# Mean of distribution
data$particle.density <- data$Density..mg.microns3.*1000000000000 # Mean

# 5th percentile
#data$particle.density <- data$X5..Density..mg.microns3.*1000000000000

# 95th percentile
#data$particle.density <- data$X95..Density..mg.microns3.*1000000000000

#Calculate relative density (delta) and add to data table
#Note that abs excludes direction of movement; Will add back in at the end
data$delta = abs(data$water.density - data$particle.density)/data$water.density 

##Calculating Dimensionless diameter (D*) [Unitless] and add to data table 
#[Note: Equation (2) in Waldschlager et al 2019 is missing the square of viscosity.]
data$D_dimless <- data$Deqi*(((data$delta*g)/(v^2))^(1/3)) 


#Filtering data to remove any particles not meeting analysis criteria
#dataset specific
data <- data %>% 
  filter(!is.na(Length..microns.)) %>% # removing rows without dimension data
  filter(!is.na(Density..mg.microns3.)) #removing rows without denisty data

################################################################################
################################################################################

#Function to assign Powers Roundness (P) Values 
#Can skip if P values for each particle are already included in the dataset

#We used Median values from Wald et al. 2020 SI dataset of 100 particles
assign_Powers <- function(Morphology..Raw.Data.) {
  if (Morphology..Raw.Data.== "fragment") {
    return(4.6)
  } else if (Morphology..Raw.Data.== "foam") {
    return(5.0)
  } else if (Morphology..Raw.Data.== "pellet") {
    return(5.7)
  }  else if (Morphology..Raw.Data.== "film") {
    return(4.35)
  }    else if (Morphology..Raw.Data.== "sphere") {
    return(6.0)
  }    else if (Morphology..Raw.Data.== "TRWP") {
    return(4.6)
  }  else {
    return(NA)
  }
}

#Add P value results to data table
data$P<- mapply(assign_Powers, data$Morphology)

#####
#If skip above and want to universally assign P as a value
######
#data$P<-4.6
#data$Morphology<-"fragment"
#####Functions to calculate Re, Cd, and W ######################################

# Function to calculate Re (depends on W)
calculate_Re <- function(W, Deqi, v) {
  return(W * Deqi / v)
}

# Function to calculate Cd (depends on Re)
#Using equations 10-13 of Waldschlager et al 2019
calculate_Cd <- function(Re, CSF, X1st.Order.Morph, particle.density, P, water.density) {
  if (X1st.Order.Morph == "Non-Fiber" & particle.density > water.density) {
    return(3 / (CSF * (Re^(1/3))))
  } else if (X1st.Order.Morph == "Non-Fiber" & particle.density < water.density) {
    return(((20 / Re) + (10 / sqrt(Re)) + sqrt(1.195 - CSF)) * ((6 / P)^(1 - CSF)))
  } else if (X1st.Order.Morph == "Fiber" & particle.density > water.density) {
    return((4.7 / sqrt(Re)) +  sqrt(CSF))
  } else if (X1st.Order.Morph == "Fiber" & particle.density < water.density) {
    return((10 / sqrt(Re)) +  sqrt(CSF))
  } else {
    return(NA)
  }
}

# Function to calculate W (depends on Cd)
calculate_W <- function(Cd, Deqi, delta, g) {
  return(sqrt((4/3) * (Deqi / Cd) * delta * g))
}  

### Iterative approach to calculating Settling velocity#########################

#Set up Iteration for a dataset
data <- data.frame(data) # data frame to calculate for
n <- nrow(data) # number of rows
max_iterations <- 100
data$W <- 0.002 #add initial guess for W to data frame to start calculation


### Iterative calculation for Re, Cd, and W ###################################
#(1) Starting with provided W alculates Re, Cd and W for each row of the dataset
#(2) Then starts over again with the new W
#(3) Iterates the calculation X # of times before stopping
# For our data it only takes ~23 to converge with test data)

for (iter in 1:max_iterations) {
  for (i in 1:n) {
    data$Re[i] <- calculate_Re(data$W[i], data$Deqi[i], v)
    data$Cd[i] <- calculate_Cd(data$Re[i], data$CSF[i], data$X1st.Order.Morph[i], data$particle.density[i], data$P[i], data$water.density[i])
    data$W[i] <- calculate_W(data$Cd[i], data$Deqi[i], data$delta[i], g)
  }
}

#print the final iteration results
#print(data)

#Adjust the final setting velocities for direction based on relative density
data$W_adj <- ifelse(data$particle.density < data$water.density, -1 * data$W, data$W) 

################################################################################
#Option to save before beginning Rouse calculations

#Write results to file
write.csv(data, "Surface_MaTCH_v3_Ws_d.csv") #NAME FILE YOUR FILE

################################################################################
