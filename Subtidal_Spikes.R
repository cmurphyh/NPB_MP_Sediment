#Code to compile Database

###Clear the R environment
rm(list=ls());graphics.off();cat("\f");

#Set working directory
setwd("~/Desktop/Papers/Sediment Paper/Code/Subtidal Only/")


#Load subtidal  Data with data restricted to 250um to 5 mm (feret diameters)
SUB_All <-read.csv("subtidal_paired_merged_data.csv")  #read in database as csv
head(SUB_All)
class(SUB_All)

data <- SUB_All %>% 
  filter(!is.na(X1st.Order.Morph)) # removing rows where microscopy deemed not plastic

##################################################################################
#Spike Recovery
Spike_data <- data %>% 
  filter(X1st.Order.Morph=="Spike") # Filter to only spikes

Spike_Table <-table(Spike_data$Color.Group, Spike_data$Sample)
Spike_Table
Spike_Total <- colSums(Spike_Table)
Spike_Table <- rbind(Spike_Table, Spike_Total)
Spike_Table

write.csv(Spike_Table,"SampleSpikeCounts.csv")

##################################################################################

