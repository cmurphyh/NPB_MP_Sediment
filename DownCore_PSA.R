# Down Core Sediment Textures

setwd("~/Desktop/Papers/Sediment Paper/Code/")
########################################

UNB1_data <- read.csv("UNB1_v2.csv") #Version with the bad 49 and 50 sample replaced with RD's. (replicates still included)
UNB1_data <- data.frame(UNB1_data)
depth <- UNB1_data$plot
D10_um <- UNB1_data$D10..mm..
D50_um <- UNB1_data$D50..mm..
D90_um <- UNB1_data$D90..mm..
D10_phi <- UNB1_data$D10..f..
D50_phi <- UNB1_data$D50..f..
D90_phi <- UNB1_data$D90..f..

par(mfrow=c(1,2))    # set the plotting area into a 1*2 array
par(cex=0.8, mai=c(0.5,0.5,0.3,0.2))
plot(D10_um,depth, xlab="", ylab="",xlim = c(0,250), ylim = c(150,0), yaxp=c(0,150,15), col="maroon", pch=19,cex=1.5)
points(D50_um,depth, ylim = c(165,0), col="cadetblue", pch=19, cex=1.5)
points(D90_um,depth, ylim = c(165,0), col="springgreen3", pch=19, cex=1.5)
title(main="UNB-1: Particle Size (um)",line=1, cex.lab=0.9)
title(xlab="particle diameter (um)",line=2, cex.lab=1)
title(ylab="depth (cm bgs)",line=2, cex.lab=1)
legend("bottomright", legend = c("D10 (um)", "D50 (um)", "D90 (um)"),
      pch = c(19, 19, 19), col = c("maroon", "cadetblue", "springgreen3"))
par(cex=0.8, mai=c(0.5,0.3,0.3,0.3))
plot(D10_phi,depth, xlab="", ylab="",xlim = c(0,12), ylim = c(150,0), yaxp=c(0,150,15), col="maroon", pch=19,cex=1.5)
points(D50_phi,depth, ylim = c(165,0), col="cadetblue", pch=19, cex=1.5)
points(D90_phi,depth, ylim = c(165,0), col="springgreen3", pch=19, cex=1.5)
title(main="UNB-1: Particle Size (phi)",line=1, cex.lab=0.9)
title(xlab="particle diameter (phi)",line=2, cex.lab=1)
legend("bottomright", legend = c("D10 (phi)", "D50 (phi)", "D90 (phi)"),
       pch = c(19, 19, 19), col = c("maroon", "cadetblue", "springgreen3"))
#type="b", lwd=3, pch=19, for a line between dots

########################################
UNB2_data <- read.csv("UNB2.csv") # (replicates still included)
UNB2_data <- data.frame(UNB2_data)
depth <- UNB2_data$plot
D10_um <- UNB2_data$D10..mm..
D50_um <- UNB2_data$D50..mm..
D90_um <- UNB2_data$D90..mm..
D10_phi <- UNB2_data$D10..f..
D50_phi <- UNB2_data$D50..f..
D90_phi <- UNB2_data$D90..f..

par(mfrow=c(1,2))    # set the plotting area into a 1*2 array
par(cex=0.8, mai=c(0.5,0.5,0.3,0.2))
plot(D10_um,depth, xlab="", ylab="",xlim = c(0,250), ylim = c(150,0), yaxp=c(0,150,15), col="maroon", pch=19,cex=1.5)
points(D50_um,depth, ylim = c(165,0), col="cadetblue", pch=19, cex=1.5)
points(D90_um,depth, ylim = c(165,0), col="springgreen3", pch=19, cex=1.5)
title(main="UNB-2: Particle Size (um)",line=1, cex.lab=0.9)
title(xlab="particle diameter (um)",line=2, cex.lab=1)
title(ylab="depth (cm bgs)",line=2, cex.lab=1)
legend("bottomright", legend = c("D10 (um)", "D50 (um)", "D90 (um)"),
       pch = c(19, 19, 19), col = c("maroon", "cadetblue", "springgreen3"))
par(cex=0.8, mai=c(0.5,0.3,0.3,0.3))
plot(D10_phi,depth, xlab="", ylab="",xlim = c(0,12), ylim = c(150,0), yaxp=c(0,150,15), col="maroon", pch=19,cex=1.5)
points(D50_phi,depth, ylim = c(165,0), col="cadetblue", pch=19, cex=1.5)
points(D90_phi,depth, ylim = c(165,0), col="springgreen3", pch=19, cex=1.5)
title(main="UNB-2: Particle Size (phi)",line=1, cex.lab=0.9)
title(xlab="particle diameter (phi)",line=2, cex.lab=1)
legend("bottomright", legend = c("D10 (phi)", "D50 (phi)", "D90 (phi)"),
       pch = c(19, 19, 19), col = c("maroon", "cadetblue", "springgreen3"))
#type="b", lwd=3, pch=19, for a line between dots


########################################
UNB3_data <- read.csv("UNB3.csv") # (replicates still included)
UNB3_data <- data.frame(UNB3_data)
depth <- UNB3_data$plot
D10_um <- UNB3_data$D10..mm..
D50_um <- UNB3_data$D50..mm..
D90_um <- UNB3_data$D90..mm..
D10_phi <- UNB3_data$D10..f..
D50_phi <- UNB3_data$D50..f..
D90_phi <- UNB3_data$D90..f..

par(mfrow=c(1,2))    # set the plotting area into a 1*2 array
par(cex=0.8, mai=c(0.5,0.5,0.3,0.2))
plot(D10_um,depth, xlab="", ylab="",xlim = c(0,250), ylim = c(150,0), yaxp=c(0,150,15), col="maroon", pch=19,cex=1.5)
points(D50_um,depth, ylim = c(165,0), col="cadetblue", pch=19, cex=1.5)
points(D90_um,depth, ylim = c(165,0), col="springgreen3", pch=19, cex=1.5)
title(main="UNB-3: Particle Size (um)",line=1, cex.lab=0.9)
title(xlab="particle diameter (um)",line=2, cex.lab=1)
title(ylab="depth (cm bgs)",line=2, cex.lab=1)
legend("bottomright", legend = c("D10 (um)", "D50 (um)", "D90 (um)"),
       pch = c(19, 19, 19), col = c("maroon", "cadetblue", "springgreen3"))
par(cex=0.8, mai=c(0.5,0.3,0.3,0.3))
plot(D10_phi,depth, xlab="", ylab="",xlim = c(0,12), ylim = c(150,0), yaxp=c(0,150,15), col="maroon", pch=19,cex=1.5)
points(D50_phi,depth, ylim = c(165,0), col="cadetblue", pch=19, cex=1.5)
points(D90_phi,depth, ylim = c(165,0), col="springgreen3", pch=19, cex=1.5)
title(main="UNB-3: Particle Size (phi)",line=1, cex.lab=0.9)
title(xlab="particle diameter (phi)",line=2, cex.lab=1)
legend("bottomright", legend = c("D10 (phi)", "D50 (phi)", "D90 (phi)"),
       pch = c(19, 19, 19), col = c("maroon", "cadetblue", "springgreen3"))
#type="b", lwd=3, pch=19, for a line between dots
