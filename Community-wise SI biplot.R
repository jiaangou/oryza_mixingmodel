########## Community-wise SI biplot for all farms of seedling stage
install.packages("SIBER")
install.packages("viridis")
library("SIBER")
library("viridis")
source("plotSiberObject.new.R")


### ORM
all.data <- read.csv("si_data.csv", header = T)
ORM_data_seedling <- subset(all.data, farm == "ORM")
ORM_data_seedling <- subset(ORM_data_seedling, stage == "seedling")
ORM_data_seedling <- subset(ORM_data_seedling, trophic_level == "producer")

ORM_data_seedling_siber <- cbind(ORM_data_seedling$d13C, ORM_data_seedling$d15N, ORM_data_seedling$trophic_level, ORM_data_seedling$species )
colnames(ORM_data_seedling_siber) <- c("iso1", "iso2", "group", "community")
ORM_data_seedling_siber <- as.data.frame(ORM_data_seedling_siber)

ORM_data_seedling_siber <- createSiberObject(ORM_data_seedling_siber)

palette(c("red", "blue"))
par(mar = c(5, 6, 4, 2), xpd = T)
plotSiberObject(ORM_data_seedling_siber, iso.order = c(1, 2), 
                ax.pad = 1, hulls = F, 
                ellipses = TRUE, 
                bty = "o", 
                xlab = list(expression({delta}^13*C), cex = 1.5),
                ylab = list(expression({delta}^15*N), cex = 1.5),
                las = 1,
                x.limits = c(-30, -17),
                y.limits = c(2, 15),
                points.order = c(17, 19))

ORM_consumer_seedling <- all.data[which(all.data$farm == "ORM"), ]
ORM_consumer_seedling <- ORM_consumer_seedling [which(ORM_consumer_seedling$stage == "seedling"), ]
ORM_consumer_seedling <- ORM_consumer_seedling [which(ORM_consumer_seedling$trophic_level == "consumer"), ]
ORM_consumer_seedling.final <- data.frame(ORM_consumer_seedling$d13C, 
                                    ORM_consumer_seedling$d15N, 
                                    ORM_consumer_seedling$species)
ORM_consumer_seedling.final$ORM_consumer_seedling.species <- substr(ORM_consumer_seedling.final$ORM_consumer_seedling.species, 1, 4)

points(ORM_consumer_seedling.final$ORM_consumer_seedling.d13C, 
       ORM_consumer_seedling.final$ORM_consumer_seedling.d15N, 
       pch = 1:length(unique(ORM_consumer_seedling.final$ORM_consumer_seedling.species)), 
       col = "blue", cex = 1.5)

text(-24, 16.5, "ORM Community SI Biplot - Seedling Stage", cex = 2.25)
legend(-30.5, 16, legend = c("BP", "OS"), pch = c(17, 19), 
       x.intersp = 0.4, y.intersp = 0.3, cex = 1, 
       title = "Source", title.adj = c(0.4,-2), 
       bty = "n")
text(-27.2, 13.9, "Trophic level")
text(-27.2, 13, "red: producer", col = "red")
text(-27.2, 12.1, "blue: consumer", col = "blue")



### CVM
all.data <- read.csv("si_data.csv", header = T)
CVM_data_seedling <- subset(all.data, farm == "CVM")
CVM_data_seedling <- subset(CVM_data_seedling, stage == "seedling")
CVM_data_seedling <- subset(CVM_data_seedling, trophic_level == "producer")

CVM_data_seedling_siber <- cbind(CVM_data_seedling$d13C, CVM_data_seedling$d15N, CVM_data_seedling$trophic_level, CVM_data_seedling$species )
colnames(CVM_data_seedling_siber) <- c("iso1", "iso2", "group", "community")
CVM_data_seedling_siber <- as.data.frame(CVM_data_seedling_siber)

CVM_data_seedling_siber <- createSiberObject(CVM_data_seedling_siber)

palette(c("red", "blue"))
par(mar = c(5, 6, 4, 2), xpd = T)
plotSiberObject(CVM_data_seedling_siber, iso.order = c(1, 2), 
                ax.pad = 1, hulls = F, 
                ellipses = TRUE, 
                bty = "o", 
                xlab = list(expression({delta}^13*C), cex = 1.5),
                ylab = list(expression({delta}^15*N), cex = 1.5),
                las = 1,
                x.limits = c(-32, -19),
                y.limits = c(-10, 13),
                points.order = c(17, 19))

CVM_consumer_seedling <- all.data[which(all.data$farm == "CVM"), ]
CVM_consumer_seedling <- CVM_consumer_seedling [which(CVM_consumer_seedling$stage == "seedling"), ]
CVM_consumer_seedling <- CVM_consumer_seedling [which(CVM_consumer_seedling$trophic_level == "consumer"), ]
CVM_consumer_seedling.final <- data.frame(CVM_consumer_seedling$d13C, 
                                          CVM_consumer_seedling$d15N, 
                                          CVM_consumer_seedling$species)
CVM_consumer_seedling.final$CVM_consumer_seedling.species <- substr(CVM_consumer_seedling.final$CVM_consumer_seedling.species, 1, 4)

points(CVM_consumer_seedling.final$CVM_consumer_seedling.d13C, 
       CVM_consumer_seedling.final$CVM_consumer_seedling.d15N, 
       pch = 1:length(unique(CVM_consumer_seedling.final$CVM_consumer_seedling.species)), 
       col = "blue", cex = 1.5)

text(-26, 15.5, "CVM Community SI Biplot - Seedling Stage", cex = 2.25)
legend(-32.5, 16, legend = c("BP", "OS"), pch = c(17, 19), 
       x.intersp = 0.4, y.intersp = 0.3, cex = 1, 
       title = "Source", title.adj = c(0.4,-2), 
       bty = "n")
text(-29.5, 12.2, "Trophic level")
text(-29.5, 10.5, "red: producer", col = "red")
text(-29.5, 9, "blue: consumer", col = "blue")



### ORG
all.data <- read.csv("si_data.csv", header = T)
ORG_data_seedling <- subset(all.data, farm == "ORG")
ORG_data_seedling <- subset(ORG_data_seedling, stage == "seedling")
ORG_data_seedling <- subset(ORG_data_seedling, trophic_level == "producer")

ORG_data_seedling_siber <- cbind(ORG_data_seedling$d13C, ORG_data_seedling$d15N, ORG_data_seedling$trophic_level, ORG_data_seedling$species )
colnames(ORG_data_seedling_siber) <- c("iso1", "iso2", "group", "community")
ORG_data_seedling_siber <- as.data.frame(ORG_data_seedling_siber)

ORG_data_seedling_siber <- createSiberObject(ORG_data_seedling_siber)

palette(c("red", "blue"))
par(mar = c(5, 6, 4, 2), xpd = T)
plotSiberObject(ORG_data_seedling_siber, iso.order = c(1, 2), 
                ax.pad = 1, hulls = F, 
                ellipses = TRUE, 
                bty = "o", 
                xlab = list(expression({delta}^13*C), cex = 1.5),
                ylab = list(expression({delta}^15*N), cex = 1.5),
                las = 1,
                x.limits = c(-34, -23),
                y.limits = c(0, 10),
                points.order = c(17, 19))

ORG_consumer_seedling <- all.data[which(all.data$farm == "ORG"), ]
ORG_consumer_seedling <- ORG_consumer_seedling [which(ORG_consumer_seedling$stage == "seedling"), ]
ORG_consumer_seedling <- ORG_consumer_seedling [which(ORG_consumer_seedling$trophic_level == "consumer"), ]
ORG_consumer_seedling.final <- data.frame(ORG_consumer_seedling$d13C, 
                                          ORG_consumer_seedling$d15N, 
                                          ORG_consumer_seedling$species)
ORG_consumer_seedling.final$ORG_consumer_seedling.species <- substr(ORG_consumer_seedling.final$ORG_consumer_seedling.species, 1, 4)

points(ORG_consumer_seedling.final$ORG_consumer_seedling.d13C, 
       ORG_consumer_seedling.final$ORG_consumer_seedling.d15N, 
       pch = 1:length(unique(ORG_consumer_seedling.final$ORG_consumer_seedling.species)), 
       col = "blue", cex = 1.5)

text(-29, 11, "ORG Community SI Biplot - Seedling Stage", cex = 2.25)
legend(-34.5, 11, legend = c("BP", "OS"), pch = c(17, 19), 
       x.intersp = 0.4, y.intersp = 0.3, cex = 1, 
       title = "Source", title.adj = c(0.4,-2), 
       bty = "n")
text(-31.8, 9.4, "Trophic level")
text(-31.8, 8.6, "red: producer", col = "red")
text(-31.8, 7.8, "blue: consumer", col = "blue")



### CVG
all.data <- read.csv("si_data.csv", header = T)
CVG_data_seedling <- subset(all.data, farm == "CVG")
CVG_data_seedling <- subset(CVG_data_seedling, stage == "seedling")
CVG_data_seedling <- subset(CVG_data_seedling, trophic_level == "producer")

CVG_data_seedling_siber <- cbind(CVG_data_seedling$d13C, CVG_data_seedling$d15N, CVG_data_seedling$trophic_level, CVG_data_seedling$species )
colnames(CVG_data_seedling_siber) <- c("iso1", "iso2", "group", "community")
CVG_data_seedling_siber <- as.data.frame(CVG_data_seedling_siber)

CVG_data_seedling_siber <- createSiberObject(CVG_data_seedling_siber)

palette(c("red", "blue"))
par(mar = c(5, 6, 4, 2), xpd = T)
plotSiberObject(CVG_data_seedling_siber, iso.order = c(1, 2), 
                ax.pad = 1, hulls = F, 
                ellipses = TRUE, 
                bty = "o", 
                xlab = list(expression({delta}^13*C), cex = 1.5),
                ylab = list(expression({delta}^15*N), cex = 1.5),
                las = 1,
                x.limits = c(-33, -20),
                y.limits = c(-3, 10),
                points.order = c(17, 19))

CVG_consumer_seedling <- all.data[which(all.data$farm == "CVG"), ]
CVG_consumer_seedling <- CVG_consumer_seedling [which(CVG_consumer_seedling$stage == "seedling"), ]
CVG_consumer_seedling <- CVG_consumer_seedling [which(CVG_consumer_seedling$trophic_level == "consumer"), ]
CVG_consumer_seedling.final <- data.frame(CVG_consumer_seedling$d13C, 
                                          CVG_consumer_seedling$d15N, 
                                          CVG_consumer_seedling$species)
CVG_consumer_seedling.final$CVG_consumer_seedling.species <- substr(CVG_consumer_seedling.final$CVG_consumer_seedling.species, 1, 4)

points(CVG_consumer_seedling.final$CVG_consumer_seedling.d13C, 
       CVG_consumer_seedling.final$CVG_consumer_seedling.d15N, 
       pch = 1:length(unique(CVG_consumer_seedling.final$CVG_consumer_seedling.species)), 
       col = "blue", cex = 1.5)

text(-27, 11.3, "CVG Community SI Biplot - Seedling Stage", cex = 2.25)
legend(-33.5, 11, legend = c("BP", "OS"), pch = c(17, 19), 
       x.intersp = 0.4, y.intersp = 0.3, cex = 1, 
       title = "Source", title.adj = c(0.4,-2), 
       bty = "n")
text(-30.5, 8.9, "Trophic level")
text(-30.5, 8, "red: producer", col = "red")
text(-30.5, 7, "blue: consumer", col = "blue")



### ORO
all.data <- read.csv("si_data.csv", header = T)
ORO_data_seedling <- subset(all.data, farm == "ORO")
ORO_data_seedling <- subset(ORO_data_seedling, stage == "seedling")
ORO_data_seedling <- subset(ORO_data_seedling, trophic_level == "producer")

ORO_data_seedling_siber <- cbind(ORO_data_seedling$d13C, ORO_data_seedling$d15N, ORO_data_seedling$trophic_level, ORO_data_seedling$species )
colnames(ORO_data_seedling_siber) <- c("iso1", "iso2", "group", "community")
ORO_data_seedling_siber <- as.data.frame(ORO_data_seedling_siber)

ORO_data_seedling_siber <- createSiberObject(ORO_data_seedling_siber)

palette(c("red", "blue"))
par(mar = c(5, 6, 4, 2), xpd = T)
plotSiberObject(ORO_data_seedling_siber, iso.order = c(1, 2), 
                ax.pad = 1, hulls = F, 
                ellipses = TRUE, 
                bty = "o", 
                xlab = list(expression({delta}^13*C), cex = 1.5),
                ylab = list(expression({delta}^15*N), cex = 1.5),
                las = 1,
                x.limits = c(-34, -18),
                y.limits = c(-1, 12),
                points.order = c(17, 19))

ORO_consumer_seedling <- all.data[which(all.data$farm == "ORO"), ]
ORO_consumer_seedling <- ORO_consumer_seedling [which(ORO_consumer_seedling$stage == "seedling"), ]
ORO_consumer_seedling <- ORO_consumer_seedling [which(ORO_consumer_seedling$trophic_level == "consumer"), ]
ORO_consumer_seedling.final <- data.frame(ORO_consumer_seedling$d13C, 
                                          ORO_consumer_seedling$d15N, 
                                          ORO_consumer_seedling$species)
ORO_consumer_seedling.final$ORO_consumer_seedling.species <- substr(ORO_consumer_seedling.final$ORO_consumer_seedling.species, 1, 4)

points(ORO_consumer_seedling.final$ORO_consumer_seedling.d13C, 
       ORO_consumer_seedling.final$ORO_consumer_seedling.d15N, 
       pch = 1:length(unique(ORO_consumer_seedling.final$ORO_consumer_seedling.species)), 
       col = "blue", cex = 1.5)

text(-26, 13.2, "ORO Community SI Biplot - Seedling Stage", cex = 2.25)
legend(-34.5, 13, legend = c("BP", "OS"), pch = c(17, 19), 
       x.intersp = 0.4, y.intersp = 0.3, cex = 1, 
       title = "Source", title.adj = c(0.4,-2), 
       bty = "n")
text(-30.5, 11, "Trophic level")
text(-30.5, 10, "red: producer", col = "red")
text(-30.5, 9, "blue: consumer", col = "blue")



### CVO
all.data <- read.csv("si_data.csv", header = T)
CVO_data_seedling <- subset(all.data, farm == "CVO")
CVO_data_seedling <- subset(CVO_data_seedling, stage == "seedling")
CVO_data_seedling <- subset(CVO_data_seedling, trophic_level == "producer")

CVO_data_seedling_siber <- cbind(CVO_data_seedling$d13C, CVO_data_seedling$d15N, CVO_data_seedling$trophic_level, CVO_data_seedling$species )
colnames(CVO_data_seedling_siber) <- c("iso1", "iso2", "group", "community")
CVO_data_seedling_siber <- as.data.frame(CVO_data_seedling_siber)

CVO_data_seedling_siber <- createSiberObject(CVO_data_seedling_siber)

palette(c("red", "blue"))
par(mar = c(5, 6, 4, 2), xpd = T)
plotSiberObject(CVO_data_seedling_siber, iso.order = c(1, 2), 
                ax.pad = 1, hulls = F, 
                ellipses = TRUE, 
                bty = "o", 
                xlab = list(expression({delta}^13*C), cex = 1.5),
                ylab = list(expression({delta}^15*N), cex = 1.5),
                las = 1,
                x.limits = c(-34, -18),
                y.limits = c(-8, 12),
                points.order = c(17, 19))

CVO_consumer_seedling <- all.data[which(all.data$farm == "CVO"), ]
CVO_consumer_seedling <- CVO_consumer_seedling [which(CVO_consumer_seedling$stage == "seedling"), ]
CVO_consumer_seedling <- CVO_consumer_seedling [which(CVO_consumer_seedling$trophic_level == "consumer"), ]
CVO_consumer_seedling.final <- data.frame(CVO_consumer_seedling$d13C, 
                                          CVO_consumer_seedling$d15N, 
                                          CVO_consumer_seedling$species)
CVO_consumer_seedling.final$CVO_consumer_seedling.species <- substr(CVO_consumer_seedling.final$CVO_consumer_seedling.species, 1, 4)

points(CVO_consumer_seedling.final$CVO_consumer_seedling.d13C, 
       CVO_consumer_seedling.final$CVO_consumer_seedling.d15N, 
       pch = 1:length(unique(CVO_consumer_seedling.final$CVO_consumer_seedling.species)), 
       col = "blue", cex = 1.5)

text(-26, 13.8, "CVO Community SI Biplot - Seedling Stage", cex = 2.25)
legend(-34.5, 14.7, legend = c("BP", "OS"), pch = c(17, 19), 
       x.intersp = 0.4, y.intersp = 0.3, cex = 1, 
       title = "Source", title.adj = c(0.4,-2), 
       bty = "n")
text(-30.5, 11.3, "Trophic level")
text(-30.5, 10, "red: producer", col = "red")
text(-30.5, 8.7, "blue: consumer", col = "blue")









