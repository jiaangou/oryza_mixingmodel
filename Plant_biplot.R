# Lilliefors test for overall normality
install.packages("nortest")
library("nortest")

plant_data <- read.csv("plant_data.csv", header = T)
L.test.d13C <- lillie.test(plant_data$d13C)  # p<0.01, non-normality
L.test.d15N <- lillie.test(plant_data$d15N)  # p<0.01, non-normality
plantdat$stage <- factor(plantdat$stage, levels = c("seedling", "tillering", "heading", "ripening"))

# ANOVA for stage effect of each farm 
split.data <- split(plant_data, plant_data$farm)
ANOVA.d13C <- lapply(lapply(split.data, function(x){aov(x$d13C ~ x$stage)}), summary)
ANOVA.d15N <- lapply(lapply(split.data, function(x){aov(x$d15N ~ x$stage)}), summary)
### significant stage effect on d15N values of "CVM", "CVG" and "CVO" ###


########## d13C and d15N biplots for each farm (group by species and community by stage) ##########
ORM_data <- split.data$ORM
CVM_data <- split.data$CVM
ORG_data <- split.data$ORG
CVG_data <- split.data$CVG
ORO_data <- split.data$ORO
CVO_data <- split.data$CVO

install.packages("SIBER")
install.packages("viridis")
library("SIBER")
library("viridis")
source("plotSiberObject.new.R")

### ORM
ORM_data_siber <- cbind(ORM_data$d13C, ORM_data$d15N, ORM_data$stage, ORM_data$species )
colnames(ORM_data_siber) <- c("iso1", "iso2", "group", "community")
ORM_data_siber <- as.data.frame(ORM_data_siber)

ORM_data_siber[, 3] <- as.factor(ORM_data_siber[, 3])
ORM_data_siber[, 4] <- as.factor(ORM_data_siber[, 4])
ORM_data_siber <- createSiberObject(ORM_data_siber)

palette("default")
par(mar = c(5, 6, 4, 2), xpd = T)
plotSiberObject(ORM_data_siber, iso.order = c(1, 2), 
                ax.pad = 1.25, hulls = F, 
                ellipses = TRUE, 
                bty = "o", 
                xlab = list(expression({delta}^13*C), cex = 1.5),
                ylab = list(expression({delta}^15*N), cex = 1.5),
                las = 1, 
                points.order = c(17, 19))
text(mean(ORM_data$d13C), max(ORM_data$d15N)+2.5, "ORM SI Biplot", cex = 2.25)
legend(min(ORM_data$d13C)-1.5, max(ORM_data$d15N)+1.8, legend = c("OS", "BP"), pch = c(17, 19), 
       x.intersp = 0.4, y.intersp = 0.3, cex = 1, 
       title = "Source", title.adj = c(0.4,-2), 
       bty = "n")
legend(min(ORM_data$d13C)-0.75, max(ORM_data$d15N)+1.8, legend = c("Seedling", "Tillering", "Heading", "Ripening"), lty = 1, 
       x.intersp = 0.4, y.intersp = 0.25, cex = 1, 
       title = "Stage",  title.adj = c(0.4, -2), 
       col = palette("default"), seg.len = 0.5, 
       text.col = palette("default"), bty = "n") 
      

### CVM
CVM_data_siber <- cbind(CVM_data$d13C, CVM_data$d15N, CVM_data$stage, CVM_data$species )
colnames(CVM_data_siber) <- c("iso1", "iso2", "group", "community")
CVM_data_siber <- as.data.frame(CVM_data_siber)

CVM_data_siber[, 3] <- as.factor(CVM_data_siber[, 3])
CVM_data_siber[, 4] <- as.factor(CVM_data_siber[, 4])
CVM_data_siber <- createSiberObject(CVM_data_siber)

palette("default")
par(mar = c(5, 5, 4, 2), xpd = T)
plotSiberObject(CVM_data_siber, iso.order = c(1, 2), 
                ax.pad = 1.25, hulls = F, 
                ellipses = TRUE, 
                bty = "o", 
                xlab = list(expression({delta}^13*C), cex = 1.5),
                ylab = list(expression({delta}^15*N), cex = 1.5),
                las = 1, 
                points.order = c(17, 19))
text(mean(CVM_data$d13C), max(CVM_data$d15N)+3.5, "CVM SI Biplot", cex = 2.25)
legend(min(CVM_data$d13C)-1.5, max(CVM_data$d15N)+3, legend = c("OS", "BP"), pch = c(17, 19), 
       x.intersp = 0.4, y.intersp = 0.3, cex = 1, 
       title = "Source", title.adj = c(0.4,-2), 
       bty = "n")
legend(min(CVM_data$d13C)-1, max(CVM_data$d15N)+3, legend = c("Seedling", "Tillering", "Heading", "Ripening"), lty = 1, 
       x.intersp = 0.4, y.intersp = 0.25, cex = 1, 
       title = "Stage",  title.adj = c(0.4, -2), 
       col = palette("default"), seg.len = 0.5, 
       text.col = palette("default"), bty = "n")


### ORG
ORG_data_siber <- cbind(ORG_data$d13C, ORG_data$d15N, ORG_data$stage, ORG_data$species )
colnames(ORG_data_siber) <- c("iso1", "iso2", "group", "community")
ORG_data_siber <- as.data.frame(ORG_data_siber)

ORG_data_siber[, 3] <- as.factor(ORG_data_siber[, 3])
ORG_data_siber[, 4] <- as.factor(ORG_data_siber[, 4])
ORG_data_siber <- createSiberObject(ORG_data_siber)

palette("default")
par(mar = c(5, 5, 4, 2), xpd = T)
plotSiberObject(ORG_data_siber, iso.order = c(1, 2), 
                ax.pad = 1.25, hulls = F, 
                ellipses = TRUE, 
                bty = "o", 
                xlab = list(expression({delta}^13*C), cex = 1.5),
                ylab = list(expression({delta}^15*N), cex = 1.5),
                las = 1, 
                points.order = c(17, 19))
text(mean(ORG_data$d13C), max(ORG_data$d15N)+2.5, "ORG SI Biplot", cex = 2.25)
legend(min(ORG_data$d13C)-1.4, max(ORG_data$d15N)+2, legend = c("OS", "BP"), pch = c(17, 19), 
       x.intersp = 0.4, y.intersp = 0.3, cex = 1, 
       title = "Source", title.adj = c(0.4,-2), 
       bty = "n")
legend(min(ORG_data$d13C)-0.8, max(ORG_data$d15N)+2, legend = c("Seedling", "Tillering", "Heading", "Ripening"), lty = 1, 
       x.intersp = 0.4, y.intersp = 0.25, cex = 1, 
       title = "Stage",  title.adj = c(0.4, -2), 
       col = palette("default"), seg.len = 0.5, 
       text.col = palette("default"), bty = "n")



### CVG
CVG_data_siber <- cbind(CVG_data$d13C, CVG_data$d15N, CVG_data$stage, CVG_data$species )
colnames(CVG_data_siber) <- c("iso1", "iso2", "group", "community")
CVG_data_siber <- as.data.frame(CVG_data_siber)

CVG_data_siber[, 3] <- as.factor(CVG_data_siber[, 3])
CVG_data_siber[, 4] <- as.factor(CVG_data_siber[, 4])
CVG_data_siber <- createSiberObject(CVG_data_siber)

palette("default")
par(mar = c(5, 5, 4, 2), xpd = T)
plotSiberObject(CVG_data_siber, iso.order = c(1, 2), 
                ax.pad = 1.25, hulls = F, 
                ellipses = TRUE, 
                bty = "o", 
                xlab = list(expression({delta}^13*C), cex = 1.5),
                ylab = list(expression({delta}^15*N), cex = 1.5),
                las = 1, 
                points.order = c(17, 19))
text(mean(CVG_data$d13C), max(CVG_data$d15N)+2.6, "CVG SI Biplot", cex = 2.25)
legend(min(CVG_data$d13C)-1.5, max(CVG_data$d15N)+2.25, legend = c("OS", "BP"), pch = c(17, 19), 
       x.intersp = 0.4, y.intersp = 0.3, cex = 1, 
       title = "Source", title.adj = c(0.4,-2), 
       bty = "n")
legend(min(CVG_data$d13C)-0.9, max(CVG_data$d15N)+2.25, legend = c("Seedling", "Tillering", "Heading", "Ripening"), lty = 1, 
       x.intersp = 0.4, y.intersp = 0.25, cex = 1, 
       title = "Stage",  title.adj = c(0.4, -2), 
       col = palette("default"), seg.len = 0.5, 
       text.col = palette("default"), bty = "n")



### ORO
ORO_data_siber <- cbind(ORO_data$d13C, ORO_data$d15N, ORO_data$stage, ORO_data$species )
colnames(ORO_data_siber) <- c("iso1", "iso2", "group", "community")
ORO_data_siber <- as.data.frame(ORO_data_siber)

ORO_data_siber[, 3] <- as.factor(ORO_data_siber[, 3])
ORO_data_siber[, 4] <- as.factor(ORO_data_siber[, 4])
ORO_data_siber <- createSiberObject(ORO_data_siber)

palette("default")
par(mar = c(5, 5, 4, 2), xpd = T)
plotSiberObject(ORO_data_siber, iso.order = c(1, 2), 
                ax.pad = 1, hulls = F, 
                ellipses = TRUE, 
                bty = "o", 
                xlab = list(expression({delta}^13*C), cex = 1.5),
                ylab = list(expression({delta}^15*N), cex = 1.5),
                las = 1, 
                points.order = c(17, 19))
text(mean(ORO_data$d13C), max(ORO_data$d15N)+2.6, "ORO SI Biplot", cex = 2.25)
legend(min(ORO_data$d13C)-1.1, max(ORO_data$d15N)+2, legend = c("OS", "BP"), pch = c(17, 19), 
       x.intersp = 0.4, y.intersp = 0.3, cex = 1, 
       title = "Source", title.adj = c(0.4,-2), 
       bty = "n")
legend(min(ORO_data$d13C)-0.5, max(ORO_data$d15N)+2, legend = c("Seedling", "Tillering", "Heading", "Ripening"), lty = 1, 
       x.intersp = 0.4, y.intersp = 0.25, cex = 1, 
       title = "Stage",  title.adj = c(0.4, -2), 
       col = palette("default"), seg.len = 0.5, 
       text.col = palette("default"), bty = "n")


### CVO
CVO_data_siber <- cbind(CVO_data$d13C, CVO_data$d15N, CVO_data$stage, CVO_data$species )
colnames(CVO_data_siber) <- c("iso1", "iso2", "group", "community")
CVO_data_siber <- as.data.frame(CVO_data_siber)

CVO_data_siber[, 3] <- as.factor(CVO_data_siber[, 3])
CVO_data_siber[, 4] <- as.factor(CVO_data_siber[, 4])
CVO_data_siber <- createSiberObject(CVO_data_siber)

palette("default")
par(mar = c(5, 5, 4, 2), xpd = T)
plotSiberObject(CVO_data_siber, iso.order = c(1, 2), 
                ax.pad = 2, hulls = F, 
                ellipses = TRUE, 
                bty = "o", 
                xlab = list(expression({delta}^13*C), cex = 1.5),
                ylab = list(expression({delta}^15*N), cex = 1.5),
                las = 1,
                points.order = c(17, 19))
text(mean(CVO_data$d13C)-0.7, max(CVO_data$d15N)+4.25, "CVO SI Biplot", cex = 2.25)
legend(min(CVO_data$d13C)-2.5, max(CVO_data$d15N)+3, legend = c("OS", "BP"), pch = c(17, 19), 
       x.intersp = 0.4, y.intersp = 0.3, cex = 1, 
       title = "Source", title.adj = c(0.4,-2), 
       bty = "n")
legend(min(CVO_data$d13C)-1.5, max(CVO_data$d15N)+3, legend = c("Seedling", "Tillering", "Heading", "Ripening"), lty = 1, 
       x.intersp = 0.4, y.intersp = 0.25, cex = 1, 
       title = "Stage",  title.adj = c(0.4, -2), 
       col = palette("default"), seg.len = 0.5, 
       text.col = palette("default"), bty = "n")


