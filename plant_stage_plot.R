########## time-series plots ##########
install.packages(c("Rmisc", "ggplot2"))
library("Rmisc")
library("ggplot2")

plantdat <- read.csv(file="plant_data.csv")
plantdat$stage <- factor(plantdat$stage, levels = c("seedling", "tillering", "heading", "ripening"))

### summary statistics - farm * stage
summary_d13C <- summarySE(plantdat, measurevar="d13C", groupvars=c("farm","stage", "species"))
summary_d15N <- summarySE(plantdat, measurevar="d15N", groupvars=c("farm","stage", "species"))
summary_d13C$stage <- factor(summary_d13C$stage, levels = c("seedling", "tillering", "heading", "ripening"))
summary_d15N$stage <- factor(summary_d15N$stage, levels = c("seedling", "tillering", "heading", "ripening"))

### ggplot of d13C for both species
summary_d13C_BP <- subset(summary_d13C, species == "BP")
p_BP_d13C <- ggplot(summary_d13C_BP, aes(x=stage, y=d13C, group = farm, color = farm)) + 
  ylim(-35, -28) +
  geom_line(size = 1) +
  geom_point(size = 2)+
  ggtitle(expression(BP~delta^13~C))+
  xlab("Stage") +
  ylab(expression(delta^13~C)) +
  theme(plot.margin = unit(c(1,0.1,1,1), "cm"))+
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 16, vjust = -5), axis.title.y = element_text(size = 16))+
  geom_errorbar(size = 1, aes(ymin=d13C-sd, ymax=d13C+sd), width=0.4,
                position=position_dodge(0.08))
print(p_BP_d13C)

summary_d13C_OS <- subset(summary_d13C, species == "OS")
p_OS_d13C <- ggplot(summary_d13C_OS, aes(x=stage, y=d13C, group = farm, color = farm)) + 
  ylim(-35, -28) +
  geom_line(size = 1) +
  geom_point(size = 2)+
  ggtitle(expression(OS~delta^13~C))+
  xlab("Stage") +
  ylab(expression(delta^13~C)) +
  theme(plot.margin = unit(c(1,0.1,1,1), "cm"))+
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 16, vjust = -5), axis.title.y = element_text(size = 16))+
  geom_errorbar(size = 1, aes(ymin=d13C-sd, ymax=d13C+sd), width=0.4,
                position=position_dodge(0.08))
print(p_OS_d13C)


### ggplot of d15N for both species
summary_d15N_BP <- subset(summary_d15N, species == "BP")
p_BP_d15N <- ggplot(summary_d15N_BP, aes(x=stage, y=d15N, group = farm, color = farm)) + 
  ylim(-2, 10)+
  geom_line(size = 1) +
  geom_point(size = 2)+
  ggtitle(expression(BP~delta^15~N))+
  xlab("Stage") +
  ylab(expression(delta^15~N)) +
  theme(plot.margin = unit(c(1,0.1,1,1), "cm"))+
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 16, vjust = -5), axis.title.y = element_text(size = 16))+
  geom_errorbar(size = 1, aes(ymin=d15N-sd, ymax=d15N+sd), width=0.4,
                position=position_dodge(0.08))
print(p_BP_d15N)

summary_d15N_OS <- subset(summary_d15N, species == "OS")
p_OS_d15N <- ggplot(summary_d15N_OS, aes(x=stage, y=d15N, group = farm, color = farm)) + 
  ylim(-10, 15) + 
  geom_line(size = 1) +
  geom_point(size = 2)+
  ggtitle(expression(OS~delta^15~N))+
  xlab("Stage") +
  ylab(expression(delta^15~N)) +
  theme(plot.margin = unit(c(1,0.1,1,1), "cm"))+
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 16, vjust = -5), axis.title.y = element_text(size = 16))+
  geom_errorbar(size = 1, aes(ymin=d15N-sd, ymax=d15N+sd), width=0.4,
                position=position_dodge(0.08))
print(p_OS_d15N)

# combine all four plots 
multiplot(p_BP_d13C, p_OS_d13C, p_BP_d15N, p_OS_d15N, cols=2)

