########## Plant SI biplot with "simmr" ##########
install.packages('simmr')
library("simmr")
library("Rmisc")
library("viridis")
source("plot.simmr_input.new.R")
source("plot.simmr_output.new.R")
source("compare_groups.new.R")


# data summary
all.data <- read.csv(file="si_data.csv")
plantdat <- read.csv(file="plant_data.csv")
summary_d13C <- summarySE(plantdat, measurevar="d13C", groupvars=c("species"))
summary_d15N <- summarySE(plantdat, measurevar="d15N", groupvars=c("species"))
summary_con_C <- summarySE(plantdat, measurevar="C_conc", groupvars=c("species"))
summary_con_N <- summarySE(plantdat, measurevar="N_conc", groupvars=c("species"))
  

# extract required data
s_names <- c("Bidens", "Oryza")
s_means <- cbind(summary_d13C$d13C, summary_d15N$d15N)
s_sds <- cbind(summary_d13C$sd, summary_d15N$sd)
concentration <- cbind(summary_con_C$C_conc/100, summary_con_N$N_conc/100)


# only herbivores are included
consumer_d13C <- c(-18.23, -22.72, -20.95, -21.39, -21.15, -24.98, -22.06, -22.34, -21.08, -24.09, -20.94, -22.04, -21.57, -21.46, -20.97, -21.20, -21.54)
consumer_d15N <- c(11.31, 8.01, 7.12, 7.03, 6.89, 7.55, 8.37, 7.26, 9.22, 9.33, 4.30, 4.27, 3.18, 2.94, 9.67, 10.48, 9.39)
mix <- cbind(consumer_d13C, consumer_d15N)
colnames(mix) = c('d13C','d15N')
consumer_group <- as.integer(c(1, 1, 2, 2, 2, 3, 4, 4, 5, 5, 5, 5, 5, 6, 6, 6, 6))
c_means = matrix(c(0.1, 0.1, 2.4, 2.4), ncol=2, nrow=2)
c_sds = matrix(c(0.01, 0.01, 0.24, 0.24), ncol=2, nrow=2)
# group number by site: ORM = 1, CVM = 2, ORG = 3, CVG = 4, ORO = 5, CVO = 6 #
  

# create simmr object
simmr_groups = simmr_load(mixtures = mix,
                          source_names = s_names,
                          source_means = s_means,
                          source_sds = s_sds,
                          correction_means = c_means,
                          correction_sds = c_sds,
                          concentration_means = concentration,
                          group = consumer_group) 

# bi-plot
plot.simmr_input.new(simmr_groups, group=1:6, xlab=expression(paste(delta^13, "C",sep="")), 
     ylab=expression(paste(delta^15, "N",sep="")),
     title='Isospace plot of Rice Farm', mix_name = "site")

# running mixing model
simmr_groups_out = simmr_mcmc(simmr_groups, mcmc.control=list(iter = 20000, burn = 2000, thin = 20, n.chain = 20))
summary(simmr_groups_out,type = c('quantiles','statistics'), group = 1:6)

# histogram of the proportion of source
titles <- c("ORM", "CVM", "ORG", "CVG", "ORO", "CVO")
for(i in 1:6){plot.simmr_output.new(simmr_groups_out, type = 'histogram', group = i, title = titles[i])}


# boxplot of the proportion of source
source("plot.simmr_output.new.R")
titles <- c("ORM", "CVM", "ORG", "CVG", "ORO", "CVO")
for(i in 1:6){plot.simmr_output.new(simmr_groups_out, type = 'boxplot', group = i, title = titles[i])}

# density plot
titles <- c("ORM", "CVM", "ORG", "CVG", "ORO", "CVO")
for(i in 1:6){plot.simmr_output.new(simmr_groups_out, type = 'density', group = i, title = titles[i])}

# matrix plot
titles <- c("ORM", "CVM", "ORG", "CVG", "ORO", "CVO")
for(i in 1:6){plot.simmr_output.new(simmr_groups_out, type = 'matrix', group = i, title = titles[i])}

# comparison of source proportion contribution 
compare_groups.new(simmr_groups_out, source = 'Bidens', groups = 1:2)
compare_groups.new(simmr_groups_out, source = 'Bidens', groups = 3:4)
compare_groups.new(simmr_groups_out, source = 'Bidens', groups = 5:6)
compare_groups.new(simmr_groups_out, source = 'Oryza', groups = 1:2)
compare_groups.new(simmr_groups_out, source = 'Oryza', groups = 3:4)
compare_groups.new(simmr_groups_out, source = 'Oryza', groups = 5:6)


