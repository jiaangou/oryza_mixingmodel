library(gridExtra)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotrix)

#####SUMMARIES
sum_plantdat <- plantdat %>%
  group_by(Farm_type,species,landscape,farm,date)%>%
  summarise_each(funs(mean,std.error),d13C,d15N)
warnings()

#overall plant means across all farms
plant_means <- plantdat %>%
  group_by(species)%>%
  summarise_each(funs(mean,std.error),d13C,d15N)

#plant means for all farms
farm_means <-plantdat %>%
  group_by(farm, species)%>%
  summarise_each(funs(mean,std.error),d13C,d15N)

#plant isotope means by farm type
farmtype_means <- plantdat%>%
  group_by(Farm_type,species)%>%
  summarise_each(funs(mean,std.error),d13C,d15N)

#plant means by landscape
landscape_means <- plantdat%>%
  group_by(landscape,species)%>%
  summarise_each(funs(mean,std.error),d13C,d15N)

#plant means by date
date_means <- plantdat%>%
  group_by(date,species)%>%
  summarise_each(funs(mean,std.error),d13C,d15N)

#linear models: d13 ~ species + farm_type + geography + farm
d13c_lm <- lm(data=rawdat, d13C ~ species+Farm_Type+geography+farm)
summary(d13c_lm)

#multiple linear models
model_output <- rawdat %>% 
  group_by(farm) %>% 
  do(tidy(lm(d13C ~ species, data = .), conf.int = TRUE)) 

View(model_output)

#ggplots
#species mean plots
overall <- ggplot(data = plant_means, aes(x = species, y = d13C_mean)) + 
  geom_point() + geom_errorbar(aes(ymin = d13C_mean - d13C_std.error, ymax = d13C_mean + d13C_std.error),width=.3) +
  ylab("d13C")+ggtitle("Overall")
  
#d13 differences of BP and OS between 6 farms
farm <- ggplot(data = farm_means, aes(x = species, y = d13C_mean,group=farm,color=farm)) + 
geom_point(data=farm_means, aes(group=farm,color=farm)) + geom_line(data=farm_means,aes(group=farm,color=farm))+
geom_errorbar(aes(ymin = d13C_mean - d13C_std.error, ymax = d13C_mean + d13C_std.error),width=.3) +
ylab("d13C")+ggtitle("Each Farm")


#farm_type plot
farmtype <- ggplot(data = farmtype_means, aes(x = species, y = d13C_mean,group=Farm_type,color=Farm_type)) + 
  geom_point(data=farmtype_means, aes(group=Farm_type,color=Farm_type)) + geom_line(data=farmtype_means,aes(group=Farm_type,color=Farm_type))+
  geom_errorbar(aes(ymin = d13C_mean - d13C_std.error, ymax = d13C_mean + d13C_std.error),width=.3) +
  ylab("d13C")+ggtitle("Farm Type")

#landscape plot
landscape <- ggplot(data = landscape_means, aes(x = species, y = d13C_mean,group=landscape,color=landscape)) + 
  geom_point(data=landscape_means, aes(group=landscape,color=landscape)) + geom_line(data=landscape_means,aes(group=landscape,color=landscape))+
  geom_errorbar(aes(ymin = d13C_mean - d13C_std.error, ymax = d13C_mean + d13C_std.error),width=.3) +
  ylab("d13C")+ggtitle("Landscape")

grid.arrange(overall,farm,farmtype,landscape,ncol=2)

#date plot
dateplot <- ggplot(date_means,aes(x=species,y=d13C_mean))+geom_point(aes(group=date,color=date)) +
  geom_errorbar(aes(ymin = d13C_mean - d13C_std.error, ymax = d13C_mean + d13C_std.error),width=.3) +
  ylab("d13C")+ggtitle("date")


###### dN15 plots ########
#Overall
overall_N <- ggplot(data = plant_means, aes(x = species, y = d15N_mean)) + 
  geom_point() + geom_errorbar(aes(ymin = d15N_mean - d15N_std.error, ymax = d15N_mean + d15N_std.error),width=.3) +
  ylab("d15N")+ggtitle("Overall")

#Farm
farm_N <- ggplot(data = farm_means, aes(x = species, y = d15N_mean,group=farm,color=farm)) + 
  geom_point(data=farm_means, aes(group=farm,color=farm)) + geom_line(data=farm_means,aes(group=farm,color=farm))+
  geom_errorbar(aes(ymin = d15N_mean - d15N_std.error, ymax = d15N_mean + d15N_std.error),width=.3) +
  ylab("d15N")+ggtitle("Each Farm")

#Farm type
farmtype_N <- ggplot(data = farmtype_means, aes(x = species, y = d15N_mean,group=Farm_type,color=Farm_type)) + 
  geom_point(data=farmtype_means, aes(group=Farm_type,color=Farm_type)) + geom_line(data=farmtype_means,aes(group=Farm_type,color=Farm_type))+
  geom_errorbar(aes(ymin = d15N_mean - d15N_std.error, ymax = d15N_mean + d15N_std.error),width=.3) +
  ylab("d15N")+ggtitle("Farm Type")

#Landscape
landscape_N <- ggplot(data = landscape_means, aes(x = species, y = d15N_mean,group=landscape,color=landscape)) + 
  geom_point(data=landscape_means, aes(group=landscape,color=landscape)) + geom_line(data=landscape_means,aes(group=landscape,color=landscape))+
  geom_errorbar(aes(ymin = d15N_mean - d15N_std.error, ymax = d15N_mean + d15N_std.error),width=.3) +
  ylab("d15N")+ggtitle("Landscape")

grid.arrange(overall_N,farm_N,farmtype_N,landscape_N,ncol=2)


#date
dateplot_N <- ggplot(date_means,aes(x=species,y=d15N_mean))+geom_point(aes(group=date,color=date)) +
  geom_errorbar(aes(ymin = d15N_mean - d15N_std.error, ymax = d15N_mean + d15N_std.error),width=.3) +
  ylab("d13N")+ggtitle("date")
dateplot_N

