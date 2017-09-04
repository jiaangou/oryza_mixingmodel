library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)
library(magrittr)
library(purrr)    ## manipulating vectors and lists
library(readr)
library(plotrix) #std.error function

rawdat<-read_csv("raw_data.csv")

#Splitting identity into corresponding variables (ie. farm type/species/date...)
rawdat$Farm_type <- substr(rawdat$Ident,2,3)
rawdat$species <- substr(rawdat$Ident,4,5)
rawdat$date <- substr(rawdat$Ident,6,13)
rawdat$landscape <- substr(rawdat$Ident,1,1)

#rename headers
names(rawdat)[3] <-"d15N"  ##some reason can't use rename() because of the space in variable name
names(rawdat)[7] <-"d13C" 
rawdat <- rawdat %>%   #concentration is in %
  rename(N_conc = 'Amt%')%>%
  rename(C_conc = 'Amt%_1')


# rename variables
rawdat$Farm_type[rawdat$Farm_type == "Or"] <- "OR"
rawdat$Farm_type[rawdat$Farm_type == "Cv"] <- "CV"
rawdat$species[rawdat$species == "Bp"] <- "BP"
rawdat$species[rawdat$species == "Os"] <- "OS"
rawdat$landscape[rawdat$landscape == "1"] <- "M"
rawdat$landscape[rawdat$landscape == "2"] <- "G"
rawdat$landscape[rawdat$landscape == "3"] <- "O"

#farm identity = (type + geography)
rawdat$farm <- with(rawdat,paste0(rawdat$Farm_type,rawdat$landscape))

#Add trophic level variable
rawdat$trophic_level <- "producer"

#remove comment columns "X5" and "X6"
rawdat<-rawdat%>%
  subset(select=c(Ident,Amt,d15N,N_conc,d13C,C_conc,Farm_type,species,date,landscape,farm,trophic_level))

#omit NA rows
rawdat <- rawdat%>%
  na.omit(rawdat)

write.csv(rawdat,file="rawdat_cleaned.csv",row.names=F)

##########################################################################################
#cleaning UCDavis data
#read orzya-1 
oryza_1 <- read.csv("oryza-1_0817.csv")
names(oryza_1)
oryza_1 <- oryza_1 %>%
  rename(Ident = 'Sample.ID')%>%
  rename(C_amt = 'C.Amount..ug.')%>%
  rename(N_amt = 'N.Amount..ug.')%>%
  rename(Amt='Amount..mg.' ) ## Amt unit is in (mg)
#select necessary columns
oryza_1 <- oryza_1 %>%
  subset(select=c(Ident,d13C,d15N,C_amt,N_amt,Amt))
oryza_1$Ident <- as.character(oryza_1$Ident) #convert factor to character 
#omit extra rows
oryza_1<-na.omit(oryza_1)
#convert C and N amount (ug) to C and N amount (%) --- 1mg = 1000ug but conversion to % is *100
oryza_1$C_conc <- round((oryza_1$C_amt/10)/oryza_1$Amt,digits=2)
oryza_1$N_conc <- round((oryza_1$N_amt/10)/oryza_1$Amt,digits=2)

#remove C_amt and N_amt
oryza_1 <- oryza_1%>%
  subset(select=c(Ident,d13C,d15N,Amt,C_conc,N_conc,trophic_level,Farm_type,landscape))

#### Create new variable columns using Ident value
#trophic level (consumer/producer)
for (i in 1:nrow(oryza_1)) {
  if (nchar(oryza_1$Ident[i]) == 17)
    oryza_1$trophic_level[i]='consumer'
  else
    oryza_1$trophic_level[i]='producer'
}
## Farm type (organic or conventional)
oryza_1$Farm_type <- substr(oryza_1$Ident,2,3)
## landscape
oryza_1$landscape <- substr(oryza_1$Ident,1,1)
oryza_1$landscape[oryza_1$landscape == "1"] <- "M"
oryza_1$landscape[oryza_1$landscape == "2"] <- "G"
oryza_1$landscape[oryza_1$landscape == "3"] <- "O"
## Farm
oryza_1$farm <- with(oryza_1,paste0(oryza_1$Farm_type,oryza_1$landscape))

## split producer and consumer into separate DF then attain date and species variables from there
prod <- oryza_1[oryza_1$trophic_level=='producer',]
cons <- oryza_1[oryza_1$trophic_level=='consumer',]

## date
prod$date <- substr(prod$Ident,6,13)
cons$date <- substr(cons$Ident,4,11)
## species
prod$species <- substr(prod$Ident,4,5)
cons$species <- substr(cons$Ident,12,17) 

#rearrange columns to the same order as rawdat
prod <- prod[,c(1,4,3,6,2,5,8,12,11,9,10,7)]
cons <- cons[,c(1,4,3,6,2,5,8,12,11,9,10,7)]


###################################################################################

## combining all plant data 
plantdat <- read.csv("rawdat_cleaned.csv",row.names=NULL)
plantdat <- rbind(plantdat,prod)

