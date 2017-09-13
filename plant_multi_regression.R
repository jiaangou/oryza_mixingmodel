install.packages(c("car", "MASS"))
library(car)  # function for regression diagnostics
library(MASS)  # function for regression diagnostics

plantdat <- read.csv(file="plant_data.csv")

########################## d13C ############################
########## multiple regression ##########
lm_plants_d13C <- lm(d13C ~ Farm_type + landscape + species + stage, 
                     data = plantdat)
summary(lm_plants_d13C)

### general diagnostic plots
par(mfrow=c(2,2))
plot(lm_plants_d13C)
par(mfrow=c(1,1))

### assessing outliers
outlierTest(lm_plants_d13C)  # Bonferonni p-value for most extreme obs
qqPlot(lm_plants_d13C)  #qq plot for studentized residual 
leveragePlots(lm_plants_d13C)  # leverage plots

### influential observations
avPlots(lm_plants_d13C,id.n = 3)  # added variable plots with the 3 most influential points

# avPlot depicts the relationship between y and one x variable, adjusting for the effects of other x variables.
# avplots help to uncover observations exerting a disproportionate influence on the regression model.
# high leverage observations show in added variable plots as points horizontally distant from the rest of the data.

cutoff <- 4/((nrow(mtcars)-length(lm_plants_d13C$coefficients)-2)) 
plot(lm_plants_d13C, which=4, cook.levels=cutoff) # Cook's distance plot

### normality of residuals
qqPlot(lm_plants_d13C) # qq plot for studentized resid

sresid <- studres(lm_plants_d13C) 
hist(sresid, breaks = 20, freq=FALSE, main="Distribution of Studentized Residuals")  # distribution of studentized residuals
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit) 

### evaluate collinearity
vif(lm_plants_d13C)  # variance inflation factors 
# the table shows that there is little collinearity among variables
# VIF is quite low

### test for autocorrelated errors
durbinWatsonTest(lm_plants_d13C)
# DW statistics = 1.3, slightly below the range of 1.5 to 3.0
# minor autocorrelation, non-independent of errors 


########## model selection ##########
# backward model selection 
step(lm_plants_d13C, direction = "backward")

# one variable is dropped in turn
drop1(lm_plants_d13C, test = "F")

# Both methods give the same results that all the variables are important 




########################## d15N ############################
########## multiple regression ##########
lm_plants_d15N <- lm(d15N ~ Farm_type + landscape + species + stage, 
                     data = plantdat)
summary(lm_plants_d15N)

### general diagnostic plots
par(mfrow=c(2,2))
plot(lm_plants_d15N)
par(mfrow=c(1,1))

### assessing outliers
outlierTest(lm_plants_d15N)  # Bonferonni p-value for most extreme obs
qqPlot(lm_plants_d15N)  #qq plot for studentized residual 
leveragePlots(lm_plants_d15N)  # leverage plots

### influential observations
avPlots(lm_plants_d15N, id.n = 3)  # added variable plots with the 3 most influential points

# avPlot depicts the relationship between y and one x variable, adjusting for the effects of other x variables.
# avplots help to uncover observations exerting a disproportionate influence on the regression model.
# high leverage observations show in added variable plots as points horizontally distant from the rest of the data.

cutoff <- 4/((nrow(mtcars)-length(lm_plants_d15N$coefficients)-2)) 
plot(lm_plants_d15N, which=4, cook.levels=cutoff) # Cook's distance plot

### normality of residuals
qqPlot(lm_plants_d15N) # qq plot for studentized resid

sresid <- studres(lm_plants_d15N) 
hist(sresid, breaks = 20, freq=FALSE, main="Distribution of Studentized Residuals")  # distribution of studentized residuals
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit) 

### evaluate collinearity
vif(lm_plants_d15N)  # variance inflation factors 
# the table shows that there is little collinearity among variables
# VIF is quite low

### test for autocorrelated errors
durbinWatsonTest(lm_plants_d15N)
# DW statistics = 1.5, in the range of 1.5 to 3.0


########## model selection ##########
# backward model selection 
step(lm_plants_d15N, direction = "backward")

# one variable is dropped in turn
drop1(lm_plants_d15N, test = "F")

# Both methods give the same results that "landscape" is not important and can be dropped









