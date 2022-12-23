rm(list=ls())

library(ggplot2)# this is going to help us plotting
library(corrplot)#this is going to help us provide a visual correlation tool, giving us a rich array of plotting options
library(tsoutliers)#this is going to help us

#we load our data set
dataa=read.csv('dataArrests.csv', header=TRUE, sep=';')
dataa
nrow(dataa)
NROW(na.omit(dataa))
#we now use the structure argument to take a look at the data set we have assigned to dataa.
str(dataa)
#lets check if there is any missing value or any NaN value
any(is.na(dataa))
#now we remove the  missing values from our data 
dataa=dataa[complete.cases(dataa),]
#we again check to find out if there is any missing value after our earlier on 
any(is.na(dataa))
#checking the data structure and classes of the Variables
class(dataa$Assault)
class(dataa$UrbanPop)
class(dataa$Traffic)
class(dataa$CarAccidents)
class(dataa$Murder)
dataa$Murder
dataa$Assault
dataa$UrbanPop
dataa$CarAccidents
dataa$Traffic

# 2. Exploratory data analysis 

# for dependent variable Murder and explanatory variables 
# Assault, UrbanProp, Traffic, CarAccidents

# loading dplyr package for numerical analysis
library(dplyr)

# Summary of the mean values of the variables
summarise(arrests,avg_urbanpop=mean(UrbanPop),avg_assaults=mean(Assault),
          avg_trafficviolations=mean(Traffic),
          avg_caraccidents=mean(CarAccidents),avg_murders=mean(Murder))

# Summary of the minimum values of the variables
summarise(arrests,min_urbanpop=min(UrbanPop),min_assaults=min(Assault),
          min_trafficviolations=min(Traffic),
          min_caraccidents=min(CarAccidents),min_murders=min(Murder))

# Summary of the maximum values of the variables
summarise(arrests,max_urbanpop=max(UrbanPop),max_assaults=max(Assault),
          max_trafficviolations=max(Traffic),
          max_caraccidents=max(CarAccidents),max_murders=max(Murder))

# From the summaries we can see the mean of the variables and min and max values
# to tell what range of values the variables take.
# The minimum summary also revealed an outlier, minimum of car accidents was
# -66. It should not have negative values.

# Let's check how many negative observations there are for car accidents

arrests$CarAccidents[arrests$CarAccidents<=0]


#visual exploratory 

par(mfrow = c(2,2)) # Create a 2 x 2 plotting matrix for variable Murder againt Assault, UrbanPop, Traffic and CarAccidents
# The next 4 plots created will be plotted next to each other
plot(dataa$Assault, 
     dataa$Murder, ylab='Murder', xlab = 'Assault', main = 'Murder againt Assault',pch = 1, col = 'blue')
plot(dataa$UrbanPop, 
     dataa$Murder,ylab='Murder', xlab = 'Urban Pop', main = 'Murder againt the Urban Population', pch = 1, col = 'blue')
plot(dataa$Traffic, 
     dataa$Murder,ylab='Murder', xlab = 'Traffic', main = 'Murder againt Traffic', pch = 1, col = 'blue')
plot(dataa$CarAccidents, 
     dataa$Murder,ylab='Murder', xlab = 'CarAccidents', main = 'Murder againt Car Accidents', pch = 1, col = 'blue')
dev.off()

#Plot of dependent variable Murder
ggplot(dataa, aes(x=Murder))+geom_bar()+ggtitle('Murder')


#Plot of Car Accidents against Traffic
ggplot(dataa, aes(x=CarAccidents, y=Traffic))+geom_point()+ggtitle('Car Accidents against Traffic')


ggplot(dataa, aes(x=Murder))+geom_bar()
ggplot(dataa, aes(x=Assault))+geom_bar()
ggplot(dataa, aes(x=Assault, y=Murder))+geom_point()
ggplot(dataa, aes(x=UrbanPop))+geom_bar()
ggplot(dataa, aes(x=Traffic))+geom_bar()
ggplot(dataa, aes(x=CarAccidents))+geom_bar()
ggplot(dataa, aes(x=CarAccidents, y=Traffic))+geom_point()

#So i perform a correlation analysis on all variables 
cor(dataa)
corrplot(cor(dataa),'number', number.cex = 0.5)# so i split the data targetvar will  contain the target variable Murder
targetvar=dataa$Murder
# indvar will contain the our explanatory variables
indvar=dataa[,2:10]
targetvar
indvar
#so i perform a correlation on all other variables expect the target variable
corrplot(cor(indvar),'number', number.cex = 0.5)

corind=abs(cor(indvar))
diag(corind)=0
#Now am going to remove the explanatory variables that have a high correlation with each other
while (max(corind)>=0.8){
  #find explanatory variables witht the highest absolute correlation
  maxvar=which(corind==max(corind), arr.ind = TRUE)
  #select variable with the highest average correlation
  maxavg =which.max(rowMeans(corind[maxvar[,1],]))  
  
  print(rownames(maxvar)[maxvar[,1]==maxvar[maxavg,1]])
  
  #removal of exploratory variables with high correlations with each other
  indvar=indvar[,-maxvar[maxavg,1]]
  corind=corind[-maxvar[maxavg,1],-maxvar[maxavg,1]]
  
}

#lets make our model now
#But lets put bank our datasets back and we shall call it my_dataa

my_dataa=cbind('Murder'=targetvar,indvar)
my_dataa

linRegModel=lm(Murder~Assault+UrbanPop+Drug+Traffic+Cyber+Kidnapping+Domestic+Alcohol, data=my_dataa)
summary(linRegModel)

#Adjusted Model without Kidnappings
linRegModel=lm(Murder~Assault+UrbanPop+Drug+Traffic+Cyber+Domestic+Alcohol, data=my_dataa)
summary(linRegModel)

#Adjusted Model without Alcohol
linRegModel=lm(Murder~Assault+UrbanPop+Drug+Traffic+Cyber+Domestic, data=my_dataa)
summary(linRegModel)

#Adjusted Model without Domestic
linRegModel=lm(Murder~Assault+UrbanPop+Drug+Traffic+Cyber, data=my_dataa)
summary(linRegModel)

#Adjusted Model without Drug
linRegModel=lm(Murder~Assault+UrbanPop+Traffic+Cyber, data=my_dataa)
summary(linRegModel)

#Adjusted Model without Traffic
linRegModel=lm(Murder~Assault+UrbanPop+Cyber, data=my_dataa)
summary(linRegModel)

#Adjusted Model without Cyber
linRegModel=lm(Murder~Assault+UrbanPop, data=my_dataa)
summary(linRegModel)

#Adjusted Model without UrbanPop
linRegModel=lm(Murder~Assault, data=my_dataa)
summary(linRegModel)

#This is our Final Model.
linRegModel=lm(Murder~Assault, data=my_dataa)
summary(linRegModel)

ggplot(my_dataa,aes(x=Assault, y=Murder))+
  geom_point()+
  geom_line(aes(x=Assault,y=fitted.values(linRegModel)),col='red')


#Checking if all 5 properties of OLS linked to the Residuals have been fulfiled
#lets check if the Residual have a zero mean.
mean(residuals(linRegModel))#the mean of the residuals is -2.386073e-17

#check for homoskedacity & 4.Residual are linearly independent
plot(residuals(linRegModel),type='p', col='blue', ylim=c(-20,20),pch=16,
     ylab='residual', main='Residuals over Time')
abline(a=3*sd(residuals(linRegModel)), b=0, col='red',lty=2) 
abline(a=-3*sd(residuals(linRegModel)), b=0, col='red',lty=2) 
abline(a=mean(residuals(linRegModel)), b=0, col='red',lty=2)

#check the correlation between the residuals and independent variables
cor(residuals(linRegModel), dataa$Assault)
#checking if the Residuals are normally distributed
JarqueBera.test(residuals(linRegModel))