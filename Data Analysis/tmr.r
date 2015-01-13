library(RHmm)
library(rgl)

setwd("C:/Users/Xiaoqian/Documents/analysis/")
source("aerojet-functions.r")
source("animate.r")
expert2<-read.csv('expert2.csv')
novice2<-read.csv('novice2_1.csv')

summary(expert2)
summary(novice2)
# Generating interactive 3D plot without animation
colarray <- c('red', 'orange', 'yellow', 'green', 'blue', 'purple');
plot3d(expert2[,1:3],col= colarray[expert2[,4]],xlab='Right Hand X', ylab='Right Hand Y', zlab='Right Hand Z');aspect3d('iso')
plot3d(novice2[,1:3],col= colarray[novice2[,6]],xlab='Right Hand X', ylab='Right Hand Y', zlab='Right Hand Z');aspect3d('iso')


#Feb 15, 2014, 
#Change runhmm function (starting from line 76)
#runhmm=function(mydata,n=6,j=c(9:11,49,50,58,59,65),train_range = 1:length(mydata$TRUTH)) {

#if the state(Truth) variable changes
#update maketruth5 function (starting from line 47)
j=c(1:3)
colarray <- c('red', 'orange', 'yellow', 'green', 'blue', 'purple');
colarray <- c( 'yellow','green', 'blue');
ex<-runhmm(const=0,expert2,4, j);aspect3d('iso')
no<-runhmm(const=3,novice2,4, j);aspect3d('iso')
summary(expert2)

chart <-read.csv("C:/Users/Xiaoqian/Desktop/Motion Capture/analysis/animation1.csv")

# Generating interactive 3D plot with animation (might be slow)
animate1(mydata=chart,sleep=0) # sleep=0 is the fastest