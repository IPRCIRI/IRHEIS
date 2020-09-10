#Hist
# Licence: GPL-3
rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Hist =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(stringr)
library(plotrix)
library(Hmisc)


years <- Settings$startyear:Settings$endyear

for(year in years){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  #load(file=paste0(Settings$HEISProcessedPath,"Y",year,"P1.rda"))
  #P1<-P1[Age<20]
  
  #out <- histbackback(split(P1$Age, P1$Sex),ylim(), main="83")
  #barplot(-out$left, col="orange" , horiz=TRUE, space=0, add=TRUE, axes=FALSE)
  #barplot(out$right, col="purple", horiz=TRUE, space=0, add=TRUE, axes=FALSE)
  
  
}

X <- c(rep(65, times=5), rep(25, times=5), rep(35, times=10), rep(45, times=4))
hist(X, prob=TRUE, col="grey")# prob=TRUE for probabilities not counts
lines(density(X), col="blue", lwd=2) # add a density estimate with defaults
lines(density(X, adjust=2), lty="dotted", col="darkgreen", lwd=2) 


foo <- c(850	,
         1250	,
         1430	,
         1560	,
         1690	,
         1980	,
         1980	,
         1980	,
         1980	,
         1980	,
         2370	,
         2370	,
         2370	,
         2370	,
         2370	,
         2700	,
         2700	,
         2700	,
         2700	,
         2700	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2460	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	,
         2010	
)
hist(foo, prob=TRUE)
curve(dnorm(x, mean=mean(foo), sd=sd(foo)), add=TRUE)


Poverty <- read_excel("C:/Users/pc1/Desktop/Poverty.xlsx",sheet = "Age2")
save(Poverty,file = "Ages_Calorie.rda")


p1<-ggplot(Poverty,aes(Age,Needed_Calorie,fill=Sex,alpha=0.5))+geom_col(alpha=0.5,position="dodge")
p2<-ggplot(Poverty,aes(Age,Needed_Calorie,fill=Sex))+stat_smooth(aes(y=Needed_Calorie,x=Age),method="gam",se=FALSE,formula=y~s(x,k=7))
p3<-ggplot(Poverty,aes(Age,Needed_Calorie,fill=Sex,alpha=0.5))+geom_density(stat="identity")

library(gridExtra)
grid.arrange(p1,p2,p3,nrow=3)
