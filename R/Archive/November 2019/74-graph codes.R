x <- MD$TFoodKCalories_Per 
h<-hist(x, breaks=10, col="red", xlab="Miles TFoodKCalories_Per Gallon", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

plot(PEngel~TFoodKCalories_Per,data=MD2R)

a<-X4$total
b<-X4$urban
c<-X4$rural
ts.plot(ts(a),ts(b),ts(c),gpars=list(xlab="year",ylab="HCR",lty=c(1:3)),col=1:4)
   

a<-X4$urban1
b<-X4$urban2
c<-X4$urban3
d<-X4$urban4
e<-X4$urban5
f<-X4$urban6
g<-X4$urban7
ts.plot(ts(a),ts(b),ts(c),ts(d),ts(e),ts(f),ts(g),gpars=list(xlab="year",ylab="HCR",lty=c(1:3)),col=1:4)

a<-X4$rural1
b<-X4$rural2
c<-X4$rural3
d<-X4$rural4
e<-X4$rural5
ts.plot(ts(a),ts(b),ts(c),ts(d),ts(e),gpars=list(xlab="year",ylab="HCR",lty=c(1:3)),col=1:4)

        