rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Nominal to Real =====================================\n")

library(yaml)
mydir <- "E:/Refah Economy/IRHEIS-master update30-9/IRHEIS-master/R"
setwd(mydir)

Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(data.table)
library(ggplot2)

bigMD <- data.table()
for(year in (88:99)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN3.rda"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"HHHouseProperties.rda"))
  
  SMD <- MD[,.(HHID,Year,Quarter,Month,Region,ProvinceCode,ProvinceName,
               CountyCode,CountyName,NewArea,NewArea_Name,Weight,House_Exp,MainHouse_Exp,MeterPrice)]
  SMD <- merge(SMD,HHHouseProperties[,.(HHID,tenure,area)])
  
  bigMD <- rbind(bigMD,SMD)
}

bigMD <- bigMD[!is.na(MeterPrice)]

bigMD[,GenClass:=ifelse(grepl("Sh_Tehran",NewArea_Name),"Tehran",NA)]
bigMD[is.na(GenClass),GenClass:=ifelse(grepl("Sh_",NewArea_Name),"BigUrbanCounty",NA)]
bigMD[is.na(GenClass) & Region=="Urban",GenClass:="Urabn"]
bigMD[is.na(GenClass) & Region=="Rural",GenClass:="Rural"]

bigMD[,HouseOwner:=tenure %in% c("OwnLandandBuilding","Apartment")]

bigMD[tenure=="OwnLandandBuilding",HouseStatus:="Owns"]
bigMD[tenure=="Apartment",HouseStatus:="Owns"]
bigMD[tenure=="Rented",HouseStatus:="Rented"]
bigMD[tenure=="Mortgage",HouseStatus:="Rented"]
bigMD[tenure=="AgainstService",HouseStatus:="ForService"]
bigMD[tenure=="Free",HouseStatus:="Free"]
bigMD[tenure=="Other",HouseStatus:="Other"]

table(bigMD$GenClass)
windowsFonts(Cal = windowsFont("Calibri"),
             CamM = windowsFont("Cambria Math"),
             BNZ = windowsFont("B Nazanin"),
             XBN = windowsFont("XB Niloofar"))

#den <- density(x = (bigMD$MeterPrice), weights = bigMD$Weight/sum(bigMD$Weight))
den <- density(x = log(bigMD$MeterPrice), weights = bigMD$Weight/sum(bigMD$Weight))

library(hrbrthemes)
bigMD[,ygn:=.N,by=.(Year,GenClass)]
bigMD[,yghn:=.N,by=.(Year,GenClass,HouseOwner)]
bigMD[,mean(yghn/ygn),by=.(Year,GenClass,HouseOwner)]
g1 <- ggplot(bigMD,aes(x=log(MeterPrice),group=factor(HouseOwner,levels = c(TRUE,FALSE))))+
  geom_density(aes(color=HouseOwner,
                   fill=HouseOwner,
                   weight=Weight/sum(Weight),
                   alpha=yghn/ygn),
               ,adjust=1.5)+
  facet_grid(Year~GenClass)+theme_bw()

ggsave("G1.png",g1,width = 20,height = 20)

TehMD <- bigMD[GenClass=="Tehran" & Year %in% c(96:99)]
TehMD[,yhn:=.N,by=.(Year,HouseStatus)]
TehMD[,yn:=.N,by=.(Year)]
TehMD[,mean(yhn/yn),by=.(Year,HouseStatus)]

g2 <- ggplot(TehMD,aes(x=log(MeterPrice),group=factor(HouseStatus,levels=c("Owns","Rented","Free","ForService"))))+
  geom_density(aes(color=HouseStatus,
                   fill=HouseStatus,
                   weight=Weight/sum(Weight),
                   alpha=yhn/yn),
               adjust=1.5)+
  facet_grid(Year~GenClass)+    theme_bw()

ggsave("G2.png",g2,width = 6,height = 8)

spdhhs <- bigMD[GenClass=="Tehran" & Year==99 & log(MeterPrice)<11,HHID]

bigMD[HHID %in% spdhhs & Year==99,.(log(MeterPrice),tenure)]
View(bigMD[HHID %in% spdhhs,.(HHID,GenClass,Year,HouseStatus,House_Exp,MainHouse_Exp,area,MeterPrice)])

par(family="XBN", cex=1)
plot(den,xaxt = "n",yaxt="n",main = "",xlab = "",ylab = "",lwd=2)

xlbln <- c(1e2,1e3,1e4,1e5,1e6,1e8)
xlbls <- c("صد","هزار","ده هزار","صد هزار","میلیون","صد میلیون")
xlbls <- paste("\u202B",xlbls)
axis(1, at=log(xlbln), labels=xlbls)
axis(2, at=c(0,.05,.1,.15,.2,.25,.30), labels=c("۰٫۰۰","۰٫۰۵","۰٫۱۰",
                                                "۰٫۱۵","۰٫۲۰","۰٫۲۵","۰٫۳۰"))
legend(13, .25, legend=c("میانگین"
                         , "مد",
                         "میانه"),
       col=c("red", "blue","green"),lty=1:3,lwd=1,text.font=2)
den$xorig <- den$x
x0 <- den$x[-length(den$x)]
x1 <- den$x[-1]
dx <- x1-x0
x <- (x0+x1)/2
maxxinD2ex <- max(which(x1<max(log(SMD$MeterPrice))))
y0 <- den$y[-length(den$y)]
y1 <- den$y[-1]
y <- (y0+y1)/2
meanp <- sum(exp(x[1:maxxinD2ex])*y[1:maxxinD2ex]*dx[1:maxxinD2ex])
modep <- exp(den$x[which.max(den$y)])
medianp <- exp(den$x[length(x) %/% 2])
#totalincome <- meanincome*sum(DT$Pop)
abline(v = log(meanp),col="red",lwd=3,lty=1)
abline(v = log(modep),col="blue",lwd=3,lty=2)
abline(v = log(medianp),col="green",lwd=3,lty=3)


endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat((endtime-starttime)["elapsed"])
cat(" seconds")
