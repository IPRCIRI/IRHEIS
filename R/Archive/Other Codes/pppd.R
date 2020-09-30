library(readxl)
library(data.table)

cat("\n\n================ Prepare Provincial Price Data =================================\n")

O.w <- data.table(read_excel("D:/GoogleDrive/The Database Project/4-Annual/CPI/Urban CPI- State Level.xlsx"))
names(O.w)[1:2] <- c("PCode","ProvFa")
O.m <- melt(O.w,id.vars = c("PCode","ProvFa"),variable.factor=FALSE)
O.m
setnames(O.m,c("variable","value"),c("Year","CPI90"))

O.m[,Geo2:=sprintf("%02d", PCode)]
O <- O.m[,.(Year,Geo2,CPI90)]

N.ww <- data.table(read_excel("D:/Downloads/شاخص مصرف کننده.xlsx"))
names(N.ww)[1:3] <- c("ProvFa","Item","Group")
pln <- unique(N.ww$ProvFa)
plo <- unique(O.w$ProvFa)

intersect(plo,pln)
setdiff(plo,pln)
setdiff(pln,plo)

N.ww <- merge(N.ww,O.w[,1:2])
N.m <- melt(N.ww,id.vars = c("ProvFa","PCode","Item","Group"),variable.factor=FALSE)
setnames(N.m,c("variable","value"),c("Year","CPI95"))
N.m[,Geo2:=sprintf("%02d", PCode)]
N.m <- N.m[!is.na(CPI95),.(Year,Geo2,Group,CPI95)]
N <- dcast(N.m, Year + Geo2 ~ Group, value.var = "CPI95")
setnames(N,"CPI","CPI95")

A <- merge(O,N,by=c("Year","Geo2"),all = TRUE)[order(Year,Geo2)]
X<-A[Year==1390,.(Geo2,B=CPI95)]
A <- merge(A,X,by="Geo2",all.x = TRUE)
A[,CPI90to95:=CPI90/100*B]
A[,CPI:=CPI95]
A[is.na(CPI),CPI:=CPI90to95]

PriceData <- copy(A)
PriceData[,CPI90:=NULL]
PriceData[,CPI95:=NULL]
PriceData[,CPI90to95:=NULL]
PriceData[,B:=NULL]
PriceData[,Year:=as.integer(Year)]
save(PriceData,file = "D:/HEIS/DataProcessed/ProvincialPriceData.rda")
