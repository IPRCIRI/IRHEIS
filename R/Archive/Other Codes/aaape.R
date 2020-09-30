rm(list=ls())

cat("\n\n================ Admin Area Analysis Panel Estimates =================================\n")

library(data.table)
library(texreg)


load("D:/HEIS/DataProcessed/BigD.rda")
TXI <- dcast(data.table(table(BigD[,.(IGeo4,Year)])), IGeo4 ~ Year)
TXF <- dcast(data.table(table(BigD[,.(FGeo4,Year)])), FGeo4 ~ Year)
TX <- dcast(data.table(table(BigD[,.(Geo4,Year)])), Geo4 ~ Year)
View(TXI)
a <- which(TXI==0,arr.ind = TRUE)
a <- unique(a[,1])
BigD <- BigD[! IGeo4 %in% c("2201","0912","0910")] # Remove Abu Musa, Qaenat & Tabas

# Province Panel ============================================
PP <- BigD[,list(c=weighted.mean(log(RXNDpc+1),Weight), 
                 x=weighted.mean(log(RXTpc+1),Weight),
                 d=weighted.mean(log(RXDpc+1),Weight),
                 size=weighted.mean(Size,Weight),
                 age=weighted.mean(HAge,Weight),
                 HHCount=.N,
                 Pop=sum(Weight)),
           by=c("Year","FGeo2")]

PP[,idx:=paste0("Pr",FGeo2)]
PP[,fg:=as.integer(FGeo2)]

library(haven)
write_dta(PP,"PP.dta")

PP[,NPD:=NULL]
PP[FGeo2=="28" & Year==83, NPD:=1]
PP[FGeo2=="29" & Year==83, NPD:=1]
PP[FGeo2=="30" & Year==90, NPD:=1]
PP[is.na(NPD),NPD:=0]

PP[,SP:="0"]
PP[FGeo2 %in% c("28","29","30"), SP:=FGeo2]

PPx <- PP[,.(c=weighted.mean(c,Pop)),by=.(Year,SP)]

qplot(Year,NPD,data = PP,col=FGeo2,geom = "line")
qplot(Year,c,data = PP,col=FGeo2,geom = "line")
qplot(Year,c,data = PPx,col=factor(SP),geom = "line")

library(plm)
pp <- pdata.frame(PP,index = c("idx","Year"))
pp$dc <- diff(pp$c)


# summary(plm(diff(c) ~ lag(NPD,0:3), data = pp))
# summary(plm(diff(c) ~ NPD, data = pp))
# summary(plm(diff(c) ~ lag(diff(c))  + NPD, data = pp))
# summary(plm(diff(c) ~ lag(diff(c))  + lag(NPD), data = pp))
# summary(plm(diff(c) ~ lag(diff(c))  + lag(NPD,0:2), data = pp))
# 
# summary(plm(dx ~ l.dx  + NPD + l.NPD, data = pp))
# 
# pb <- plm(dc ~ l.dc  + NPD, data = pp, model = "between")
# pfi <- plm(dc ~ l.dc  + NPD, data = pp, model = "within", effect = "individual")
# pft <- plm(dc ~ l.dc  + NPD, data = pp, model = "within", effect = "time")
# pfd <- plm(dc ~ l.dc  + NPD, data = pp, model = "within", effect = "twoways")
# po <- plm(dc ~ l.dc  + NPD, data = pp, model = "pooling")
# pr <- plm(dc ~ l.dc  + NPD, data = pp, model = "random")
# 
# pFtest(pfi,po)
# pFtest(pft,po)
# pFtest(pfd,po)
# 
# summary(pb)
# Province Cohort10 Panel ============================================
PCP10 <- BigD[,list(c=weighted.mean(log(RXNDpc+1),Weight), 
                  x=weighted.mean(log(RXTpc+1),Weight),
                  d=weighted.mean(log(RXDpc+1),Weight),
                  size=weighted.mean(Size,Weight),
                  age=weighted.mean(HAge,Weight),
                  HHCount=.N,
                  Pop=sum(Weight)),
            by=c("Year","FGeo2","Cohort10")]

PCP10 <- PCP10[!((Cohort10=="1276-85") | (Cohort10=="1286-95") |
               (Cohort10=="1296-05" & Year >=91) |
               (Cohort10=="1356-65" & Year <=80) |
               (Cohort10=="1366-75" & Year <=90) |
               (Cohort10=="1376-85")),]
PCP10[,idx:=paste0("Pr",FGeo2,":",Cohort10)]

PCP10[,NPD:=NULL]
PCP10[FGeo2=="28" & Year==83, NPD:=1]
PCP10[FGeo2=="29" & Year==83, NPD:=1]
PCP10[FGeo2=="30" & Year==90, NPD:=1]
PCP10[is.na(NPD),NPD:=0]

library(plm)
pcp10 <- pdata.frame(PCP10,index = c("idx","Year"))
pcp10$d.c <- diff(pcp10$c) 
pcp10$l.d.c <- lag(diff(pcp10$c))
pcp10$l2.d.c <- lag(diff(pcp10$c),2)
pcp10$d.x <- diff(pcp10$x) 
pcp10$l.d.x <- lag(diff(pcp10$x))
pcp10$l2.d.x <- lag(diff(pcp10$x),2)
pcp10$d.d <- diff(pcp10$d) 
pcp10$l.d.d <- lag(diff(pcp10$d))
pcp10$l2.d.d <- lag(diff(pcp10$d),2)
pcp10$l.NPD <- lag(pcp10$NPD)
pcp10$l2.NPD <- lag(pcp10$NPD,2)
pcp10$l3.NPD <- lag(pcp10$NPD,3)

mcl0 <- plm(d.c ~  l.d.c  + NPD , data = pcp10)
mcl1 <- plm(d.c ~  l.d.c  + NPD + l.NPD, data = pcp10)
mcl2 <- plm(d.c ~  l.d.c  + NPD + l.NPD + l2.NPD , data = pcp10)
mcl3 <- plm(d.c ~  l.d.c  + NPD + l.NPD + l2.NPD + l3.NPD, data = pcp10)

mcn0 <- plm(d.c ~  NPD , data = pcp10)
mcn1 <- plm(d.c ~  NPD + l.NPD, data = pcp10)
mcn2 <- plm(d.c ~  NPD + l.NPD + l2.NPD , data = pcp10)
mcn3 <- plm(d.c ~  NPD + l.NPD + l2.NPD + l3.NPD, data = pcp10)

screenreg(list(mcl0,mcl1,mcl2,mcl3,
               mcn0,mcn1,mcn2,mcn3))
htmlreg(file = "D:/Dropbox/ToPublish/AdminAreas/pcp10-c.doc",
        list(mcl0,mcl1,mcl2,mcl3,
             mcn0,mcn1,mcn2,mcn3))

mxl0 <- plm(d.x ~  l.d.x  + NPD , data = pcp10)
mxl1 <- plm(d.x ~  l.d.x  + NPD + l.NPD, data = pcp10)
mxl2 <- plm(d.x ~  l.d.x  + NPD + l.NPD + l2.NPD , data = pcp10)
mxl3 <- plm(d.x ~  l.d.x  + NPD + l.NPD + l2.NPD + l3.NPD, data = pcp10)

mxn0 <- plm(d.x ~  NPD , data = pcp10)
mxn1 <- plm(d.x ~  NPD + l.NPD, data = pcp10)
mxn2 <- plm(d.x ~  NPD + l.NPD + l2.NPD , data = pcp10)
mxn3 <- plm(d.x ~  NPD + l.NPD + l2.NPD + l3.NPD, data = pcp10)

screenreg(list(mxl0,mxl1,mxl2,mxl3,
               mxn0,mxn1,mxn2,mxn3))
htmlreg(file = "D:/Dropbox/ToPublish/AdminAreas/pcp10-x.doc",
        list(mxl0,mxl1,mxl2,mxl3,
             mxn0,mxn1,mxn2,mxn3))

mdl0 <- plm(d.d ~  l.d.d  + NPD , data = pcp10)
mdl1 <- plm(d.d ~  l.d.d  + NPD + l.NPD, data = pcp10)
mdl2 <- plm(d.d ~  l.d.d  + NPD + l.NPD + l2.NPD , data = pcp10)
mdl3 <- plm(d.d ~  l.d.d  + NPD + l.NPD + l2.NPD + l3.NPD, data = pcp10)

mdn0 <- plm(d.d ~  NPD , data = pcp10)
mdn1 <- plm(d.d ~  NPD + l.NPD, data = pcp10)
mdn2 <- plm(d.d ~  NPD + l.NPD + l2.NPD , data = pcp10)
mdn3 <- plm(d.d ~  NPD + l.NPD + l2.NPD + l3.NPD, data = pcp10)

screenreg(list(mdl0,mdl1,mdl2,mdl3,
               mdn0,mdn1,mdn2,mdn3))

# Province Cohort5 Panel ============================================
PCP5 <- BigD[,list(c=weighted.mean(log(RXNDpc+1),Weight), 
                  x=weighted.mean(log(RXTpc+1),Weight),
                  d=weighted.mean(log(RXDpc+1),Weight),
                  size=weighted.mean(Size,Weight),
                  age=weighted.mean(HAge,Weight),
                  HHCount=.N,
                  Pop=sum(Weight)),
            by=c("Year","FGeo2","Cohort5")]
ifelse(table(PCP5[HHCount>10,.(Cohort5,Year)])/table(PCP5[,.(Cohort5,Year)])>0.2,1,0)

PCP5 <- PCP5[!((Cohort5=="1276-80") |(Cohort5=="1281-85") | (Cohort5=="1286-90") |(Cohort5=="1291-95") | (Cohort5=="1296-00") |
               (Cohort5=="1296-00" & Year >=82) |
               (Cohort5=="1301-05" & Year >=93) |
               (Cohort5=="1356-60" & Year <=77) |
               (Cohort5=="1361-65" & Year <=84) |
               (Cohort5=="1366-70" & Year <=87) |
               (Cohort5=="1371-75") | (Cohort5=="1376-80") | (Cohort5=="1381-85") | (Cohort5=="1386-90")),]
PCP5[,idx:=paste0("Pr",FGeo2,":",Cohort5)]

PCP5[,NPD:=NULL]
PCP5[FGeo2=="28" & Year==83, NPD:=1]
PCP5[FGeo2=="29" & Year==83, NPD:=1]
PCP5[FGeo2=="30" & Year==90, NPD:=1]
PCP5[is.na(NPD),NPD:=0]

library(plm)
pcp5 <- pdata.frame(PCP5,index = c("idx","Year"))
pcp5$d.c <- diff(pcp5$c) 
pcp5$l.d.c <- lag(diff(pcp5$c))
pcp5$l2.d.c <- lag(diff(pcp5$c),2)
pcp5$d.x <- diff(pcp5$x) 
pcp5$l.d.x <- lag(diff(pcp5$x))
pcp5$l2.d.x <- lag(diff(pcp5$x),2)
pcp5$d.d <- diff(pcp5$d) 
pcp5$l.d.d <- lag(diff(pcp5$d))
pcp5$l2.d.d <- lag(diff(pcp5$d),2)
pcp5$l.NPD <- lag(pcp5$NPD)
pcp5$l2.NPD <- lag(pcp5$NPD,2)
pcp5$l3.NPD <- lag(pcp5$NPD,3)

mcl0 <- plm(d.c ~  l.d.c  + NPD , data = pcp5)
mcl1 <- plm(d.c ~  l.d.c  + NPD + l.NPD, data = pcp5)
mcl2 <- plm(d.c ~  l.d.c  + NPD + l.NPD + l2.NPD , data = pcp5)
mcl3 <- plm(d.c ~  l.d.c  + NPD + l.NPD + l2.NPD + l3.NPD, data = pcp5)

mcn0 <- plm(d.c ~  NPD , data = pcp5)
mcn1 <- plm(d.c ~  NPD + l.NPD, data = pcp5)
mcn2 <- plm(d.c ~  NPD + l.NPD + l2.NPD , data = pcp5)
mcn3 <- plm(d.c ~  NPD + l.NPD + l2.NPD + l3.NPD, data = pcp5)

screenreg(list(mcl0,mcl1,mcl2,mcl3,
               mcn0,mcn1,mcn2,mcn3))
htmlreg(file = "D:/Dropbox/ToPublish/AdminAreas/pcp5-c.doc",
        list(mcl0,mcl1,mcl2,mcl3,
             mcn0,mcn1,mcn2,mcn3))

mxl0 <- plm(d.x ~  l.d.x  + NPD , data = pcp5)
mxl1 <- plm(d.x ~  l.d.x  + NPD + l.NPD, data = pcp5)
mxl2 <- plm(d.x ~  l.d.x  + NPD + l.NPD + l2.NPD , data = pcp5)
mxl3 <- plm(d.x ~  l.d.x  + NPD + l.NPD + l2.NPD + l3.NPD, data = pcp5)

mxn0 <- plm(d.x ~  NPD , data = pcp5)
mxn1 <- plm(d.x ~  NPD + l.NPD, data = pcp5)
mxn2 <- plm(d.x ~  NPD + l.NPD + l2.NPD , data = pcp5)
mxn3 <- plm(d.x ~  NPD + l.NPD + l2.NPD + l3.NPD, data = pcp5)

screenreg(list(mxl0,mxl1,mxl2,mxl3,
               mxn0,mxn1,mxn2,mxn3))
htmlreg(file = "D:/Dropbox/ToPublish/AdminAreas/pcp5-x.doc",
        list(mxl0,mxl1,mxl2,mxl3,
             mxn0,mxn1,mxn2,mxn3))

mdl0 <- plm(d.d ~  l.d.d  + NPD , data = pcp5)
mdl1 <- plm(d.d ~  l.d.d  + NPD + l.NPD, data = pcp5)
mdl2 <- plm(d.d ~  l.d.d  + NPD + l.NPD + l2.NPD , data = pcp5)
mdl3 <- plm(d.d ~  l.d.d  + NPD + l.NPD + l2.NPD + l3.NPD, data = pcp5)

mdn0 <- plm(d.d ~  NPD , data = pcp5)
mdn1 <- plm(d.d ~  NPD + l.NPD, data = pcp5)
mdn2 <- plm(d.d ~  NPD + l.NPD + l2.NPD , data = pcp5)
mdn3 <- plm(d.d ~  NPD + l.NPD + l2.NPD + l3.NPD, data = pcp5)

screenreg(list(mdl0,mdl1,mdl2,mdl3,
               mdn0,mdn1,mdn2,mdn3))

# County Level Panel ========================================
CP <- BigD[,list(c=weighted.mean(log(RXNDpc+1),Weight), 
                 x=weighted.mean(log(RXTpc+1),Weight),
                 d=weighted.mean(log(RXDpc+1),Weight),
                 size=weighted.mean(Size,Weight),
                 age=weighted.mean(HAge,Weight),
                 HHCount=.N,
                 FGeo2=FGeo2[1],
                 Pop=sum(Weight)),
           by=c("Year","IGeo4")]
CP[,idx:=paste0("Pr",IGeo4)]

CP[,NPD:=NULL]
CP[,NPCD:=NULL]

CP[FGeo2=="28" & Year==83, NPD:=1]
CP[FGeo2=="29" & Year==83, NPD:=1]
CP[FGeo2=="30" & Year==90, NPD:=1]

CP[IGeo4=="0902" & Year==83, NPCD:=1]
CP[IGeo4=="0903" & Year==83, NPCD:=1]
CP[IGeo4=="2305" & Year==90, NPCD:=1]


CP[is.na(NPD),NPD:=0]
CP[is.na(NPCD),NPCD:=0]


library(plm)
cp <- pdata.frame(CP,index = c("idx","Year"))

cp$d.c <- diff(cp$c) 
cp$l.d.c <- lag(diff(cp$c))
cp$l2.d.c <- lag(diff(cp$c),2)
cp$d.x <- diff(cp$x) 
cp$l.d.x <- lag(diff(cp$x))
cp$l2.d.x <- lag(diff(cp$x),2)
cp$d.d <- diff(cp$d) 
cp$l.d.d <- lag(diff(cp$d))
cp$l2.d.d <- lag(diff(cp$d),2)
cp$l.NPD <- lag(cp$NPD)
cp$l2.NPD <- lag(cp$NPD,2)
cp$l3.NPD <- lag(cp$NPD,3)
cp$l.NPCD <- lag(cp$NPCD)
cp$l2.NPCD <- lag(cp$NPCD,2)
cp$l3.NPCD <- lag(cp$NPCD,3)

mcl0N <- plm(d.c ~  l.d.c  + NPD , data = cp)
mcl1N <- plm(d.c ~  l.d.c  + NPD + l.NPD, data = cp)
mcl2N <- plm(d.c ~  l.d.c  + NPD + l.NPD + l2.NPD , data = cp)
mcl3N <- plm(d.c ~  l.d.c  + NPD + l.NPD + l2.NPD + l3.NPD, data = cp)
mclN0 <- plm(d.c ~  l.d.c  + NPCD , data = cp)
mclN1 <- plm(d.c ~  l.d.c  + NPCD + l.NPCD, data = cp)
mclN2 <- plm(d.c ~  l.d.c  + NPCD + l.NPCD + l2.NPCD , data = cp)
mclN3 <- plm(d.c ~  l.d.c  + NPCD + l.NPCD + l2.NPCD + l3.NPCD, data = cp)
mcl00 <- plm(d.c ~  l.d.c  + NPD + NPCD , data = cp)
mcl11 <- plm(d.c ~  l.d.c  + NPD + l.NPD + NPCD + l.NPCD, data = cp)
mcl22 <- plm(d.c ~  l.d.c  + NPD + l.NPD + l2.NPD + NPCD + l.NPCD+ l2.NPCD , data = cp)

mcn0N <- plm(d.c ~ NPD , data = cp)
mcn1N <- plm(d.c ~ NPD + l.NPD, data = cp)
mcn2N <- plm(d.c ~ NPD + l.NPD + l2.NPD , data = cp)
mcn3N <- plm(d.c ~ NPD + l.NPD + l2.NPD + l3.NPD, data = cp)
mcnN0 <- plm(d.c ~ NPCD , data = cp)
mcnN1 <- plm(d.c ~ NPCD + l.NPCD, data = cp)
mcnN2 <- plm(d.c ~ NPCD + l.NPCD + l2.NPCD , data = cp)
mcnN3 <- plm(d.c ~ NPCD + l.NPCD + l2.NPCD + l3.NPCD, data = cp)
mcn00 <- plm(d.c ~ NPD + NPCD , data = cp)
mcn11 <- plm(d.c ~ NPD + l.NPD + NPCD + l.NPCD, data = cp)
mcn22 <- plm(d.c ~ NPD + l.NPD + l2.NPD + NPCD + l.NPCD+ l2.NPCD , data = cp)

screenreg(list(mcl0N,mcl1N,mcl2N,mclN0,mclN1,mclN2,
               mcl00,mcl11,mcl22))
screenreg(list(mcn0N,mcn1N,mcn2N,mcnN0,mcnN1,mcnN2,
               mcn00,mcn11,mcn22))

htmlreg(file = "D:/Dropbox/ToPublish/AdminAreas/cp-c-1.doc",
          list(mcl0N,mcl1N,mcl2N,mclN0,mclN1,mclN2,
               mcl00,mcl11,mcl22))
htmlreg(file = "D:/Dropbox/ToPublish/AdminAreas/cp-c-2.doc",
          list(mcn0N,mcn1N,mcn2N,mcnN0,mcnN1,mcnN2,
               mcn00,mcn11,mcn22))
