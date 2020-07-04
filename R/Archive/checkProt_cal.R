library(ggplot2)
SP=data.table()
for(year in (90:Settings$endyear)){
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"FINALPOORS.rda"))

SD50 <- MD[TOriginalFoodExpenditure_Per>0.7*FPLine 
            &TOriginalFoodExpenditure_Per<1.3*FPLine
            &FoodProtein_Per>50&Area>15*Size,
            .(p50=.N, En50=weighted.mean(TOriginalFoodExpenditure/Total_Exp_Month,Weight)),by=c("NewArea_Name","Region")]



SD500 <- MD[TOriginalFoodExpenditure_Per>0.7*FPLine 
            &TOriginalFoodExpenditure_Per<1.3*FPLine, 
            .(.N,mean_p=weighted.mean(FoodProtein_Per, Weight*Size),
              median_p=weighted.median(FoodProtein_Per, Weight*Size),
              max=max(FoodProtein_Per),min=min(FoodProtein_Per),
              En=weighted.mean(TOriginalFoodExpenditure/Total_Exp_Month,Weight)),by=c("NewArea_Name","Region")]
SD500 <- merge(SD500,SD50)
SD500 <- SD500[,Year:=year]
SP <- rbind(SP,SD500)

}

SP <- SP[,ratio:=p50/N]
SP <- SP[,Net:=1/En-1/En50]
ggplot(SP[NewArea_Name=="Sh_Bandarabas"])+
  geom_line(mapping = aes(x=Year,y=mean_p,col=factor(NewArea_Name)))

a<-SP$Net
hist(a)


SD <- merge(SD300, SD500)
SD <- merge(SD, SD700)
SD <- merge(SD, SD900)

Sp70 <- MD[FoodProtein_Per<70,.(p70=.N),by=c("NewArea_Name","Region")]
Sp50 <- MD[FoodProtein_Per<50,.(p50=.N),by=c("NewArea_Name","Region")]
Sp90 <- MD[FoodProtein_Per<90,.(p90=.N),by=c("NewArea_Name","Region")]
SP<-merge(Sp50,Sp70)
SP<-merge(SP,Sp90)
SDP<-merge(SP,SD)



p_p70 <- MD[TOriginalFoodExpenditure_Per>0.8*FPLine &  TOriginalFoodExpenditure_Per<1.2*FPLine& FoodProtein_Per<70,.(p_p70=.N),by=c("NewArea_Name","Region")]
p_p50 <- MD[TOriginalFoodExpenditure_Per>0.8*FPLine &  TOriginalFoodExpenditure_Per<1.2*FPLine& FoodProtein_Per<50,.(p_p50=.N),by=c("NewArea_Name","Region")]
p_p90 <- MD[TOriginalFoodExpenditure_Per>0.8*FPLine &  TOriginalFoodExpenditure_Per<1.2*FPLine& FoodProtein_Per<90,.(p_p90=.N),by=c("NewArea_Name","Region")]

SPP<-merge(p_p50,p_p70)
SPP<-merge(SPP,p_p90)
SDP<-merge(SPP,SDP)