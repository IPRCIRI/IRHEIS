
cpi97 <- read_excel("C:/Users/pc1/Desktop/cpi97.xlsx")
cpi97<-as.data.table(cpi97)
cpi97<-cpi97[Month>0]
cpi97<-cpi97[,Year:=NULL]
cpi97<-cpi97[,PeriodYear:=NULL]
cpi97<-cpi97[,.(FoodDrink_Index,Cigar_Index,Cloth_Index,HouseEnergy_Index,
                Furniture_Index,Hygiene_Index,Transportation_Index,
                Communication_Index,Amusement_Index,Hotel_Index,
                Other_Index,Durable_Index,Total_Index)]

MD<-merge(MD,Index_Dataset,by=c("Month"),all.x=TRUE)

FoodDrink_Index<-219.7
Cigar_Index<-281.1
Cloth_Index<-197.8
HouseEnergy_Index<-155.1
Furniture_Index<-225.1
Hygiene_Index<-158.1
Transportation_Index<-195.9
Communication_Index<-146.7
Amusement_Index<-224.5
Hotel_Index<-182.0
Other_Index<-199.5
Durable_Index<-259.2
Total_Index<-185.1





