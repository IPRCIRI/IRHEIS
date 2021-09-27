#Reweighting
Pop_U90<-54524.533
Pop_R90<-20625.136
Pop_U91<-55419
Pop_R91<-20656
Pop_U92<-56328
Pop_R92<-20687
Pop_U93<-57253
Pop_R93<-20718
Pop_U94<-58192
Pop_R94<-20748
Pop_U95<-59147
Pop_R95<-20779
Pop_U96<-60281
Pop_R96<-20789
Pop_U97<-61329
Pop_R97<-20754
Pop_U98<-62367
Pop_R98<-20708




if (year==90){
  Weight_U<-MD[Region=="Urban",sum(Weight*Size)/1000]
  Weight_R<-MD[Region=="Rural",sum(Weight*Size)/1000]
  MD[,Weight:=ifelse(Region=="Urban",Weight*Pop_U90/Weight_U,Weight*Pop_R90/Weight_R)]  
}

if (year==91){
  Weight_U<-MD[Region=="Urban",sum(Weight*Size)/1000]
  Weight_R<-MD[Region=="Rural",sum(Weight*Size)/1000]
  MD[,Weight:=ifelse(Region=="Urban",Weight*Pop_U91/Weight_U,Weight*Pop_R91/Weight_R)] 
}

if (year==92){
  Weight_U<-MD[Region=="Urban",sum(Weight*Size)/1000]
  Weight_R<-MD[Region=="Rural",sum(Weight*Size)/1000]
  MD[,Weight:=ifelse(Region=="Urban",Weight*Pop_U92/Weight_U,Weight*Pop_R92/Weight_R)]  
}

if (year==93){
  Weight_U<-MD[Region=="Urban",sum(Weight*Size)/1000]
  Weight_R<-MD[Region=="Rural",sum(Weight*Size)/1000]
  MD[,Weight:=ifelse(Region=="Urban",Weight*Pop_U93/Weight_U,Weight*Pop_R93/Weight_R)]  
}

if (year==94){
  Weight_U<-MD[Region=="Urban",sum(Weight*Size)/1000]
  Weight_R<-MD[Region=="Rural",sum(Weight*Size)/1000]
  MD[,Weight:=ifelse(Region=="Urban",Weight*Pop_U94/Weight_U,Weight*Pop_R94/Weight_R)]  
}

if (year==95){
  Weight_U<-MD[Region=="Urban",sum(Weight*Size)/1000]
  Weight_R<-MD[Region=="Rural",sum(Weight*Size)/1000]
  MD[,Weight:=ifelse(Region=="Urban",Weight*Pop_U95/Weight_U,Weight*Pop_R95/Weight_R)]  
}

if (year==96){
  Weight_U<-MD[Region=="Urban",sum(Weight*Size)/1000]
  Weight_R<-MD[Region=="Rural",sum(Weight*Size)/1000]
  MD[,Weight:=ifelse(Region=="Urban",Weight*Pop_U96/Weight_U,Weight*Pop_R96/Weight_R)] 
}

if (year==97){
  Weight_U<-MD[Region=="Urban",sum(Weight*Size)/1000]
  Weight_R<-MD[Region=="Rural",sum(Weight*Size)/1000]
  MD[,Weight:=ifelse(Region=="Urban",Weight*Pop_U97/Weight_U,Weight*Pop_R97/Weight_R)]  
}

if (year==98){
  Weight_U<-MD[Region=="Urban",sum(Weight*Size)/1000]
  Weight_R<-MD[Region=="Rural",sum(Weight*Size)/1000]
  MD[,Weight:=ifelse(Region=="Urban",Weight*Pop_U98/Weight_U,Weight*Pop_R98/Weight_R)]   
}

Pop<-MD[,sum(Weight*Size),by=c("CountyName")]
Geoo <- as.data.table(read_excel("C:/HEIS/DataResults/Geo.xlsx",
                                 sheet = "Sheet1"))

Pop<-merge(Pop,Geoo[,.(CountyName,Province,County)],by=c("CountyName"))
write_xlsx(Pop,path=paste0(Settings$HEISResultsPath,"/Pop.xlsx"),col_names=T)
