

load(file=paste0(Settings$HEISProcessedPath,"Y",90,"FinalPoor.rda"))
A90<-MD

load(file=paste0(Settings$HEISProcessedPath,"Y",91,"FinalPoor.rda"))
A91<-MD

load(file=paste0(Settings$HEISProcessedPath,"Y",92,"FinalPoor.rda"))
A92<-MD

load(file=paste0(Settings$HEISProcessedPath,"Y",93,"FinalPoor.rda"))
A93<-MD

load(file=paste0(Settings$HEISProcessedPath,"Y",94,"FinalPoor.rda"))
A94<-MD

load(file=paste0(Settings$HEISProcessedPath,"Y",95,"FinalPoor.rda"))
A95<-MD

load(file=paste0(Settings$HEISProcessedPath,"Y",96,"FinalPoor.rda"))
A96<-MD

load(file=paste0(Settings$HEISProcessedPath,"Y",97,"FinalPoor.rda"))
A97<-MD

load(file=paste0(Settings$HEISProcessedPath,"Y",98,"FinalPoor.rda"))
A98<-MD

Data<-rbind(A90,A91)
Data<-rbind(Data,A92)
Data<-rbind(Data,A93)
Data<-rbind(Data,A94)
Data<-rbind(Data,A95)
Data<-rbind(Data,A96)
Data<-rbind(Data,A97)
Data<-rbind(Data,A98)

write.csv(Data,file="Data.csv")




