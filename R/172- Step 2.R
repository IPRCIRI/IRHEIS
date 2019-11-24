#172- Step 2.R
# 
# Copyright © 2018: Majid Einian & Arin Shahbazian
# Licence: GPL-3

rm(list=ls())

starttime <- proc.time()
cat("\n\n================ Calculationg Per values =====================================\n")

library(yaml)
Settings <- yaml.load_file("Settings.yaml")

library(readxl)
library(stringr)
library(data.table)
library(ggplot2)
library(spatstat)

for(year in (Settings$startyear:Settings$endyear)){
  cat(paste0("\n------------------------------\nYear:",year,"\n"))
  
  load(file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN1.rda"))
  
  MD[,Total_Exp_Month_Per:=Total_Exp_Month/EqSizeRevOECD]
  MD[,Total_Exp_Month_Per_nondurable:=Total_Exp_Month_nondurable/EqSizeRevOECD]
  
  
  #MD<-merge(MD,BigFoodPrice,by=c("NewArea","Region"),all.x = TRUE)
  MD<-MD[Size!=0 & OriginalFoodExpenditure!=0 & !is.na(FoodKCaloriesHH)]
  #MD[,Home_Per_Metr:=MetrPrice/EqSizeRevOECD]
  
  #Calculate Per Values
  MD[,EqSizeCalory3 :=(Size-NKids) + NKids*(Settings$KCaloryNeed_Child/Settings$KCaloryNeed_Adult)]
  
  MD[,EqSizeCalory :=
       NAge0B	*(Settings$	KCaloryNeed_B0	/Calorie_Need_WorldBank)+
       NAge1B	*(Settings$	KCaloryNeed_B1	/Calorie_Need_WorldBank)+
       NAge2B	*(Settings$	KCaloryNeed_B2	/Calorie_Need_WorldBank)+
       NAge3B	*(Settings$	KCaloryNeed_B3	/Calorie_Need_WorldBank)+
       NAge4B	*(Settings$	KCaloryNeed_B4	/Calorie_Need_WorldBank)+
       NAge5B	*(Settings$	KCaloryNeed_B5	/Calorie_Need_WorldBank)+
       NAge6B	*(Settings$	KCaloryNeed_B6	/Calorie_Need_WorldBank)+
       NAge7B	*(Settings$	KCaloryNeed_B7	/Calorie_Need_WorldBank)+
       NAge8B	*(Settings$	KCaloryNeed_B8	/Calorie_Need_WorldBank)+
       NAge9B	*(Settings$	KCaloryNeed_B9	/Calorie_Need_WorldBank)+
       NAge10B	*(Settings$	KCaloryNeed_B10	/Calorie_Need_WorldBank)+
       NAge11B	*(Settings$	KCaloryNeed_B11	/Calorie_Need_WorldBank)+
       NAge12B	*(Settings$	KCaloryNeed_B12	/Calorie_Need_WorldBank)+
       NAge13B	*(Settings$	KCaloryNeed_B13	/Calorie_Need_WorldBank)+
       NAge14B	*(Settings$	KCaloryNeed_B14	/Calorie_Need_WorldBank)+
       NAge15B	*(Settings$	KCaloryNeed_B15	/Calorie_Need_WorldBank)+
       NAge16B	*(Settings$	KCaloryNeed_B16	/Calorie_Need_WorldBank)+
       NAge17B	*(Settings$	KCaloryNeed_B17	/Calorie_Need_WorldBank)+
       NAge18B	*(Settings$	KCaloryNeed_B18	/Calorie_Need_WorldBank)+
       NAge19B	*(Settings$	KCaloryNeed_B19	/Calorie_Need_WorldBank)+
       NAge20B	*(Settings$	KCaloryNeed_B20	/Calorie_Need_WorldBank)+
       NAge21B	*(Settings$	KCaloryNeed_B21	/Calorie_Need_WorldBank)+
       NAge22B	*(Settings$	KCaloryNeed_B22	/Calorie_Need_WorldBank)+
       NAge23B	*(Settings$	KCaloryNeed_B23	/Calorie_Need_WorldBank)+
       NAge24B	*(Settings$	KCaloryNeed_B24	/Calorie_Need_WorldBank)+
       NAge25B	*(Settings$	KCaloryNeed_B25	/Calorie_Need_WorldBank)+
       NAge26B	*(Settings$	KCaloryNeed_B26	/Calorie_Need_WorldBank)+
       NAge27B	*(Settings$	KCaloryNeed_B27	/Calorie_Need_WorldBank)+
       NAge28B	*(Settings$	KCaloryNeed_B28	/Calorie_Need_WorldBank)+
       NAge29B	*(Settings$	KCaloryNeed_B29	/Calorie_Need_WorldBank)+
       NAge30B	*(Settings$	KCaloryNeed_B30	/Calorie_Need_WorldBank)+
       NAge31B	*(Settings$	KCaloryNeed_B31	/Calorie_Need_WorldBank)+
       NAge32B	*(Settings$	KCaloryNeed_B32	/Calorie_Need_WorldBank)+
       NAge33B	*(Settings$	KCaloryNeed_B33	/Calorie_Need_WorldBank)+
       NAge34B	*(Settings$	KCaloryNeed_B34	/Calorie_Need_WorldBank)+
       NAge35B	*(Settings$	KCaloryNeed_B35	/Calorie_Need_WorldBank)+
       NAge36B	*(Settings$	KCaloryNeed_B36	/Calorie_Need_WorldBank)+
       NAge37B	*(Settings$	KCaloryNeed_B37	/Calorie_Need_WorldBank)+
       NAge38B	*(Settings$	KCaloryNeed_B38	/Calorie_Need_WorldBank)+
       NAge39B	*(Settings$	KCaloryNeed_B39	/Calorie_Need_WorldBank)+
       NAge40B	*(Settings$	KCaloryNeed_B40	/Calorie_Need_WorldBank)+
       NAge41B	*(Settings$	KCaloryNeed_B41	/Calorie_Need_WorldBank)+
       NAge42B	*(Settings$	KCaloryNeed_B42	/Calorie_Need_WorldBank)+
       NAge43B	*(Settings$	KCaloryNeed_B43	/Calorie_Need_WorldBank)+
       NAge44B	*(Settings$	KCaloryNeed_B44	/Calorie_Need_WorldBank)+
       NAge45B	*(Settings$	KCaloryNeed_B45	/Calorie_Need_WorldBank)+
       NAge46B	*(Settings$	KCaloryNeed_B46	/Calorie_Need_WorldBank)+
       NAge47B	*(Settings$	KCaloryNeed_B47	/Calorie_Need_WorldBank)+
       NAge48B	*(Settings$	KCaloryNeed_B48	/Calorie_Need_WorldBank)+
       NAge49B	*(Settings$	KCaloryNeed_B49	/Calorie_Need_WorldBank)+
       NAge50B	*(Settings$	KCaloryNeed_B50	/Calorie_Need_WorldBank)+
       NAge51B	*(Settings$	KCaloryNeed_B51	/Calorie_Need_WorldBank)+
       NAge52B	*(Settings$	KCaloryNeed_B52	/Calorie_Need_WorldBank)+
       NAge53B	*(Settings$	KCaloryNeed_B53	/Calorie_Need_WorldBank)+
       NAge54B	*(Settings$	KCaloryNeed_B54	/Calorie_Need_WorldBank)+
       NAge55B	*(Settings$	KCaloryNeed_B55	/Calorie_Need_WorldBank)+
       NAge56B	*(Settings$	KCaloryNeed_B56	/Calorie_Need_WorldBank)+
       NAge57B	*(Settings$	KCaloryNeed_B57	/Calorie_Need_WorldBank)+
       NAge58B	*(Settings$	KCaloryNeed_B58	/Calorie_Need_WorldBank)+
       NAge59B	*(Settings$	KCaloryNeed_B59	/Calorie_Need_WorldBank)+
       NAge60B	*(Settings$	KCaloryNeed_B60	/Calorie_Need_WorldBank)+
       NAge61B	*(Settings$	KCaloryNeed_B61	/Calorie_Need_WorldBank)+
       NAge62B	*(Settings$	KCaloryNeed_B62	/Calorie_Need_WorldBank)+
       NAge63B	*(Settings$	KCaloryNeed_B63	/Calorie_Need_WorldBank)+
       NAge64B	*(Settings$	KCaloryNeed_B64	/Calorie_Need_WorldBank)+
       NAge65B	*(Settings$	KCaloryNeed_B65	/Calorie_Need_WorldBank)+
       NAge66B	*(Settings$	KCaloryNeed_B66	/Calorie_Need_WorldBank)+
       NAge67B	*(Settings$	KCaloryNeed_B67	/Calorie_Need_WorldBank)+
       NAge68B	*(Settings$	KCaloryNeed_B68	/Calorie_Need_WorldBank)+
       NAge69B	*(Settings$	KCaloryNeed_B69	/Calorie_Need_WorldBank)+
       NAge70B	*(Settings$	KCaloryNeed_B70	/Calorie_Need_WorldBank)+
       NAge71B	*(Settings$	KCaloryNeed_B71	/Calorie_Need_WorldBank)+
       NAge72B	*(Settings$	KCaloryNeed_B72	/Calorie_Need_WorldBank)+
       NAge73B	*(Settings$	KCaloryNeed_B73	/Calorie_Need_WorldBank)+
       NAge74B	*(Settings$	KCaloryNeed_B74	/Calorie_Need_WorldBank)+
       NAge75B	*(Settings$	KCaloryNeed_B75	/Calorie_Need_WorldBank)+
       NAge76B	*(Settings$	KCaloryNeed_B76	/Calorie_Need_WorldBank)+
       NAge77B	*(Settings$	KCaloryNeed_B77	/Calorie_Need_WorldBank)+
       NAge78B	*(Settings$	KCaloryNeed_B78	/Calorie_Need_WorldBank)+
       NAge79B	*(Settings$	KCaloryNeed_B79	/Calorie_Need_WorldBank)+
       NAge80B	*(Settings$	KCaloryNeed_B80	/Calorie_Need_WorldBank)+
       NAge81B	*(Settings$	KCaloryNeed_B81	/Calorie_Need_WorldBank)+
       NAge82B	*(Settings$	KCaloryNeed_B82	/Calorie_Need_WorldBank)+
       NAge83B	*(Settings$	KCaloryNeed_B83	/Calorie_Need_WorldBank)+
       NAge84B	*(Settings$	KCaloryNeed_B84	/Calorie_Need_WorldBank)+
       NAge85B	*(Settings$	KCaloryNeed_B85	/Calorie_Need_WorldBank)+
       NAge86B	*(Settings$	KCaloryNeed_B86	/Calorie_Need_WorldBank)+
       NAge87B	*(Settings$	KCaloryNeed_B87	/Calorie_Need_WorldBank)+
       NAge88B	*(Settings$	KCaloryNeed_B88	/Calorie_Need_WorldBank)+
       NAge89B	*(Settings$	KCaloryNeed_B89	/Calorie_Need_WorldBank)+
       NAge90B	*(Settings$	KCaloryNeed_B90	/Calorie_Need_WorldBank)+
       NAge91B	*(Settings$	KCaloryNeed_B91	/Calorie_Need_WorldBank)+
       NAge92B	*(Settings$	KCaloryNeed_B92	/Calorie_Need_WorldBank)+
       NAge93B	*(Settings$	KCaloryNeed_B93	/Calorie_Need_WorldBank)+
       NAge94B	*(Settings$	KCaloryNeed_B94	/Calorie_Need_WorldBank)+
       NAge95B	*(Settings$	KCaloryNeed_B95	/Calorie_Need_WorldBank)+
       NAge96B	*(Settings$	KCaloryNeed_B96	/Calorie_Need_WorldBank)+
       NAge97B	*(Settings$	KCaloryNeed_B97	/Calorie_Need_WorldBank)+
       NAge98B	*(Settings$	KCaloryNeed_B98	/Calorie_Need_WorldBank)+
       NAge99B	*(Settings$	KCaloryNeed_B99	/Calorie_Need_WorldBank)+
       NAge0G	*(Settings$	KCaloryNeed_G0	/Calorie_Need_WorldBank)+
       NAge1G	*(Settings$	KCaloryNeed_G1	/Calorie_Need_WorldBank)+
       NAge2G	*(Settings$	KCaloryNeed_G2	/Calorie_Need_WorldBank)+
       NAge3G	*(Settings$	KCaloryNeed_G3	/Calorie_Need_WorldBank)+
       NAge4G	*(Settings$	KCaloryNeed_G4	/Calorie_Need_WorldBank)+
       NAge5G	*(Settings$	KCaloryNeed_G5	/Calorie_Need_WorldBank)+
       NAge6G	*(Settings$	KCaloryNeed_G6	/Calorie_Need_WorldBank)+
       NAge7G	*(Settings$	KCaloryNeed_G7	/Calorie_Need_WorldBank)+
       NAge8G	*(Settings$	KCaloryNeed_G8	/Calorie_Need_WorldBank)+
       NAge9G	*(Settings$	KCaloryNeed_G9	/Calorie_Need_WorldBank)+
       NAge10G	*(Settings$	KCaloryNeed_G10	/Calorie_Need_WorldBank)+
       NAge11G	*(Settings$	KCaloryNeed_G11	/Calorie_Need_WorldBank)+
       NAge12G	*(Settings$	KCaloryNeed_G12	/Calorie_Need_WorldBank)+
       NAge13G	*(Settings$	KCaloryNeed_G13	/Calorie_Need_WorldBank)+
       NAge14G	*(Settings$	KCaloryNeed_G14	/Calorie_Need_WorldBank)+
       NAge15G	*(Settings$	KCaloryNeed_G15	/Calorie_Need_WorldBank)+
       NAge16G	*(Settings$	KCaloryNeed_G16	/Calorie_Need_WorldBank)+
       NAge17G	*(Settings$	KCaloryNeed_G17	/Calorie_Need_WorldBank)+
       NAge18G	*(Settings$	KCaloryNeed_G18	/Calorie_Need_WorldBank)+
       NAge19G	*(Settings$	KCaloryNeed_G19	/Calorie_Need_WorldBank)+
       NAge20G	*(Settings$	KCaloryNeed_G20	/Calorie_Need_WorldBank)+
       NAge21G	*(Settings$	KCaloryNeed_G21	/Calorie_Need_WorldBank)+
       NAge22G	*(Settings$	KCaloryNeed_G22	/Calorie_Need_WorldBank)+
       NAge23G	*(Settings$	KCaloryNeed_G23	/Calorie_Need_WorldBank)+
       NAge24G	*(Settings$	KCaloryNeed_G24	/Calorie_Need_WorldBank)+
       NAge25G	*(Settings$	KCaloryNeed_G25	/Calorie_Need_WorldBank)+
       NAge26G	*(Settings$	KCaloryNeed_G26	/Calorie_Need_WorldBank)+
       NAge27G	*(Settings$	KCaloryNeed_G27	/Calorie_Need_WorldBank)+
       NAge28G	*(Settings$	KCaloryNeed_G28	/Calorie_Need_WorldBank)+
       NAge29G	*(Settings$	KCaloryNeed_G29	/Calorie_Need_WorldBank)+
       NAge30G	*(Settings$	KCaloryNeed_G30	/Calorie_Need_WorldBank)+
       NAge31G	*(Settings$	KCaloryNeed_G31	/Calorie_Need_WorldBank)+
       NAge32G	*(Settings$	KCaloryNeed_G32	/Calorie_Need_WorldBank)+
       NAge33G	*(Settings$	KCaloryNeed_G33	/Calorie_Need_WorldBank)+
       NAge34G	*(Settings$	KCaloryNeed_G34	/Calorie_Need_WorldBank)+
       NAge35G	*(Settings$	KCaloryNeed_G35	/Calorie_Need_WorldBank)+
       NAge36G	*(Settings$	KCaloryNeed_G36	/Calorie_Need_WorldBank)+
       NAge37G	*(Settings$	KCaloryNeed_G37	/Calorie_Need_WorldBank)+
       NAge38G	*(Settings$	KCaloryNeed_G38	/Calorie_Need_WorldBank)+
       NAge39G	*(Settings$	KCaloryNeed_G39	/Calorie_Need_WorldBank)+
       NAge40G	*(Settings$	KCaloryNeed_G40	/Calorie_Need_WorldBank)+
       NAge41G	*(Settings$	KCaloryNeed_G41	/Calorie_Need_WorldBank)+
       NAge42G	*(Settings$	KCaloryNeed_G42	/Calorie_Need_WorldBank)+
       NAge43G	*(Settings$	KCaloryNeed_G43	/Calorie_Need_WorldBank)+
       NAge44G	*(Settings$	KCaloryNeed_G44	/Calorie_Need_WorldBank)+
       NAge45G	*(Settings$	KCaloryNeed_G45	/Calorie_Need_WorldBank)+
       NAge46G	*(Settings$	KCaloryNeed_G46	/Calorie_Need_WorldBank)+
       NAge47G	*(Settings$	KCaloryNeed_G47	/Calorie_Need_WorldBank)+
       NAge48G	*(Settings$	KCaloryNeed_G48	/Calorie_Need_WorldBank)+
       NAge49G	*(Settings$	KCaloryNeed_G49	/Calorie_Need_WorldBank)+
       NAge50G	*(Settings$	KCaloryNeed_G50	/Calorie_Need_WorldBank)+
       NAge51G	*(Settings$	KCaloryNeed_G51	/Calorie_Need_WorldBank)+
       NAge52G	*(Settings$	KCaloryNeed_G52	/Calorie_Need_WorldBank)+
       NAge53G	*(Settings$	KCaloryNeed_G53	/Calorie_Need_WorldBank)+
       NAge54G	*(Settings$	KCaloryNeed_G54	/Calorie_Need_WorldBank)+
       NAge55G	*(Settings$	KCaloryNeed_G55	/Calorie_Need_WorldBank)+
       NAge56G	*(Settings$	KCaloryNeed_G56	/Calorie_Need_WorldBank)+
       NAge57G	*(Settings$	KCaloryNeed_G57	/Calorie_Need_WorldBank)+
       NAge58G	*(Settings$	KCaloryNeed_G58	/Calorie_Need_WorldBank)+
       NAge59G	*(Settings$	KCaloryNeed_G59	/Calorie_Need_WorldBank)+
       NAge60G	*(Settings$	KCaloryNeed_G60	/Calorie_Need_WorldBank)+
       NAge61G	*(Settings$	KCaloryNeed_G61	/Calorie_Need_WorldBank)+
       NAge62G	*(Settings$	KCaloryNeed_G62	/Calorie_Need_WorldBank)+
       NAge63G	*(Settings$	KCaloryNeed_G63	/Calorie_Need_WorldBank)+
       NAge64G	*(Settings$	KCaloryNeed_G64	/Calorie_Need_WorldBank)+
       NAge65G	*(Settings$	KCaloryNeed_G65	/Calorie_Need_WorldBank)+
       NAge66G	*(Settings$	KCaloryNeed_G66	/Calorie_Need_WorldBank)+
       NAge67G	*(Settings$	KCaloryNeed_G67	/Calorie_Need_WorldBank)+
       NAge68G	*(Settings$	KCaloryNeed_G68	/Calorie_Need_WorldBank)+
       NAge69G	*(Settings$	KCaloryNeed_G69	/Calorie_Need_WorldBank)+
       NAge70G	*(Settings$	KCaloryNeed_G70	/Calorie_Need_WorldBank)+
       NAge71G	*(Settings$	KCaloryNeed_G71	/Calorie_Need_WorldBank)+
       NAge72G	*(Settings$	KCaloryNeed_G72	/Calorie_Need_WorldBank)+
       NAge73G	*(Settings$	KCaloryNeed_G73	/Calorie_Need_WorldBank)+
       NAge74G	*(Settings$	KCaloryNeed_G74	/Calorie_Need_WorldBank)+
       NAge75G	*(Settings$	KCaloryNeed_G75	/Calorie_Need_WorldBank)+
       NAge76G	*(Settings$	KCaloryNeed_G76	/Calorie_Need_WorldBank)+
       NAge77G	*(Settings$	KCaloryNeed_G77	/Calorie_Need_WorldBank)+
       NAge78G	*(Settings$	KCaloryNeed_G78	/Calorie_Need_WorldBank)+
       NAge79G	*(Settings$	KCaloryNeed_G79	/Calorie_Need_WorldBank)+
       NAge80G	*(Settings$	KCaloryNeed_G80	/Calorie_Need_WorldBank)+
       NAge81G	*(Settings$	KCaloryNeed_G81	/Calorie_Need_WorldBank)+
       NAge82G	*(Settings$	KCaloryNeed_G82	/Calorie_Need_WorldBank)+
       NAge83G	*(Settings$	KCaloryNeed_G83	/Calorie_Need_WorldBank)+
       NAge84G	*(Settings$	KCaloryNeed_G84	/Calorie_Need_WorldBank)+
       NAge85G	*(Settings$	KCaloryNeed_G85	/Calorie_Need_WorldBank)+
       NAge86G	*(Settings$	KCaloryNeed_G86	/Calorie_Need_WorldBank)+
       NAge87G	*(Settings$	KCaloryNeed_G87	/Calorie_Need_WorldBank)+
       NAge88G	*(Settings$	KCaloryNeed_G88	/Calorie_Need_WorldBank)+
       NAge89G	*(Settings$	KCaloryNeed_G89	/Calorie_Need_WorldBank)+
       NAge90G	*(Settings$	KCaloryNeed_G90	/Calorie_Need_WorldBank)+
       NAge91G	*(Settings$	KCaloryNeed_G91	/Calorie_Need_WorldBank)+
       NAge92G	*(Settings$	KCaloryNeed_G92	/Calorie_Need_WorldBank)+
       NAge93G	*(Settings$	KCaloryNeed_G93	/Calorie_Need_WorldBank)+
       NAge94G	*(Settings$	KCaloryNeed_G94	/Calorie_Need_WorldBank)+
       NAge95G	*(Settings$	KCaloryNeed_G95	/Calorie_Need_WorldBank)+
       NAge96G	*(Settings$	KCaloryNeed_G96	/Calorie_Need_WorldBank)+
       NAge97G	*(Settings$	KCaloryNeed_G97	/Calorie_Need_WorldBank)+
       NAge98G	*(Settings$	KCaloryNeed_G98	/Calorie_Need_WorldBank)+
       NAge99G	*(Settings$	KCaloryNeed_G99	/Calorie_Need_WorldBank)+
       lactating*(Settings$KCaloryNeed_lactating/Calorie_Need_WorldBank)]
  
  MD[,EqSizeCalory2 :=
       NAge1_A_B*(Settings$KCaloryNeed_A_B1/Calorie_Need_Anstitoo) +
       NAge2_A_B*(Settings$KCaloryNeed_A_B2/Calorie_Need_Anstitoo) +
       NAge3_A_B*(Settings$KCaloryNeed_A_B3/Calorie_Need_Anstitoo) +
       NAge4_A_B*(Settings$KCaloryNeed_A_B4/Calorie_Need_Anstitoo) +
       NAge5_A_B*(Settings$KCaloryNeed_A_B5/Calorie_Need_Anstitoo) +
       NAge6_A_B*(Settings$KCaloryNeed_A_B6/Calorie_Need_Anstitoo) +
       NAge7_A_B*(Settings$KCaloryNeed_A_B7/Calorie_Need_Anstitoo) +
       NAge8_A_B*(Settings$KCaloryNeed_A_B8/Calorie_Need_Anstitoo) +
       NAge9_A_B*(Settings$KCaloryNeed_A_B9/Calorie_Need_Anstitoo) +
       NAge1_A_G*(Settings$KCaloryNeed_A_G1/Calorie_Need_Anstitoo) +
       NAge2_A_G*(Settings$KCaloryNeed_A_G2/Calorie_Need_Anstitoo) +
       NAge3_A_G*(Settings$KCaloryNeed_A_G3/Calorie_Need_Anstitoo) +
       NAge4_A_G*(Settings$KCaloryNeed_A_G4/Calorie_Need_Anstitoo) +
       NAge5_A_G*(Settings$KCaloryNeed_A_G5/Calorie_Need_Anstitoo) +
       NAge6_A_G*(Settings$KCaloryNeed_A_G6/Calorie_Need_Anstitoo) +
       NAge7_A_G*(Settings$KCaloryNeed_A_G7/Calorie_Need_Anstitoo) +
       NAge8_A_G*(Settings$KCaloryNeed_A_G8/Calorie_Need_Anstitoo) +
       NAge9_A_G*(Settings$KCaloryNeed_A_G9/Calorie_Need_Anstitoo)+
       lactating*(Settings$KCaloryNeed_lactating/Calorie_Need_Anstitoo)]
  
  MD[,EqSizeCalory4 :=
       NAge1B*(Settings$KCaloryNeed_B1/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge2B*(Settings$KCaloryNeed_B2/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge3B*(Settings$KCaloryNeed_B3/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge4B*(Settings$KCaloryNeed_B4/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge5B*(Settings$KCaloryNeed_B5/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge6B*(Settings$KCaloryNeed_B6/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge7B*(Settings$KCaloryNeed_B7/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge8B*(Settings$KCaloryNeed_B8/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge9B*(Settings$KCaloryNeed_B9/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge10B*(Settings$KCaloryNeed_B10/Settings$KCaloryNeed_Adult_WorldBank)+
       NAge1G*(Settings$KCaloryNeed_G1/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge2G*(Settings$KCaloryNeed_G2/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge3G*(Settings$KCaloryNeed_G3/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge4G*(Settings$KCaloryNeed_G4/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge5G*(Settings$KCaloryNeed_G5/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge6G*(Settings$KCaloryNeed_G6/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge7G*(Settings$KCaloryNeed_G7/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge8G*(Settings$KCaloryNeed_G8/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge9G*(Settings$KCaloryNeed_G9/Settings$KCaloryNeed_Adult_WorldBank) +
       NAge10G*(Settings$KCaloryNeed_G10/Settings$KCaloryNeed_Adult_WorldBank)+
       lactating*(Settings$KCaloryNeed_lactating/Settings$KCaloryNeed_Adult_WorldBank)]
  
  MD[,EqSizeCalory5 :=
       NAge1_A_B*(Settings$KCaloryNeed_A_B1/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge2_A_B*(Settings$KCaloryNeed_A_B2/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge3_A_B*(Settings$KCaloryNeed_A_B3/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge4_A_B*(Settings$KCaloryNeed_A_B4/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge5_A_B*(Settings$KCaloryNeed_A_B5/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge6_A_B*(Settings$KCaloryNeed_A_B6/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge7_A_B*(Settings$KCaloryNeed_A_B7/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge8_A_B*(Settings$KCaloryNeed_A_B8/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge9_A_B*(Settings$KCaloryNeed_A_B9/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge1_A_G*(Settings$KCaloryNeed_A_G1/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge2_A_G*(Settings$KCaloryNeed_A_G2/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge3_A_G*(Settings$KCaloryNeed_A_G3/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge4_A_G*(Settings$KCaloryNeed_A_G4/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge5_A_G*(Settings$KCaloryNeed_A_G5/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge6_A_G*(Settings$KCaloryNeed_A_G6/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge7_A_G*(Settings$KCaloryNeed_A_G7/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge8_A_G*(Settings$KCaloryNeed_A_G8/Settings$KCaloryNeed_Adult_Anstitoo) +
       NAge9_A_G*(Settings$KCaloryNeed_A_G9/Settings$KCaloryNeed_Adult_Anstitoo)+
       lactating*(Settings$KCaloryNeed_lactating/Settings$KCaloryNeed_Adult_Anstitoo)]
  
  save(MD, file=paste0(Settings$HEISProcessedPath,"Y",year,"Merged4CBN2.rda"))

}



endtime <- proc.time()
cat("\n\n============================\nIt took ")
cat(endtime-starttime)