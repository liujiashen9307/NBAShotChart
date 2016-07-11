library(sqldf)
library(plyr)
data14<-read.csv("shotdata14.csv")
data15<-read.csv("shotdata15.csv")
Distance14_All<-sqldf("SELECT PLAYER_NAME, SHOT_DISTANCE,PERIOD , count(SHOT_DISTANCE) as Shot_Quantity from data14 group by SHOT_DISTANCE, PLAYER_NAME,PERIOD")
Distance15_All<-sqldf("SELECT PLAYER_NAME,SHOT_DISTANCE,PERIOD , count(SHOT_DISTANCE) as Shot_Quantity from data15 group by SHOT_DISTANCE, PLAYER_NAME,PERIOD")
Distance14_Made<-sqldf("SELECT PLAYER_NAME, SHOT_DISTANCE,PERIOD , count(SHOT_DISTANCE) as Made_Quantity from data14 WHERE EVENT_TYPE is 'Made Shot' group by SHOT_DISTANCE, PLAYER_NAME,PERIOD")
Distance15_Made<-sqldf("SELECT PLAYER_NAME, SHOT_DISTANCE,PERIOD , count(SHOT_DISTANCE) as Made_Quantity from data15 WHERE EVENT_TYPE is 'Made Shot' group by SHOT_DISTANCE, PLAYER_NAME,PERIOD")
Distance14<-join(Distance14_All,Distance14_Made,type="full")
Distance15<-join(Distance15_All,Distance15_Made,type="full")
###Minute
Minute14_All<-sqldf("SELECT PLAYER_NAME, MINUTES_REMAINING ,PERIOD, count(MINUTES_REMAINING) as Shot_Quantity from data14 group by MINUTES_REMAINING, PLAYER_NAME,PERIOD")
Minute15_All<-sqldf("SELECT PLAYER_NAME, MINUTES_REMAINING , PERIOD,count(MINUTES_REMAINING) as Shot_Quantity from data15 group by MINUTES_REMAINING, PLAYER_NAME,PERIOD")
Minute14_Made<-sqldf("SELECT PLAYER_NAME, MINUTES_REMAINING ,PERIOD, count(MINUTES_REMAINING) as Made_Quantity from data14 WHERE EVENT_TYPE is 'Made Shot' group by MINUTES_REMAINING, PLAYER_NAME,PERIOD")
Minute15_Made<-sqldf("SELECT PLAYER_NAME, MINUTES_REMAINING ,PERIOD, count(MINUTES_REMAINING) as Made_Quantity from data15 WHERE EVENT_TYPE is 'Made Shot' group by MINUTES_REMAINING, PLAYER_NAME,PERIOD")
Minute14<-join(Minute14_All,Minute14_Made,type="full")
Minute15<-join(Minute15_All,Minute15_Made,type="full")

########Dealing NA####
for(i in (1:33919)){
  if(is.na(Distance14$Made_Quantity[i])){
    Distance14$Made_Quantity[i]<-0
  }
}
for(i in (1:32739)){
  if(is.na(Distance15$Made_Quantity[i])){
    Distance15$Made_Quantity[i]<-0
  }
}
for(i in (1:18000)){
  if(is.na(Minute14$Made_Quantity[i])){
    Minute14$Made_Quantity[i]<-0
  }
}
for(i in (1:17230)){
  if(is.na(Minute15$Made_Quantity[i])){
    Minute15$Made_Quantity[i]<-0
  }
}

#####Percentage####

Distance14<-data.frame(Distance14,Percentage=Distance14$Made_Quantity/Distance14$Shot_Quantity)
Distance15<-data.frame(Distance15,Percentage=Distance15$Made_Quantity/Distance15$Shot_Quantity)
Minute14<-data.frame(Minute14,Percentage=Minute14$Made_Quantity/Minute14$Shot_Quantity)
Minute15<-data.frame(Minute15,Percentage=Minute15$Made_Quantity/Minute15$Shot_Quantity)
#####AVERAGE###
temp1<-sqldf("SELECT SHOT_DISTANCE,PERIOD,COUNT(SHOT_DISTANCE) as Average_Shot_Quantity from data14 group by SHOT_DISTANCE,PERIOD")
temp2<-sqldf("SELECT SHOT_DISTANCE,PERIOD,COUNT(SHOT_DISTANCE) as Average_Made_Quantity from data14 where EVENT_TYPE is 'Made Shot' group by SHOT_DISTANCE,PERIOD")
Distance_Ave_14<-join(temp1,temp2)
Distance_Ave_14<-data.frame(Distance_Ave_14,Average_Percentage=Distance_Ave_14$Average_Made_Quantity/Distance_Ave_14$Average_Shot_Quantity)
temp1<-sqldf("SELECT SHOT_DISTANCE,PERIOD,COUNT(SHOT_DISTANCE) as Average_Shot_Quantity from data15 group by SHOT_DISTANCE,PERIOD")
temp2<-sqldf("SELECT SHOT_DISTANCE,PERIOD,COUNT(SHOT_DISTANCE) as Average_Made_Quantity from data15 where EVENT_TYPE is 'Made Shot' group by SHOT_DISTANCE,PERIOD")
Distance_Ave_15<-join(temp1,temp2)
Distance_Ave_15<-data.frame(Distance_Ave_15,Average_Percentage=Distance_Ave_15$Average_Made_Quantity/Distance_Ave_15$Average_Shot_Quantity)
temp1<-sqldf("SELECT MINUTES_REMAINING,PERIOD,COUNT(MINUTES_REMAINING) as Average_Shot_Quantity from data14 group by MINUTES_REMAINING,PERIOD")
temp2<-sqldf("SELECT MINUTES_REMAINING,PERIOD,COUNT(MINUTES_REMAINING) as Average_Made_Quantity from data14 where EVENT_TYPE is 'Made Shot' group by MINUTES_REMAINING,PERIOD")
Minute_Ave_14<-join(temp1,temp2)
Minute_Ave_14<-data.frame(Minute_Ave_14,Average_Percentage=Minute_Ave_14$Average_Made_Quantity/Minute_Ave_14$Average_Shot_Quantity)
temp1<-sqldf("SELECT MINUTES_REMAINING,PERIOD,COUNT(MINUTES_REMAINING) as Average_Shot_Quantity from data15 group by MINUTES_REMAINING,PERIOD")
temp2<-sqldf("SELECT MINUTES_REMAINING,PERIOD,COUNT(MINUTES_REMAINING) as Average_Made_Quantity from data15 where EVENT_TYPE is 'Made Shot' group by MINUTES_REMAINING,PERIOD")
Minute_Ave_15<-join(temp1,temp2)
Minute_Ave_15<-data.frame(Minute_Ave_15,Average_Percentage=Minute_Ave_15$Average_Made_Quantity/Minute_Ave_15$Average_Shot_Quantity)
Distance_Ave_14$Average_Shot_Quantity<-Distance_Ave_14$Average_Shot_Quantity/390;Distance_Ave_14$Average_Made_Quantity<-Distance_Ave_14$Average_Made_Quantity/390
Distance_Ave_15$Average_Shot_Quantity<-Distance_Ave_15$Average_Shot_Quantity/390;Distance_Ave_15$Average_Made_Quantity<-Distance_Ave_15$Average_Made_Quantity/390
Minute_Ave_14$Average_Shot_Quantity<-Minute_Ave_14$Average_Shot_Quantity/390;Minute_Ave_14$Average_Made_Quantity<-Minute_Ave_14$Average_Made_Quantity/390
Minute_Ave_15$Average_Shot_Quantity<-Minute_Ave_15$Average_Shot_Quantity/390;Minute_Ave_15$Average_Made_Quantity<-Minute_Ave_15$Average_Made_Quantity/390
ccc<-join(Minute14[Minute14$PLAYER_NAME=="Kobe Bryant",],Minute_Ave_14)

write.csv(Distance_Ave_14,"DAV14.csv")
write.csv(Distance_Ave_15,"DAV15.csv")
write.csv(Minute_Ave_14,"MAV14.csv")
write.csv(Minute_Ave_15,"MAV15.csv")


write.csv(Distance14,"Distance1.csv")
write.csv(Distance15,"Distance2.csv")
write.csv(Minute14,"Minute1.csv")
write.csv(Minute15,"Minute2.csv")
