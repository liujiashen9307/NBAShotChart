data14<-read.csv("shotdata14.csv")
data15<-read.csv("shotdata15.csv")
Distance14<-read.csv("Distance1.csv")[,-1]
Distance14<-Distance14[Distance14$SHOT_DISTANCE<=30,]
Distance15<-read.csv("Distance2.csv")[,-1]
Distance15<-Distance15[Distance15$SHOT_DISTANCE<=30,]
Minute14<-read.csv("Minute1.csv")[,-1]
Minute14<-Minute14[Minute14$MINUTES_REMAINING<12,]
Minute15<-read.csv("Minute2.csv")[,-1]
Minute15<-Minute15[Minute15$MINUTES_REMAINING<12,]
DAVE14<-read.csv("DAV14.csv")[,-1]
DAVE15<-read.csv("DAV15.csv")[,-1]
DAVE14<-DAVE14[DAVE14$SHOT_DISTANCE<=30,]
DAVE15<-DAVE15[DAVE15$SHOT_DISTANCE<=30,]
MAVE14<-read.csv("MAV14.csv")[,-1]
MAVE14<-MAVE14[MAVE14$MINUTES_REMAINING<12,]
MAVE15<-read.csv("MAV15.csv")[,-1]
MAVE15<-MAVE15[MAVE15$MINUTES_REMAINING<12,]
Type14<-read.csv("Type_14.csv")[,-1]
Type15<-read.csv("Type_15.csv")[,-1]
Type14_Quart<-read.csv("Type_14_In_Quarter.csv")[,-1]
Type15_Quart<-read.csv("Type_15_In_Quarter.csv")[,-1]
TypeDistance_14<-read.csv("DistanceType14.csv")[,-1]
TypeDistance_15<-read.csv("DistanceType15.csv")[,-1]
TypeDistance_14_spe<-read.csv("DistanceType14Spec.csv")[,-1]
TypeDistance_15_spe<-read.csv("DistanceType15Spec.csv")[,-1]
library(shiny)
library(ggplot2)
library(hexbin)
library(sqldf)
library(grid)
library(jpeg)
library(rjson)
library(request)
library(RCurl)
library(plyr)
library(shinythemes)
library(plotly)
######Court Image####
courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
                    width=unit(1,"npc"), height=unit(1,"npc"))
#######Query Conducted for Essential Data####
###Distance
shinyServer(function(input,output){
  Type_Distance<-reactive({
    if(input$seat==1){
      if(input$pnametype=="All"){
        TypeDistance_14[TypeDistance_14$SHOT_DISTANCE<=30,]
      }else{
        TypeDistance_14_spe[TypeDistance_14$SHOT_DISTANCE<=30&TypeDistance_14_spe$PLAYER_NAME==input$pnametype,]
      }
    }else{
      
      if(input$pnametype=="All"){
        TypeDistance_15[TypeDistance_15$SHOT_DISTANCE<=30,]
      }else{
        TypeDistance_15_spe[TypeDistance_15$SHOT_DISTANCE<=30&TypeDistance_15_spe$PLAYER_NAME==input$pnametype,]
      }
      
    }
  })
  
  
  output$plot24<-renderPlotly({
    ggplotly(ggplot(Type_Distance(),aes(x=SHOT_DISTANCE,y=Shot_Quantity,fill=Type))+geom_bar(position="stack",stat = "identity")+theme(legend.position="none")+theme_bw()+ggtitle("Shot Type Choice By Distance"))
  })
  output$downloadType3<-downloadHandler(
    
    filename = function() { 
      paste(input$pnametype,'.csv', sep='') 
    },
    content = function(file) {
      write.csv(Type_Distance(), file)
    }
  )
  data1<-reactive({
    if(input$QT==1){
      data14
    }else{
      data14[data14$PERIOD==as.numeric(input$QT)-1,]
    }
  })
  
  data2<-reactive({
    if(input$QT==1){
      data15
    }else{
      data15[data15$PERIOD==as.numeric(input$QT)-1,]
    }
  })
  Distance1<-reactive({
    if(input$QT!=1){
    temp1<-Distance14[Distance14$PLAYER_NAME==input$name&Distance14$PERIOD==as.numeric(input$QT)-1,]
    Sum_Shot_Quantity<-sum(temp1$Shot_Quantity)
    Shot_Percentage<-temp1$Shot_Quantity/Sum_Shot_Quantity
    temp1<-data.frame(temp1,Shot_Percentage)
    temp2<-DAVE14[DAVE14$PERIOD==as.numeric(input$QT)-1,]
    Average_Shot_Percentage<-temp2$Average_Shot_Quantity/sum(temp2$Average_Shot_Quantity)
    temp2<-data.frame(temp2,Average_Shot_Percentage)
    temp<-join(temp2,temp1)
    }else{
      temp1<-Distance14[Distance14$PLAYER_NAME==input$name,]
      temp2<-DAVE14
      temp<-join(temp2,temp1)
      temp<-sqldf("SELECT SHOT_DISTANCE,sum(Shot_Quantity) as Shot_Quantity, sum(Made_Quantity) as Made_Quantity, sum(Average_Shot_Quantity) as Average_Shot_Quantity, sum(Average_Made_Quantity) as Average_Made_Quantity from temp group by SHOT_DISTANCE ")
      Sum_Shot_Quantity<-sum(temp$Shot_Quantity,na.rm = TRUE)
      Shot_Percentage<-temp$Shot_Quantity/Sum_Shot_Quantity
      Average_Shot_Percentage<-temp$Average_Shot_Quantity/sum(temp$Average_Shot_Quantity)
      temp<-data.frame(temp,Shot_Percentage,Average_Shot_Percentage, Percentage=temp$Made_Quantity/temp$Shot_Quantity, Average_Percentage=temp$Average_Made_Quantity/temp$Average_Shot_Quantity)
    }
    temp<-data.frame(temp,Shot_Attempt_Difference=temp$Shot_Quantity-temp$Average_Shot_Quantity,Accuracy_Difference=temp$Percentage-temp$Average_Percentage,Shot_Percentage_Difference=Shot_Percentage-Average_Shot_Percentage)
    temp
  })
  Minute1<-reactive({
    if(input$QT!=1){
      temp1<-Minute14[Minute14$PLAYER_NAME==input$name&Minute14$PERIOD==as.numeric(input$QT)-1,]
      Sum_Shot_Quantity<-sum(temp1$Shot_Quantity)
      Shot_Percentage<-temp1$Shot_Quantity/Sum_Shot_Quantity
      temp1<-data.frame(temp1,Shot_Percentage)
      temp2<-MAVE14[MAVE14$PERIOD==as.numeric(input$QT)-1,]
      Average_Shot_Percentage<-temp2$Average_Shot_Quantity/sum(temp2$Average_Shot_Quantity)
      temp2<-data.frame(temp2,Average_Shot_Percentage)
      temp<-join(temp2,temp1)
    }else{
      temp1<-Minute14[Minute14$PLAYER_NAME==input$name,]
      temp2<-MAVE14
      temp<-join(temp2,temp1)
      temp<-sqldf("SELECT MINUTES_REMAINING,sum(Shot_Quantity) as Shot_Quantity, sum(Made_Quantity) as Made_Quantity, sum(Average_Shot_Quantity) as Average_Shot_Quantity, sum(Average_Made_Quantity) as Average_Made_Quantity from temp group by MINUTES_REMAINING ")
      Sum_Shot_Quantity<-sum(temp$Shot_Quantity)
      Shot_Percentage<-temp$Shot_Quantity/Sum_Shot_Quantity
      Average_Shot_Percentage<-temp$Average_Shot_Quantity/sum(temp$Average_Shot_Quantity)
      temp<-data.frame(temp,Shot_Percentage,Average_Shot_Percentage, Percentage=temp$Made_Quantity/temp$Shot_Quantity, Average_Percentage=temp$Average_Made_Quantity/temp$Average_Shot_Quantity)
    }
    temp<-data.frame(temp,Shot_Attempt_Difference=temp$Shot_Quantity-temp$Average_Shot_Quantity,Accuracy_Difference=temp$Percentage-temp$Average_Percentage,Shot_Percentage_Difference=Shot_Percentage-Average_Shot_Percentage)
    temp
  })
  Distance2<-reactive({
    if(input$QT!=1){
      temp1<-Distance15[Distance15$PLAYER_NAME==input$name&Distance15$PERIOD==as.numeric(input$QT)-1,]
      Sum_Shot_Quantity<-sum(temp1$Shot_Quantity)
      Shot_Percentage<-temp1$Shot_Quantity/Sum_Shot_Quantity
      temp1<-data.frame(temp1,Shot_Percentage)
      temp2<-DAVE15[DAVE15$PERIOD==as.numeric(input$QT)-1,]
      Average_Shot_Percentage<-temp2$Average_Shot_Quantity/sum(temp2$Average_Shot_Quantity)
      temp2<-data.frame(temp2,Average_Shot_Percentage)
      temp<-join(temp2,temp1)
      }else{
        temp1<-Distance15[Distance15$PLAYER_NAME==input$name,]
        temp2<-DAVE15
        temp<-join(temp2,temp1)
        temp<-sqldf("SELECT SHOT_DISTANCE,sum(Shot_Quantity) as Shot_Quantity, sum(Made_Quantity) as Made_Quantity, sum(Average_Shot_Quantity) as Average_Shot_Quantity, sum(Average_Made_Quantity) as Average_Made_Quantity from temp group by SHOT_DISTANCE ")
        Sum_Shot_Quantity<-sum(temp$Shot_Quantity,na.rm = TRUE)
        Shot_Percentage<-temp$Shot_Quantity/Sum_Shot_Quantity
        Average_Shot_Percentage<-temp$Average_Shot_Quantity/sum(temp$Average_Shot_Quantity)
        temp<-data.frame(temp,Shot_Percentage,Average_Shot_Percentage, Percentage=temp$Made_Quantity/temp$Shot_Quantity, Average_Percentage=temp$Average_Made_Quantity/temp$Average_Shot_Quantity)
      }
    temp<-data.frame(temp,Shot_Attempt_Difference=temp$Shot_Quantity-temp$Average_Shot_Quantity,Accuracy_Difference=temp$Percentage-temp$Average_Percentage,Shot_Percentage_Difference=Shot_Percentage-Average_Shot_Percentage)
    temp
  })
  Minute2<-reactive({
    if(input$QT!=1){
      temp1<-Minute15[Minute15$PLAYER_NAME==input$name&Minute15$PERIOD==as.numeric(input$QT)-1,]
      Sum_Shot_Quantity<-sum(temp1$Shot_Quantity)
      Shot_Percentage<-temp1$Shot_Quantity/Sum_Shot_Quantity
      temp1<-data.frame(temp1,Shot_Percentage)
      temp2<-MAVE15[MAVE15$PERIOD==as.numeric(input$QT)-1,]
      Average_Shot_Percentage<-temp2$Average_Shot_Quantity/sum(temp2$Average_Shot_Quantity)
      temp2<-data.frame(temp2,Average_Shot_Percentage)
      temp<-join(temp2,temp1)
    }else{
      temp1<-Minute15[Minute15$PLAYER_NAME==input$name,]
      temp2<-MAVE15
      temp<-join(temp2,temp1)
      temp<-sqldf("SELECT MINUTES_REMAINING,sum(Shot_Quantity) as Shot_Quantity, sum(Made_Quantity) as Made_Quantity, sum(Average_Shot_Quantity) as Average_Shot_Quantity, sum(Average_Made_Quantity) as Average_Made_Quantity from temp group by MINUTES_REMAINING ")
      Sum_Shot_Quantity<-sum(temp$Shot_Quantity)
      Shot_Percentage<-temp$Shot_Quantity/Sum_Shot_Quantity
      Average_Shot_Percentage<-temp$Average_Shot_Quantity/sum(temp$Average_Shot_Quantity)
      temp<-data.frame(temp,Shot_Percentage,Average_Shot_Percentage, Percentage=temp$Made_Quantity/temp$Shot_Quantity, Average_Percentage=temp$Average_Made_Quantity/temp$Average_Shot_Quantity)
    }
    temp<-data.frame(temp,Shot_Attempt_Difference=temp$Shot_Quantity-temp$Average_Shot_Quantity,Accuracy_Difference=temp$Percentage-temp$Average_Percentage,Shot_Percentage_Difference=Shot_Percentage-Average_Shot_Percentage)
    temp
  })
  Type1<-reactive({
      if(input$QT==1){
        data<-Type14[Type14$PLAYER_NAME==input$name,]
      }else{
        data<-Type14_Quart[Type14_Quart$PLAYER_NAME==input$name&Type14_Quart$PERIOD==as.numeric(input$QT)-1,]
      }
      Missed_Shot<-data$Shot_Quantity-data$Made_Quantity
      data<-data.frame(data,Missed_Shot)
      data
    })
  Type1a<-reactive({
      if(input$QT==1){
        data<-Type15[Type15$PLAYER_NAME==input$name,]
      }else{
        data<-Type15_Quart[Type15_Quart$PLAYER_NAME==input$name&Type15_Quart$PERIOD==as.numeric(input$QT)-1,]
      }
      Missed_Shot<-data$Shot_Quantity-data$Made_Quantity
      data<-data.frame(data,Missed_Shot)
      data
    })
  Type2<-reactive({
      if(input$QT2==1){
        data<-Type14[Type14$PLAYER_NAME==input$nameA,]
      }else{
        data<-Type14_Quart[Type14_Quart$PLAYER_NAME==input$nameA&Type14_Quart$PERIOD==as.numeric(input$QT2)-1,]
      }
      Missed_Shot<-data$Shot_Quantity-data$Made_Quantity
      data<-data.frame(data,Missed_Shot)
      data
    })
  Type2a<-reactive({
      if(input$QT2==1){
        data<-Type15[Type15$PLAYER_NAME==input$name,]
      }else{
        data<-Type15_Quart[Type15_Quart$PLAYER_NAME==input$nameA&Type15_Quart$PERIOD==as.numeric(input$QT2)-1,]
      }
      Missed_Shot<-data$Shot_Quantity-data$Made_Quantity
      data<-data.frame(data,Missed_Shot)
      data
    })
  
  Type3<-reactive({
      if(input$QT2==1){
        data<-Type14[Type14$PLAYER_NAME==input$nameB,]
      }else{
        data<-Type14_Quart[Type14_Quart$PLAYER_NAME==input$nameB&Type14_Quart$PERIOD==as.numeric(input$QT2)-1,]
      }
      Missed_Shot<-data$Shot_Quantity-data$Made_Quantity
      data<-data.frame(data,Missed_Shot)
      data
    })
  Type3a<-reactive({
      if(input$QT2==1){
        data<-Type15[Type15$PLAYER_NAME==input$nameB,]
      }else{
        data<-Type15_Quart[Type15_Quart$PLAYER_NAME==input$nameB&Type15_Quart$PERIOD==as.numeric(input$QT2)-1,]
      }
      Missed_Shot<-data$Shot_Quantity-data$Made_Quantity
      data<-data.frame(data,Missed_Shot)
      data
    })
  
  output$plot10a<-renderPlotly({
    if(input$sea1==1){
    p<-plot_ly(x=Type1()$Type,y=Type1()$Made_Quantity,name="Made Shot",type="bar")
    p2<-add_trace(p,x=Type1()$Type,y=Type1()$Missed_Shot,name="Missed Shot",type="bar")
    p3 <- layout(p2, barmode = "stack")
    p3%>%layout(title="# Shot By Shot Types ")}else{
      
      p<-plot_ly(x=Type1a()$Type,y=Type1a()$Made_Quantity,name="Made Shot",type="bar")
      p2<-add_trace(p,x=Type1a()$Type,y=Type1a()$Missed_Shot,name="Missed Shot",type="bar")
      p3 <- layout(p2, barmode = "stack")
      p3%>%layout(title="# Shot By Shot Types ") 
    }
  })
  output$plot11a<-renderPlotly({
    if(input$sea1==1){
    plot_ly(Type1(), labels = Type1()$Type, values =Type1()$Shot_Quantity, type = "pie", hole = 0.6, showlegend = F) %>%
      layout(title = "Preference of Shooting Types")}else{
        
        plot_ly(Type1a(), labels = Type1a()$Type, values =Type1a()$Shot_Quantity, type = "pie", hole = 0.6, showlegend = F) %>%
          layout(title = "Preference of Shooting Types")
      }
  })
  output$plot16<-renderPlotly({
    if(input$sea==1){
    p<-plot_ly(x=Type2()$Type,y=Type2()$Made_Quantity,name="Made Shot",type="bar")
    p2<-add_trace(p,x=Type2()$Type,y=Type2()$Missed_Shot,name="Missed Shot",type="bar")
    p3 <- layout(p2, barmode = "stack")
    p3%>%layout(title="# Shot By Shot Types ")}else{
      
      p<-plot_ly(x=Type2a()$Type,y=Type2a()$Made_Quantity,name="Made Shot",type="bar")
      p2<-add_trace(p,x=Type2a()$Type,y=Type2a()$Missed_Shot,name="Missed Shot",type="bar")
      p3 <- layout(p2, barmode = "stack")
      p3%>%layout(title="# Shot By Shot Types ")
    }
      
  })
  output$plot17<-renderPlotly({
    if(input$sea==1){
    p<-plot_ly(x=Type3()$Type,y=Type3()$Made_Quantity,name="Made Shot",type="bar")
    p2<-add_trace(p,x=Type3()$Type,y=Type3()$Missed_Shot,name="Missed Shot",type="bar")
    p3 <- layout(p2, barmode = "stack")
    p3%>%layout(title="# Shot By Shot Types ")}else{
      
      p<-plot_ly(x=Type3a()$Type,y=Type3a()$Made_Quantity,name="Made Shot",type="bar")
      p2<-add_trace(p,x=Type3a()$Type,y=Type3a()$Missed_Shot,name="Missed Shot",type="bar")
      p3 <- layout(p2, barmode = "stack")
      p3%>%layout(title="# Shot By Shot Types ")
    }
  })
  output$plot18<-renderPlotly({
    if(input$sea==1){
    plot_ly(Type2(), labels = Type2()$Type, values =Type2()$Shot_Quantity, type = "pie", hole = 0.6, showlegend = F) %>%
      layout(title = "Preference of Shooting Types")}else{
        
        plot_ly(Type2a(), labels = Type2a()$Type, values =Type2a()$Shot_Quantity, type = "pie", hole = 0.6, showlegend = F) %>%
          layout(title = "Preference of Shooting Types")
      }
    
  })
  output$plot19<-renderPlotly({
    if(input$sea==1){
    plot_ly(Type3(), labels = Type3()$Type, values =Type3()$Shot_Quantity, type = "pie", hole = 0.6, showlegend = F) %>%
      layout(title = "Preference of Shooting Types")}else{
        
        plot_ly(Type3a(), labels = Type3a()$Type, values =Type3a()$Shot_Quantity, type = "pie", hole = 0.6, showlegend = F) %>%
          layout(title = "Preference of Shooting Types")
      }
    
  })
  output$plot1<-renderPlot({
    if(input$sea1==1){
          if(input$ct==1){
            if(input$mdo!=T){
          ggplot(data1()[data1()$PLAYER_NAME==input$name,], aes(x=LOC_X, y=LOC_Y)) + 
          annotation_custom(court, -250, 250, -50, 420) +
          geom_point(aes(colour = EVENT_TYPE)) +
          xlim(-250, 250) +
          ylim(-50, 420)+theme_bw()+ggtitle(paste(input$name,"'s shoting chart of 2014-15 season",sep=" "))+xlab("")+ylab("")
         }else{
           
           ggplot(data1()[data1()$PLAYER_NAME==input$name&data1()$EVENT_TYPE=="Made Shot",], aes(x=LOC_X, y=LOC_Y)) + 
             annotation_custom(court, -250, 250, -50, 420) +
             geom_point(aes(colour = EVENT_TYPE)) +
             xlim(-250, 250) +
             ylim(-50, 420)+theme_bw()+ggtitle(paste(input$name,"'s shoting chart of 2014-15 season",sep=" "))+xlab("")+ylab("")
           
         } }else{
            ggplot(data1()[data1()$PLAYER_NAME==input$name,], aes(x=LOC_X, y=LOC_Y)) + 
              annotation_custom(court, -250, 250, -52, 418) +
              stat_binhex(bins = 25, colour = "gray", alpha = 0.7) +
              scale_fill_gradientn(colours = c("yellow","orange","red")) +
              guides(alpha = FALSE, size = FALSE) +
              xlim(250, -250) +
              ylim(-52, 418) +
              geom_rug(alpha = 0.2) +
              coord_fixed() +
              
              theme(line = element_blank(),
                    axis.title.x = element_blank(),
                    axis.title.y = element_blank(),
                    axis.text.x = element_blank(),
                    axis.text.y = element_blank(),
                    legend.title = element_blank()
                    )+theme_bw()+ggtitle(paste(input$name,"'s shoting chart of 2014-15 season",sep=" "))+xlab("")+ylab("")
         }}else{
           
           if(input$ct==1){
             if(input$mdo!=T){
               ggplot(data2()[data2()$PLAYER_NAME==input$name,], aes(x=LOC_X, y=LOC_Y)) + 
                 annotation_custom(court, -250, 250, -50, 420) +
                 geom_point(aes(colour = EVENT_TYPE)) +
                 xlim(-250, 250) +
                 ylim(-50, 420)+theme_bw()+ggtitle(paste(input$name,"'s shoting chart of 2015-16 season",sep=" "))+xlab("")+ylab("")}else{
                   ggplot(data2()[data2()$PLAYER_NAME==input$name&data2()$EVENT_TYPE=="Made Shot",], aes(x=LOC_X, y=LOC_Y)) + 
                     annotation_custom(court, -250, 250, -50, 420) +
                     geom_point(aes(colour = EVENT_TYPE)) +
                     xlim(-250, 250) +
                     ylim(-50, 420)+theme_bw()+ggtitle(paste(input$name,"'s shoting chart of 2015-16 season",sep=" "))+xlab("")+ylab("")
                 }
           }else{
             
             ggplot(data2()[data2()$PLAYER_NAME==input$name,], aes(x=LOC_X, y=LOC_Y)) + 
               annotation_custom(court, -250, 250, -52, 418) +
               stat_binhex(bins = 25, colour = "gray", alpha = 0.7) +
               scale_fill_gradientn(colours = c("yellow","orange","red")) +
               guides(alpha = FALSE, size = FALSE) +
               xlim(250, -250) +
               ylim(-52, 418) +
               geom_rug(alpha = 0.2) +
               coord_fixed() +
               
               theme(line = element_blank(),
                     axis.title.x = element_blank(),
                     axis.title.y = element_blank(),
                     axis.text.x = element_blank(),
                     axis.text.y = element_blank(),
                     legend.title = element_blank()
               )+theme_bw()+ggtitle(paste(input$name,"'s shoting chart of 2015-16 season",sep=" "))+xlab("")+ylab("")
             
           }
          }
      })
  output$plot3<-renderPlotly({
    if(input$sea1==1){
      if(input$ty==1){
        ggplotly(ggplot(Distance1(),aes(x = SHOT_DISTANCE))+geom_line(aes(y=Shot_Percentage,col="Shot_Percentage"))+geom_line(aes(y=Average_Shot_Percentage,col="Average_Shot_Percentage"))+theme_bw()+ggtitle("Shooting preference by shot distance")+geom_point(aes(y=Shot_Percentage,col="Shot_Percentage"))+geom_point(aes(y=Average_Shot_Percentage,col="Average_Shot_Percentage")))
      }else{
        plot_ly(x=Distance1()$SHOT_DISTANCE,y=Distance1()$Shot_Percentage_Difference,type="bar")%>%layout(title="% Shot Preference and league average Comparison",xaxis = list(title="SHOT_DISTANCE"), yaxis = list(title="Shot_Percentage_Difference"))
          
      }
      
    }else{
      if(input$ty==1){
        ggplotly(ggplot(Distance2(),aes(x = SHOT_DISTANCE))+geom_line(aes(y=Shot_Percentage,col="Shot_Percentage"))+geom_line(aes(y=Average_Shot_Percentage,col="Average_Shot_Percentage"))+theme_bw()+ggtitle("Shooting preference by shot distance")+geom_point(aes(y=Shot_Percentage,col="Shot_Percentage"))+geom_point(aes(y=Average_Shot_Percentage,col="Average_Shot_Percentage")))
      }else{
        plot_ly(x=Distance2()$SHOT_DISTANCE,y=Distance2()$Shot_Percentage_Difference,type="bar")%>%layout(title="% Shot Preference and league average Comparison",xaxis = list(title="SHOT_DISTANCE"), yaxis = list(title="Shot_Percentage_Difference"))
      }
      }
  })
  output$plot4<-renderPlotly({
    if(input$sea1==1){
      if(input$ty==1){
        ggplotly(ggplot(Distance1(),aes(x = SHOT_DISTANCE))+geom_line(aes(y=Percentage,col="Percentage"))+geom_line(aes(y=Average_Percentage,col="Average_Percentage"))+geom_point(aes(y=Percentage,col="Percentage"))+geom_point(aes(y=Average_Percentage,col="Average_Percentage"))+theme_bw()+ggtitle("% Field Goal by Distance "))
      }else{
        plot_ly(x=Distance1()$SHOT_DISTANCE,y=Distance1()$Accuracy_Difference,type="bar")%>%layout(title="% Field Goal and league average Comparison",xaxis = list(title="SHOT_DISTANCE"), yaxis = list(title="Accuracy_Difference"))
      }
      
    }else{
      
      if(input$ty==1){
        ggplotly(ggplot(Distance2(),aes(x = SHOT_DISTANCE))+geom_line(aes(y=Percentage,col="Percentage"))+geom_line(aes(y=Average_Percentage,col="Average_Percentage"))+geom_point(aes(y=Percentage,col="Percentage"))+geom_point(aes(y=Average_Percentage,col="Average_Percentage"))+theme_bw()+ggtitle("% Field Goal by Distance"))
      }else{
        plot_ly(x=Distance2()$SHOT_DISTANCE,y=Distance2()$Accuracy_Difference,type="bar")%>%layout(title="% Field Goal and league average Comparison",xaxis = list(title="SHOT_DISTANCE"), yaxis = list(title="Accuracy_Difference"))
      }
      
    }
    
  })
  
 
  output$plot5<-renderPlotly({
    if(input$sea1==1){
    if(input$ty==1){
      ggplotly(ggplot(Minute1(),aes(x = MINUTES_REMAINING))+geom_line(aes(y=Shot_Quantity,col="Shot_Quantity"))+geom_line(aes(y=Average_Shot_Quantity,col="Average_Shot_Quantity"))+geom_point(aes(y=Shot_Quantity,col="Shot_Quantity"))+geom_point(aes(y=Average_Shot_Quantity,col="Average_Shot_Quantity"))+theme_bw()+ggtitle("# Shot Attempt by Minutes Remaining"))
    }else{
      plot_ly(x=Minute1()$MINUTES_REMAINING,y=Minute1()$Shot_Attempt_Difference,type="bar")%>%layout(title="# Shot and league average Comparison",xaxis = list(title="MINUTES_REMAINING"), yaxis = list(title="Shot_Attempt_Difference"))
    }}else{
      
      if(input$ty==1){
        ggplotly(ggplot(Minute2(),aes(x = MINUTES_REMAINING))+geom_line(aes(y=Shot_Quantity,col="Shot_Quantity"))+geom_line(aes(y=Average_Shot_Quantity,col="Average_Shot_Quantity"))+geom_point(aes(y=Shot_Quantity,col="Shot_Quantity"))+geom_point(aes(y=Average_Shot_Quantity,col="Average_Shot_Quantity"))+theme_bw()+ggtitle("# Shot Attempt by Minutes Remaining "))
      }else{
        plot_ly(x=Minute2()$MINUTES_REMAINING,y=Minute2()$Shot_Attempt_Difference,type="bar")%>%layout(title="# Shot and league average Comparison",xaxis = list(title="MINUTES_REMAINING"), yaxis = list(title="Shot_Attempt_Difference"))
      }
      
    }
  })
  output$plot6<-renderPlotly({
    if(input$sea1==1){
      
      if(input$ty==1){
        ggplotly(ggplot(Minute1(),aes(x = MINUTES_REMAINING))+geom_line(aes(y=Percentage,col="Percentage"))+geom_line(aes(y=Average_Percentage,col="Average_Percentage"))+geom_point(aes(y=Percentage,col="Percentage"))+geom_point(aes(y=Average_Percentage,col="Average_Percentage"))+theme_bw()+ggtitle("% Field Goal by Minutes Remaining"))
      }else{
        plot_ly(x=Minute1()$MINUTES_REMAINING,y=Minute1()$Accuracy_Difference,type="bar")%>%layout(title="% Field Goal and league average Comparison",xaxis = list(title="MINUTES_REMAINING"), yaxis = list(title="Accuracy_Difference"))
      }
      
    }else{
      
      if(input$ty==1){
        ggplotly(ggplot(Minute2(),aes(x = MINUTES_REMAINING))+geom_line(aes(y=Percentage,col="Percentage"))+geom_line(aes(y=Average_Percentage,col="Average_Percentage"))+geom_point(aes(y=Percentage,col="Percentage"))+geom_point(aes(y=Average_Percentage,col="Average_Percentage"))+theme_bw()+ggtitle("% Field Goal by Minutes Remaining"))
      }else{
        plot_ly(x=Minute2()$MINUTES_REMAINING,y=Minute2()$Accuracy_Difference,type="bar")%>%layout(title="% Field Goal and league average Comparison",xaxis = list(title="MINUTES_REMAINING"), yaxis = list(title="Accuracy_Difference"))
      }
      
    }
  })

  output$plot9a<-renderPlotly({
    if(input$sea1==1){
      if(input$ty==1){
        ggplotly(ggplot(Distance1(),aes(x = SHOT_DISTANCE))+geom_line(aes(y=Shot_Quantity,col="Shot_Quantity"))+geom_line(aes(y=Average_Shot_Quantity,col="Average_Shot_Quantity"))+geom_point(aes(y=Shot_Quantity,col="Shot_Quantity"))+geom_point(aes(y=Average_Shot_Quantity,col="Average_Shot_Quantity"))+theme_bw()+ggtitle("# Shot Attempt by Distance"))
      }else{
        
        plot_ly(x=Distance1()$SHOT_DISTANCE,y=Distance1()$Shot_Attempt_Difference,type="bar")%>%layout(title="# Shot and league average Comparison",xaxis = list(title="SHOT_DISTANCE"), yaxis = list(title="Shot_Attempt_Difference"))
        
      }}else{
        
        if(input$ty==1){
          ggplot(Distance2(),aes(x = SHOT_DISTANCE))+geom_line(aes(y=Shot_Quantity,col="Shot_Quantity"))+geom_line(aes(y=Average_Shot_Quantity,col="Average_Shot_Quantity"))+geom_point(aes(y=Shot_Quantity,col="Shot_Quantity"))+geom_point(aes(y=Average_Shot_Quantity,col="Average_Shot_Quantity"))+theme_bw()+ggtitle("# Shot Attempt by Distance")
        }else{
          plot_ly(x=Distance2()$SHOT_DISTANCE,y=Distance2()$Shot_Attempt_Difference,type="bar")%>%layout(title="# Shot and league average Comparison",xaxis = list(title="SHOT_DISTANCE"), yaxis = list(title="Shot_Attempt_Difference"))
        }
        
      }
  })
  
  
  
  
  
  playerA<-reactive({
    if(input$QT2==1){
    if(input$sea==1){
      data14[data14$PLAYER_NAME==input$nameA,]
    }else{
      data15[data15$PLAYER_NAME==input$nameA,]
    }}else{
      if(input$sea==1){
        data14[data14$PLAYER_NAME==input$nameA&data14$PERIOD==as.numeric(input$QT2)-1,]
      }else{
        data15[data15$PLAYER_NAME==input$nameA&data14$PERIOD==as.numeric(input$QT2)-1,]
      }
    }
  })
  playerB<-reactive({
    if(input$QT2==1){
    if(input$sea==1){
      data14[data14$PLAYER_NAME==input$nameB,]
    }else{
      data15[data15$PLAYER_NAME==input$nameB,]
    }}else{
      if(input$sea==1){
        data14[data14$PLAYER_NAME==input$nameB&data14$PERIOD==as.numeric(input$QT2)-1,]
      }else{
        data15[data15$PLAYER_NAME==input$nameB&data14$PERIOD==as.numeric(input$QT2)-1,]
      }
      
    }
  })
  Distance1A<-reactive({
    if(input$QT2!=1){
    temp1<-Distance14[Distance14$PLAYER_NAME==input$nameA&Distance14$PERIOD==as.numeric(input$QT2)-1,-1]
    colnames(temp1)<-c("SHOT_DISTANCE","PERIOD","PlayerA_Shot_Quantity","PlayerA_Made_Quantity","PlayerA_Percentage")
    PlayerA_Shot_Percentage<-temp1$PlayerA_Shot_Quantity/sum(temp1$PlayerA_Shot_Quantity)
    temp1<-data.frame(temp1,PlayerA_Shot_Percentage)
    temp2<-Distance14[Distance14$PLAYER_NAME==input$nameB&Distance14$PERIOD==as.numeric(input$QT2)-1,-1]
    colnames(temp2)<-c("SHOT_DISTANCE","PERIOD","PlayerB_Shot_Quantity","PlayerB_Made_Quantity","PlayerB_Percentage")
    PlayerB_Shot_Percentage<-temp2$PlayerB_Shot_Quantity/sum(temp2$PlayerB_Shot_Quantity)
    temp2<-data.frame(temp2,PlayerB_Shot_Percentage)
    temp3<-DAVE14[DAVE14$PERIOD==as.numeric(input$QT2)-1,]
    Average_Shot_Percentage<-temp3$Average_Shot_Quantity/sum(temp3$Average_Shot_Quantity)
    temp3<-data.frame(temp3,Average_Shot_Percentage)
    temp<-join(temp3,temp2)
    temp<-join(temp,temp1)
    }
    else{
      temp1<-Distance14[Distance14$PLAYER_NAME==input$nameA,-1]
      colnames(temp1)<-c("SHOT_DISTANCE","PERIOD", "Shot_Quantity_A", "Made_Quantity_A", "Percentage_A")
      temp2<-Distance14[Distance14$PLAYER_NAME==input$nameB,-1]
      colnames(temp2)<-c("SHOT_DISTANCE","PERIOD","Shot_Quantity_B", "Made_Quantity_B", "Percentage_B")
      temp3<-DAVE14
      temp<-join(temp3,temp2)
      temp<-join(temp,temp1)
      temp<-sqldf("SELECT SHOT_DISTANCE,sum(Shot_Quantity_A) as PlayerA_Shot_Quantity, sum(Made_Quantity_A) as PlayerA_Made_Quantity,sum(Shot_Quantity_B) as PlayerB_Shot_Quantity, sum(Made_Quantity_B) as PlayerB_Made_Quantity,sum(Average_Shot_Quantity) as Average_Shot_Quantity, sum(Average_Made_Quantity) as Average_Made_Quantity from temp group by SHOT_DISTANCE ")
      PlayerA_Shot_Percentage<-temp$PlayerA_Shot_Quantity/sum(temp$PlayerA_Shot_Quantity,na.rm = "TRUE")
      PlayerB_Shot_Percentage<-temp$PlayerB_Shot_Quantity/sum(temp$PlayerB_Shot_Quantity,na.rm = "TRUE")
      Average_Shot_Percentage<-temp$Average_Shot_Quantity/sum(temp$Average_Shot_Quantity,na.rm = "TRUE")
      temp<-data.frame(temp,PlayerA_Shot_Percentage ,PlayerB_Shot_Percentage ,Average_Shot_Percentage,PlayerA_Percentage=temp$PlayerA_Made_Quantity/temp$PlayerA_Shot_Quantity,PlayerB_Percentage=temp$PlayerB_Made_Quantity/temp$PlayerB_Shot_Quantity ,Average_Percentage=temp$Average_Made_Quantity/temp$Average_Shot_Quantity)
    }
    temp<-data.frame(temp,Difference_Attempts=temp$PlayerA_Shot_Quantity-temp$PlayerB_Shot_Quantity,Difference_Percentage=temp$PlayerA_Percentage-temp$PlayerB_Percentage,Difference_Shot_Percentage=temp$PlayerA_Shot_Percentage-temp$PlayerB_Shot_Percentage)
    temp
  })
  
  Distance2A<-reactive({
    if(input$QT2!=1){
      temp1<-Distance15[Distance15$PLAYER_NAME==input$nameA&Distance15$PERIOD==as.numeric(input$QT2)-1,-1]
      colnames(temp1)<-c("SHOT_DISTANCE","PERIOD","PlayerA_Shot_Quantity","PlayerA_Made_Quantity","PlayerA_Percentage")
      PlayerA_Shot_Percentage<-temp1$PlayerA_Shot_Quantity/sum(temp1$PlayerA_Shot_Quantity)
      temp1<-data.frame(temp1,PlayerA_Shot_Percentage)
      temp2<-Distance15[Distance15$PLAYER_NAME==input$nameB&Distance15$PERIOD==as.numeric(input$QT2)-1,-1]
      colnames(temp2)<-c("SHOT_DISTANCE","PERIOD","PlayerB_Shot_Quantity","PlayerB_Made_Quantity","PlayerB_Percentage")
      PlayerB_Shot_Percentage<-temp2$PlayerB_Shot_Quantity/sum(temp2$PlayerB_Shot_Quantity)
      temp2<-data.frame(temp2,PlayerB_Shot_Percentage)
      temp3<-DAVE15[DAVE15$PERIOD==as.numeric(input$QT2)-1,]
      Average_Shot_Percentage<-temp3$Average_Shot_Quantity/sum(temp3$Average_Shot_Quantity)
      temp3<-data.frame(temp3,Average_Shot_Percentage)
      temp<-join(temp3,temp2)
      temp<-join(temp,temp1)
    }
    else{
      temp1<-Distance15[Distance15$PLAYER_NAME==input$nameA,-1]
      colnames(temp1)<-c("SHOT_DISTANCE","PERIOD", "Shot_Quantity_A", "Made_Quantity_A", "Percentage_A")
      temp2<-Distance15[Distance15$PLAYER_NAME==input$nameB,-1]
      colnames(temp2)<-c("SHOT_DISTANCE","PERIOD","Shot_Quantity_B", "Made_Quantity_B", "Percentage_B")
      temp3<-DAVE15
      temp<-join(temp3,temp2)
      temp<-join(temp,temp1)
      temp<-sqldf("SELECT SHOT_DISTANCE,sum(Shot_Quantity_A) as PlayerA_Shot_Quantity, sum(Made_Quantity_A) as PlayerA_Made_Quantity,sum(Shot_Quantity_B) as PlayerB_Shot_Quantity, sum(Made_Quantity_B) as PlayerB_Made_Quantity,sum(Average_Shot_Quantity) as Average_Shot_Quantity, sum(Average_Made_Quantity) as Average_Made_Quantity from temp group by SHOT_DISTANCE ")
      PlayerA_Shot_Percentage<-temp$PlayerA_Shot_Quantity/sum(temp$PlayerA_Shot_Quantity,na.rm = TRUE)
      PlayerB_Shot_Percentage<-temp$PlayerB_Shot_Quantity/sum(temp$PlayerB_Shot_Quantity,na.rm = TRUE)
      Average_Shot_Percentage<-temp$Average_Shot_Quantity/sum(temp$Average_Shot_Quantity)
      temp<-data.frame(temp,PlayerA_Shot_Percentage ,PlayerB_Shot_Percentage ,Average_Shot_Percentage,PlayerA_Percentage=temp$PlayerA_Made_Quantity/temp$PlayerA_Shot_Quantity,PlayerB_Percentage=temp$PlayerB_Made_Quantity/temp$PlayerB_Shot_Quantity ,Average_Percentage=temp$Average_Made_Quantity/temp$Average_Shot_Quantity)
    }
    temp<-data.frame(temp,Difference_Attempts=temp$PlayerA_Shot_Quantity-temp$PlayerB_Shot_Quantity,Difference_Percentage=temp$PlayerA_Percentage-temp$PlayerB_Percentage,Difference_Shot_Percentage=temp$PlayerA_Shot_Percentage-temp$PlayerB_Shot_Percentage)
    temp
  })
  
  Minute1A<-reactive({
    if(input$QT2!=1){
      temp1<-Minute14[Minute14$PLAYER_NAME==input$nameA&Minute14$PERIOD==as.numeric(input$QT2)-1,-1]
      colnames(temp1)<-c("MINUTES_REMAINING","PERIOD","PlayerA_Shot_Quantity","PlayerA_Made_Quantity","PlayerA_Percentage")
      PlayerA_Shot_Percentage<-temp1$PlayerA_Shot_Quantity/sum(temp1$PlayerA_Shot_Quantity)
      temp1<-data.frame(temp1,PlayerA_Shot_Percentage)
      temp2<-Minute14[Minute14$PLAYER_NAME==input$nameB&Minute14$PERIOD==as.numeric(input$QT2)-1,-1]
      colnames(temp2)<-c("MINUTES_REMAINING","PERIOD","PlayerB_Shot_Quantity","PlayerB_Made_Quantity","PlayerB_Percentage")
      PlayerB_Shot_Percentage<-temp2$PlayerB_Shot_Quantity/sum(temp2$PlayerB_Shot_Quantity)
      temp2<-data.frame(temp2,PlayerB_Shot_Percentage)
      temp3<-MAVE14[MAVE14$PERIOD==as.numeric(input$QT2)-1,]
      Average_Shot_Percentage<-temp3$Average_Shot_Quantity/sum(temp3$Average_Shot_Quantity)
      temp3<-data.frame(temp3,Average_Shot_Percentage)
      temp<-join(temp3,temp2)
      temp<-join(temp,temp1)
    }
    else{
      temp1<-Minute14[Minute14$PLAYER_NAME==input$nameA,-1]
      colnames(temp1)<-c("MINUTES_REMAINING","PERIOD", "Shot_Quantity_A", "Made_Quantity_A", "Percentage_A")
      temp2<-Minute14[Minute14$PLAYER_NAME==input$nameB,-1]
      colnames(temp2)<-c("MINUTES_REMAINING","PERIOD","Shot_Quantity_B", "Made_Quantity_B", "Percentage_B")
      temp3<-MAVE14
      temp<-join(temp3,temp2)
      temp<-join(temp,temp1)
      temp<-sqldf("SELECT MINUTES_REMAINING,sum(Shot_Quantity_A) as PlayerA_Shot_Quantity, sum(Made_Quantity_A) as PlayerA_Made_Quantity,sum(Shot_Quantity_B) as PlayerB_Shot_Quantity, sum(Made_Quantity_B) as PlayerB_Made_Quantity,sum(Average_Shot_Quantity) as Average_Shot_Quantity, sum(Average_Made_Quantity) as Average_Made_Quantity from temp group by MINUTES_REMAINING ")
      PlayerA_Shot_Percentage<-temp$PlayerA_Shot_Quantity/sum(temp$PlayerA_Shot_Quantity,na.rm = TRUE)
      PlayerB_Shot_Percentage<-temp$PlayerB_Shot_Quantity/sum(temp$PlayerB_Shot_Quantity,na.rm = TRUE)
      Average_Shot_Percentage<-temp$Average_Shot_Quantity/sum(temp$Average_Shot_Quantity)
      temp<-data.frame(temp,PlayerA_Shot_Percentage ,PlayerB_Shot_Percentage ,Average_Shot_Percentage,PlayerA_Percentage=temp$PlayerA_Made_Quantity/temp$PlayerA_Shot_Quantity,PlayerB_Percentage=temp$PlayerB_Made_Quantity/temp$PlayerB_Shot_Quantity ,Average_Percentage=temp$Average_Made_Quantity/temp$Average_Shot_Quantity)
    }
    temp<-data.frame(temp,Difference_Attempts=temp$PlayerA_Shot_Quantity-temp$PlayerB_Shot_Quantity,Difference_Percentage=temp$PlayerA_Percentage-temp$PlayerB_Percentage,Difference_Shot_Percentage=temp$PlayerA_Shot_Percentage-temp$PlayerB_Shot_Percentage)
    temp
  })
  Minute2A<-reactive({
    if(input$QT2!=1){
      temp1<-Minute15[Minute15$PLAYER_NAME==input$nameA&Minute15$PERIOD==as.numeric(input$QT2)-1,-1]
      colnames(temp1)<-c("MINUTES_REMAINING","PERIOD","PlayerA_Shot_Quantity","PlayerA_Made_Quantity","PlayerA_Percentage")
      PlayerA_Shot_Percentage<-temp1$PlayerA_Shot_Quantity/sum(temp1$PlayerA_Shot_Quantity)
      temp1<-data.frame(temp1,PlayerA_Shot_Percentage)
      temp2<-Minute15[Minute15$PLAYER_NAME==input$nameB&Minute15$PERIOD==as.numeric(input$QT2)-1,-1]
      colnames(temp2)<-c("MINUTES_REMAINING","PERIOD","PlayerB_Shot_Quantity","PlayerB_Made_Quantity","PlayerB_Percentage")
      PlayerB_Shot_Percentage<-temp2$PlayerB_Shot_Quantity/sum(temp2$PlayerB_Shot_Quantity)
      temp2<-data.frame(temp2,PlayerB_Shot_Percentage)
      temp3<-MAVE15[MAVE15$PERIOD==as.numeric(input$QT2)-1,]
      Average_Shot_Percentage<-temp3$Average_Shot_Quantity/sum(temp3$Average_Shot_Quantity)
      temp3<-data.frame(temp3,Average_Shot_Percentage)
      temp<-join(temp3,temp2)
      temp<-join(temp,temp1)
    }
    else{
      temp1<-Minute15[Minute15$PLAYER_NAME==input$nameA,-1]
      colnames(temp1)<-c("MINUTES_REMAINING","PERIOD", "Shot_Quantity_A", "Made_Quantity_A", "Percentage_A")
      temp2<-Minute15[Minute15$PLAYER_NAME==input$nameB,-1]
      colnames(temp2)<-c("MINUTES_REMAINING","PERIOD","Shot_Quantity_B", "Made_Quantity_B", "Percentage_B")
      temp3<-MAVE15
      temp<-join(temp3,temp2)
      temp<-join(temp,temp1)
      temp<-sqldf("SELECT MINUTES_REMAINING,sum(Shot_Quantity_A) as PlayerA_Shot_Quantity, sum(Made_Quantity_A) as PlayerA_Made_Quantity,sum(Shot_Quantity_B) as PlayerB_Shot_Quantity, sum(Made_Quantity_B) as PlayerB_Made_Quantity,sum(Average_Shot_Quantity) as Average_Shot_Quantity, sum(Average_Made_Quantity) as Average_Made_Quantity from temp group by MINUTES_REMAINING ")
      PlayerA_Shot_Percentage<-temp$PlayerA_Shot_Quantity/sum(temp$PlayerA_Shot_Quantity)
      PlayerB_Shot_Percentage<-temp$PlayerB_Shot_Quantity/sum(temp$PlayerB_Shot_Quantity)
      Average_Shot_Percentage<-temp$Average_Shot_Quantity/sum(temp$Average_Shot_Quantity)
      temp<-data.frame(temp,PlayerA_Shot_Percentage ,PlayerB_Shot_Percentage ,Average_Shot_Percentage,PlayerA_Percentage=temp$PlayerA_Made_Quantity/temp$PlayerA_Shot_Quantity,PlayerB_Percentage=temp$PlayerB_Made_Quantity/temp$PlayerB_Shot_Quantity ,Average_Percentage=temp$Average_Made_Quantity/temp$Average_Shot_Quantity)
    }
    temp<-data.frame(temp,Difference_Attempts=temp$PlayerA_Shot_Quantity-temp$PlayerB_Shot_Quantity,Difference_Percentage=temp$PlayerA_Percentage-temp$PlayerB_Percentage,Difference_Shot_Percentage=temp$PlayerA_Shot_Percentage-temp$PlayerB_Shot_Percentage)
    temp
  })
  
  
  output$plot9<-renderPlot({
    if(input$mdo2!=T){
    ggplot(playerA(), aes(x=LOC_X, y=LOC_Y)) + 
      annotation_custom(court, -250, 250, -50, 420) +
      geom_point(aes(colour = EVENT_TYPE)) +
      xlim(-250, 250) +
      ylim(-50, 420)+theme_bw()+ggtitle(paste(input$nameA,"'s shoting chart ",sep=" "))+xlab("")+ylab("")   
}else{
  ggplot(playerA()[playerA()$EVENT_TYPE=="Made Shot",], aes(x=LOC_X, y=LOC_Y)) + 
    annotation_custom(court, -250, 250, -50, 420) +
    geom_point(aes(colour = EVENT_TYPE)) +
    xlim(-250, 250) +
    ylim(-50, 420)+theme_bw()+ggtitle(paste(input$nameA,"'s shoting chart ",sep=" "))+xlab("")+ylab("")   
}  })
  output$plot10<-renderPlot({
    if(input$mdo2!=T){
    ggplot(playerB(), aes(x=LOC_X, y=LOC_Y)) + 
      annotation_custom(court, -250, 250, -50, 420) +
      geom_point(aes(colour = EVENT_TYPE)) +
      xlim(-250, 250) +
      ylim(-50, 420)+theme_bw()+ggtitle(paste(input$nameB,"'s shoting chart ",sep=" "))+xlab("")+ylab("")   
 }else{
   
   ggplot(playerB()[playerB()$EVENT_TYPE=="Made Shot",], aes(x=LOC_X, y=LOC_Y)) + 
     annotation_custom(court, -250, 250, -50, 420) +
     geom_point(aes(colour = EVENT_TYPE)) +
     xlim(-250, 250) +
     ylim(-50, 420)+theme_bw()+ggtitle(paste(input$nameB,"'s shoting chart ",sep=" "))+xlab("")+ylab("")   
   
   
 } })
  output$plot11<-renderPlotly({
    if(input$sea==1){
      if(input$ty2==1){
        ggplotly(ggplot(Distance1A(),aes(x = SHOT_DISTANCE))+geom_line(aes(y=PlayerA_Shot_Quantity,col="PlayerA_Shot_Quantity"))+geom_line(aes(y=PlayerB_Shot_Quantity,col="PlayerB_Shot_Quantity"))+geom_line(aes(y=Average_Shot_Quantity,col="Average_Shot_Quantity"))+theme_bw()+ggtitle("Comparison of # shooting attempts by distance")+geom_point(aes(y=PlayerA_Shot_Quantity,col="PlayerA_Shot_Quantity"))+geom_point(aes(y=PlayerB_Shot_Quantity,col="PlayerB_Shot_Quantity"))+geom_point(aes(y=Average_Shot_Quantity,col="Average_Shot_Quantity")))}
      else{
        plot_ly(x=Distance1A()$SHOT_DISTANCE,y=Distance1A()$Difference_Attempt,type="bar")%>%layout(title="Comparison of # Shooting Attempts between players",xaxis = list(title="SHOT_DISTANCE"), yaxis = list(title="Shot_Attempt_Difference"))
      }}else{
        if(input$ty2==1){
          ggplotly(ggplot(Distance2A(),aes(x = SHOT_DISTANCE))+geom_line(aes(y=PlayerA_Shot_Quantity,col="PlayerA_Shot_Quantity"))+geom_line(aes(y=PlayerB_Shot_Quantity,col="PlayerB_Shot_Quantity"))+geom_line(aes(y=Average_Shot_Quantity,col="Average_Shot_Quantity"))+theme_bw()+ggtitle("Comparison of # shooting attempts by distance")+geom_point(aes(y=PlayerA_Shot_Quantity,col="PlayerA_Shot_Quantity"))+geom_point(aes(y=PlayerB_Shot_Quantity,col="PlayerB_Shot_Quantity"))+geom_point(aes(y=Average_Shot_Quantity,col="Average_Shot_Quantity")))}
        else{
          plot_ly(x=Distance2A()$SHOT_DISTANCE,y=Distance2A()$Difference_Attempt,type="bar")%>%layout(title="Comparison of # Shooting Attempts between players",xaxis = list(title="SHOT_DISTANCE"), yaxis = list(title="Shot_Attempt_Difference"))
        }
      }
  })
  output$plot13<-renderPlotly({
    if(input$sea==1){
      if(input$ty2==1){
        ggplotly(ggplot(Distance1A(),aes(x = SHOT_DISTANCE))+geom_line(aes(y=PlayerA_Percentage,col="PlayerA_Percentage"))+geom_line(aes(y=PlayerB_Percentage,col="PlayerB_Percentage"))+geom_line(aes(y=Average_Percentage,col="Average_Percentage"))+theme_bw()+ggtitle("Comparison of % Field Goal by distance")+geom_point(aes(y=PlayerA_Percentage,col="PlayerA_Percentage"))+geom_point(aes(y=PlayerB_Percentage,col="PlayerB_Percentage"))+geom_point(aes(y=Average_Percentage,col="Average_Percentage")))}
      else{
        plot_ly(x=Distance1A()$SHOT_DISTANCE,y=Distance1A()$Difference_Percentage,type="bar")%>%layout(title="Comparison of % Field Goal between players",xaxis = list(title="SHOT_DISTANCE"), yaxis = list(title="% Field Goal Difference"))
       
      }}else{
        if(input$ty2==1){
          ggplotly(ggplot(Distance2A(),aes(x = SHOT_DISTANCE))+geom_line(aes(y=PlayerA_Percentage,col="PlayerA_Percentage"))+geom_line(aes(y=PlayerB_Percentage,col="PlayerB_Percentage"))+geom_line(aes(y=Average_Percentage,col="Average_Percentage"))+theme_bw()+ggtitle("Comparison of % Field Goal by distance")+geom_point(aes(y=PlayerA_Percentage,col="PlayerA_Percentage"))+geom_point(aes(y=PlayerB_Percentage,col="PlayerB_Percentage"))+geom_point(aes(y=Average_Percentage,col="Average_Percentage")))}
        else{
          plot_ly(x=Distance2A()$SHOT_DISTANCE,y=Distance2A()$Difference_Percentage,type="bar")%>%layout(title="Comparison of % Field Goal between players",xaxis = list(title="SHOT_DISTANCE"), yaxis = list(title="% Field Goal Difference"))
        }
      }
  })
 output$plot12<-renderPlotly({
   if(input$sea==1){
     if(input$ty2==1){
       ggplotly(ggplot(Minute1A(),aes(x = MINUTES_REMAINING))+geom_line(aes(y=PlayerA_Shot_Quantity,col="PlayerA_Shot_Quantity"))+geom_line(aes(y=PlayerB_Shot_Quantity,col="PlayerB_Shot_Quantity"))+geom_line(aes(y=Average_Shot_Quantity,col="Average_Shot_Quantity"))+theme_bw()+ggtitle("Comparison of # shooting attempts by Time")+geom_point(aes(y=PlayerA_Shot_Quantity,col="PlayerA_Shot_Quantity"))+geom_point(aes(y=PlayerB_Shot_Quantity,col="PlayerB_Shot_Quantity"))+geom_point(aes(y=Average_Shot_Quantity,col="Average_Shot_Quantity")))}
     else{
       plot_ly(x=Minute1A()$SHOT_DISTANCE,y=Minute1A()$Difference_Attempt,type="bar")%>%layout(title="Comparison of # Shooting Attempts between players",xaxis = list(title="MINUTES_REMAINING"), yaxis = list(title="Shot_Attempt_Difference"))
     }}else{
       if(input$ty2==1){
         ggplotly(ggplot(Minute2A(),aes(x = MINUTES_REMAINING))+geom_line(aes(y=PlayerA_Shot_Quantity,col="PlayerA_Shot_Quantity"))+geom_line(aes(y=PlayerB_Shot_Quantity,col="PlayerB_Shot_Quantity"))+geom_line(aes(y=Average_Shot_Quantity,col="Average_Shot_Quantity"))+theme_bw()+ggtitle("Comparison of # shooting attempts by Time")+geom_point(aes(y=PlayerA_Shot_Quantity,col="PlayerA_Shot_Quantity"))+geom_point(aes(y=PlayerB_Shot_Quantity,col="PlayerB_Shot_Quantity"))+geom_point(aes(y=Average_Shot_Quantity,col="Average_Shot_Quantity")))}
       else{
         plot_ly(x=Minute2A()$SHOT_DISTANCE,y=Minute2A()$Difference_Attempt,type="bar")%>%layout(title="Comparison of # Shooting Attempts between players",xaxis = list(title="MINUTES_REMAINING"), yaxis = list(title="Shot_Attempt_Difference"))
       }
     }
   
 })
 output$plot14<-renderPlotly({
   if(input$sea==1){
     if(input$ty2==1){
       ggplotly(ggplot(Minute1A(),aes(x = MINUTES_REMAINING))+geom_line(aes(y=PlayerA_Percentage,col="PlayerA_Percentage"))+geom_line(aes(y=PlayerB_Percentage,col="PlayerB_Percentage"))+geom_line(aes(y=Average_Percentage,col="Average_Percentage"))+theme_bw()+ggtitle("Comparison of % Field Goal by Minutes Left")+geom_point(aes(y=PlayerA_Percentage,col="PlayerA_Percentage"))+geom_point(aes(y=PlayerB_Percentage,col="PlayerB_Percentage"))+geom_point(aes(y=Average_Percentage,col="Average_Percentage")))}
     else{
       plot_ly(x=Minute1A()$SHOT_DISTANCE,y=Minute1A()$Difference_Percentage,type="bar")%>%layout(title="Comparison of % Field Goal between players",xaxis = list(title="SHOT_DISTANCE"), yaxis = list(title="% Field Goal Difference"))
       
     }}else{
       if(input$ty2==1){
         ggplotly(ggplot(Minute2A(),aes(x = MINUTES_REMAINING))+geom_line(aes(y=PlayerA_Percentage,col="PlayerA_Percentage"))+geom_line(aes(y=PlayerB_Percentage,col="PlayerB_Percentage"))+geom_line(aes(y=Average_Percentage,col="Average_Percentage"))+theme_bw()+ggtitle("Comparison of % Field Goal by Minutes Left")+geom_point(aes(y=PlayerA_Percentage,col="PlayerA_Percentage"))+geom_point(aes(y=PlayerB_Percentage,col="PlayerB_Percentage"))+geom_point(aes(y=Average_Percentage,col="Average_Percentage")))}
       else{
         plot_ly(x=Minute2A()$SHOT_DISTANCE,y=Minute2A()$Difference_Percentage,type="bar")%>%layout(title="Comparison of % Field Goal between players",xaxis = list(title="SHOT_DISTANCE"), yaxis = list(title="% Field Goal Difference"))
         
       }
     }
 })
 output$plot15<-renderPlotly({
   if(input$sea==1){
     if(input$ty2==1){
       ggplotly(ggplot(Distance1A(),aes(x = SHOT_DISTANCE))+geom_line(aes(y=PlayerA_Shot_Percentage,col="PlayerA_Shot_Percentage"))+geom_line(aes(y=PlayerB_Shot_Percentage,col="PlayerB_Shot_Percentage"))+geom_line(aes(y=Average_Shot_Percentage,col="Average_Shot_Percentage"))+theme_bw()+ggtitle("Comparison of # shooting attempts by distance")+geom_point(aes(y=PlayerA_Shot_Percentage,col="PlayerA_Shot_Percentage"))+geom_point(aes(y=PlayerB_Shot_Percentage,col="PlayerB_Shot_Percentage"))+geom_point(aes(y=Average_Shot_Percentage,col="Average_Shot_Percentage")))}
     else{
       
       plot_ly(x=Distance1A()$SHOT_DISTANCE,y=Distance1A()$Difference_Shot_Percentage,type="bar")%>%layout(title="Comparison of % Shooting Preference between players",xaxis = list(title="SHOT_DISTANCE"), yaxis = list(title="% Shot Attempts Difference"))
       
       
     }}else{
       if(input$ty2==1){
         ggplotly(ggplot(Distance2A(),aes(x = SHOT_DISTANCE))+geom_line(aes(y=PlayerA_Shot_Percentage,col="PlayerA_Shot_Percentage"))+geom_line(aes(y=PlayerB_Shot_Percentage,col="PlayerB_Shot_Percentage"))+geom_line(aes(y=Average_Shot_Percentage,col="Average_Shot_Percentage"))+theme_bw()+ggtitle("Comparison of # shooting attempts by distance")+geom_point(aes(y=PlayerA_Shot_Percentage,col="PlayerA_Shot_Percentage"))+geom_point(aes(y=PlayerB_Shot_Percentage,col="PlayerB_Shot_Percentage"))+geom_point(aes(y=Average_Shot_Percentage,col="Average_Shot_Percentage")))}
       else{ 
         plot_ly(x=Distance2A()$SHOT_DISTANCE,y=Distance2A()$Difference_Shot_Percentage,type="bar")%>%layout(title="Comparison of % Shooting Preference between players",xaxis = list(title="SHOT_DISTANCE"), yaxis = list(title="% Shot Attempts Difference"))}
     }
 })
 
 Typeleag<-reactive({
   if(input$seat==1){
     Type14[Type14$Type==input$type1,]
   }else{
     Type15[Type15$Type==input$type2,]
   }
 })
 output$plot20<-renderPlotly({
   plot_ly(Typeleag(),x=Shot_Quantity,y=Percentage,text=PLAYER_NAME,mode="markers")%>%layout(title="Fact of Shot Type in the league")
 })
 output$sum<-renderPrint({
   data<-Typeleag()[,c(1,3,4,5)]
   summary(data)
 })
 output$downloadType1<-downloadHandler(
   
   filename = function() { 
     paste(input$type1,'.csv', sep='') 
   },
   content = function(file) {
     write.csv(Typeleag(), file)
   }
   )
 output$downloadType2<-downloadHandler(
   
   filename = function() { 
     paste(input$type2,'.csv', sep='') 
   },
   content = function(file) {
     write.csv(Typeleag(), file)
   }
 )
 
 shotlog<-reactive({
   request = GET(
     "http://stats.nba.com/stats/shotchartdetail",
     query = list(
       PlayerID=input$pid,
       Season = input$csea,
       ContextMeasure = "FGA",
       DateFrom = "",
       DateTo = "",
       GameID = "",
       GameSegment = "",
       LastNGames = 0,
       LeagueID = "00",
       Location = "",
       Month = 0,
       OpponentTeamID = 0,
       Outcome = "",
       Period = 0,
       Position = "",
       RookieYear = "",
       SeasonSegment = "",
       SeasonType = "Regular Season",
       TeamID = 0,
       VsConference = "",
       VsDivision = ""
     )
   )
   data<-content(request)
   if(is.null(unlist(data$resultSets[[1]][[3]]))!=T){
   shotDataf <- data.frame(matrix(unlist(data$resultSets[[1]][[3]]), ncol=21, byrow = TRUE))
   colnames(shotDataf) <- data$resultSets[[1]][[2]]}
   shotDataf
 })
 output$shotlog<-renderDataTable({
   shotlog()
 })
 output$downloadData<-downloadHandler(
   filename = function() { 
     paste(input$pid, '.csv', sep='') 
   },
   content = function(file) {
     write.csv(shotlog(), file)
   }
 )
 data20<-reactive({
   if(input$seac==1){
     data14[data14$PLAYER_NAME==input$namec,c(7,8)]
   }else{
     data15[data15$PLAYER_NAME==input$namec,c(7,8)]
   }
 })
 data21<-reactive({
   if(input$seac==1){
     data14[data14$PLAYER_NAME==input$namec&data14$EVENT_TYPE=="Made Shot",c(7,8)]
   }else{
     data15[data15$PLAYER_NAME==input$namec&data15$EVENT_TYPE=="Made Shot",c(7,8)]
   }
 })
 output$plotc1<-renderPlot({
   cluster<-kmeans(data20(),centers = input$cluster,nstart = 10)
   data<-data.frame(data20(),cluster=as.character(cluster$cluster))
   
   ggplot(data, aes(x=LOC_X, y=LOC_Y)) + 
     annotation_custom(court, -250, 250, -50, 420) +
     geom_point(aes(colour = cluster,size=2)) +
     xlim(-250, 250) +
     ylim(-50, 420)+theme_bw()+ggtitle(paste(input$namec,"'s shoting clustering analysis",sep=" "))+xlab("")+ylab("")
   
 })
 output$plotc2<-renderPlot({
   
   cluster<-kmeans(data21(),centers = input$cluster,nstart = 10)
   data<-data.frame(data21(),cluster=as.character(cluster$cluster))
   
   ggplot(data, aes(x=LOC_X, y=LOC_Y)) + 
     annotation_custom(court, -250, 250, -50, 420) +
     geom_point(aes(colour = cluster,size=2)) +
     xlim(-250, 250) +
     ylim(-50, 420)+theme_bw()+ggtitle(paste(input$namec,"'s made shot clustering analysis",sep=" "))+xlab("")+ylab("")
   
   
 })
 
})





