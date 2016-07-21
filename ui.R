name<-read.csv("name.csv")[,2]
type1<-as.character(unique(read.csv("Type_14.csv")[,3]))
type2<-as.character(unique(read.csv("Type_15.csv")[,3]))
library(shiny)
library(ggplot2)
library(hexbin)
library(shinythemes)
library(plotly)
shinyUI(navbarPage(theme = shinytheme("united"),"NBA Shot Plot",
        tabPanel("Player Data",sidebarLayout(
          sidebarPanel(
            HTML('<Center><img src="NBA.png" width="250" height="100"></Center>'),
            br(),
            p("Attention: It will take sometime to render the plots. The rendering time will be less than 1 minutes, please be patient.",align="Justify"),
            selectInput("name","Select Player",choices = as.character(name),selected = as.character(name)[1]),
            radioButtons("sea1","Choose Season",choices = list("2014-15"=1,"2015-16"=2),selected = 1),
            br(),
            
            selectInput("QT","Quarter",choices=list("All"=1,"First Quarter"=2,"Second Quarter"=3,"Third Quarter"=4,"Fourth Quarter"=5)),
            br(),
            h4("Options of Shot Chart"),
            checkboxInput("mdo","Show Made Shot Only",value=F),
            selectInput("ct","Chart Type",choices = list("Shot Chart"=1,"Hexbin Shot Chart"=2),selected=1),
            br(),
            h4("Types of Performance Charts"),
            radioButtons("ty","Type of Charts",choices = list("Line"=1,"Bar"=2),selected = 1),
            em("Line chart plots the league average values and performance of player while bar chart plots the difference. A positive value in bar chart means performance of player is better than league average level.",align="Justify"),
            br(),
            br(),
            br(),
            br(),
            h5("Author"),
            a(h5("Jiashen Liu"),href="https://nl.linkedin.com/in/jiashen-liu-4658aa112",target="_blank"),
            a(h5("Code"),href="https://github.com/liujiashen9307/NBAShotChart/",target="_blank")
            
            
                       ),
          mainPanel(
                    h4("Shot Chart of Player"),
                    fluidRow(column=8,
                    plotOutput("plot1",width = "800", height = "600"),align="Center"),
                    br(),
                    br(),
                    br(),
                    br(),
                    h4("Shot Statistics by Shooting Distance"),
                    splitLayout(cellwidths=c("50%","50%"),plotlyOutput("plot3"),plotlyOutput("plot4")),
                    h4("Shot Statistics by Minutes Left"),
                    splitLayout(cellwidths=c("50%","50%"),plotlyOutput("plot5"),plotlyOutput("plot6")),
                    h4("Additional: Shot Attempts By Distance"),
                    plotlyOutput("plot9a"),
                    h4("Shot Type Display"),
                    splitLayout(cellwidths=c("50%","50%"),plotlyOutput("plot10a"),plotlyOutput("plot11a")))
                    
        )),
        tabPanel("Player Comparison",sidebarLayout(
          sidebarPanel(
            radioButtons("sea","Choose Season",choices = list("2014-2015"=1,"2015-2016"=2),selected = 1),
            br(),
     
            selectInput("nameA","Select Player A",choices = as.character(name),selected = as.character(name)[1]),
            selectInput("nameB","Select Player B",choices = as.character(name),selected = as.character(name)[2]),
            checkboxInput("mdo2","Show Made Shot Only",value=F),
            selectInput("QT2","Quarter",choices=list("All"=1,"First Quarter"=2,"Second Quarter"=3,"Third Quarter"=4,"Fourth Quarter"=5)),
            radioButtons("ty2","Type of Charts",choices = list("Line"=1,"Bar"=2),selected = 1),
            em("Line chart plots the direction among the data of two players and the league average level. Bar chart plots the direct comparison between two players. Bars with positive value mean player A is advantageous in specific performance.",align="Justify")
          ),
          mainPanel(
            h4("Shot Chart of Players"),
            splitLayout(cellwidths=c("50%","50%"),plotOutput("plot9"),plotOutput("plot10")),
            h4("Comparison of shot attempts"),
            splitLayout(cellwidths=c("50%","50%"),plotlyOutput("plot11"),plotlyOutput("plot12")),
            h4("Comparison of % Field Goal"),
            splitLayout(cellwidths=c("50%","50%"),plotlyOutput("plot13"),plotlyOutput("plot14")),
            h4("Comparison of shooting preference by Distance"),
            plotlyOutput("plot15"),
            h4("Comparison of Offense Diversity (Bar)"),
            splitLayout(cellwidths=c("50%","50%"),plotlyOutput("plot16"),plotlyOutput("plot17")),
            h4("Comparison of Offense Diversity (Pie)"),
            splitLayout(cellwidths=c("50%","50%"),plotlyOutput("plot18"),plotlyOutput("plot19"))
          )
        )),tabPanel("Shot Type Analysis for Two Seasons",
                    sidebarLayout(sidebarPanel(radioButtons("seat","Choose Season",choices=list("2014-15"=1,"2015-16"=2)),
                                               br(),
                                               h4("Group by Type?"),
                                               conditionalPanel(condition='input.seat==1',selectInput("type1","Shot Type",choices = type1,selected = type1[1])),
                                               conditionalPanel(condition='input.seat==2',selectInput("type2","Shot Type",choices = type2,selected = type2[1])),
                                               p("Draw your own plot?"),
                                               conditionalPanel(condition='input.seat==1',downloadButton('downloadType1', 'Download (csv File)')),
                                               conditionalPanel(condition='input.seat==2',downloadButton('downloadType2', 'Download (csv File)')),
                                               br(),
                                               h4("Group by Distance?"),
                                               selectInput("pnametype","Choose player you want to explore",choices = append("All",as.character(name)),selected = "All"),
                                               p("Draw your own plot?"),
                                               downloadButton('downloadType3', 'Download (csv File)')
                                               ),
                                  mainPanel(h4("Shot Type Comparison"),
                                            plotlyOutput("plot20"),
                                            h4("Fast Fact"),
                                            verbatimTextOutput("sum"),
                                            h4("Shot Type Summary By Distance"),
                                            plotlyOutput("plot24"))
                                           )),
        tabPanel("Research Tool---Clustering Analysis of Player's Shooting Activities",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("namec","Select Player",choices = as.character(name),selected = as.character(name)[1]),
                     br(),
                     radioButtons("seac","Choose Season",choices = list("2014-15"=1,"2015-16"=2),selected = 1),
                     br(),
                     numericInput("cluster","Number of clusters",value=6,min=3,max=9)
                   ),
                   mainPanel(
                     h4("Description"),
                     p(h5("This special part serves the research function of re-clustering the habit of shot of players. Traditionally, the league always cluster the shot of players by the defined area of the court. However, the revolution of the training technique makes some players really crazy. Therefore, it is not quite scientific to research the shot data by the traditional court division. Therefore, we implement K-means cluster analysis to re-group the shot data of each player. We believe that clustering the shot data for each player will be beneficial to formulate the defending strategy against the player. For a commonly asked question referring to the cluster analysis, the number of clusters, we think the # of clusters is better decided by the user him/herself. The scientific number of clusters can be visualized by the specific shooting plot",align="Justify")),
                     h4("Clustering analysis for all shot data"),
                     p(h5("Firstly, we group all data of the player. Different colors represent different clusters")),
                     fluidRow(column=8,
                              plotOutput("plotc1",width = "800", height = "600"),align="Center"),
                     br(),
                     h4("Clustering analysis for all made shot"),
                     p(h5("Also, a similar analysis is conducted with the data of made shot. The player may be able to shoot more accurately in some specific area. Therefore, clustering the made shot will be also helpful to formulate a specific defending strategy."),align="Justify"),
                     fluidRow(column=8,
                              plotOutput("plotc2",width = "800", height = "600"),align="Center")
                    
                
                   )
                 )),

        tabPanel("Shot Log Download",
                   sidebarLayout(
                     sidebarPanel(
                       textInput("csea", label = h4("Choose Season"), value = "2014-15"),
                       p(" Format: The format of season should follow this format: 2014-2015"),
                       textInput("pid",label =h4("Type in Player ID"),value="977" ),
                       p("The specific player_id can be found on offical website of NBA"),
                       downloadButton('downloadData', 'Download (csv File)')
                     ),
                     mainPanel(dataTableOutput("shotlog"))
                   ))
         ))