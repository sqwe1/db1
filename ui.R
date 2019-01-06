#Getting the required packages

# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("ECharts2Shiny")
# install.packages("wordcloud")
# install.packages("RColorBrewer")
# install.packages("plotly")

library(shiny)
library(shinydashboard)
library(plotly)
# library(ECharts2Shiny)
 library(wordcloud)
 library(RColorBrewer)

# title1 <- tags$a(href='https://www.google.com',
#                tags$img(src="radio-tower.png", height = '50', width = '50'),
#                'Trustpilot reviews', target="_blank")
title1<-"Trustpilot reviews"

shinyUI(
  dashboardPage(title="title1", skin = "purple",
    dashboardHeader(title = title1),
     dashboardSidebar(disable = T),
    #   #sliderInput("bins","Number of breaks",1,20,5),
    #   selectInput(inputId = "dataset",
    #               label = "Choose a dataset:",
    #               choices = c("Th","Vo","Te","Vi","EE","Oo"))),
      # selectInput(inputId="emotion",
      #           label = "select an emotion",
      #           choices = c("Positive","Negative"))),
      #selectInput("inField1","Select a field to create a Histogram", choices = names(Th[,3:12])),
      #selectInput("inField2","Choose a month",choices=names(Th[,18:36]))),
    dashboardBody(
      radioButtons(inputId = "dataset",
                   label = "Choose a dataset:",
                   choices = c("Th","Vo","Te","Vi","EE","Oo"),inline = T),
    #   fluidPage(
    #   dateRangeInput("dates", 
    #                  "Date range",
    #                  start = "2015-01-01", 
    #                  end = as.character(Sys.Date())),
    #   textOutput("DateRange")
    # ),
      
                fluidRow(
                  
                  infoBoxOutput("No.ofreviews", width = 3),
                  infoBoxOutput("Positive", width = 3),
                  infoBoxOutput("Neutral", width = 3),
                  infoBoxOutput("Negative", width = 3),

                    # radioButtons(inputId = "dataset",
                    #              label = "Choose a dataset:",
                    #              choices = c("Th","Vo","Te","Vi","EE","Oo"),inline = T),
                  mainPanel(  
                  tabsetPanel(
                      
                  tabPanel("All companies",
                           box(plotOutput("all"),width=25)),
                  tabPanel("Sentiment Analysis : Volume",fluidRow(
                    box(plotOutput("bar2")),box(plotOutput("pie1")))
                  ),
                  tabPanel("Sentiments values and Emotion scores",fluidRow(#box(plotOutput("histogram1")),
                    radioButtons("inField1","select a field for emotion",choices = names(Th[,3:12]),inline = T),
                    sliderInput("bins","Number of breaks",1,10,10),
                    
                                             box(plotOutput("hist3"),width = 6),
                                             box(plotOutput("histogram2"),width = 6))),
                  tabPanel("Word cloud",fluidRow(radioButtons("emotion","Select an emotion to make a word cloud",choices = c("Positive","Negative"),inline = T),
                                                 box(plotOutput("word10"),width = 6,title = "Three"),
                                                box(plotOutput("word11"),width = 6,title = "Vodafone"),
                                                box(plotOutput("word12"),width = 6,title = "O2"),
                                                box(plotOutput("word13"),width = 6,title = "Virgin Mobile"),
                                                box(plotOutput("word14"),width = 6,title = "EE"),
                                                box(plotOutput("word15"),width = 6,title = "Tesco Mobile")
                                                ))
                  
      ),style='width: 1200px; height: 1000px'
      
      
    )
  )
)))


