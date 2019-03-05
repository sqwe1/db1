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
library(ECharts2Shiny)
library(wordcloud)
library(RColorBrewer)

title1 <- "Trustpilot reviews"

shinyUI(
  dashboardPage(
    title = "title1",
    skin = "purple",
    dashboardHeader(title = title1),
    dashboardSidebar(disable = T),
    dashboardBody(
      radioButtons(
        inputId = "dataset",
        label = "Choose a dataset:",
        choices = c(
          "ThreeUK",
          "VodafoneUK",
          "TescoMobile",
          "VirginMobile",
          "EE",
          "O2"
        ),
        inline = T
      ),
      fluidRow(
        infoBoxOutput("No.ofreviews", width = 3),
        infoBoxOutput("Positive", width = 3),
        infoBoxOutput("Neutral", width = 3),
        infoBoxOutput("Negative", width = 3),
        
        
        mainPanel(
          tabsetPanel(
            tabPanel("All companies",
                     box(plotOutput("all"), width = 25)),
            tabPanel("Sentiment Analysis : Volume", fluidRow(box(plotOutput(
              "bar2"
            )), box(plotOutput(
              "pie1"
            )))),
            tabPanel(
              "Sentiments values and Emotion scores",
              fluidRow(
                radioButtons(
                  "inField1",
                  "select a field for emotion",
                  choices = names(ThreeUK[, 3:12]),
                  inline = T
                ),
                sliderInput("bins", "Number of breaks",1, 10, 10),
                
                box(plotOutput("hist3"), width = 6),
                box(plotOutput("histogram2"), width = 6)
              )
            ),
            tabPanel(
              "Word cloud",
              fluidRow(
                radioButtons(
                  "emotion",
                  "Select an emotion to make a word cloud",
                  choices = c("Positive", "Negative"),
                  inline = T
                ),
                box(plotOutput("word10"), width = 6, title = "ThreeUK"),
                box(plotOutput("word11"), width = 6, title = "VodafoneUK"),
                box(plotOutput("word12"), width = 6, title = "O2"),
                box(plotOutput("word13"), width = 6, title = "VirginMobile Mobile"),
                box(plotOutput("word14"), width = 6, title = "EE"),
                box(plotOutput("word15"), width = 6, title = "TescoMobile Mobile")
              )
            )
            
          ),
          style = 'width: 1200px; height: 1000px'
          
          
        )
      )
    )
  )
)
