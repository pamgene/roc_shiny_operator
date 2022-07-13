library(shiny)
library(shinyjs)

ui <- shinyUI(fluidPage(
  shinyjs::useShinyjs(),
  tags$script(HTML('setInterval(function(){ $("#hiddenButton").click(); }, 1000*30);')),
  tags$footer(shinyjs::hidden(actionButton(inputId = "hiddenButton", label = "hidden"))),
  
  titlePanel("ROC"),
  
  sidebarPanel(
      selectInput("posclass", "Set positive class", choices = "")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("ROC",
               plotOutput("roc")
      ),
      tabPanel("Metrics",
                h3("Positive class used"),
                textOutput("ispos"),
                h3("Confusion Matrix"),
                tableOutput("conmat"),
               fluidRow(
                 column(6, tableOutput("overall")),
                 column(6, tableOutput("byclass"))
               )
      )     
    )
  )
  
))