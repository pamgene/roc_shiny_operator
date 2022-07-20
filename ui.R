library(shiny)
library(shinyjs)

ui <- shinyUI(fluidPage(
  shinyjs::useShinyjs(),
  tags$script(HTML('setInterval(function(){ $("#hiddenButton").click(); }, 1000*30);')),
  tags$footer(shinyjs::hidden(actionButton(inputId = "hiddenButton", label = "hidden"))),
  
  titlePanel("ROC"),
  
  sidebarPanel(
      selectInput("posclass", "Set positive class", choices = ""),
      sliderInput("thr", "Set threshold", min = 0, max = 1, value = 0.5, step = 0.01),
      actionButton("done", "Return predicted class", enable = FALSE),
      h5(textOutput("msg"))
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("ROC",
               plotOutput("roc")
      ),
      tabPanel("Metrics",
                h4("Positive class used"),
                textOutput("ispos"),
                h4("Confusion Matrix"),
                tableOutput("conmat"),
               fluidRow(
                 column(6, tableOutput("overall")),
                 column(6, tableOutput("byclass"))
               )
      ),
      tabPanel("Waterfall",
               plotOutput("wf")
      )
    )
  )
  
))