library(shiny)
library(tercen)
library(tidyverse)
library(caret)
library(pROC)

############################################
#### This part should not be modified
getCtx <- function(session) {
  # retreive url query parameters provided by tercen
  query <- parseQueryString(session$clientData$url_search)
  token <- query[["token"]]
  taskId <- query[["taskId"]]
  
  # create a Tercen context object using the token
  ctx <- tercenCtx(taskId = taskId, authToken = token)
  return(ctx)
}
####
############################################

server <- shinyServer(function(input, output, session) {
  
  dataInput <- reactive({
    getValues(session)
  })
  
  mode = reactive({
    getMode()
  })
  
  observe({
    df = dataInput()
    updateSelectInput(session, "posclass", choices = levels(df$class.label))
    
    setPosClass = reactive({
        if (input$posclass != ""){
          ref  = input$posclass
        } else {
          ref = levels(df$class.label)[1]
        }
        df %>%
          mutate(class.label = relevel(class.label, ref = ref),
                 class.pred = relevel(class.pred, ref = ref))
      
    })
    
    output$roc = renderPlot({
      #browser()
      setPosClass() %>%
        dplyr::select(class.label,  .y) %>%
        as.data.frame() %>%
        droc() %>%
        plot(print.auc = TRUE)
    })
    
    getMetrics = reactive({
      df_set = setPosClass()
      df_set %>%
        dplyr::select(class.pred, class.label) %>%
        as.data.frame() %>%        
        cmat(posclass = levels(df_set$class.label)[1])
    })
    
    output$overall = renderTable({
      res = getMetrics()
      data.frame(parameter = res$overall %>% names, value = res$overall)
    })
    
    output$byclass = renderTable({
      res = getMetrics()
      data.frame(parameter = res$byClass %>% names, value = res$byClass)
    })
    
    output$ispos = renderText({
      res = getMetrics()
      res$positive
    })
    
    output$conmat = renderTable({
      res = getMetrics()
      res$table %>%
        as.data.frame()
    })
  })
})

getValues <- function(session){
  ctx <- getCtx(session)
  df = ctx %>% 
    select(.y, .ri,.ci) 
  if(length(ctx$colors) != 1) stop("Define predicted class using single variable as color in Tercen")
  if(length(ctx$labels) != 1) stop("Define known class using a single variable as label in Tercen")
  
  df %>%
    bind_cols(ctx$select(ctx$colors)) %>%
    bind_cols(ctx$select(ctx$labels)) %>%
    setNames(c(".y", ".ri", ".ci", "class.pred", "class.label")) %>%
    mutate(class.pred = class.pred %>% as.factor,
           class.label = class.label %>% as.factor)
}

getMode <- function(session){
  # retreive url query parameters provided by tercen
  query = parseQueryString(session$clientData$url_search)
  return(query[["mode"]])
}

droc = function(df){
  aRoc = roc(response = df[,1], predictor = df[,2])
}

cmat = function(df, posclass = NULL){
  caret::confusionMatrix(df[,1], reference = df[,2], positive = posclass)
}