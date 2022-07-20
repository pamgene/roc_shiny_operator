library(shiny)
library(tercen)
library(tidyverse)
library(caret)
library(pROC)
library(ggsci)
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
    getMode(session)
  })
  
  predAtInput = reactive({
    anyPredAtInput(session)
  })

  observe({
    df = dataInput()
    updateSelectInput(session, "posclass", choices = levels(df$class.label))
    shinyjs::disable("done")
    if (predAtInput()){
      shinyjs::disable("thr")
    } else {
      thr.min = df$.y %>%
        min() %>%
        floor()
      thr.max = df$.y %>%
        max() %>%
        ceiling()
      thr.val = mean(c(thr.max, thr.min))
      updateSliderInput(session, "thr", min = thr.min, max = thr.max, value = thr.val)
    }
    
    set_df = reactive({
      result = df %>% 
        setPosClass(input$posclass) %>%
        mutate(observation = (1:n()) %>% as.character)
        if(!predAtInput()){
          result = result %>%
            setPredictedClass(input$thr)
        }
        result
    })
    
    output$roc = renderPlot({
        set_df() %>%
        dplyr::select(class.label,  .y) %>%
        as.data.frame() %>%
        droc() %>%
        plot(print.auc = TRUE)
    })
    
    getMetrics = reactive({
      df_set = set_df()
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
      aTab = getMetrics()$table
      tabdf = data.frame(prediction = rownames(aTab), neg = aTab[,1], pos = aTab[,2])
      colnames(tabdf) = c("prediction", rownames(aTab))
      tabdf
    },title = "Reference")
    
    output$wf = renderPlot({
      wf = set_df() %>%
        ggplot(aes(x = reorder(observation, .y), y = .y, fill = class.label)) + geom_bar(stat = "identity")
      wf + geom_hline(yintercept = input$thr, colour = "gray") + coord_flip() + scale_fill_jama()
    }, 
    height = 650)
    
    m = mode()
    if ( !is.null(m) && m == "run" && !predAtInput()) {
      shinyjs::enable("done")
    }
    
    observeEvent(input$done, {
      shinyjs::disable("done")
      msgReactive$msg = "Running ... please wait ..."
      tryCatch({
        set_df() %>%
          select(.ri, .ci, observation, predicted_class = class.pred )
          ctx$addNamespace() %>%
          ctx$save()
        msgReactive$msg = "Done"
      }, error = function(e) {
        msgReactive$msg = paste0("Failed : ", toString(e))
        print(paste0("Failed : ", toString(e)))
      })
    })
    
  })
})

getValues <- function(session){
  ctx <- getCtx(session)
  df = ctx %>% 
    select(.y, .ri,.ci) 
  
  if(length(ctx$colors) > 1) stop("Define predicted class using single variable as color in Tercen")
  if(length(ctx$labels) != 1) stop("Define known class using a single variable as label in Tercen")
  
  if(length(ctx$colors)>0){
    df = df %>%
      bind_cols(ctx$select(ctx$colors)) %>%
      bind_cols(ctx$select(ctx$labels)) %>%
      setNames(c(".y", ".ri", ".ci", "class.pred", "class.label")) %>%
      mutate(class.pred = class.pred %>% as.factor,
             class.label = class.label %>% as.factor)
  } else {
    df = df %>%
      bind_cols(ctx$select(ctx$labels)) %>%
      setNames(c(".y", ".ri", ".ci", "class.label")) %>%
      mutate(class.label = class.label %>% as.factor,
             class.pred = class.label)
  }
  df
}

getMode <- function(session){
  # retreive url query parameters provided by tercen
  query = parseQueryString(session$clientData$url_search)
  return(query[["mode"]])
}

anyPredAtInput = function(session)({
  ctx <- getCtx(session)
  length(ctx$colors) > 0
})

setPredictedClass = function(d, thr){
    d %>%
      mutate(class.pred = case_when(.y < thr ~ levels(class.label)[2],
                                    TRUE ~ levels(class.label)[1]),
             class.pred = class.pred %>% as.factor)
}

setPosClass = function(d, posclass){
  if (posclass != ""){
    ref  = posclass
  } else {
    ref = levels(d$class.label)[1]
  }
  d %>%
    mutate(class.label = relevel(class.label, ref = ref),
           class.pred = relevel(class.pred, ref = ref))
}

droc = function(df){
  aRoc = roc(response = df[,1], predictor = df[,2])
}

cmat = function(df, posclass = NULL){
  caret::confusionMatrix(df[,1], reference = df[,2], positive = posclass)
}