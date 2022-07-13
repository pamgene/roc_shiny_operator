source("ui.R")
source("server.R")

options("tercen.workflowId"= "09e8f702e70501eafb4338545d05d20f")
options("tercen.stepId"= "1c3bf4a6-69da-432d-97eb-0cfee44112b2")

runApp(shinyApp(ui, server))  