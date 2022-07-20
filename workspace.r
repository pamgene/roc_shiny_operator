source("ui.R")
source("server.R")

options("tercen.workflowId"= "09e8f702e70501eafb4338545d05d20f")
options("tercen.stepId"= "af712efa-9233-4199-a29b-4db2cb71424a")

runApp(shinyApp(ui, server))  