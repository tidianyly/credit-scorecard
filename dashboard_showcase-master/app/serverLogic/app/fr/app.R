library(shiny)
library(googleVis)
library(plyr)
library(dplyr)
library(reshape2)
library(DT)
library(RColorBrewer)

# custom R libraries
library(FrissC3Charts)
library(FrissMessageBox)
library(FrissIntroJS)
library(FrissSwitch)
library(FrissNotie)

# module definitions
source("modules/filterModule.R")
source("modules/piesModule.R")
source("modules/frontPanelModule.R")
source("modules/flowChartModule.R")
source("modules/hitsModule.R")

if (interactive()) {
  
  ui <- fluidPage(
    checkboxGroupButtons(inputId = "somevalue",
                         label = "Make a choice: ",
                         choices = c("A", "B", "C")),
    verbatimTextOutput("value")
  )
  server <- function(input, output) {
    output$value <- renderText({ input$somevalue })
  }
  shinyApp(ui, server)
}
