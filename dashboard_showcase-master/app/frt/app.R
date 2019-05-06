app <- shinyApp(
  
  ui = fluidPage(
    actionButton("Update","Update"),
    FrissC3BarChartOutput("Chart")
  ),
  
  server = function(input, output) {
    output$Chart <- renderFrissC3BarChart({
      
      cat("\nUpdate")
      input$Update
      Data <- data.frame(suspected = runif(3,8,11), investigated =
                           c(6, 9, 1), suspicious = c(7, 9, 12), proven = c(2, 3, 25), row.names =
                           c("actual", "planned", "average"))
      
      # make number of columns data change
      if(input$Update %% 2 == 0) Columns <- 2:3
      else Columns <- 1:3
      
      Data    <- Data[,Columns]
      
      FrissC3BarChart(Data = Data, yUnit = "", width = "100%",
                      height = "500px", padding = list(top = 40, right = 100, bottom = 40, left
                                                       = 100), rotated = FALSE, legend_show = TRUE, legend_position = "bottom")
    })
  }
)

 