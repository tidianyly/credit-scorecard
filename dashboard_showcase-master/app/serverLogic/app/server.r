options(tz="Africa/Casablanca")
shinyServer(function(input, output, session){

  ### This reactive lets the module know which page is currently active
  ### It is used to load the proper help data with the current dynamic values
  HelpReactive <- reactive({
    input$main
  })
  
  ###
  ### Modules
  ###
  
   # GlobalDataOverview
   callModule(frontPanelModule,"Dashboard",ModuleDataReactive,HelpReactive,helpData[["Dashboard"]],nextPage=NA)
  
   # FlowCharts
   callModule(module = flowChartModule   , id = "FlowCharts"      , ModuleDataReactive,HelpReactive,helpData[["FlowCharts"]],nextPage=NA)

   # HitsModule
   callModule(module = hitsModule, id = "HitsModule",ModuleDataReactive,HelpReactive,helpData[["HitsModule"]],nextPage=NA)

  ###
  ### filter and help system logic
  
  
  ModuleResults <- reactiveValues(filterResult = reactive({}))  # filterSelection is the reactive expression returned from the filter module
  # and contains the current selected groups
  output$Gauge0 <- renderFrissC3Gauge({
    a= data.frame(loan_amnt=input$loan_amnt,annual_inc=input$annual_inc, purpose=input$purpos,
                  installment=input$installment,emp=input$empi,
                  term=input$term,rate=input$rat,
                  grade=input$grad,sub_grade=input$sub_grad,
                  Home_Ownership=input$Home_Ownershi)
    FrissC3Gauge(value = sss(a$loan_amnt,a$term,a$installment,a$grade,a$sub_grade, a$Home_Ownership, a$annual_inc, a$purpose, 
                             a$rate, a$emp), min = 300, max = 600, text = "Score client",
                 gaugeWidth = 20, color = "red", showMinMax = TRUE, width = NULL,
                 height = NULL, transition = 250)
  })
  output$G <- renderFrissC3Gauge({
    
    FrissC3Gauge(value = 0.217 , min =  0, max = 1, text = "PD Moyen",
                 gaugeWidth = 20, color = "red", showMinMax = TRUE, width = NULL,
                 height = NULL, transition = 250)
  })
  output$G1 <- renderFrissC3Gauge({
     
    FrissC3Gauge(value = 40225 , min =  0, max = 185159, text = "Nombre de defauts",
                 gaugeWidth = 20, color = "red", showMinMax = TRUE, width = NULL,
                 height = NULL, transition = 250 )
  })
  output$G2 <- renderFrissC3Gauge({
    
    FrissC3Gauge(value = 12964.18 , min =  0, max = 35000, text = "Montant Moyen de pret",
                 gaugeWidth = 20, color = "red", showMinMax = TRUE, width = NULL,
                 height = NULL, transition = 250 )
  })
  
  ####
  output$teb = renderDataTable(data.frame( loan_amnt=input$loan_amnt,annual_inc=input$annual_inc, purpose=input$purpos,
                                           installment=input$installment,emp=input$empi,
                                           term=input$term,rate=input$rat,
                                           grade=input$grad,sub_grade=input$sub_grad,
                                           Home_Ownership=input$Home_Ownershi  ))
  
  ####
  output$Pi <-  renderFrissC3PieChart({
   
    
    FrissC3PieChart(data.frame(A = 30430, B = 55450	,C=47513, D = 30134,E=14204,F=1496),height=250, legendPosition='right',dataHidden=diff)
  })
  output$Pi2 <-  renderFrissC3PieChart({
    FrissC3PieChart(data.frame(ANY= 1, MORTGAGE=88461, NONE =24, OTHER= 109,OWN =15741,RENT=80823),height=250, legendPosition='right',dataHidden=diff)
  })
  output$Pi3 <-  renderFrissC3PieChart({
    
    
    FrissC3PieChart(data.frame(car=2942, credit_card=35491, debt_consolidation=108294, educational=287, home_improvement=10364, house=1262, major_purchase=4892,
                               medical=2143, moving=1575, other=11064, renewable_energy=199, small_business= 3712,vacation=1231, wedding=1703),height=250, legendPosition='right',dataHidden=diff)
  })
  output$Pi4 <-  renderFrissC3PieChart({
    
    
    FrissC3PieChart(data.frame( Homme= 77,femme=23), legendPosition='right',dataHidden=diff)
  })
  
  RV <- reactiveValues(signaleringen.Filter = NULL,          # signaleringen.Filter contains the various groups that should be filtered out.
                       # It is extracted from the filterResult reactive.
                       storedFilters        = dataFilters, # contains list with saved filters. If a filter with saved filters (filters.RData)
                       # is present its contents will be put here. If not dataFilters is initialized with 
                       # an empty list in global.r
                       applyFilter          = FALSE,         # applyFilter indicates wheater the current filter should be applied
                       # applyFilter can become true when apply filter is clicked from the filter module OR
                       # when an initial default filter is loaded at startup.
                       mapping              = mapping,       # contains either a saved mapping if one is present or the default mapping
                       mapData              = TRUE,          # triggers mapping of the data
                       init                 = TRUE,          # Flag indicating we are starting the app. This flag will be set to false after the 
                       # initial filter is loaded
                       dynamicValues        = list()
  )
  output$h1 <- renderHighchart({
    highchart() %>%
      hc_xAxis(categories =  f$Variable ) %>%
      hc_add_series(data = f , type = "bar", hcaes(y =points, color = bin),
                    showInLegend = 1) 
       
  })
  # Mapped data contains the mapped unfiltered data.
  # We use this data to provide the filter app with all fields it can filter on
  MappedData <- reactiveValues(DD.Signaleringen = DD.Signaleringen)
  
  # This reactive is what is actualy passed on to the modules. When the filter is updated this reactive is triggered and all modules will be updated.
  # This reactive filters mappedData
  ModuleDataReactive <- reactive({
    
    # Make sure filter is not empty
    validFilter <- sum(RV$filterResult$Filter.signaleringen)>0
    
    if(validFilter){
      Filtered.DD.Signaleringen <- MappedData$DD.Signaleringen[RV$filterResult$Filter.signaleringen,]
      Filtered.DD.LogIndicator  <- MappedData$DD.LogIndicator [RV$filterResult$Filter.logindicator, ]
    }else{
      Filtered.DD.Signaleringen <- MappedData$DD.Signaleringen
      Filtered.DD.LogIndicator  <- MappedData$DD.LogIndicator 
    }
    
    return(list(DD.Signaleringen=Filtered.DD.Signaleringen,DD.LogIndicator=Filtered.DD.LogIndicator))
  })
  
  source("serverLogic/mappingLogic.R",local = TRUE)
  source("serverLogic/filterLogic.R", local = TRUE)
  
  ###
  ### This block makes sure help is automatically started if we click to the next page from the help
  ###
  observeEvent({input$autoStartHelp},{
    if(input$autoStartHelp==1){
      session$sendCustomMessage("startHelp",NA)
    }
  })
  
  
  
})


