
shinyUI(
  
  navbarPage(id="main", windowTitle = "Friss analytics", position = "fixed-top", title = NULL, header = FrissHeader,
             
    # tabPanel(title = "dashboard", value="Dashboard", icon = icon("dashboard"),  addMessageBoxToPage(), frontPanelModuleUI("Dashboard"))ù,
     tabPanel(title = "Credit Scoring",
              h3("Calcul du score client"),
              
              fluidRow(
              
               br() 
              ),
              
              
              tabPanel(                "Test",
                                        br() 
                                       ,fluidRow(
                                         fluidRow( column(3,    FrissC3GaugeOutput("Gauge0"))
                                                   ,column(3,    FrissC3GaugeOutput("G")), 
                                                   column(3,    FrissC3GaugeOutput("G1")),
                                                   column(3,    FrissC3GaugeOutput("G2"))
                                                   )), fluidRow(
                                                     
                                                     br(),br(),column(3, radioGroupButtons('term',list("36 mois"="36 months",
                                                                                                 "60 mois" = "60 months"                                                     ),
                                                                                    
                                                                            label = 'Duree du pret'),
                                                                      
                                                                      radioGroupButtons('grad',list("A"="A","B"="B","C"="C","D"="D","E"="E",      
                                                                                              "F" = "F" ) , 
                                                                        label = 'Grade client')
                                                                      , sliderInput("empi",
                                                                                    label = "Anciennete",
                                                                                    min = 0, max = 15, value = 4
                                                                      )),
                                                     column(3, radioGroupButtons('sub_grad',
                                                                          list( "A1"="A1","B1"="B1","C1"="C1","D1"="D1","E1"="E1",      
                                                                                "F1" = "F1","A2"="A2","B2"="B2","C2"="C2","D2"="D2","E2"="E2",      
                                                                                "F2" = "F2", "A3"="A3","B3"="B3","C3"="C3","D3"="D3","E3"="E3",      
                                                                                "F3" = "F3","A4"="A4","B4"="B4","C4"="C4","D4"="D4","E4"="E4",      
                                                                                "F4" = "F4","A5"="A5","B5"="B5","C5"="C5","D5"="D5","E5"="E5",      
                                                                                "F5" = "F5"),
                                                                            
                                                                          label = 'Sous grade'),
                                                     sliderInput("annual_inc",
                                                                    label = "Salaire annuel",
                                                                    
                                                                    min = 20000, max = 180000, value = 65000
                                                     )),
                                                     column(3,
                                                            radioGroupButtons( 'Home_Ownershi',
                                                                         list( "Location"="RENT" ,"Proprietaire"="OWN","Pret"="MORTGAGE",
                                                                               "Autre"="OTHER", "None"= "NONE"  ), 
                                                                         label = "Propriete Logement" ),      
                                                                                         
                                                                             
                                                            sliderInput("installment",
                                                                           label = "Montant Mensualite",
                                                                           min = 50, max = 1000, value =200
                                                     ), sliderInput("loan_amnt",
                                                                    label = "Montant credit",
                                                                    
                                                                    min = 5000, max = 45000, value = 10000
                                                     )
                                                     
                                                    ), column(3, sliderInput("rat",
                                                                                label = "Taux",
                                                                                min = 0, max = 40, value =7
                                                     ), 
                                                     radioGroupButtons('purpos',
                                                                    list("credit_card"="credit_card","car"="Car","small_business"="small_business",
                                                                                     "other"="other","wedding"=" wedding",      
                                                                                     "debt_consolidation" = "debt_consolidation", 
                                                                                     "home_improvement"="home_improvement","major_purchase"="major_purchase",
                                                                                    "medical"="medical",
                                                                                     "moving"="moving",      
                                                                                     "vacation" = "vacation", "house"="house","renewable_energy"="renewable_energy",
                                                                                    "educational"="educational"  ),
                                                                    
                                                                     label = 'Objectif credit')
                                                     
                                                   ))
                                                   ),
                                       fluidRow(  column(3,
                                                         h4("Grade", style = "text-align:center"),
                                                         FrissC3PieChartOutput("Pi")
                                       ),column(3,
                                                h4("Grade", style = "text-align:center"),
                                                FrissC3PieChartOutput("Pi2")
                                       ),
                                       column(3,
                                              h4("Grade", style = "text-align:center"),
                                              FrissC3PieChartOutput("Pi3")
                                       ),
                                       column(3,
                                              h4("Sexe", style = "text-align:center"),
                                              FrissC3PieChartOutput("Pi4")
                                       )
                                       )), 
      
 
     # tabPanel(title = "prediction", icon = icon("prediction"), value="Filter",filterModuleUI("Filter")),
     
     navbarMenu("reports", icon = icon("bar-chart-o"), tabPanel(title = "hits", value= 'HitsModule',icon = icon("bar-chart-o"),hitsModuleUI("HitsModule")),
                
                tabPanel(title = "flow charts",value="FlowCharts", icon = icon("bar-chart-o"), flowChartModuleUI('FlowCharts'))
     
    )
  )
)
