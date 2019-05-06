 

# public R libraries
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

# helper routines
source("helpers.r")
sss=function(loan_amnt,term,installment,grade,sub_grade,home_ownership,annual_inc,purpose,rate,emp ){
  s=480
  s=s+ 7*(loan_amnt<5000)
  s=s+ 5*(loan_amnt<5000 && loan_amnt<=10000)
  s=s  + 2*(loan_amnt>10000 && loan_amnt<=15000)
  s=s+ -4* (loan_amnt>15000 && loan_amnt<=20000)
  s=s-7*(loan_amnt>20000 && loan_amnt<=30000 ) 
  s=s-13*(loan_amnt>30000   ) 
  s=s+7*(term=="36 months" )
  s=s-20*(term=="60 months")
  s=s+  10*(installment<250)   
  s=s-7*(installment>250 && installment<=500)
  s=s -11*(installment>500 && installment<=750)
  s=s-9*(installment>750 && installment<= 1000)
  s=s+20*(installment > 1000)
  s=s+20*(grade=="A")
  s=s+7*(grade=="B")
  s=s+-2*(grade=="C")
  s=s+-8*(grade=="D")
  s=s+-15*(grade %in%c( "E","F","G")) 
  s=s+28*(sub_grade %in%c( "A1","A2","A3"))
  s=s+18*(sub_grade %in%c( "A4","A5" ))
  s=s+6*(sub_grade %in%c( "B1","B2","B3","B4","B5","C1"))
  s=s-5*(sub_grade %in% c("C2", "C3","C4","C5","D1","D2","D3"))
  s=s-14*(sub_grade %in% c("D4","D5","E1","G4","E2","E3","E4","F1","F2","E5","G5","F3","G2","F4","G1","F5","G3"))
  s=s+5*(home_ownership %in% c( "ANY","MORTGAGE"))  
  s=s+-2*(home_ownership %in% c( "OWN")) 
  s=s-5*(home_ownership %in% c( "RENT","OTHER","NONE")) 
  s=s-27*(annual_inc<45000)
  s=s-7*(annual_inc>45000 && annual_inc<=65000)
  s=s+7*(annual_inc>65000 && annual_inc<=75000)
  s=s+20*(annual_inc>75000 && annual_inc<=90000)
  s=s+37*(annual_inc>90000)
  s=s+14*(purpose %in% c( "car","wedding","major_purchase"))
  s=s+5*(purpose %in% c("debt_consolidation","medical","renewable_energy"))
  s=s+5*(purpose %in% c("credit_card","vacation","house"))
  s=s-2*(purpose %in% c("debt_consolidation","medical","renewable_energy"))
  s=s-8*(purpose %in% c("other","moving","small_business"))
  s=s+31*(rate<=10)
  s=s+7*(rate >10 && rate<=15)
  s=s-12*(rate >15 && rate<=20)
  s=s-27*(rate >20 && rate<=25)
  s=s-32*(rate >25 && rate<=30)
  s=s-3*(emp<=3)
  s=s+4*(emp >3 && emp<=6)
  s=s-1*(emp >6 && emp<=8)
  s=s+1*(emp>8)
  return(s)  }
# temp header
FrissHeader <- list(

  tags$a(href = "http://www.friss.eu/en", tags$img(src="friss_small2.svg", id = "FrissLogo")),

  singleton(includeScript("www/underscore.js")),
  singleton(includeScript("www/jquery-ui.js")),
  singleton(includeScript("www/shinyState.js")),
  singleton(includeScript("www/d3.js")),
  singleton(includeCSS("www/friss.css")),
  singleton(includeCSS("www/app.css")),

  # when shiny is busy the following panels will create an overlay which will hide the main content until shiny is done computing
  div(id = "busy"),
  div(id = "BusyDIV"),

  # add help
  addIntroJS(useVoice=FALSE),

  # add notie type messages
  addNotieToPage()
)

load("dashboard_data.RData")

# init
dataFilters   <-  list()
mapping       <-  list()

# init help
source("initHelp.R")
