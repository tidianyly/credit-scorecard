# helper function to perform data result mapping
PerformMapping <- function(mapping,Data, MaxNrOfUniqueEntries = 100){

  ###
  Columns <- c("label","proces","branche","product")
  nCols   <- length(Columns)

  for(i in 1:nCols){

    SelectedColumn    <- Columns[i]
    NrOfUniqueEntries <- length(unique(Data[,SelectedColumn]))

    if( NrOfUniqueEntries > MaxNrOfUniqueEntries){
      Data[,SelectedColumn] <- SelectedColumn
    }
  }
  
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
  ### Perform mapping
  nMappings  <- nrow(mapping)

  uFields    <- unique(mapping$field)
  fields_org <- paste0(uFields,"_org")

  Data[,fields_org] <- Data[,uFields]

  # Apply mapping to currently loaded data
  nVars   <- length(unique(uFields))

  for(i in 1:nVars){

    selectedVar <- as.character(uFields[i])
    posMap      <- match(Data[,selectedVar],mapping$from)
    posMap      <- posMap[!is.na(posMap)]
    indData     <- Data[,selectedVar] %in% mapping$from[posMap]

    posTo      <- match(Data[,selectedVar],mapping$to)
    posTo      <- posTo[!is.na(posTo)]
    indAlreadyMapped <- Data[,selectedVar] %in% mapping$from[posTo]

    # Only set endresult fields to unmapped if no mappings is available
    if(selectedVar=='eindargumentatie')
      Data[!indAlreadyMapped,selectedVar]          <- 'UNMAPPED'

    Data[indData,selectedVar]   <- as.character(mapping$to[posMap])

  }

  return(Data)
}

# shorthand function to constuct Pie chart
DashBoardPie <- function(PieData){
  gvisPieChart(PieData,options=list(width= "100%", pieHole = 0.4, legend = 'bottom',
                                    backgroundColor = '{fill: "transparent"}',
                                    chartArea = '{left:0,top:10,width:"100%",height:"80%"}'))
}

GetFrontPanelData <- function(DD.Signaleringen,ResultType){

  # determine time stats last alert
  m          <- which.max(DD.Signaleringen$TimeJS)
  MaxYear    <- DD.Signaleringen$Year[m]
  MaxQuarter <- DD.Signaleringen$Quarter[m]
  MaxMonth   <- DD.Signaleringen$MonthName[m]
  MaxWeek    <- DD.Signaleringen$Week[m]

  # for each scenario determine which records belong to it
  m1         <- which(DD.Signaleringen$Year == MaxYear)
  m2         <- which(DD.Signaleringen$Year == MaxYear & DD.Signaleringen$Quarter    == MaxQuarter)
  m3         <- which(DD.Signaleringen$Year == MaxYear & DD.Signaleringen$MonthName  == MaxMonth)
  m4         <- which(DD.Signaleringen$Year == MaxYear & DD.Signaleringen$Week       == MaxWeek)

  # get total counts
  total1     <- length(m1)
  total2     <- length(m2)
  total3     <- length(m3)
  total4     <- length(m4)

  # get counts for result type
  count1     <- sum(DD.Signaleringen$eindargumentatie[m1] == ResultType)
  count2     <- sum(DD.Signaleringen$eindargumentatie[m2] == ResultType)
  count3     <- sum(DD.Signaleringen$eindargumentatie[m3] == ResultType)
  count4     <- sum(DD.Signaleringen$eindargumentatie[m4] == ResultType)

  DD <- data.frame(Gauge  = c("year","quarter","month","week"),
                   Total  = c(total1,total2,total3,total4),
                   Count  = c(count1,count2,count3,count4))

  DD$Percentage = round(100*DD$Count / DD$Total,2)

  Max           = max(DD$Percentage,na.rm=TRUE)
  DD$Max        = min(c(Max - (Max %% 10) + 10, 100),na.rm=TRUE)

  GaugeData <- DD

  # additional gauge data
  tt1 <- table(DD.Signaleringen$eindargumentatie[m1])
  tt2 <- table(DD.Signaleringen$eindargumentatie[m2])
  tt3 <- table(DD.Signaleringen$eindargumentatie[m3])
  tt4 <- table(DD.Signaleringen$eindargumentatie[m4])

  tt1p <- round(100*prop.table(tt1),2)
  tt2p <- round(100*prop.table(tt2),2)
  tt3p <- round(100*prop.table(tt3),2)
  tt4p <- round(100*prop.table(tt4),2)

  GaugeDataAll <- list(tt1=tt1,tt2=tt2,tt3=tt3,tt4=tt4,tt1p=tt1p,tt2p=tt2p,tt3p=tt3p,tt4p=tt4p)

  #return result
  L <- list(GaugeData=GaugeData, GaugeDataAll=GaugeDataAll)
  return(L)
}


# creates table of counts and percentages for a given input vector x
GetCountTable <- function(x,Name, Decreasing = TRUE, Digits = 3){
  x          <- as.character(x)
  tt         <- table(x)
  tt         <- tt[order(tt,decreasing = Decreasing)]
  tt.p       <- round(100*prop.table(tt),Digits)
  Names      <- names(tt)
  Count      <- tt
  Percentage <- tt.p
  dd         <- data.frame(Names, Count, Percentage, row.names=NULL)
  colnames(dd) <- c(Name,"Count","Percentage")
  return(dd)
}

# headers with centered text
h1c <- function(...){h1(..., style = "text-align:center")}
h2c <- function(...){h2(..., style = "text-align:center")}
h3c <- function(...){h3(..., style = "text-align:center")}
h4c <- function(...){h4(..., style = "text-align:center")}
h5c <- function(...){h5(..., style = "text-align:center")}
h6c <- function(...){h6(..., style = "text-align:center")}


###
### createDateFilter ESC
###

createDateFilter <- function(dateVector){

  # initialize data filter ----
  DateFilter <- renderUI({

    Dates <- ymd(as.character( substr(dateVector,1,10)))
    start <- as.character(min(Dates,na.rm=TRUE))
    end   <- as.character(max(Dates,na.rm=TRUE))

    dateRangeInput("DateRange", "Date range:",
                   start  = start,
                   end    = end,
                   min    = start,
                   max    = end,
                   format = "yy/mm/dd",
                   separator = " - ")
  })

  return(DateFilter)

}


#' Adds javascript message handler to the page
#' @keywords javascript message handler
#' @export
addMessageHandler <- function() {

  return(tags$head(tags$script(HTML(
    'Shiny.addCustomMessageHandler("jsCode",
    function(message) {
    console.log(message)
    eval(message.code);
    }
    );'))))
}

#' GetClientName
#'
#' get client name assuming current directory name ends with it
#' @export
#'

GetClientName <- function(){
  x <- getwd()
  x <- unlist(strsplit(x = getwd(),"/"))
  return(x[length(x)-2])
}

### Disables a ui control from the server side
disableControl <- function(id,session,bDisable=TRUE) {

  if(bDisable==TRUE){
    session$sendCustomMessage(type="jsCode",
                              list(code= paste("$('#",id,"').prop('disabled',true)"
                                               ,sep="")))
  }else{
    session$sendCustomMessage(type="jsCode",
                              list(code= paste("$('#",id,"').prop('disabled',false)"
                                               ,sep="")))
  }
}

### Wrapper for disableControl to enable controls
enableControl <- function(id,session,bEnable=TRUE) {
  disableControl(id,session,bDisable=!bEnable)
}

frissColorSort <- function(vector){

  # Sort colors according to the friss priority
  frissColors <- c('GREEN','ORANGE','RED','OTHER')
  pos     <- match(frissColors,vector)
  pos     <- pos[!is.na(pos)]
  pos.tmp <- 1:length(vector)
  pos     <- c(pos,setdiff(pos.tmp,pos))
  return(pos)
}
