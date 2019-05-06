###
### Whenever the mapping changes the Mapped data object is updated
### Subsequently the ModuleDataReactive will be triggered and all modules using the this reactive will be updated
###
observeEvent({RV$mapping},
             {
                MappedData$DD.Signaleringen      <- DD.Signaleringen
                MappedData$DD.LogIndicator       <- DD.LogIndicator
                
             }
             ,priority=100) # set high priority such that mapping is always performed before filtering


#' helper function to perform data result mapping
#' mapping: data frame containing the mapping
#' Data: Data to map, this can be eiter DD.Signaleringen or DD.Logindicator
#' MaxNrOfUniqueEntries: Maximum alowed maximum values. This is to prevent fields with many unique values from being loaded in the app
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

# Wrap data to provide to the mapping module in a reactive
# This reactive provides the mapping module with the current mapping and an export of the mapping based on the data
# The data can contain more values then are present in the mapping so the current mapping and mapping export can differ
mappingReactive <- reactive({
  
  isolate({mapping.export <- createMappingExport(MappedData$DD.Signaleringen,RV$mapping)})
  
  return(list(mapping=RV$mapping,mapping.export=mapping.export))})

### 
### Helper function to create downloadable mapping
###
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
createMappingExport <- function(DD.Signaleringen,mapping,MaxNrOfUniqueEntries=100){
  
  ### Remove columns with many unique values
  Columns <- c("label","proces","branche","product")
  nCols   <- length(Columns)
  
  for(i in 1:nCols){
    
    SelectedColumn    <- Columns[i]
    NrOfUniqueEntries <- length(unique(DD.Signaleringen[,SelectedColumn]))
    
    if( NrOfUniqueEntries > MaxNrOfUniqueEntries){
      DD.Signaleringen[,SelectedColumn] <- SelectedColumn
    }
  }
  
  mappingFields <- unique(c("label","proces","branche","product","eindargumentatie",unique(as.character(mapping$field))))
  
  mapping.export <- data.frame(field=character(),from=character(),to=character())
  
  nMappingFields <- length(mappingFields)
  
  for(i in 1:nMappingFields){
    
    orgField <- paste0(mappingFields[i],"_org")
    
    if(!orgField %in% names(DD.Signaleringen))
      orgField <- mappingFields[i]
    
    tbl <- unique(DD.Signaleringen[,c(orgField,mappingFields[i])])
    
    tbl <- cbind(mappingFields[i],tbl)
    names(tbl) <- c("field","from","to")
    
    mapping.export <- rbind(mapping.export,tbl)
  }
  
  return(mapping.export)
  
}
