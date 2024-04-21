#Functions that are required to run reportingDemo

#Overwrite function which is located in import.R 

# Function might be needed in future pilot to load the true and actual rates. 
# taken from Gians Bachelor Thesis

# library(quantmod)
# retrieve_rates <- function(){
#   # Set the FRED API key
#   fred_api_key <- "c26226691feffdfaced45c83539150ca"
#   
#   curve_sections <- c("FEDFUNDS", "GS1M", "GS3M", "GS6M", "GS1", "GS2", "GS3", "GS5", 
#                       "GS7", "GS10", "GS20", "GS30")
#   rate_names <- c("1D", "1M", "3M", "6M", "1Y", "2Y", "3Y", "5Y",
#                   "7Y", "10Y", "20Y", "30Y")
#   
#   options(quandl.api_key = fred_api_key)
#   
#   # Create an empty xts object
#   merged_xts <- xts(order.by = as.Date(character(0)))
#   
#   # Loop over curve_sections and merge each xts object with merged_xts
#   for (rates in curve_sections){
#     rate_xts <- getSymbols(rates, src = "FRED", auto.assign = FALSE)
#     rate_xts <- setNames(rate_xts, rate_names[match(rates, curve_sections)])
#     merged_xts <- merge.xts(merged_xts, rate_xts)
#   }
#   
#   merged_xts <- na.omit(merged_xts)
#   colnames(merged_xts) <- rate_names
#   return(merged_xts)
# }
# 
# rates <- retrieve_rates()
# InterestRateDate <- as.Date(paste0(year(as.Date(Sys.Date())), "-01-01")) # first of year of the respective year
# relevantRates <- rates[index(rates) >= InterestRateDate] 


create_EventTable_fromEventSeries <- function(EventSeries=EventSeries, reportingLine=0, reportingLineDesc=""){
  
  nrOfevents <- dim(EventSeries$events_df)[1]
  contractID <- rep(EventSeries$contractID,nrOfevents) 
  contractType <- rep(EventSeries$contractType,nrOfevents)
  riskFactorID <- rep(EventSeries$riskFactors[[1]]$riskFactorID,nrOfevents)
  reportingLine <- rep(reportingLine,nrOfevents)
  reportingLineDesc <- rep(trimws(reportingLineDesc),nrOfevents)
  
  reportingCashFlowTable <- as.data.table(cbind(contractID, contractType, riskFactorID, EventSeries$events_df, reportingLine, reportingLineDesc)) 
  
  return(reportingCashFlowTable) 
}


createReportingCashFlowTable_fromPortfolio <- function(portfolio_csv, mapping_csv="", ReferenceIndexList =  list(rfx_rising), serverURL) {
  
  
  EventTable <-data.table()
  
  portfolio  <-  samplePortfolio(cdfn = portfolio_csv)
  suppliedPositions <- length(portfolio$contracts)
  if (nchar(mapping_csv) > 1) {
    mapping <- read.csv(header=TRUE, mapping_csv, sep=",", quote="\"")
    suppliedMappings <- dim(mapping)[1]
  } else{
    suppliedMappings <-0
  }
  
  if (suppliedMappings != suppliedPositions)
    print( "Mapping Table is not sufficent - review file")
  
  else{
    
    loops <- length(portfolio$contracts)
    
    for (i in (1:loops)) {
      
      EventSeriesSingleContract <-generateEventSeries(portfolio$contracts[[i]], ReferenceIndexList , serverURL)
      EventTable <- rbind(EventTable,create_EventTable_fromEventSeries(EventSeriesSingleContract, reportingLine = mapping[i,"ReportingLine" ],reportingLineDesc =mapping[i,"ReportingLineDesc" ])) 
      
    }
    
  }
  
  return(EventTable)
  
}


addIRRBBTimeBuckets <- function(EventTable, ReportingDate=as.Date(Sys.Date())){
  #Shows when new columns is created through shallow copies, i.e. by reference
  #options(datatable.verbose=TRUE)
  #options(datatable.verbose=FALSE)
  #Functions adds time buckets
  tempTable <- copy(EventTable)
  tempTable[, Date := as.Date(time)]
  tempTable[, ReportingDate := ReportingDate] #should be made flexible
  tempTable[ , DiffToReportingDate := .(as.Date(time)- ReportingDate), ]
  tempTable[ , TimeBucket := 999, ]
  #EventTable_WithDates <- EventTable_WithDates[ , TimeBucket := NULL, ] to delete a column
  
  tempTable[ DiffToReportingDate > 0 & DiffToReportingDate <= 1  , TimeBucket := 1 ]
  tempTable[ DiffToReportingDate > 1 & DiffToReportingDate <= 30  , TimeBucket := 2 ]
  tempTable[ DiffToReportingDate > 30 & DiffToReportingDate <= 90  , TimeBucket := 3 ]
  tempTable[ DiffToReportingDate > 90 & DiffToReportingDate <= 180  , TimeBucket := 4 ]
  tempTable[ DiffToReportingDate > 180 & DiffToReportingDate <= 270  , TimeBucket := 5 ]
  tempTable[ DiffToReportingDate > 270 & DiffToReportingDate <= 365  , TimeBucket := 6 ]
  tempTable[ DiffToReportingDate > 365 & DiffToReportingDate <= 545  , TimeBucket := 7 ]
  tempTable[ DiffToReportingDate > 545 & DiffToReportingDate <= 730  , TimeBucket := 8 ] #18 monts to 2 years
  tempTable[ DiffToReportingDate > 730 & DiffToReportingDate <= 1095  , TimeBucket := 9 ] #2 to 3 years
  tempTable[ DiffToReportingDate > 1095 & DiffToReportingDate <= 1460  , TimeBucket := 10 ] #up to 4
  tempTable[ DiffToReportingDate > 1460 & DiffToReportingDate <= 1825  , TimeBucket := 11] # up 5
  tempTable[ DiffToReportingDate > 1825 & DiffToReportingDate <= 2190  , TimeBucket := 12 ] #up 6y
  tempTable[ DiffToReportingDate > 2190 & DiffToReportingDate <= 2555  , TimeBucket := 13 ] #up 7y
  tempTable[ DiffToReportingDate > 2555 & DiffToReportingDate <= 2920  , TimeBucket := 14 ]#up8y
  tempTable[ DiffToReportingDate > 2920 & DiffToReportingDate <= 3285  , TimeBucket := 15 ] #up 9y
  tempTable[ DiffToReportingDate > 3285 & DiffToReportingDate <= 3650  , TimeBucket := 16 ] #up10y
  tempTable[ DiffToReportingDate > 3650 & DiffToReportingDate <= 5475  , TimeBucket := 17 ] #up 15
  tempTable[ DiffToReportingDate > 5475 & DiffToReportingDate <= 7300  , TimeBucket := 18 ] #up 20y
  tempTable[ DiffToReportingDate > 7300 , TimeBucket := 19 ] #> 20y
  
  outTable <- copy(tempTable) 
  return(outTable)
}



## TUTORIAL - UNDERSTAND TYPE
# type
# option: 0, identifier: monitoring, name: Monitoring, acronym: AD, description: Monitoring of contract. Evaluates all contract states, sequence: 22
# option: 1, identifier: initialExchange, name: Initial Exchange, acronym: IED, description: Scheduled date of initial exchange of e.g. principal value in fixed income products, sequence: 1
# Option: 2, identifier: feePayment, name: Fee Payment, acronym: FP, description: Scheduled fee payments, sequence: 2
# option: 3, identifier: principalRedemption, name: Principal Redemption, acronym: PR, description: Scheduled principal redemption payment, sequence: 3
# option: 4, identifier: principalDrawing, name: Principal Drawing, acronym: PD, description: Drawing of principal amount e.g. in a credit line, sequence: 4
# option: 5, identifier: principalPaymentAmountFixing, name: Principal Payment Amount Fixing, acronym: PRF, description: Scheduled fixing of principal payment amount, sequence: 5
# option: 6, identifier: penalytPayment, name: Penalty Payment, acronym: PY, description: Scheduled payment of a penalty, sequence: 6
# option: 7, identifier: principalPrepayment, name: Principal Prepayment, acronym: PP, description: Unscheduled early repayment of principal, sequence: 7
# option: 8, identifier: interestPayment, name: Interest Payment, acronym: IP, description: Scheduled interest payment, sequence: 8
# option: 9, identifier: interestCapitalization, name: Interest Capitalization, acronym: IPCI, description: Scheduled capitalization of accrued interest, sequence: 9
# option: 10, identifier: creditEvent, name: Credit Event, acronym: CE, description: Credit event of counterparty to a contract, sequence: 10
# option: 11, identifier: rateResetFixed, name: Rate Reset Fixing with Known Rate, acronym: RRF, description: Scheduled fixing of variable rate with known new rate, sequence: 11 
# option: 12, identifier: rateResetVariable, name: Rate Reset Fixing with Unknown Rate, acronym: RR, description: Scheduled fixing of variable rate with unknown new rate, sequence: 12
# option: 13, identifier: dividendPayment, name: Dividend Payment, acronym: DV, description: Payment of dividends, sequence: 13
# option: 14, identifier: purchase, name: Purchase, acronym: PRD, description: Purchase of a contract, sequence: 14
# option: 15, identifier: marginCall, name: Margin Call, acronym: MR, description: Scheduled margin call, sequence: 15
# option: 16, identifier: termination, name: Termination, acronym: TD, description: Termination of a contract, sequence: 16
# option: 17, identifier: scalingIndexFixing, name: Scaling Index Fixing, acronym: SC, description: Scheduled fixing of a scaling index, sequence: 17
# option: 18, identifier: interestCalculationBaseFixing, name: Interest Calculation Base Fixing, acronym: IPCB, description: Scheduled fixing of the interest calculation base, sequence: 18
# option: 19, identifier: maturity, name: Maturity, acronym: MD, description: Maturity of a contract, sequence: 19
# option: 20, identifier: exercise, name: Exercise, acronym: XD, description: Exercise of a contractual feature such as an optionality, sequence: 20
# option: 21, identifier: settlement, name: Settlement, acronym: STD, description: Settlement of an exercised contractual claim, sequence: 21
# option: 22, identifier: boundaryMonitor, name: Boundary Monitor, acronym: BDM, description: Compare asset price with boundary value, sequence: 22 
# option: 23, identifier: boundary, name: Boundary, acronym: BDC, description: Underlying asset price crossed boundary, sequence 23 



### TUTORIAL - UNDERSTAND DATA.TABLE PACKAGE
## all interest rate Cash Flows due from Banks
#https://www.statology.org/filter-data-table-in-r/
# EventTable[reportingLine == 3, "reportingLineDesc"][1]
# EventTable[reportingLine == 3,]
# EventTable[reportingLineDesc == "Due from banks",]
# EventTable[reportingLine == 3 & type %in% c("IP", "PR", "MD"), ]
# EventTable[reportingLine == 3 & type == "MD", sum(payoff)]
# temp <- EventTable[type== "IP" | type=="MD", c("type", "time", "payoff", "currency")  ]
# temp [order(type),]
# temp2 <- EventTable[type== "IP" | type=="MD", .(InterestType = type, Cashflow =payoff)]
# temp2
# temp3 <- EventTable[type== "IP" | type=="MD", .(AverageCashflow = mean(payoff))]
# temp3
# temp4 <- EventTable[type== "IP" | type=="MD", . (.N), by =.(type) ]
# temp4
# temp5 <- EventTable[type== "IP" | type=="MD", .(PayoffSum = sum(payoff), InceptionNominal= max(nominalValue)), by =.(time,contractID, riskFactorID,currency) ]
# temp5
#set(EventTable, i=which(EventTable[["type"]]==""), j="NoType", value=NA)
###-----------------------###