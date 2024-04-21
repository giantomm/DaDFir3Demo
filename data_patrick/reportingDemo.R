# start clean version Here
# Step 1.0 SETUP ENVIRONMENT, LIBRARIES, SAMPLE DATA 


rm(list = ls(all.names = TRUE))
if (!require("devtools")) install.packages("devtools")
library(devtools) 
if (!require("FEMSdevPkg")) install_github("fnparr/FEMSdevPkg")
library(FEMSdevPkg)
library(data.table) # required for aggregating over tables for reporting
if (!require("openxlsx")) install.packages("openxlsx")
library(openxlsx) #required to write into regulatory reports
#if (!require("funr")) install.packages("funr")
#library(funr)

# 1. set environment and source functions

filepath <- paste0(getwd(), "/inst/code-examples/") #path to this script
source(paste0(filepath, "reportingDemoFiles/fnc_ReportingDemo.R"))
datapath <- paste0(filepath, "reportingDemoFiles/") #path to the demo files
serverURL <- "https://dadfir3-app.zhaw.ch/"
apppath <- paste0(getwd(), "/inst/shiny-examples/DaDFir3-Reporting/")

currentDate <- as.Date(Sys.Date()) #global variable
reportingDate <- as.Date(paste0(year(currentDate), "-01-01")) 

# 2a. Load some interest curves
falling_fp <- paste0(datapath,"/UST5Y_fallingRates.csv")
rising_fp <-  paste0(datapath,"/UST5Y_risingRates.csv")
steady_fp <- paste0(datapath,"/UST5Y_steadyRates.csv")


# 2b. Prepare data to obtain right AcTUS format
rfx_falling <- sampleReferenceIndex(falling_fp,"UST5Y_fallingRates", 
                                    "YC_EA_AAA",100)
rfx_rising <- sampleReferenceIndex(rising_fp,"UST5Y_risingRates",
                                   "YC_EA_AAA",100)
rfx_steady <- sampleReferenceIndex(steady_fp,"UST5Y_steadyRates",
                                   "YC_EA_AAA",100)

#falling_fp_data <- rfx_falling$data
#rising_fp_data <- rfx_rising$data
#plot(falling_fp_data)
#plot(rising_fp_data)
#View(falling_fp_data)


# 4. Load portfolio data and mapping to reporting categories
portfolio_csv <- paste0(datapath,"/AnnuityPortfolio_CHF_USD.csv")
mapping_csv <- paste0(datapath,"/AnnuityPortfolio_IRRBBMapping.csv")



# 5.0 Portfolio Reporting - Cash Flow generation and IRRBB specific mapping
#     Missing up two know: Valuation.
#     In order to get the Economic Value of Equity, we need the valuation of each instrument under a different pre-defined scenario
#     --> the valuation needs to be dependent on the rate shift scenario: rising is baseline +200bp, falling is baseline -200bp.
#     Basis point shocks, in real, might be scenario dependent.

#evTab_rising <- createReportingCashFlowTable_fromPortfolio (portfolio_csv, mapping_csv, ReferenceIndexList =  list(rfx_rising), serverURL = "https://dadfir3-app.zhaw.ch/")
#evTab_falling <- createReportingCashFlowTable_fromPortfolio (portfolio_csv, mapping_csv, ReferenceIndexList =  list(rfx_falling), serverURL = "https://dadfir3-app.zhaw.ch/")
evTab_steadyRates <- createReportingCashFlowTable_fromPortfolio (portfolio_csv, mapping_csv, ReferenceIndexList =  list(rfx_steady), serverURL = "https://dadfir3-app.zhaw.ch/")


# 6a. Report-specific calculation: Add Time Buckets 
evTab_WithDates_steady <- addIRRBBTimeBuckets(EventTable=evTab_steadyRates, ReportingDate = reportingDate) 


# 6b. Save results of mapping
fwrite(evTab_WithDates_steady, paste0(datapath, "evTab_WithDates_steady",".csv"), sep=";",row.names=FALSE)


# 7: Write into Reporting Template based on report specific aggregation rules for each field

# Aggregate according to the specific reports aggregation level
InterestRateCashflow <- copy(evTab_WithDates_steady [(type == "IP") & TimeBucket < 21 & currency =="CHF" ,  .(Cashflow=sum(payoff)), by= .(reportingLine,TimeBucket)])
NominalCashflows <- copy(evTab_WithDates_steady [(type == "PR" | type =="MD") & TimeBucket < 21 & currency =="CHF" , .(Cashflow=sum(payoff)), by= .(reportingLine,TimeBucket)])


# Define the file_path for the reporting document
reportpath <- paste0(datapath,"/ZIR1_CHF.xlsx")  # Replace with your desired file path, orignal files of FINMA/ECB might not be working directly due to formatting issues
wb <-loadWorkbook(reportpath)

# Write in the specific fields of the report, needs to be re-programmed for each report

for (rec in (1:dim(InterestRateCashflow)[1])) {
  openxlsx::writeData(wb, sheet= "Sheet1", x = InterestRateCashflow [rec, Cashflow], startRow = InterestRateCashflow [rec, reportingLine]+1, startCol = 13 + InterestRateCashflow [rec, TimeBucket])
  openxlsx::writeData(wb, sheet= "Sheet1", x = NominalCashflows [rec, Cashflow], startRow = NominalCashflows [rec, reportingLine]+2, startCol = 13 + InterestRateCashflow [rec, TimeBucket])
  }
#Unfortunate: we cannot drill down from the Excel report to the underlying data. Ideally, we would have an interactive mirror of the report in a BI software.

# Save the workbook
saveWorkbook(wb, file = reportpath, overwrite = TRUE)

print(paste("Report under", reportpath, " has been populated."))

#IRRBBreport <- loadWorkbook(reportpath)

# 8.  Open latestshiny application with browser controlled review examples
shiny::runApp(apppath)




