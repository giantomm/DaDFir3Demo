shinycssloaders::withSpinner(
  plotOutput("plot")
)

# Define UI for application that visualizes cashflows for a example bond
fluidPage(
  theme = shinytheme("cerulean"),
  #top images 
  img(src = "actus-logo.png", height = 77, width = 220,
      style="float:right; padding-right:25px"),
  img(src="Logo_Weiss.png",height = 80, width = 100),
  # centered
  img(src = "WIP.png", height = 80, width = 150,
      style = "margin 0 auto;"),
  # Title and bar with tabs
  navbarPage("
             DaDFiR3 Demo",   #navbar App title
             tabPanel("Financial Model",
                      sidebarLayout(
                        sidebarPanel(
                          with = 3,
                          # textInput for the financial model ID
                          textInput(inputId = "financialModelID",
                                    label = "Financial Model ID",
                                    placeholder = "Enter a Financial Model ID"
                          ),
                          # textInput for the financial model description
                          textInput(inputId = "financialModelDescription",
                                    label = "Financial Model Description",
                                    placeholder = "Enter a description"
                          ),
                          # textInput for the enterprise ID
                          textInput(inputId = "enterpriseID",
                                    label = "Enterprise ID",
                                    placeholder = "Enter an enterprise ID"
                          ),
                          # selectInput for the accounts
                          selectInput(inputId = "selectAccounts",
                                      label = "Choose an Institution", 
                                      choices = c("Model Bank", "Uploaded"),
                                      selected = "Model Bank"
                          ),
                          # fileInput for the accounts
                          fileInput(
                            inputId = "accountsFile",
                            label = "Upload a yaml-file",
                            accept = c(".yaml")
                          ),
                          # selectInput for the portfolio
                          selectInput(inputId = "selectPortfolio",
                                      label = "Choose a Portfolio", 
                                      choices = c("Test Portfolio", "Uploaded"),
                                      selected = "Test Portfolio"
                          ),
                          # fileInput for the portfolio
                          fileInput(
                            inputId = "portfolioFile",
                            label = "Upload a csv-file",
                            accept = c(".csv")
                          ),
                          # selectInput for the currency
                          selectInput(inputId = "selectCurrency",
                                      label = "Choose a Currency", 
                                      choices = c("USD", "CHF", "EUR"),
                                      selected = "USD"
                          ),
                          h4("Setup Timeline"),
                          # dateInput for the Status date
                          dateInput(inputId = "statusDate",
                                    label = "Status Date",
                                    value = "2023-01-01"
                          ),
                          # # numericInput for the months per period
                          # numericInput(inputId = "monthsPerPeriod",
                          #              label = "Months per Period",
                          #              value = 3
                          # ),
                          # # numericInput for the reports
                          # numericInput(inputId = "reportCount",
                          #              label = "Reports",
                          #              value = 4
                          # ),
                          # # numericInput for the periods
                          # numericInput(inputId = "periodCount",
                          #              label = "Periods",
                          #              value = 8
                          # ),
                          # input for report frequency
                          selectInput(inputId = "reportFrequency",
                                      label = "Report Frequency",
                                      choices = c("Quarterly", "Semi-Annual", "Annual"),
                                      selected = "Annual"
                          ),
                          # input for the number of reports
                          numericInput(inputId = "reportCount",
                                       label = "Number of Reports",
                                       value = 2
                          ),
                          # custom ReportDates
                          checkboxInput(inputId = "customReportDates",
                                        label = "Custom Report Dates",
                                        value = FALSE
                          ),
                          uiOutput("customReportDates"),
                          # actionButton for setting the financial model
                          actionButton(inputId = "setFinancialModelButton",
                                       label = "Set Financial Model",
                                       width = "100%"
                          ),
                        ),  #sidebar panel close
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Current Settings",
                                     h4("Accounts Tree"),
                                     verbatimTextOutput("curAccounts"),
                                     h4("Mapping"),
                                     DTOutput("curTreemap"),
                                     h4("Portofolio"),
                                     DTOutput("curPortfolioTable"),
                                     h4("Timeline"),
                                     verbatimTextOutput("curTimeline"),
                                     ),
                            tabPanel("Stored Financial Model",
                                     h4("Accounts Tree"),
                                     verbatimTextOutput("fmAccounts"),
                                     h4("Mapping"),
                                     DTOutput("fmTreemap"),
                                     h4("Timeline"),
                                     verbatimTextOutput("fmTimeline"),
                                     ),
                          ), #tabsetPanel close
                        ),  #main panel close
                      ),  #sidebarLayout close
             ), #tabpanel close
             tabPanel("Modify Financial Model",
                      sidebarLayout(
                        # sidebar panel with input for the institution
                        sidebarPanel(width = 3,
                                     # selectInput for the type of contract
                                     selectInput("selectContractAttribute",
                                                 "Select Contract Type",
                                                 choices = c("ACTUS Contract", "Formula Contract"),
                                                 selected = "ACTUS Contract"
                                     ),
                                     # textInput for adding a contract
                                     textInput(inputId = "contractID",
                                               label = "Contract ID",
                                               placeholder = "Enter a contract ID"
                                     ),
                                     h4("Here we will have a Contract Creation Tool"),
                                     # selectInput for the Nod eto which the new contract should belong
                                     selectInput("selectNode",
                                                 "Select Node for the Contract",
                                                 choices = NULL),
                                     # actionButton for adding a contract
                                     actionButton(inputId = "addContractButton",
                                                  label = "Add"
                                     ),
                                     # actionButton to apply the changes
                                     actionButton(inputId = "applyChangesButton",
                                                  label = "Apply Changes"
                                     ),
                        ),  #sidebar panel close
                        # main panel with the tree
                        mainPanel(
                          h4("Contract Created"),
                          # output the contract created
                          verbatimTextOutput("contractCreated"),
                          verbatimTextOutput("addingContract"),
                          h4("Current Map of the Tree"),
                          # output the current treemap
                          DTOutput("treemap"),
                          h4("Current Portfolio"),
                          # output the current portfolio
                          # DTOutput("portfolioTable"),
                        )  #main panel close
                      )  #sidebar close
             ),  #tabpanel close
             tabPanel("Simulation",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          # textInput for the analysis ID
                          textInput(inputId = "scenarioID",
                                    label = "Scneario ID",
                                    placeholder = "Enter a Scenario ID"
                          ),
                          # selectInput for the YieldCurve
                          selectInput(inputId = "selectYieldCurve",
                                      label = "Choose a Yield Curve", 
                                      choices = c("Flat", "Steep"),
                                      selected = "Flat"
                          ),
                          # selectInput for the scenario
                          selectInput(inputId = "selectScenario",
                                      label = "Choose a Scenario", 
                                      choices = c("steady", "rising", "falling", "recovering", "custom"),
                                      selected = "steady"
                          ),
                          h4("For custom reference Index"),
                          fileInput(
                            inputId = "scenarioFile",
                            label = "Upload a csv-file",
                            accept = c(".csv")
                          ),
                          textInput(inputId = "refID",
                                    label = "Reference Index ID",
                                    placeholder = "Enter an ID"
                          ),
                          textInput(inputId = "moc",
                                    label = "Market Object Code",
                                    placeholder = "Enter a MOC"
                          ),
                          selectInput(inputId = "base",
                                      label = "Base of the Values",
                                      choices = c(100, 1),
                                      selected = 100
                          ),
                          # actionButton for setting the analysis
                          actionButton(inputId = "setAnalysisButton",
                                       label = "Simulate"
                          ),
                        ), #sidebar panel close
                        mainPanel(
                          h4("Current Scenario"),
                          plotOutput("ratesPlot"),
                          h4("Simulation Log"),
                          h5("Events Generated"),
                          htmlOutput("eventsLog"),
                          h5("Nominal Value Reports"),
                          htmlOutput("NMVLog"),
                          h5("Liquidty Reports"),
                          htmlOutput("LQLog"),
                          h5("Net Present Value Reports"),
                          htmlOutput("NPVLog"),
                          h5("Income Reports"),
                          htmlOutput("INCLog"),
                          # DTOutput("nomDT"),
                          #output dataframe of the respective portfolio
                        )  #main panel close
                      )  #sidebarLayout close
             ),  #tabpanel close
             tabPanel("Analyical Results",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          h4("Display Options"),
                          # selectInput for the report type
                          selectInput(inputId = "selectReportType",
                                      label = "Choose a Report Type", 
                                      choices = c("Liquidity", "Nominal Value", "Net Present Value", "Income"),
                                      selected = "Liquidity"
                          ),
                          selectInput(inputId = "selectScale",
                                      label = "Choose a Scaling Factor", 
                                      choices = c("None", "1k", "1M", "1B"),
                                      selected = "None"
                          ),
                          numericInput(inputId = "selectDigits",
                                       label = "Choose Digits for Rounding",
                                       value = 0
                          ),
                          # actionButton for the analysis
                          # actionButton(inputId = "analysisButton",
                          #              label = "Display"
                          # ),
                          # drilldown
                          h4("Drilldown"),
                          selectInput("selectDrilldownNode",
                                      "Select Node to Drilldown",
                                      choices = NULL),
                        ), #sidebar panel close
                        mainPanel(
                          # verbatimTextOutput("scale"),
                          h3("Here we will display the results in the tree structure"),
                          htmlOutput("displaySettings"),
                          uiOutput("display"),
                          h3("There will be a drilldown possibility to the contracts"),
                          DTOutput("drilldown"),
                        )  #main panel close
                      )  #sidebarLayout close
               
             ), #tabpanel close
             tabPanel("Financial Reports",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          h4("Ratios"),
                          checkboxGroupInput("displayLiqRatios", "Choose Liquidity Ratios to Display",
                                             choices = c("Current Ratio", "Loan to Deposit Ratio"),
                                             selected = NULL),
                          checkboxGroupInput("displayProfRatios", "Choose Profit Ratios to Display",
                                             choices = c("Return on Equity", "Return on Assets"),
                                             selected = NULL),
                          checkboxGroupInput("displayLevRatios", "Choose Leverage Ratios to Display",
                                             choices = c("Debt to Equity Ratio", "Equity Ratio"),
                                             selected = NULL),
                          # tests
                          sliderInput(inputId = "currentRatio", label = "Current Ratio", min = 0, max = 2, value = 0.5, step = 0.1),
                          sliderInput(inputId = "loanToDepositRatio", label = "Loan to Deposit Ratio", min = 0, max = 2 ,value = 1.0, step = 0.1),
                          sliderInput(inputId = "returnOnEquity", label = "Return on Equity", min = -0.5, max = 0.5, value = 0.1, step = 0.1),
                          sliderInput(inputId = "returnOnAssets", label = "Return on Assets", min = -0.02, max = 0.03, value = 0.02, step = 0.005),
                          sliderInput(inputId = "debtToEquityRatio", label = "Debt to Equity Ratio", min = 3, max = 15, value = 9, step = 0.5),
                          sliderInput(inputId = "equityRatio", label = "Equity Ratio", min = 0, max = 0.3, value = 0.08, step = 0.01),
                          
                          selectInput(inputId = "selectPeriodDate",
                                      label = "Choose Period/Date to Display", 
                                      choices = NULL,
                          ),
                          h4("Position Development over time"),
                          selectInput(inputId = "selectPosition",
                                      label = "Choose a Position", 
                                      choices = NULL,
                          ),
                          # select input for the
                          h4("Download"),
                          downloadButton("downloadPDF", "Download PDF Report"),
                          p("The downloaded report contains the selected ratios for all periods as well as the development over time. 
                            Additionally, all reports in the tree structure are included.")
                          
                        ), #sidebar panel close
                        mainPanel(
                          fluidRow(
                            column(
                              width = 12,
                              verbatimTextOutput("selected"),
                              h4("Liquidity Ratios"),
                              column(
                                width = 6,
                                uiOutput("currentRatioBox"),
                                # value_box(title = "Current Ratio", height = "200px",value = 0.5, showcase = plotlyOutput("plot_currentRatio"), theme = value_box_theme("yellow"))
                              ),
                              column(
                                width = 6,
                                uiOutput("loanToDepositRatioBox"),
                                # value_box(title = "Loan to Deposit Ratio", height = "200px",value = 1.0, showcase = plotlyOutput("plot_loanToDepositRatio"), theme = value_box_theme(bg="green"))
                              )
                            )
                          ),
                          fluidRow(
                            column(
                              width = 12,
                              h4("Profit Ratios"),
                              column(
                                width = 6,
                                uiOutput("returnOnEquityBox"),
                                # value_box(title = "Return on Equity", height = "200px",value = 0.8, showcase = plotlyOutput("plot_returnOnEquity"), theme = value_box_theme(bg="yellow"))
                              ),
                              column(
                                width = 6,
                                uiOutput("returnOnAssetsBox"),
                                # value_box(title = "Return on Assets", height = "200px",value = 0.3, showcase = plotlyOutput("plot_returnOnAssets"), theme = value_box_theme(bg="red"))
                              )
                            )
                          ),
                          fluidRow(
                            column(
                              width = 12,
                              h4("Leverage Ratios"),
                              column(
                                width = 6,
                                uiOutput("debtToEquityRatioBox"),
                                # value_box(title = "Debt to Equity Ratio", height = "200px", value = 0.5, showcase = plotlyOutput("plot_debtToEquityRatio"), theme = value_box_theme(bg="purple"))
                              ),
                              column(
                                width = 6,
                                uiOutput("equityRatioBox"),
                                # value_box(title = "Equity Ratio", height = "200px",value = 0.5, showcase = plotlyOutput("plot_equityRatio"), theme = value_box_theme(bg="orange"))
                              )
                            )
                          ),
                          fluidRow(
                            column(
                              width = 12,
                              h3("Development of selected position over time"),
                            ),
                            column(width = 6,
                                   plotOutput("liquidityPlot"),),
                            column(width = 6,
                                   plotOutput("NMVPlot"),),
                            column(width = 6,
                                   plotOutput("NPVPlot"),),
                            column(width = 6,
                                   plotOutput("incomePlot"),),
                          ),
                          fluidRow(
                            column(
                              width = 12,
                              h3("Here we will display some plots")
                            )
                          ),
                          fluidRow(
                            column(
                              width = 12,
                              h3("There will be a download button (PDF) for the reports")
                            )
                          )
                        )  #main panel close
                      )  #sidebarLayout close
               
             ), #tabpanel close
             tabPanel("Cash Flow Analysis & Reporting",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     selectInput("repdate","Choose Reporting Date",
                                                 choices = c("2023-01-01", "2024-01-01"),
                                                 selected = "2023-01-01"
                                     ),
                                     selectInput("ptfFile_rep","Choose portfolio",
                                                 choices = c("2-Currency-MortgagePortfolio"),
                                                 selected = "2-Currency-MortgagePortfolio"
                                     ),
                                     selectInput("mapping_rep","Define Mapping of each position to reporting category",
                                                 choices = c("Mapping for Swiss interest rate risk report (ZIR)"),
                                                 selected = "Mapping for Swiss interest rate risk report (ZIR)"
                                     ),
                                     selectInput("repline_rep","Choose reporting line",
                                                 choices = c("Money market mortgages","Fixed-rate mortgages" ),
                                                 selected = "Money market mortgages"
                                     ),
                                     selectInput(
                                       inputId = "evType_rep",
                                       label = "Choose the cash flow type",
                                       choices =  c("contractual interest rate cash flow (IP)",
                                                    "principal redemption (PR)",
                                                    "nominal cash flows (MD)"),
                                       selected = "contractual scenario-specific interest rate cash flow (IP)"
                                     ),
                                     selectInput(
                                       inputId = "rfScenario_rep",
                                       label = "Choose the Risk Factor Scenario",
                                       choices = c("increasing Rates",
                                                   "decreasing Rates",
                                                   "steady Rates"
                                       )
                                     )
                        ),   #sidebarpanel Close
                        
                        mainPanel(
                          h2("Cash Flow Analysis - using contractual cash flows, not repricing cash flows."),
                          h4("Time Buckets follow FINMA convention: 1: Overnight, 2= 1 to 30 days, 3: 1-3 months, 4: 3-6 months, 5: 6-9 months, 6: 9-12 months."),
                          h4("7: 1y to 1.5y, 8:1.5y to 2y, 9: 2-3 y, 10: 3-4y,..., 17: 10 to 15y, 18: 15y-20y, 19: >20 y."),
                          
                          #width = 8 ,shinycssloaders::withSpinner(
                          #plotOutput("CFPlot")
                          #),   #spinner close
                          column(
                            dataTableOutput("portfolioDF"),
                            width = 12) #column close
                        )   #main panel close
                      )   #sidebarLayout close
             ),  #tabPanel close
             tabPanel("Help",
                      sidebarLayout(
                        sidebarPanel(
                          textInput("serverURL","Specify your server URL",value = "https://dadfir3-app.zhaw.ch/")
                        ),
                        mainPanel(
                          h2("Disclaimer"),
                          h4("This application is being developed in the framework of the DaDFiR3 project. 
                          Its purpose is to showcase that the ACTUS standard can be used for financial report generation. 
                          Please note that this app is a work in progress and therefore, bugs or other errors are possible. 
                             In case you encounter any problems, please contact the developers via: ", style ="color:black"), h4("info@dadfir3.ch",  style ="color:blue"),
                          h4("The use of this application is at the users own risk. The DaDFiR3 team cannot be held responsible 
                             for any actions taken based on the output of this application.", style = "color:black"),
                          h2("Financial Model"),
                          h4("In this tab, one can specify the financial model for analysis. Apart from the ID and description, 
                          which are optional fields. One must specify a portfolio of ACTUS contracts, a currency, and the timeline for the analysis of the financial model. 
                             The timeline defines the number of reports to be generated.", style ="color:black"),
                          h4("Suggestion: Ideally the mapping file in the yaml format consists of all contract IDs from the Portfolio.", style = "color:black"),
                          h2("Modify Financial Model"),
                          h4("In this tab, one can modify the financial model. It is possible to add contracts to the enterprise tree from the portfolio or create 
                             new contracts and add them to the portfolio and the institution tree.", style ="color:black"),
                          h2("Simulation"),
                          h4("In this tab, one can set the scenario for the analysis. One can either choose an example scenario or specify a scenario by 
                             uploading csv files for reference indices. The currently active scenario is plotted in the top part of this tab. Please note 
                             that the corresponding Market Object Code needs to be set according to the contracts which are simulated. Once the scenario is specified, 
                             one can simulate the financial model in this scenario by clicking 'Simulate'.", style ="color:black"),
                          h2("Analytical Results"),
                          h4("In this tab, one can display the analytical results from the simulation. To display the results, one must set the report type, 
                             optionally a scaling factor and the digits for rounding. To provide further insights into the reports, one can drill down to the 
                             desired level by choosing a Node in the drop-down menu. By selecting a Node in the drop-down menu, a list of all events contributing to 
                             the result in the selected node is shown.", style = "color:black"),
                          h2("Financial Reports"),
                          h4("In this tab, one can create financial reports. The reports are created by selecting the desired elements. 
                             Currently, this demo app allows to select KPI’s and plot the development of KPI’s or enterprise accounts over time. The financial report 
                             which is created can be downloaded using the download PDF Report button.", style ="color:black"),
                          h2("Cash Flow Analysis & Reporting"),
                          h4("In this tab, one can create the Swiss interest risk Report (ZIR) for the financial model which is analyzed. Please Note that this tab 
                             is currently only working for the example institution and portfolio which are included in the app. The function to create repricing cashflows 
                             is currently under development.", style = "color:black"),
                          h2("Data"),
                          h4("All included data can be downloaded from the github repository: DaDFiR3/financialModelAnalysis. In case you have problems uploading your own data, 
                             please carefully check if your data is in the same format as the examples.", style ="color:black"),
                          h2("Contact"),
                          h4("Please note that this App is a Work in Progress. 
                             If you encounter any error messages or other malfunctions, 
                             please contact the developers via:", style ="color:black"),h4("info@dadfir3.ch",  style ="color:blue"),
                          h4("If you have any suggestions for new features or improvments
                             of the app, we appreciate your inputs.",style ="color:black")
                        ) #main panel close
                      )#sidebarLayout close
             )  #tab panel close
  )   #navbarPAGE closet
)   #fluid Page close
