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
                                      choices = c("Powerplant", "Uploaded"),
                                      selected = "Powerplant"
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
                                      choices = c("Bonds", "Annuities", "Uploaded"),
                                      selected = "Bonds"
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
                                    value = "2015-01-01"
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
                          # output current tree structure
                          verbatimTextOutput("tree"),
                          DTOutput("portfolioTable"),
                        ),  #main panel close
                      ),  #sidebarLayout close
             ), #tabpanel close
             tabPanel("Modify Portfolio",
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
                          textInput(inputId = "analysisID",
                                    label = "Analysis ID",
                                    placeholder = "Enter an analysis ID"
                          ),
                          # textInput for the analysis description
                          textInput(inputId = "analysisDescription",
                                    label = "Analysis Description",
                                    placeholder = "Enter a description"
                          ),
                          # selectInput for the YieldCurve
                          selectInput(inputId = "selectYieldCurve",
                                      label = "Choose a Yield Curve", 
                                      choices = c("Empty", "Flat", "Steep"),
                                      selected = "Empty"
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
                          # verbatimTextOutput("srv"),
                          # verbatimTextOutput("strings"),
                          # verbatimTextOutput("yc"),
                          # verbatimTextOutput("ptf"),
                          # verbatimTextOutput("sc"),
                          h4("Current Scenario"),
                          plotOutput("ratesPlot"),
                          verbatimTextOutput("tl"),
                          # verbatimTextOutput("cfla"),
                          # verbatimTextOutput("list"),
                          verbatimTextOutput("events"),
                          # verbatimTextOutput("liqvec"),
                          verbatimTextOutput("liqrep"),
                          verbatimTextOutput("increp"),
                          verbatimTextOutput("nomrep"),
                          DTOutput("eventsTable"),
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
                                      choices = c("Income", "Liquidity", "Nominal Value"),
                                      selected = "Income"
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
                          actionButton(inputId = "analysisButton",
                                       label = "Display"
                          ),
                          # drilldown
                          h4("Drilldown"),
                          selectInput("selectDrilldownNode",
                                      "Select Node to Drilldown",
                                      choices = NULL),
                        ), #sidebar panel close
                        mainPanel(
                          # verbatimTextOutput("scale"),
                          h3("Here we will display the results in the tree structure"),
                          h3("There will be a drilldown possibility to the contracts"),
                        )  #main panel close
                      )  #sidebarLayout close
               
             ), #tabpanel close
             tabPanel("Financial Reports",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
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
                              h3("Here we will display some ratios")
                            )
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
                          h2("Interest Rate Scenarios"),
                          h4("In this tab you can view the four predefined interest rate scenarios. 
                             Please note that these scenarios are simple linear projections. 
                             We aim to include the posibility to upload your own risk factor scenarios.", style ="color:black"),
                          h2("Loan Contract Cashflow"),
                          h4("In this tab one can specify the terms for a single ACTUS loan contract. 
                             A cashflow plot and an event list for the specific contract is then generated.", style ="color:black"),
                          h4("ANN = Mortgage / PAM = Bond", style = "color:black"),
                          h2("Portfolio Analysis"),
                          h4("In this tab one can carry out analytics on predefined Portfolios. Income and liquidity metrics are 
                          shown in the plot and the respective contracts of the portfolio are displayed in the DataTable", style ="color:black"),
                          h2("Uploaded Portfolio Analysis"),
                          h4("You can upload a data file from your workstation specifying a portfolio 
                                        of loan contracts and request ACTUS contract simulation and analysis.",
                             "The uploaded file must be .csv format and patterned on files:",
                             tags$a("BondPortfolio.csv", 
                                    href="https://github.com/fnparr/FEMSdevBase/tree/main/inst/extdata/BondPortfolio.csv", target = "_blank"),
                             ",  and ",
                             tags$a("AnnuityPortfolio.csv", 
                                    href="https://github.com/fnparr/FEMSdevBase/tree/main/inst/extdata/AnnuityPortfolio.csv", target = "_blank"),
                             " - with any variable rate setting based on Market Object Code YC_EA_AAA.",
                             "For a more detailed explanation of each contract term, consult the ",
                             tags$a("ACTUS Data Dictionary",     href="https://www.actusfrf.org/dictionary", target = "_blank"), style ="color:black"),
                          h2("Specification of ServerURL"),
                          h4("In case you want to use a local installation of an ACTUS server, you can specify the serverURL in the sidebar of the Help Tab. By default, the ServerURL is set to:", 
                             tags$a("https://dadfir3-app.zhaw.ch/",href = "https://dadfir3-app.zhaw.ch", target = "_blank") ,"(public actus server)", style = "color:black"),
                          h4(tags$b("IMPORTANT:"),"If you use a docker version of actus-server your serverURL must be: host.docker.internal:PORT/)", style = "color:black"), 
                          h4("(Use host.docker.internal:8083/ as default)", style = "color:black"),
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
