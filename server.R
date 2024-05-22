function(input, output, session) {
  # serverURL from Help tab
  serverURL <- reactive({input$serverURL})
  # serverURL <<- "https://dadfir3-app.zhaw.ch/" # needs to be fixed!!!
  # setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  
  # reference Index
  rfx_falling <- sampleReferenceIndex("./data/UST5Y_fallingRates.csv",
                                      rfID = "UST5Y_fallingRates", 
                                      moc = "YC_EA_AAA",base = 100)
  rfx_rising <- sampleReferenceIndex("./data/UST5Y_risingRates.csv","UST5Y_risingRates",
                                     "YC_EA_AAA",100 )
  rfx_steady <- sampleReferenceIndex("./data/UST5Y_steadyRates.csv","UST5Y_steadyRates",
                                     "YC_EA_AAA",100 )
  rfx_recovering <- sampleReferenceIndex("./data/UST5Y_recoveringRates.csv","UST5Y_recoveringRates",
                                         "YC_EA_AAA",100 )
  
  ### Financial Model tab
  financialModelID <- reactive({input$financialModelID})
  financialModelDescription <- reactive({input$financialModelDescription})
  enterpriseID <- reactive({input$enterpriseID})
  currency <- reactive({input$selectCurrency})
  
  accounts <- reactive({
    if (input$selectAccounts == "Model Bank") {
      tree <- AccountsTree("./data/modelBank.yaml")
    } else if (input$selectAccounts == "Uploaded"){
      req(input$accountsFile)
      tree <- AccountsTree(input$accountsFile$datapath)
    }
    tree
  })
  
  portfolio <- reactiveVal()
  portfolio_df <- reactiveVal()
  observe({
    if (input$selectPortfolio == "Test Portfolio") {
      ptf <- csvx2ptf("./data/fmTestPortfolio.csv")
      ptf_df <- read.csv("./data/fmTestPortfolio.csv")
    } else if (input$selectPortfolio == "Uploaded"){
      req(input$portfolioFile)
      ptf <- csvx2ptf(input$portfolioFile$datapath)
      ptf_df <- read.csv(input$portfolioFile$datapath)
    }
    portfolio(ptf)
    ptf_df <- ptf_df[,c("contractType","statusDate","contractRole","contractID",
                        "nominalInterestRate","currency","initialExchangeDate",
                        "premiumDiscountAtIED","maturityDate","notionalPrincipal",
                        "rateSpread","description")]
    portfolio_df(ptf_df)
  })
  ## Timeline setup
  
  statusDate <- reactive({as.character(input$statusDate)})
  reportFrequency <- reactive({input$reportFrequency})
  # monthsPerPeriod <- reactive({input$monthsPerPeriod})
  reportCount <- reactive({input$reportCount})
  # periodCount <- reactive({input$periodCount})
  
  timeline <- reactive({
    timevec <- c(3, 6, 12)
    names(timevec) <- c("Quarterly", "Semi-Annual", "Annual")
    tl <- Timeline(
      statusDate = statusDate(),
      monthsPerPeriod = as.numeric(timevec[reportFrequency()]),
      reportCount = reportCount(),
      periodCount = 2*reportCount()
    )
    if(input$customReportDates){
      tl$reportDates <- as.character(input$reportDates)
    }
    tl
  })
  
  observe({
    if(input$customReportDates){
      output$customReportDates <- renderUI({
        airDatepickerInput("reportDates", "Custom Report Dates", value = statusDate(), multiple = TRUE)
      })
    }
  })
  
  financialModel <- reactiveVal({})
  
  observeEvent(input$setFinancialModelButton, {
    fm <- initFinancialModel(
      fmID = financialModelID(),
      fmDescr = financialModelDescription(),
      entprID = enterpriseID(),
      accntsTree = accounts(),
      ptf = portfolio(),
      curr = currency(),
      timeline = timeline(),
      serverURL = serverURL()
    )
    financialModel(fm)
  })
  
  # Current Settings tabpanel
  output$curAccounts <- renderPrint({
    accounts()$root
  })
  
  output$curTreemap <- renderDataTable({
    ToDataFrameTable(accounts()$root, "name", "nodeID", "actusCIDs", "formulaCIDs")
  })
  
  output$curPortfolioTable <- renderDataTable(portfolio_df(), 
                                           options = list(autoWidth = TRUE, scrollX = TRUE))
  
  output$curTimeline <- renderPrint({
    timeline()
  })
  
  # Stored Financial Model tabpanel
  output$fmAccounts <- renderPrint({
    req(financialModel())
    financialModel()$accountsTree$root
  })
  
  output$fmTreemap <- renderDataTable({
    req(financialModel())
    ToDataFrameTable(financialModel()$accountsTree$root, "name", "nodeID", "actusCIDs", "formulaCIDs")
  })
  
  output$fmTimeline <- renderPrint({
    req(financialModel())
    financialModel()$timeline
  })
  
  ### Portfolio Analysis Tab
  contractAttribute <- reactive({
    if(input$selectContractAttribute == "ACTUS Contract"){
      "actusCIDs"
    } else if(input$selectContractAttribute == "Functional Contracts"){
      "functionIDs"
    }
  })
  
  leaves <- reactive({
    # req(financialModel())
    accounts()$root$Get("nodeID", filterFun = isLeaf)
    })
  
  contractList <- reactive({
    # req(financialModel())
    accounts()$root$Get(contractAttribute(), filterFun = function(x) c(x$nodeID %in% input$selectNode))
    })
  
  # observeEvent(input$addContractButton,{
  #   req(input$contractID)
  #   # append(contractList(), input$contractID)
  #   Navigate(accounts()$root, paste(accounts()$root$Assets$Current$path))$Set(contractAttribute(), append(contractList(), input$contractID))
  # })
  
  output$addingContract <- renderPrint({
    contractList()
  })
  
  observe({
    updateSelectInput(session, "selectNode", choices = leaves())
  })
  
  # treemap <- reactive({
  #   # req(financialModel())
  #   ToDataFrameTable(accounts()$root, "name", "nodeID", "actusCIDs", "formulaCIDs")
  # })
  # 
  # output$treemap <- renderDataTable({
  #   treemap()
  # })
  
  # output$portfolio <- renderPrint({
  #   unlist(portfolio()$contracts)[1:2]
  # })
  
  # output$portfolioTable <- renderDataTable(portfolio_df(), 
  #                                          options = list(autoWidth = TRUE, scrollX = TRUE))
  
  ### Simulation Tab
  scenarioID <- reactive({input$scenarioID})
  
  yieldcurve <- reactive({
    if (input$selectYieldCurve == "Flat"){
      tr <- c(1, 1.5, 2, 2.5)/100
      names(tr) <- c("1M", "1Y", "5Y", "15Y")
      yc <- YieldCurve(
        yieldCurveID = "Flat",
        referenceDate = "2015-01-01",
        tenorRates = tr,
        dayCountConvention = "30E360",
        compoundingFrequency = "CONTINUOUS"
      )
    } else if (input$selectYieldCurve == "Steep"){
      tr <- c(1, 2, 4, 6)/100
      names(tr) <- c("1M", "1Y", "5Y", "15Y")
      yc <- YieldCurve(
        yieldCurveID = "Steep",
        referenceDate = "2015-01-01",
        tenorRates = tr,
        dayCountConvention = "30E360",
        compoundingFrequency = "CONTINUOUS"
      )
    }
    yc
  })
  
  scenario <- reactiveVal()
  scenario_df <- reactiveVal()
  observe({
    if (input$selectScenario == "steady"){
      sc <- rfx_steady
    } else if (input$selectScenario == "falling"){
      sc <- rfx_falling
    } else if (input$selectScenario == "rising"){
      sc <- rfx_rising
    } else if (input$selectScenario == "recovering"){
      sc <- rfx_recovering
    } else if(input$selectScenario == "custom"){
      sc <- sampleReferenceIndex(input$scenarioFile$datapath, input$refID, input$moc, as.numeric(input$base))
    }
    scenario(sc)
    sc_df <- data.frame(Date = rownames(sc$data), Rate = as.numeric(sc$data), row.names = NULL)
    scenario_df(monthlyAverageRate(sc_df))
  })
  
  marketData <- reactive({list(scenario())})
  
  plt <- reactive({
    ggplot(scenario_df(), aes(x=Date,y=Rate)) +
      geom_line(colour = "black") +
      labs(title = paste("Scenario: ", input$selectScenario, " for ", scenario()$marketObjectCode))})
  
  output$ratesPlot <- renderPlot({plt()})
  
  # add scneario to the financial Model
  
  
  # output$srv <- renderText({
  #   serverURL()
  # })
  
  # observeEvent(input$setAnalysisButton, {
  # })
  
  # output$strings <- renderPrint({
  #   paste(analysisID(), analysisDescription(), enterpriseID(), currency())
  # })
  # 
  # output$yc <- renderPrint({
  #   yieldcurve()
  # })
  # 
  # output$ptf <- renderPrint({
  #   portfolio()$contracts[1:2]
  # })
  # 
  # output$sc <- renderPrint({
  #   scenario()
  # })
  # 
  output$tl <- renderPrint({
    timeline()
  })
  
  msg1 <- reactiveVal()
  msg2 <- reactiveVal()
  msg3 <- reactiveVal()
  msg4 <- reactiveVal()
  msg5 <- reactiveVal()
  msg6 <- reactiveVal()
  msg7 <- reactiveVal()
  msg8 <- reactiveVal()
  
  observeEvent(input$setAnalysisButton, {
    req(financialModel())
    addScenarioAnalysis(fm = financialModel(), scnID = scenarioID(), 
                        rfxs = marketData(), yc = yieldcurve())
    msg1(NULL)
    msg2(NULL)
    msg3(NULL)
    msg4(NULL)
    msg5(NULL)
    msg6(NULL)
    msg7(NULL)
    msg8(NULL)
    msg1(generateEvents(host = financialModel()))
    msg2(events2dfByPeriod(host = financialModel()))
    msg3(nominalValueReports(host = financialModel()))
    msg4(accountNMVreports(host = financialModel()))
    msg5(liquidityReports(host = financialModel()))
    msg6(accountLQreports(host = financialModel()))
    msg7(netPresentValueReports(host = financialModel()))
    msg8(accountNPVreports(host = financialModel()))
  })
  
  output$eventsLog <- renderText({
    req(msg1(), msg2())
    HTML(paste(msg1(), paste("Events dataframe:", msg2()), sep="<br/>"))
  })
  
  output$NMVLog <- renderText({
    req(msg3(), msg4())
    HTML(paste(msg3(), msg4(), sep="<br/>"))
  })
  
  output$LQLog <- renderText({
    req(msg5(), msg6())
    HTML(paste(msg5(), msg6(), sep="<br/>"))
  })
  
  output$NPVLog <- renderText({
    req(msg7(), msg8())
    HTML(paste(msg7(), msg8(),sep="<br/>"))
  })
  
  ### Analytical Results Tab
  print_sc <- reactive({
    print_scale <- c("", "thousand", "million", "Billion")
    names(print_scale) <- c("None", "1k", "1M", "1B")
    as.character(print_scale[input$selectScale])
  })
  
  scale <- reactive({
    scales <- c(1, 1e3, 1e6, 1e9)
    names(scales) <- c("None", "1k", "1M", "1B")
    as.numeric(scales[input$selectScale])
    
  })
  
  roundDigits <- reactive({input$selectDigits})
  
  # output$scale <- renderPrint({
  #   scale()
  # })
  
  # output$nommatrix <- renderPrint({
  #   req(msg4())
  #   getNMVreports(financialModel(),scale(), roundDigits())
  # })
  
  output$nomrep <- renderPrint({
    req(msg4())
    showNMVreports(financialModel(), scale(), roundDigits())
  })
  
  output$lqrep <- renderPrint({
    req(msg6())
    showLQreports(financialModel(), scale(), roundDigits())
  })
  
  output$npvrep <- renderPrint({
    req(msg8())
    showNPVreports(financialModel(), scale(), roundDigits())
  })
  
  output$notAvailable <- renderPrint({
    "Not available yet."
  })
  
  output$display <- renderUI({
    if(input$selectReportType == "Liquidity"){
      verbatimTextOutput("lqrep")
    } else if(input$selectReportType == "Nominal Value"){
      verbatimTextOutput("nomrep")
    } else if(input$selectReportType == "Net Present Value"){
      verbatimTextOutput("npvrep")
    } else if(input$selectReportType == "Income"){
      verbatimTextOutput("notAvailable")
    }
  })
  
  # output$nomDT <- renderDataTable({
  #   req(msg4())
  #   showNMVreports(financialModel())
  # })
  
  nodes <- reactive({
    req(financialModel())
    names(accounts()$root$Get("path"))
  })

  # output$nodes <- renderUI({
  #   selectInput("selectDrilldownNode", "Select Node to Drilldown", nodes())
  # })
  observe({
    updateSelectInput(session, "selectDrilldownNode", choices = nodes())
  })
  
  output$displaySettings <- renderPrint({
    HTML(paste(input$selectReportType, "in", print_sc(), currency()))
  })
  
  drilldown_df <- reactive({
    if(input$selectReportType == "Liquidity"){
      showContractLQs(financialModel(), scale(), roundDigits())
    } else if(input$selectReportType == "Nominal Value"){
      showContractNMVs(financialModel(), scale(), roundDigits())
    } else if(input$selectReportType == "Net Present Value"){
      showContractNPVs(financialModel(), scale(), roundDigits())
    } else if(input$selectReportType == "Income"){
      data.frame()
    }
  })
  
  output$drilldown <- renderDataTable({
    req(input$selectDrilldownNode)
    df <- drilldown_df()
    contracts <- fm$accountsTree$root$Get("actusCIDs", filterFun = function(node) node$name %in% names(FindNode(fm$accountsTree$root, input$selectDrilldownNode)$Get("children")))
    unlist(contracts)
    df <- df[df$actusCIDs %in% unlist(contracts),]
    df
  })
  
  # Financial Reports Tab
  #reportCols <- reactive({timeline()$reportDates})

  observe({
    updateSelectInput(session, "selectPeriodDate", choices = c(timeline()$reportDates, "Development over Time"))
  })
  
  #### Current Ratio
  currentRatio <- reactive({input$currentRatio})
  
  currentRatioCol <- reactive({
    if (currentRatio() >= 1) {
      "green"
    } else if(currentRatio() >= 0.6) {
      "orange"
    } 
    else {
      "red"
    }
  })
  
  output$plot_currentRatio <- renderPlotly({
    # Example pie chart for Current Ratio
    fig <- plot_ly(labels = c("Assets", "Liabilities"), values = c(3, 2), type = "pie", textinfo = "label+percent", marker = list(colors = c("blue", "red")))
    fig
  })
  
  observe({
    if("Current Ratio" %in% input$displayLiqRatios){
      output$currentRatioBox <- renderUI({
        value_box(
          value = currentRatio(),
          title = "Current Ratio",
          theme = value_box_theme(bg=currentRatioCol()),
          height = "200px",
          showcase = plotlyOutput("plot_currentRatio"),
        )
      })
    } else {
      output$currentRatioBox <- renderUI({})
    }
  })
  
  #### Loan to Deposit Ratio
  loanToDepositRatio <- reactive({input$loanToDepositRatio})
  
  loanToDepositRatioCol <- reactive({
    if (0 < loanToDepositRatio() & loanToDepositRatio() <= 0.63) {
      "green"
    } else if(0 < loanToDepositRatio() & loanToDepositRatio() <= 0.8) {
      "orange"
    } 
    else {
      "red"
    }
  })
  
  output$plot_loanToDepositRatio <- renderPlotly({
    # Example pie chart for Loan to Deposit Ratio
    fig <- plot_ly(labels = c("Loans", "Deposits"), values = c(2, 1), type = "pie", textinfo = "label+percent", marker = list(colors = c("green", "orange")))
    fig
  })
  
  observe({
    if("Loan to Deposit Ratio" %in% input$displayLiqRatios){
      output$loanToDepositRatioBox <- renderUI({
        value_box(
          value = loanToDepositRatio(),
          title = "Loan to Deposit Ratio",
          theme = value_box_theme(bg=loanToDepositRatioCol()),
          height = "200px",
          showcase = plotlyOutput("plot_loanToDepositRatio"),
        )
      })
    } else {
      output$loanToDepositRatioBox <- renderUI({})
    }
  })
  
  #### Return on Equity
  returnOnEquity <- reactive({input$returnOnEquity})
  
  returnOnEquityCol <- reactive({
    if (returnOnEquity() >= 0.1) {
      "green"
    } else if(returnOnEquity() >= 0.06) {
      "orange"
    } 
    else {
      "red"
    }
  })
  
  output$plot_returnOnEquity <- renderPlotly({
    # Example pie chart for Return on Equity
    fig <- plot_ly(labels = c("Income", "Expenses"), values = c(4, 3), type = "pie", textinfo = "label+percent", marker = list(colors = c("yellow", "brown")))
    fig
  })
  
  observe({
    if("Return on Equity" %in% input$displayProfRatios){
      output$returnOnEquityBox <- renderUI({
        value_box(
          value = returnOnEquity(),
          title = "Return on Equity",
          theme = value_box_theme(bg=returnOnEquityCol()),
          height = "200px",
          showcase = plotlyOutput("plot_returnOnEquity"),
        )
      })
    } else {
      output$returnOnEquityBox <- renderUI({})
    }
  })
  
  #### Return on Assets
  returnOnAssets <- reactive({input$returnOnAssets})
  
  returnOnAssetsCol <- reactive({
    if (returnOnAssets() >= 0.01) {
      "green"
    } else if(returnOnAssets() >= 0.005) {
      "orange"
    } 
    else {
      "red"
    }
  })
  
  output$plot_returnOnAssets <- renderPlotly({
    # Example pie chart for Return on Assets
    fig <- plot_ly(labels = c("Income", "Expenses"), values = c(5, 2), type = "pie", textinfo = "label+percent", marker = list(colors = c("green", "red")))
    fig
  })
  
  observe({
    if("Return on Assets" %in% input$displayProfRatios){
      output$returnOnAssetsBox <- renderUI({
        value_box(
          value = returnOnAssets(),
          title = "Return on Assets",
          theme = value_box_theme(bg=returnOnAssetsCol()),
          height = "200px",
          showcase = plotlyOutput("plot_returnOnAssets"),
        )
      })
    } else {
      output$returnOnAssetsBox <- renderUI({})
    }
  })
  
  #### Debt to Equity Ratio
  debtToEquityRatio <- reactive({input$debtToEquityRatio})
  
  debtToEquityRatioCol <- reactive({
    if (0 < debtToEquityRatio() & debtToEquityRatio() <= 9) {
      "green"
    } else if(0 < debtToEquityRatio() & debtToEquityRatio() <= 10) {
      "orange"
    } 
    else {
      "red"
    }
  })
  
  output$plot_debtToEquityRatio <- renderPlotly({
    # Example pie chart for Debt to Equity Ratio
    fig <- plot_ly(labels = c("Debt", "Equity"), values = c(3, 2), type = "pie", textinfo = "label+percent", marker = list(colors = c("purple", "orange")))
    fig
  })
  
  observe({
    if("Debt to Equity Ratio" %in% input$displayLevRatios){
      output$debtToEquityRatioBox <- renderUI({
        value_box(
          value = debtToEquityRatio(),
          title = "Debt to Equity Ratio",
          theme = value_box_theme(bg=debtToEquityRatioCol()),
          height = "200px",
          showcase = plotlyOutput("plot_debtToEquityRatio"),
        )
      })
    } else {
      output$debtToEquityRatioBox <- renderUI({})
    }
  })
  
  #### Equity Ratio
  equityRatio <- reactive({input$equityRatio})
  
  equityRatioCol <- reactive({
    if (equityRatio() >= 0.1) {
      "green"
    } else if(equityRatio() >= 0.07) {
      "orange"
    } 
    else {
      "red"
    }
  })
  
  output$plot_equityRatio <- renderPlotly({
    # Example pie chart for Equity Ratio
    fig <- plot_ly(labels = c("Equity", "Liabilities"), values = c(3, 2), type = "pie", textinfo = "label+percent", marker = list(colors = c("orange", "red")))
    fig
  })
  
  observe({
    if("Equity Ratio" %in% input$displayLevRatios){
      output$equityRatioBox <- renderUI({
        value_box(
          value = equityRatio(),
          title = "Equity Ratio",
          theme = value_box_theme(bg=equityRatioCol()),
          height = "200px",
          showcase = plotlyOutput("plot_equityRatio"),
        )
      })
    } else {
      output$equityRatioBox <- renderUI({})
    }
  })
  
  ## Plots
  # get available positions from drill down part
  observe({
    updateSelectInput(session, "selectPosition", choices = nodes())
  })
  
  sign <- reactive({
    if(input$selectPosition %in% names(FindNode(fm$accountsTree$root, "Liabilities")$Get("children"))){
      - 1
    } else{
      1
    }
  })
  
  output$liquidityPlot <- renderPlot({
    req(input$selectPosition)
    matrix <- getLQreports(financialModel(), scale(), roundDigits())
    if(!(input$selectPosition %in% rownames(matrix))){return(NULL)}
    plot_data <- (data.frame(date = colnames(matrix), values = matrix[input$selectPosition,]))
    
    ggplot(data = plot_data, aes(x = date, y = values)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Liquidity for", input$selectPosition), x = "Date", y = paste("Liquidity in", print_sc(), currency()))
  })
  
  output$NMVPlot <- renderPlot({
    req(input$selectPosition)
    matrix <- getNMVreports(financialModel(), scale(), roundDigits())
    if(!(input$selectPosition %in% rownames(matrix))){return(NULL)}
    plot_data <- (data.frame(date = colnames(matrix), values = sign()*matrix[input$selectPosition,]))
    
    ggplot(data = plot_data, aes(x = date, y = values)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Nominal Value for", input$selectPosition), x = "Date", y = paste("Nominal Value in", print_sc(), currency()))
  })
  
  output$NPVPlot <- renderPlot({
    req(input$selectPosition)
    matrix <- getNPVreports(financialModel(), scale(), roundDigits())
    if(!(input$selectPosition %in% rownames(matrix))){return(NULL)}
    plot_data <- (data.frame(date = colnames(matrix), values = sign()*matrix[input$selectPosition,]))
    
    ggplot(data = plot_data, aes(x = date, y = values)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Net Present Value for", input$selectPosition), x = "Date", y = paste("Net Present Value in", print_sc(), currency()))
  })
  
  output$incomePlot <- renderPlot({
    req(input$selectPosition)
    return(NULL)
    matrix <- getNPVreports(financialModel(), scale(), roundDigits())
    if(!(input$selectPosition %in% rownames(matrix))){return(NULL)}
    plot_data <- (data.frame(date = colnames(matrix), values = matrix[input$selectPosition,]))
    
    ggplot(data = plot_data, aes(x = date, y = values)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Income for", input$selectPosition), x = "Date", y = paste("Income in", print_sc(), currency()))
  })
  
  ### Reporting Tab
  source(paste0("./data_patrick/reportingDemoFiles/fnc_reportingDemo.R"))
  datapath <- paste0("./data_patrick/reportingDemoFiles/") #path to the demo files
  
  # Observe reporting
  observe({
    if(input$ptfFile_rep == "2-Currency-MortgagePortfolio"){
      portfoliopath <- paste0(datapath,"/AnnuityPortfolio_CHF_USD.csv")
    }
    
    if(input$mapping_rep == "Mapping for Swiss interest rate risk report (ZIR)"){
      mappingpath<- paste0(datapath,"/AnnuityPortfolio_IRRBBMapping.csv")
    }
    
    reportingDate<- reactive({
      input$repdate 
    })
    
    
    evTab_Rates <- reactive({
      if(input$rfScenario_rep == "increasing Rates"){
        createReportingCashFlowTable_fromPortfolio(portfoliopath, mappingpath, ReferenceIndexList = list(rfx_rising), serverURL =  serverURL())
      } else if(input$rfScenario_rep == "decreasing Rates"){
        createReportingCashFlowTable_fromPortfolio(portfoliopath, mappingpath, ReferenceIndexList = list(rfx_falling), serverURL =  serverURL()) 
      } else if(input$rfScenario_rep == "steady Rates"){
        createReportingCashFlowTable_fromPortfolio(portfoliopath, mappingpath, ReferenceIndexList = list(rfx_steady), serverURL =  serverURL())
      }
      
    })
    
    basefilter <- "(TimeBucket < 21)"
    customfilter<- reactive({
      if(input$evType_rep == "contractual interest rate cash flow (IP)"){
        if (input$repline_rep == "Money market mortgages")
          paste0(basefilter, " & " , "(type == \"IP\") & ", "reportingLineDesc == \"Money market mortgages\"") else paste0(basefilter, " & " , "(type == \"IP\") & ", "reportingLineDesc == \"Fixed-rate mortgages\"") 
      } else if(input$evType_rep  == "principal redemption (PR)"){
        if (input$repline_rep == "Money market mortgages")
          paste0(basefilter, " & " , "(type == \"PR\") & ", "reportingLineDesc == \"Money market mortgages\"") else paste0(basefilter, " & " , "(type == \"PR\") & ", "reportingLineDesc == \"Fixed-rate mortgages\"") 
      } else if(input$evType_rep == "nominal cash flows (MD)"){
        if (input$repline_rep == "Money market mortgages")
          paste0(basefilter, " & " , "(type == \"MD\") & ", "reportingLineDesc == \"Money market mortgages\"") else paste0(basefilter, " & " , "(type == \"MD\") & ", "reportingLineDesc == \"Fixed-rate mortgages\"") 
      }
    })
    
    evTab_WithDates <- reactive({ 
      addIRRBBTimeBuckets(EventTable=evTab_Rates(), ReportingDate = as.Date(reportingDate())) 
    })
    
    
    
    output$portfolioDF <- renderDataTable(evTab_WithDates()[eval(parse(text=customfilter())),.(Cashflow=round(sum(payoff), 0), NrOfAggregatedPositions =.N), by= .(reportingLineDesc, reportingLine, TimeBucket, type, currency)],
                                          options = list(autoWidth = TRUE, scrollX = TRUE))
    
  })
  
}      #server close