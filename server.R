function(input, output, session) {
  # serverURL from Help tab
  serverURL <- reactive({input$serverURL})
  # serverURL <<- "https://dadfir3-app.zhaw.ch/" # needs to be fixed!!!
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  
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
  # accounts <- reactiveVal()
  # portfolio <- reactiveVal()
  # timeline <- reactiveVal()
  # portfolio_df <- reactiveVal()
  
  accounts <- reactive({
    if (input$selectAccounts == "Powerplant") {
      tree <- AccountsTree("./data/powerplant.yaml")
    } else if (input$selectAccounts == "Uploaded"){
      req(input$accountsFile)
      tree <- AccountsTree(input$accountsFile$datapath)
    }
    # tree$Set(nodeID = 1:tree$totalCount)
    tree
  })
  
  portfolio <- reactiveVal()
  portfolio_df <- reactiveVal()
  observe({
    if (input$selectPortfolio == "Bonds") {
      ptf <- csvx2ptf("./data/BondPortfolio.csv")
      ptf_df <- read.csv("./data/BondPortfolio.csv")
    } else if (input$selectPortfolio == "Annuities") {
      ptf <- csvx2ptf("./data/AnnuityPortfolio.csv")
      ptf_df <- read.csv("./data/AnnuityPortfolio.csv")
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
  
  # portfolio_df <- reactive({
  #   if (input$selectPortfolio == "Bonds") {
  #     ptf_df <- read.csv("./data/BondPortfolio.csv")
  #   } else if (input$selectPortfolio == "Annuities") {
  #     ptf_df <- read.csv("./data/AnnuityPortfolio.csv")
  #   } else if (input$selectPortfolio == "Uploaded"){
  #     req(input$portfolioFile)
  #     ptf_df <- read.csv(input$portfolioFile$datapath)
  #   }
  #   ptf_df <- ptf_df[,c("contractType","statusDate","contractRole","contractID",
  #                       "nominalInterestRate","currency","initialExchangeDate",
  #                       "premiumDiscountAtIED","maturityDate","notionalPrincipal",
  #                       "rateSpread","description")]
  #   ptf_df
  # })
  # 
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
  
  
  # observeEvent(input$timelineButton, {
  #   req(input$statusDate, input$monthsPerPeriod, input$reportCount, input$periodCount)
  #   tl <- Timeline(
  #     statusDate = as.character(input$statusDate),
  #     monthsPerPeriod = input$monthsPerPeriod,
  #     reportCount = input$reportCount,
  #     periodCount = input$periodCount
  #   )
  #   timeline(tl)
  # })
  
  financialModel <- reactiveVal({})
  
  observeEvent(input$setFinancialModelButton, {
    # # set accounts
    # if (input$selectAccounts == "Powerplant") {
    #   tree <- AccountsTree("./data/powerplant.yaml")
    # } else if (input$selectAccounts == "Uploaded"){
    #   req(input$accountsFile)
    #   tree <- AccountsTree(input$accountsFile$datapath)
    # }
    # accounts(tree)
    
    # # set portfolio
    # if (input$selectPortfolio == "Bonds") {
    #   ptf <- csvx2ptf("./data/BondPortfolio.csv")
    #   ptf_df <- read.csv("./data/BondPortfolio.csv")
    # } else if (input$selectPortfolio == "Annuities") {
    #   ptf <- csvx2ptf("./data/AnnuityPortfolio.csv")
    #   ptf_df <- read.csv("./data/AnnuityPortfolio.csv")
    # } else if (input$selectPortfolio == "Uploaded"){
    #   req(input$portfolioFile)
    #   ptf <- csvx2ptf(input$portfolioFile$datapath)
    #   ptf_df <- read.csv(input$portfolioFile$datapath)
    # }
    # portfolio(ptf)
    # 
    # ptf_df <- ptf_df[,c("contractType","statusDate","contractRole","contractID",
    #                     "nominalInterestRate","currency","initialExchangeDate",
    #                     "premiumDiscountAtIED","maturityDate","notionalPrincipal",
    #                     "rateSpread","description")]
    # portfolio_df(ptf_df)
    
    # # set timeline
    # timevec <- c(3, 6, 12)
    # names(timevec) <- c("Quarterly", "Semi-Annual", "Annual")
    # tl <- Timeline(
    #   statusDate = statusDate(),
    #   monthsPerPeriod = as.numeric(timevec[reportFrequency()]),
    #   reportCount = reportCount(),
    #   periodCount = 2*reportCount()
    # )
    # if(input$customReportDates){
    #   tl$reportDates <- as.character(input$reportDates)
    # }
    # timeline(tl)
    
    fm <- initFinancialModel(
      fmID = financialModelID(),
      fmDescr = financialModelDescription(),
      entprID = enterpriseID(),
      accounts = accounts()$root,
      ptf = portfolio(),
      curr = currency(),
      timeline = timeline(),
      serverURL = serverURL()
    )
    financialModel(fm)
  })
  
  output$financialMod <- renderPrint({
    # financialModelID()
    # financialModelDescription()
    # enterpriseID()
    # currency()
    # accounts()$root
    # portfolio()$contracts[1:2]
    # timeline()
    # serverURL()
    # financialModel()$timeline
  })

  output$tree <- renderPrint({
    accounts()$root
  })
  
  output$portfolioTable <- renderDataTable(portfolio_df(), 
                                           options = list(autoWidth = TRUE, scrollX = TRUE))
  
  
  
  ### Portfolio Analysis Tab
  contractAttribute <- reactive({
    if(input$selectContractAttribute == "ACTUS Contract"){
      "actusCIDs"
    } else if(input$selectContractAttribute == "Formula Contract"){
      "formulaCIDs"
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
  
  treemap <- reactive({
    # req(financialModel())
    ToDataFrameTable(accounts()$root, "name", "nodeID", "actusCIDs", "formulaCIDs")
  })
  
  output$treemap <- renderDataTable({
    treemap()
  })
  
  # output$portfolio <- renderPrint({
  #   unlist(portfolio()$contracts)[1:2]
  # })
  
  # output$portfolioTable <- renderDataTable(portfolio_df(), 
  #                                          options = list(autoWidth = TRUE, scrollX = TRUE))
  
  ### Simulation Tab
  analysisID <- reactive({input$analysisID})
  analysisDescription <- reactive({input$analysisDescription})
  
  yieldcurve <- reactive({
    if (input$selectYieldCurve == "Empty"){
      yc <- YieldCurve()
    } else if (input$selectYieldCurve == "Flat"){
      tr <- c(1, 1.5, 2, 2.5)/100
      names(tr) <- c("1M", "1Y", "5Y", "15Y")
      yc <- YieldCurve(
        yieldCurveID = "Flat",
        referenceDate = "2015-01-01",
        tenorRates = tr,
        dayCountConvention = "30E360",
        compoundingFrequency = "NONE"
      )
    } else if (input$selectYieldCurve == "Steep"){
      tr <- c(1, 2, 4, 6)/100
      names(tr) <- c("1M", "1Y", "5Y", "15Y")
      yc <- YieldCurve(
        yieldCurveID = "Steep",
        referenceDate = "2015-01-01",
        tenorRates = tr,
        dayCountConvention = "30E360",
        compoundingFrequency = "NONE"
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
  
  plt <- reactive({
    ggplot(scenario_df(), aes(x=Date,y=Rate)) +
      geom_line(colour = "black") +
      labs(title = paste("Scenario: ", input$selectScenario, " for ", scenario()$marketObjectCode))})
  
  output$ratesPlot <- renderPlot({plt()})
  
  # add scneario to the financial Model
  
  
  # output$srv <- renderText({
  #   serverURL()
  # })
  
  cfla <- reactiveVal({
    # req(input$analysisID, input$analysisDescription, input$enterpriseID)
    # cfla <- ContractAnalysis(
    #   analysisID = analysisID(),
    #   analysisDescription = analysisDescription(),
    #   enterpriseID = enterpriseID(),
    #   yieldCurve = yieldcurve(),
    #   portfolio = portfolio(),
    #   currency = currency(),
    #   scenario = list(scenario()),
    #   actusServerURL = serverURL(),
    #   timeline = timeline()
    # )
    # cfla
  })
  
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
  
  observeEvent(input$setAnalysisButton, {
    req(financialModel())
    cfa <- initContractAnalysis(
      analysisID = analysisID(),
      analysisDescription = analysisDescription(),
      enterpriseID = enterpriseID(),
      yieldCurve = yieldcurve(),
      portfolio = portfolio(),
      currency = currency(),
      scenario = list(scenario()),
      actusServerURL = serverURL(),
      timeline = timeline()
    )
    cfla(cfa)
    msg1(NULL)
    msg2(NULL)
    msg3(NULL)
    msg4(NULL)
    msg5(NULL)
    msg6(NULL)
    msg1(generateEvents(cntan = cfla()))
    msg2(events2dfByPeriod(cfla = cfla()))
    msg3(liquidityByPeriod2vec(cfla = cfla()))
    msg4(lv2LiquidityReports(cfla= cfla()))
    msg5(eventsdf2incomeReports(cfla = cfla()))
    # msg6(nominalValueReports(cntan = cfla()))
  })
  
  # output$cfla <- renderPrint({
  #   cfla()$analysisID
  #   cfla()$timeline
  # })
  
  # output$list <- renderPrint({
  #   req(msg1())
  #   cfla()$cashflowEventsLoL[[1]]
  # })
  
  output$events <- renderPrint({
    req(msg2())
    head(cfla()$cashflowEventsByPeriod, 20)
  })
  
  # output$liqvec <- renderPrint({
  #   req(msg3())
  #   cfla()$contractLiquidityVectors
  # })
  
  output$liqrep <- renderPrint({
    req(msg4())
    cfla()$liquidityReports
  })
  
  output$increp <- renderPrint({
    req(msg5())
    cfla()$incomeReports
  })
  
  output$nomrep <- renderPrint({
    req(msg6())
    cfla()$nominalValueReports
  })
  
  output$eventsTable <- renderDataTable({
    req(msg2())
    cfla()$cashflowEventsByPeriod},
    options = list(autoWidth = TRUE, scrollX = TRUE)
  )
  
  ### Analytical Results Tab
  scale <- reactive({
    scales <- c(1, 1e3, 1e6, 1e9)
    names(scales) <- c("None", "1k", "1M", "1B")
    as.numeric(scales[input$selectScale])
  })
  
  roundDigits <- reactive({input$selectDigits})
  
  # output$scale <- renderPrint({
  #   scale()
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
  
  selectedPtf <- reactive({
    req(input$selectDrilldownNode)
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
  
  ### Reporting Tab
  source(paste0("./data_patrick/reportingDemoFiles/fnc_ReportingDemo.R"))
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
    
    
    
    output$portfolioDF <- renderDataTable(evTab_WithDates()[eval(parse(text=customfilter())),.(Cashflow=sum(payoff), NrOfAggregatedPositions =.N), by= .(reportingLineDesc, reportingLine, TimeBucket, type, currency)],
                                          options = list(autoWidth = TRUE, scrollX = TRUE))
    
  })
  
}      #server close