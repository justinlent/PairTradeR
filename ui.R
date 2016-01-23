useAmazonS3 <- TRUE

library(RAmazonS3)  # for accessing Amazon S3 bucket files
library(shiny)
library(shinyAce)
library(shinyIncubator)
library(stringr)
# library(data.table)   # data.table is faster than data.frame in lots of instances

dbBacktestResultsDir_stub <- "RstrategyBacktests/basketTrader/"
dbBacktestResultsDir <- "RstrategyBacktests/basketTrader/all/"
dbBacktestResultsDir_good <- "RstrategyBacktests/basketTrader/good/"
dbBacktestResultsDir_excellent <- "RstrategyBacktests/basketTrader/excellent/"
dbBacktestResultsDir_flag <- "RstrategyBacktests/basketTrader/flag/"

# CHANGE ME:  Enter your Amazaon S3 creds below
if( useAmazonS3 ){
  S3authCode <<- c('XXXXXXXXXX' = "YYYYYYYY") 
  boS3 <- Bucket( "XXXXXXXXXXX", auth=S3authCode )   
}

lastVec <- function( theVec ){
  return( theVec[ length(theVec) ] )  
}

listDirS3 <- function( bucketObjS3, filterFilesByFolderStr=NULL ){
  # exclude log directories/files
  bucketFiles <- names(bucketObjS3)[ !str_detect(names(bucketObjS3),"logs/") ]
  if( is.null(filterFilesByFolderStr) ){
    
  } else {
    bucketFiles <- names(bucketObjS3)[ str_detect(names(bucketObjS3),paste(filterFilesByFolderStr,"/",sep="")) ]
    tempSplitList <- lapply(bucketFiles, str_split, pattern="/")
    tempBucketFiles <- c()
    # print(length(tempSplitList))
    # print(tempSplitList)
    for( i in seq(1:length(tempSplitList)) ){
      tempLast <- lastVec( tempSplitList[[ i ]][[ 1 ]] )
      # print(tempLast)
      
      if( tempLast != "" ){
        tempBucketFiles <- c(tempBucketFiles, tempLast)  
      }
    }
    bucketFiles <- tempBucketFiles
  }
  
  return( bucketFiles )
}

shinyUI(pageWithSidebar(
  
  # Application title
  h4("Pair & Basket Trading Strategy Backtester (Built using R, using R Shiny API for web, and saves results to Amazon S3)"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    # progressInit() ,
    actionButton(inputId="selectOper1_AB",label="Click Here when Ready to Run Backtest...") ,
    
    radioButtons( inputId="selectFromChoices_RB", label="Pair or Basket?"
                  , choices=c( "Pair" = "selectOper0_1" 
                               , "Basket" = "selectOper0_2" 
                               , "Load a Saved Backtest" = "selectOper0_3"
                  ) 
    ) ,
    
    conditionalPanel(
      condition = "input.selectFromChoices_RB == 'selectOper0_1' ",
      radioButtons( inputId="selectFirstOper_RB", label="Choose how to create the spread: ",selected="selectOper1"
                    , choices=c( "Price Levels, beta-weighted ( ie:  SPY - beta * QQQ )" = "selectOper1" 
                                 , "Log-Price Levels, beta-weighted ( ie:  log(SPY) - beta * log(QQQ) )" = "selectOper2" 
                                 , "Ratio (ie: SPY / QQQ)" = "selectOper3" 
                                 # , "View Details of Previous Backtest" = "selectOper4"
                                 # , "Price Level, volatility-weighted" = "selectOper5" 
                                 
                    ) 
      ) 
    ) ,
    
    conditionalPanel(
      condition = "input.selectFromChoices_RB == 'selectOper0_2' ",
      radioButtons( inputId="selectFirstOper_RB", label="Choose how to create the spread: ",selected="selectOper1"
                    , choices=c( "Price Levels, beta-weighted ( ie:  SPY - beta * QQQ )" = "selectOper1" 
                                 # , "Log-Price Levels, beta-weighted ( ie:  log(SPY) - beta * log(QQQ) )" = "selectOper2" 
                                 # , "Ratio (ie: SPY / QQQ)" = "selectOper3" 
                                 # , "View Details of Previous Backtest" = "selectOper4"
                                 # , "Price Level, volatility-weighted" = "selectOper5" 
                                 
                    ) 
      ) 
    ) ,
    
    conditionalPanel(
      condition = "input.selectFromChoices_RB == 'selectOper0_3' ",
      wellPanel(
        radioButtons( inputId="selectGoodExcellentFlag_RB", label="Backtests to choose from:"
                      , choices=c( "All" = "all"
                                   , "Good" = "good" 
                                   , "Excellent" = "excellent" 
                                   , "Flagged for Followup" = "flag"
                      ) 
        ) 
        , conditionalPanel(
          condition = "input.selectGoodExcellentFlag_RB == 'all' "
          , selectInput( inputId="selectSavedBacktest_SI", label="Select a previously saved backtest result:", width='100%'
                         #  , choices=list.files(dbBacktestResultsDir) 
                         , choices=listDirS3(boS3,filterFilesByFolderStr="all")
          )
        )
        , conditionalPanel(
          condition = "input.selectGoodExcellentFlag_RB == 'good' "
          , selectInput( inputId="selectSavedBacktest_SI", label="Select a previously saved backtest result:", width='100%'
                         #  , choices=list.files(dbBacktestResultsDir_good) 
                         , choices=listDirS3(boS3,filterFilesByFolderStr="good")
          )
        )
        , conditionalPanel(
          condition = "input.selectGoodExcellentFlag_RB == 'excellent' "
          , selectInput( inputId="selectSavedBacktest_SI", label="Select a previously saved backtest result:", width='100%'
                         #   , choices=list.files(dbBacktestResultsDir_excellent) 
                         , choices=listDirS3(boS3,filterFilesByFolderStr="excellent")
          )
        )
        , conditionalPanel(
          condition = "input.selectGoodExcellentFlag_RB == 'flag' "
          , selectInput( inputId="selectSavedBacktest_SI", label="Select a previously saved backtest result:", width='100%'
                         # , choices=list.files(dbBacktestResultsDir_flag) 
                         , choices=listDirS3(boS3,filterFilesByFolderStr="flag")
                         
          )
        )
        , actionButton(inputId="loadPrevBacktest_AB",label="Load Saved Results")
      )
    ) ,
    
    radioButtons( inputId="selectSecondOper_RB", label="Choose the type of parameters to run the backtests over: "
                  , choices=c( "Spread Entry & Exit Z-scores for Trade Triggers" = "selectOper2_1"
                               ,"Lookback for Hedge-ratio, and Lookback for Z-score . (This would be for a continuous investment strategy, where position size is equal to the negative-Zscore)" = "selectOper2_2" 
                               
                  ) 
    ) ,
    
    wellPanel(
      checkboxInput(inputId="useMonthlyPrices_CB", label="Use monthly prices instead of daily", value=FALSE) 
      , checkboxInput(inputId="useMSCIdata_CB", label="If available, stitch additional index price history with corresponding ETF", value=FALSE)  
    ) ,
    
    conditionalPanel(
      condition = "input.selectFromChoices_RB == 'selectOper0_1' ",
      wellPanel( 
        helpText( "Enter the 2 stocks in the pair." ) ,
        helpText(" (Use symbols valid on Yahoo Finance)" ) ,
        textInput(inputId="stock1_TI", label="Stock 1", value="USO") ,
        textInput(inputId="stock2_TI", label="Stock 2", value="GLD")
      ) 
    ) ,
    
    conditionalPanel(
      condition = "input.selectFromChoices_RB == 'selectOper0_2' ",
      wellPanel( 
        helpText( "Enter the ticker symbols for your basket" )
        , helpText(" Use symbols valid on Yahoo Finance" )
        , checkboxInput(inputId="showQuandl_CB", label="See how to use Quandl data...", value=FALSE)
        , conditionalPanel(
          condition = "input.showQuandl_CB == 1 ",
          wellPanel(
            helpText("QUANDL DATA REQUESTS ")
            , helpText(" Pull any dataset from Quandl by adding this prefix before a valid Quandl symbol")
            , helpText(" Q:columnNumber:MySymbolName: ")
            , helpText(" Example: To Pull settlement price of gold futures with the Quandl symbol 'OFDP/FUTURE_GC1' ")
            , helpText(" Q:4:MyGoldPrice:OFDP/FUTURE_GC1 ")
            , helpText(" Example: To backtest a basket of Crude Oil, Gold, Silver (CL1, GC1, SI1) futures, enter the following below")
            , helpText(" Q:4:CL1:OFDP/FUTURE_CL1, Q:4:GC1:OFDP/FUTURE_GC1, Q:4:SI1:OFDP/FUTURE_SI1 ")
            , helpText(" Example: To backtest a basket of Crude Oil, the XLE Energy ETF, and XOM stock enter the following")
            , helpText(" Q:4:CL1:OFDP/FUTURE_CL1, XLE, XOM ")
          ) 
        )
        # , textInput(inputId="stockBasket_TI", label="Basket of Securities (Separate each with a comma)", value="USO,GLD")
        , helpText( "Basket of Securities (Separate each with a comma)" ) 
        , tags$textarea(id="stockBasket_TI", rows=5, cols=100, "USO,GLD")
        , checkboxInput(inputId="useMSCIdata_CB", label="If available, stitch additional index price history with corresponding ETF", value=FALSE)
        # , checkboxInput(inputId="useAdvancedMR_CB", label="Show Advanced Mean-Reversion filters...", value=FALSE)
        , conditionalPanel(
          condition = "input.selectSecondOper_RB == 'selectOper2_1' " ,
          checkboxInput(inputId="useAdvancedMR_CB", label="Show Advanced Mean-Reversion filters...", value=FALSE)
          , conditionalPanel(
            condition = "input.useAdvancedMR_CB == 1 " ,
            wellPanel(
              helpText("Advanced filters to apply to spread before triggering a trade" ) 
              , checkboxInput(inputId="filterADFp_CB", label="ADF-test P-value", value=FALSE)
              , numericInput(inputId="ADFmaxP_NI", label="Max Acceptable P-value:", value=0.05) 
              , checkboxInput(inputId="filterOU_CB", label="O-U process 'mean-reverting/trend' term", value=FALSE)
              , numericInput(inputId="maxOUtrend_NI", label="Max Acceptable O-U process trend term (< 0.0 implies mean-reversion):", value=0.0)
              , checkboxInput(inputId="filterHalflife_CB", label="Half-life of mean reversion", value=FALSE)
              , numericInput(inputId="maxHalflife_NI", label="Max Acceptable Half-life:", value=63)
              , checkboxInput(inputId="filterHurst_CB", label="Hurst Exponent (< 0.5 implies mean-reversion)", value=FALSE)
              , numericInput(inputId="maxHurst_NI", label="Max Acceptable Hurst exponent value", value=0.50)
            )
          )
        )
      ) 
    ) ,
    
    conditionalPanel(
      # Z-score
      condition = "input.selectSecondOper_RB == 'selectOper2_1' ",
      
      wellPanel(
        helpText("Specify parameters for creating the spread.") 
        , conditionalPanel(
          condition = "input.selectFirstOper_RB == 'selectOper1' || input.selectFirstOper_RB == 'selectOper2' "
          , numericInput(inputId="signalZscoreLookback_NI", label="Z-score lookback days (or months) for trade signals", value=20,min=2,max=999999)  
        )
        , numericInput(inputId="betaLookback_NI", label="Hedge-ratio (ie: beta) calculation lookback days (or months, if selected above)", value=20,min=5,max=999999) 
        , numericInput(inputId="betaComputeFreq_NI", label="Recalculate pair trade hedge-ratio (ie: beta) every 'X' days (or months)", value=1,min=1,max=999999) 
        
      )
    ) ,
    
    conditionalPanel(
      condition = "input.selectSecondOper_RB == 'selectOper2_2' ",
      wellPanel(
        helpText("Specify Lookback Windows (in days, or months if selected above) for each:")
        , helpText("2) Z-score Lookback (Position size will be equal to the negative Z-score)")
        , numericInput(inputId="zScoreLookback_min_NI", label="* minimum", value=10,min=5,max=999999)
        , numericInput(inputId="zScoreLookback_max_NI", label="* maximum", value=20,min=5,max=999999)
        , numericInput(inputId="zScoreLookback_step_NI", label="* increment", value=10,min=1,max=999999)
        , helpText("1) Hedge-Ratio Lookback (ie: beta)")
        , numericInput(inputId="betaLookback_min_NI", label="* minimum", value=20,min=5,max=999999)
        , numericInput(inputId="betaLookback_max_NI", label="* maximum", value=40,min=5,max=999999)
        , numericInput(inputId="betaLookback_step_NI", label="* increment", value=20,min=1,max=999999)
      )
    ) ,
    
    wellPanel(
      radioButtons( inputId="tagGoodExcellentFlag_RB", label="Tag the Current Backtest Result as..."
                    , choices=c( "Good" = "good" 
                                 , "Excellent" = "excellent" 
                                 , "Flag for Followup" = "flag"
                    ) 
      ) ,
      
      actionButton(inputId="tagBacktest_AB",label="Tag Completed Backtest Result")
    ) ,
    
    wellPanel(
      helpText("(Optional) Specify start and end dates for the backtest.") ,
      helpText("Leave blank to use all available data.") ,
      textInput(inputId="backtestStartDate_TI", label="Backtest Start date (example: 2000-1-31)", value="") ,
      textInput(inputId="backtestEndDate_TI", label="Backtest End date (example: 2013-12-31)", value="")
    ) 
    , width = 3
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    progressInit() ,
    
    wellPanel( 
      # h4("Current Data After the Applied Operations, Etc.") ,
      # h5("Backtest Results")
      verbatimTextOutput(outputId="reportTitle")
      , tabsetPanel(id="outputTabs",
                    tabPanel(title="Signal Performance Heatmaps", plotOutput(outputId="tab_backtestResultsHeatmaps",width="100%",height="475px"), value=1) 
                    , tabPanel(title="ETF's with Extended History (Monthly Prices only for MSCI)", verbatimTextOutput(outputId = "tab_extHistory"), value=2)
                    , tabPanel(title="All Saved Backtests", verbatimTextOutput(outputId = "tab_savedBacktests"), value=3)
                    , tabPanel(title="(Excellent)", verbatimTextOutput(outputId = "tab_savedBacktestsExcellent"), value=4)
                    , tabPanel(title="(Good)", verbatimTextOutput(outputId = "tab_savedBacktestsGood"), value=5)
                    , tabPanel(title="(Flagged for Followup))", verbatimTextOutput(outputId = "tab_savedBacktestsFlag"), value=6)
                    #    , tabPanel(title="Specific Run Details", verbatimTextOutput(outputId = "tab_specificRun"), value=2)
                    #    , tabPanel(title="Command Log", verbatimTextOutput(outputId="tab_backtestLog"), value=3)
                    # , tabPanel(title="About", verbatimTextOutput(outputId="tab_about"), value=9)
      )
      
    ) ,
    
    wellPanel(
      h5("Details of Best Total Return Strategy Computed Above") ,
      
      tabsetPanel(id="tabs_Returns",
                  tabPanel(title="Monthly & Annual Returns", plotOutput(outputId="tab_CalRetHeatmap_best", width="100%",height="700px"), value=30) 
                  , tabPanel(title="Table", dataTableOutput(outputId="tab_CalendarTable_best"), value=31) 
                  , tabPanel(title="Raw Text", verbatimTextOutput(outputId = "tab_CalendarRaw_best"), value=32)     
                  #   , tabPanel(title="Spread Details", plotOutput(outputId="tab_spreadDetails",width="100%",height="700px"), value=33)
      )
    ) ,
    
    wellPanel(
      # h5("(1)-Stocks & Spread, (2)-Spread Z-score, (3)-Strategy Performance if Continuously Investing 'Z' units") ,
      h5("Top 10 Strategy NAV's & Timeseries Plots") ,
      tabsetPanel(id="tabs_Returns"
                  , tabPanel(title="Plots", plotOutput(outputId="tab_stocksAndSpread",width="100%",height="750px"), value=20)
                  
      ) ,
      # verbatimTextOutput(outputId="contInv_perfStats")
      plotOutput(outputId="contInv_perfStats",width="100%",height="75px")
    ) ,
    
    wellPanel(
      h5("Monthly/Annual Returns (For the basic continusouly invested strategy directly above.)") ,
      h5("NOTE: Only shown when testing over Z-score triggers)") ,
      
      tabsetPanel(id="tabs_Returns",
                  tabPanel(title="Monthly & Annual Returns", plotOutput(outputId="tab_CalRetHeatmap", width="100%",height="700px"), value=10) 
                  , tabPanel(title="Table", dataTableOutput(outputId="tab_CalendarTable"), value=11) 
                  , tabPanel(title="Raw Text", verbatimTextOutput(outputId = "tab_CalendarRaw"), value=12)     
      )
    ) ,
    
    wellPanel(
      h5("Stock, Spread, & Signal Raw Data") ,
      h5("NOTE: Only shown when testing over Z-score triggers)") ,
      dataTableOutput(outputId="tab_rawData") 
    ) 
    
    , width = 9
  )
) ) 

