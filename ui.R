
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(shinyAce)
library(shinyIncubator)

shinyUI(pageWithSidebar(
  
  # Application title
  h4("Pair Trading Strategy Backtester"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    # progressInit() ,
    radioButtons( inputId="selectFirstOper_RB",label="Choose how to create the pair spread: ",
                  choices=c( "Price Levels, beta-weighted (ie:  SPY - beta * QQQ)" = "selectOper1" 
                  #           , "View Details of Previous Backtest" = "selectOper2"
                  #          , "Ratio (ie: SPY / QQQ)" = "selectOper2" 
                  #          , "Price Level, volatility-weighted" = "selectOper3" 
                  #          , "'Collect RDD' & Save in R format" = "selectOper4"
                  ) 
    ) ,
    
    conditionalPanel(
      condition = "input.selectFirstOper_RB == 'selectOper1'",
      wellPanel( 
        helpText("Enter the 2 stocks in the pair.") ,
        textInput(inputId="stock1_TI", label="Stock 1", value="USO") ,
        textInput(inputId="stock2_TI", label="Stock 2", value="GLD") ,
        helpText("Specify parameters for creating the spread.") ,
        numericInput(inputId="betaLookback_NI", label="Hedge-ratio (ie: beta) calculation lookback days", value=20,min=5,max=999999) ,
        numericInput(inputId="betaComputeFreq_NI", label="Recalculate pair trade hedge-ratio (ie: beta) every 'X' days", value=1,min=1,max=999999) ,
        numericInput(inputId="signalZscoreLookback_NI", label="Z-score lookback days for trade signals", value=20,min=2,max=999999)
        
      ) ,
      wellPanel(
        helpText("(Optional) Specify start and end dates for the backtest.") ,
        helpText("Leave blank to use all available data.") ,
        textInput(inputId="backtestStartDate_TI", label="Backtest Start date (example: 2000-1-31)", value="") ,
        textInput(inputId="backtestEndDate_TI", label="Backtest End date (example: 2013-12-31)", value="")
      ) ,
      
      actionButton(inputId="selectOper1_AB",label="Run Backtest") 
      
    )
    
  ),

# Show a plot of the generated distribution
mainPanel(
  progressInit() ,
  
  wellPanel( 
    # h4("Current Data After the Applied Operations, Etc.") ,
    h5("Backtest Results") ,
    tabsetPanel(id="outputTabs",
                tabPanel(title="Z-score Signal Performance Heatmaps (1-unit per trade)", plotOutput(outputId="tab_backtestResultsHeatmaps",width="100%",height="475px"), value=1) ,
                tabPanel(title="Specific Run Details", verbatimTextOutput(outputId = "tab_specificRun"), value=3) ,
                tabPanel(title="Command Log", verbatimTextOutput(outputId="tab_backtestLog"), value=9) 
                
    ) 
  ) ,
  
  wellPanel(
    h5("(1)-Stocks & Spread, (2)-Spread Z-score, (3)-Strategy Performance if Continuously Investing 'Z' units") ,
    plotOutput(outputId="tab_stocksAndSpread",width="100%",height="600px")
  ) ,
  
  wellPanel(
    h5("Monthly/Yearly Returns (for continusouly invested strategy directly above") ,
    verbatimTextOutput(outputId = "table_calendarReturns") 
  ) ,
  
  wellPanel(
    h5("Stock, Spread, & Signal Raw Data") ,
    dataTableOutput(outputId="tab_rawData") 
  ) 
  
  
)
) ) 
