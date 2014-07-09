
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

require(quantmod)     # lots of timeseries utilities
require(xts)         # timeseries library 
require(ggplot2)      # used for time plots/graphing 
require(scales)       # used for rescaling ggplot2 colors
require(ggthemes)     # more themes and plot styles for ggplot2
require(PerformanceAnalytics)   # some additional timeseries utilities specific to testing trading strategies
require(TTR)          # used for rolling window computations on timeseries
require(reshape2)     # used for nice colors in plotting timeseries/histograms/barcharts/scatterplots
require(RColorBrewer) # used for nice colors in plotting timeseries/histograms/barcharts/scatterplots
require(gdata)        # exposes read.xls functions, as well as A LOT of other general utilities
require(urca)       # this is for cointegration testing
require(tseries)    # this is for cointegration testing and financial timeseries functions
require(shiny)      # R Shiny web UI
require(stringr)    # helper functions for working with strings
require(rbenchmark)  # good for benchmarking R performance
require(timeDate)   # helper functions for working with times and dates for timeseries data
require(plyr)       # super useful utilities package
require(dplyr)      # Requires R v3.0 update to 'plyr' package above. faster -- all written in C
require(gmodels)  # for contingency tables
require(formula.tools) # utilities for working with formulas. like printing them easily as strings, etc
require(data.table)   # data.table is faster than data.frame in lots of instances
# require(RQuantLib)
require(devtools) # for installing packages directly from github
require(sos)        # for easier searching of R help docs
require(RcppRoll)
require(sde)        # for O-U process functions
require(ouch)       # for O-U process functions

require(DAAG)       # k-fold cross validation. and Data Analysis And Graphics
require(DAAGxtras)
require(cvTools)    # package for easy kfold cross validation of models (e.g.: cv.lm() )
require(bootstrap)  # also includes a cross-validation function crossval() to evaluate R2/RSQ shrinkage
require(bootStepAIC)# stepwise regression via AIC selection
require(MASS)       # for stepwise regression
require(nnls)       # non-negative least squares algo for linear regressions with only positive coefficients
require(rms)        # Regression Modeling Strategies
require(car)        # Companion to Applied Regression
# require(nls)        # non-linear regression
require(nls2)        # non-linear regression
# require(rlm)        # robust regression
require(relaimpo)   # determine relative importance of variables
require(bestglm)    # algo to determine best fit linear models
require(betareg)    # beta regression is good for fitting models when the dependent variable 'y' is a rate/yield btwn 0 and 1
require(CADFtest)   # the the Cointegrated Augmented-Dickey-Fuller (ADF) test 
require(fUnitRoots) # for a better ADF test and other unit root test
require(fArma)      # for more time series modeling, Hurst exponent, FFT, wavelets, etc
require(fractal)    # for Hurst exponent
require(pracma)     # for Hurst Exponent, and numerical math functions

require(shinyIncubator) # for progress bar

# library(shiny)
# library(quantmod)
# library(PerformanceAnalytics)

# -------- CHART PLOTTING FUNCTIONS -----------------------------------------------
source('~/Dropbox/LBCMRMetrics/FunctionLibrary/generalUtilities.R')
# -------- END CHART PLOTTING -----------------------------------------------------

# -------- TRADING FUNCTIONS ------------------------------------------
source('~/Dropbox/LBCMRMetrics/FunctionLibrary/strategySim.R')
source('~/Dropbox/LBCMRMetrics/FunctionLibrary/zScore.R')
source('~/Dropbox/R_Justin/spreadTrading/spreadFunctions.R')
# -------- END TRADING FUNCTIONS ------------------------------------------------

prev_selectOper1_AB <<- 0
symbolsLoaded <<- c()
runningLogStack <<- NULL
runningLogStack <<- list()
runningLogStack_count <<- 1
currentDataDF <<- data.frame(A="Aa", B="Bb")
backtestResult <<- list()
startDate <<- ''
endDate <<- ''

stock1Str <<- ""
stock2Str <<- ""
stock1DF <<- xts()
stock2DF <<- xts()

stock1dates <<- c()
stock1startDate <<- ''
stock1endDate <<- ''
stock2dates <<- c()
stock2startDate <<- ''
stock2endDate <<- ''

shinyServer( function(input, output, session) {

  runBacktest <- function() {  
      stock1Str <<- upperCase(input$stock1_TI)
      stock2Str <<- upperCase(input$stock2_TI)
      
      startDate <<- input$backtestStartDate_TI
      endDate <<- input$backtestEndDate_TI
      betaLookbackDays <- input$betaLookback_NI
      betaRecalcFreq <- input$betaComputeFreq_NI
      zScoreLookbackDays <- input$signalZscoreLookback_NI
      
      if( input$backtestStartDate_TI == "" && input$backtestEndDate_TI == "" ) {
        startDate <<- '1900-1-1'
        endDate <<- '2100-1-1'
      }
      
      symbolsLoaded <<- getSymbols(c(stock1Str, stock2Str), from=startDate, auto.assign=TRUE)
      # can then do, for example, get(symbolsLoaded[1]) to get stock1 
      stock1dates <<- index( get(stock1Str) )
      stock1startDate <<- stock1dates[1]
      stock1endDate <<- stock1dates[ length(stock1dates) ]
      stock2dates <<- index( get(stock2Str) )
      stock2startDate <<- stock2dates[1]
      stock2endDate <<- stock2dates[ length(stock2dates) ]
      
      newMsg <- paste("Symbols loaded:", 
                      stock1Str, stock1startDate, stock1endDate , "; " ,
                      stock2Str, stock2startDate, stock2endDate )
      
      runningLogStack[[runningLogStack_count]] <<- newMsg
      runningLogStack_count <<- runningLogStack_count + 1
      
      # Run the steps to run the actual backtest
      stock1DF <<- get(stock1Str)
      stock2DF <<- get(stock2Str)
      
      # cat(dim(stock1DF),"\n")
      # cat(dim(stock2DF),"\n")
      
      computeRollingResults <- pairSpreadTestRolling(stock1DF[,6], stock2DF[,6], startDate, endDate, rollingWindowLength=betaRecalcFreq, totalLookbackLength=betaLookbackDays )
      
      newMsg <- paste("Prepared Data and Computed rolling hedge ratios for the spread.")
      runningLogStack[[runningLogStack_count]] <<- newMsg
      runningLogStack_count <<- runningLogStack_count + 1
      
      backtestResult <<- computeAllZscoreSignals(resultsXTS=computeRollingResults,zScoreLookback=zScoreLookbackDays,showMultiplots=FALSE,showAllOnSingleChart=FALSE,calledFromShinyWithProgressBar=TRUE)
      
      newMsg <- paste("Backtest Run on symbols:", stock1Str, stock2Str)
      runningLogStack[[runningLogStack_count]] <<- newMsg
      runningLogStack_count <<- runningLogStack_count + 1
      
      # currentDataDF <<- backtestResult[["rawData"]]$rawInputDataDF
      currentDataDF <<- backtestResult[["rawData"]]$rawDataAndSpreadAndZscores
  } 
  
  observe( {
    if( input$selectOper1_AB > 0 ) {
        isolate( {
          numSteps <- ((3.0 - 0.5) / 0.25) * ((2.0 - 0.0) / 0.25) + 5
          withProgress( session, min=1, max=numSteps, {
            setProgress(message = 'Backtest Calculations in progress',
                        detail = 'Time depends on dates of backtest and # of trades triggered... ')
            setProgress(value = 5)
            
            runBacktest()
            
            setProgress(value = numSteps)
          } )
        } )
    }
  } )
  
  output$tab_rawData <- renderDataTable( {
    if( input$selectOper1_AB > 0 ) {
      # grab the dates from the XTS index
      dates <- as.character(as.Date(index(currentDataDF)))
      # names(datesVec) <- c("Date")
      # grab the Z-score and merge with rest of data
      
      return( cbind( dates , as.data.frame(currentDataDF)[ , 1:12 ] ) )  
    }
  } , options = list( iDisplayLength = 560 
                      , aLengthMenu = c(10,30,50,100,400,1000)
                    #  , aoColumns = c( c("asSorting", c("desc")) )
                    #  , aoColumns = c( c("desc"), c("desc") ) 
                    )
  )
  
  output$tab_backtestLog <- renderPrint( {
    if( input$selectOper1_AB > 0 ) {
      isolate( {
          return(  runningLogStack )
      } )
    }
  } )
  
  output$tab_backtestResultsHeatmaps <- renderPlot( {
    
      if( input$selectOper1_AB > 0 ) {
        isolate( {
          return( print( multiplot(plotlist=backtestResult[["performanceHeatmaps"]]$heatmaps_list,cols=3) ) )      
        } )
      }
    
  } )
  
  output$tab_stocksAndSpread <- renderPlot( {
    if( input$selectOper1_AB > 0 ) {
      
      isolate( {
        # cat( summary(backtestResult) )
        # cat( class(backtestResult[["generalInfo"]]$spread ) )
        commonStartDate <- index(backtestResult[["continuouslyInvested"]]$NAV)[1]
        # commonEndDate <- index(stock1DF)[length(index(stock1DF))]
        commonEndDate <- index(backtestResult[["continuouslyInvested"]]$NAV)[length(index(backtestResult[["continuouslyInvested"]]$NAV))]
        
        # HELPER:  mergeDF[index(mergeTS) >= startDate & index(mergeTS) <= endDate ,]
        
        # stocksPlot <- plotTimeseries( list(stock1DF[,6], stock2DF[,6], backtestResult[["generalInfo"]]$spread), legendPosition="top" )
        stocksPlot <- plotTimeseries( list(stock1DF[index(stock1DF) >= commonStartDate & index(stock1DF) <= commonEndDate, 6], 
                                           stock2DF[index(stock2DF) >= commonStartDate & index(stock2DF) <= commonEndDate, 6], 
                                           backtestResult[["generalInfo"]]$spread[index(backtestResult[["generalInfo"]]$spread) >= commonStartDate & index(backtestResult[["generalInfo"]]$spread) <= commonEndDate, ]
                                           ), legendPosition="top" )
        
        # NAVplot <- plotTimeseries( list(backtestResult[["continuouslyInvested"]]$NAV), legendPosition="top" )
        # spreadZscore <- plotTimeseries( list(backtestResult[["generalInfo"]]$spreadZscore), legendPosition="top" )
        
        NAVplot <- plotTimeseries( list(backtestResult[["continuouslyInvested"]]$NAV[index(backtestResult[["continuouslyInvested"]]$NAV) >= commonStartDate 
                                                                                     & index(backtestResult[["continuouslyInvested"]]$NAV) <= commonEndDate, ]), legendPosition="top" )
        
        spreadZscore <- plotTimeseries( list(backtestResult[["generalInfo"]]$spreadZscore[index(backtestResult[["generalInfo"]]$spreadZscore) >= commonStartDate 
                                                                                          & index(backtestResult[["generalInfo"]]$spreadZscore) <= commonEndDate, ]), legendPosition="top" )
        
        return( print(multiplot(plotlist=list(stocksPlot
                                              , spreadZscore
                                              , NAVplot
                                              )
                                              , cols=1 ) 
              ) )       
      } )
    }
    
  } )
  
  
} )
