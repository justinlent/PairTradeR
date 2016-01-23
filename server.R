# --- GLOBAL FLAGS --------------
#
#

useAmazonS3 <- TRUE

usePreloadedStocks <- FALSE
showDefaultResults <- TRUE

# usePreloadedStocks <- TRUE
# showDefaultResults <- FALSE

#
# --- END GLOBAL FLAGS ----------

# library(rDrop)    # for accessing Dropbox data files
library(RAmazonS3)  # for accessing Amazon S3 bucket files
library(Quandl)     # for pulling Quanda data
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

# -------- CHART PLOTTING FUNCTIONS -----------------------------------------------
source('utilityFunctions/generalUtilities.R')
# -------- END CHART PLOTTING -----------------------------------------------------

# -------- TRADING FUNCTIONS ------------------------------------------------------
source('utilityFunctions/strategySim.R')
source('utilityFunctions/zScore.R')
source('utilityFunctions/spreadFunctions.R')
# -------- END TRADING FUNCTIONS --------------------------------------------------


if( useAmazonS3 ){
  # CHANGE ME:  Enter your personal codes/key here
  S3authCode <<- c('XXXXXXXXX' = "YYYYYYYYYYYY") 
  boS3 <- Bucket( "s3baskettrader", auth=S3authCode )   
}

#
# -------- END Amazon AUTH ------------------------------------------------------------

# -------- load Quandl auth  -----------------------------------------------------------
# CHANGE ME:  Put your Quandl auth key here
Quandl.auth("QQQQQQQQQQQQ")
# -------- END Quandl AUTH ------------------------------------------------------------

dbBacktestResultsDir_stub <- "RstrategyBacktests/basketTrader/"
dbBacktestResultsDir <- "RstrategyBacktests/basketTrader/all/"
dbBacktestResultsDir_good <- "RstrategyBacktests/basketTrader/good/"
dbBacktestResultsDir_excellent <- "RstrategyBacktests/basketTrader/excellent/"
dbBacktestResultsDir_flag <- "RstrategyBacktests/basketTrader/flag/"

# dbBacktestResultsDir <- "savedBacktests/"

if( usePreloadedStocks ){
  # preloadStocks <- c("^GSPC","^NDX", "SPY", "IWM", "QQQ"
  #                   , "EWA","EWC","EFA", "EEM","EWU","EWT","EZA","EWJ","EWH","EWG","SCZ","ACWI","EZU"
  #                   , "DGT", "XME","XLY","XLP","XLF","XLI","XRT","XLE"
  #                   , "XOM","GE", "IBM", "GS", "AAPL", "GOOG","GG","NEM"
  #                   , "GLD","SLV","GDX","GDXJ","USO"
  #                   , "^TYX","^TNX","TLT"
  # )
  # preloadStocks_noCarets <- str_replace(preloadStocks,"[\\^\\]", "")
  # getSymbols(preloadStocks, from='1970-01-01', auto.assign=TRUE)  
  # save(file="histdata/preloadedStocks/preloadedStocks.RData",list=c(preloadStocks_noCarets,"preloadStocks","preloadStocks_noCarets"))
  if( useAmazonS3 ){
    
  } else {
    
  }
  load(file="histdata/preloadedStocks/preloadedStocks.RData")
  symbolsLoaded <<- preloadStocks_noCarets
}

msciPath <- "histdata/msci/"
msciStocks <- c("EWA","EWC","EFA", "EEM","EWU","EWT","EZA","EWJ","EWH","EWG","SCZ","ACWI","EZU")

otherExtraHistorySymbols <- c("SPY","QQQ")
saveFileName <<- ""

# prev_selectOper1_AB <<- 0
symbolsLoaded <<- c()
runningLogStack <<- NULL
runningLogStack <<- list()
runningLogStack_count <<- 1
currentDataDF <<- data.frame(A="Aa", B="Bb")
backtestResult <<- list()
savedBacktest <<- list()
startDate <<- ''
endDate <<- ''

stock1Str <<- ""
stock2Str <<- ""
stock1DF <<- xts()
stock2DF <<- xts()
basketStocks <<- NULL
basketStocksDataList <<- list()
depVarVec <<- xts()
indVarsList <<- list()

stock1dates <<- c()
stock1startDate <<- ''
stock1endDate <<- ''
stock2dates <<- c()
stock2startDate <<- ''
stock2endDate <<- ''

tempMonthlyRet <<- NULL
tempMonthlyRet_withYearMon <<- NULL
calendarReturnTableDF <<- NULL
calendarReturnTableDF_best <<- NULL
tempPerfStats <<- NULL

applyADF <<- FALSE
applyHurst <<- FALSE
applyOU <<- FALSE
applyHalflife <<- FALSE
applyMRoverall <<- FALSE
applyMeanReversionFilters <<- FALSE
inputCutoffADF <<- FALSE
inputCutoffHurst <<- FALSE
inputCutoffOU <<- FALSE
inputCutoffHalflife <<- FALSE

priceColToUse_yahoo <<- 6       # default to the Adj Close col for Yahoo Finance
priceColToUse_msci <<- 1
priceColToUse <<- priceColToUse_yahoo   # default to the Adj Close col for Yahoo Finance

useMonthlyPrices <<- FALSE
usingQuandl <<- FALSE
basketStocksWithQuandl <<- list()

if( showDefaultResults ){
  load(file='savedBacktests/defaultBacktest')
  currentDataDF <<- backtestResult[["rawData"]]$rawDataAndSpreadAndZscores  
}

lastVec <- function( theVec ){
  return( theVec[ length(theVec) ] )  
}

parseBasketSymbolInput <- function( allSymbolsInputStr ){
  sym1 <- str_split(string=allSymbolsInputStr,pattern=",")
  sym2 <- lapply(sym1, str_split, pattern=":")
  sym3 <- sym2[[1]]
  print(sym3)
  for( i in seq(from=1,to=length(sym3)) ){
    if( trim(sym3[[i]][[1]]) != "Q" ) {
      # its a Yahoo Finance ticker
      print("getting yhoo")
      tempSym <- getSymbols( c( trim(sym3[[i]][[1]]) ), from='1970-01-01', auto.assign=TRUE ) 
      basketStocksDataList[[ i ]] <<- get(tempSym[1])[,priceColToUse_yahoo]
      print(names(basketStocksDataList[[ i ]]))
      print(tempSym[1])
      print(head(basketStocksDataList[[ i ]]))
      names(basketStocksDataList[[ i ]]) <<- tempSym[1]  
    } else {
      # its a Quandl symbol
      usingQuandl <<- TRUE
      print(sym3[[i]][[1]])
      print(sym3[[i]][[2]])
      print(sym3[[i]][[3]])
      print(sym3[[i]][[4]])
      print("getting Quandl")
      tempQ <- getQuandlTimeseries( quandlSymbolStr=trim(sym3[[i]][[4]]), quandlColumnNumber=as.integer(trim(sym3[[i]][[2]])), mySymbolStr=trim(sym3[[i]][[3]]) )
      print(names(tempQ))
      basketStocksDataList[[ i ]] <<- tempQ
    }
    if( i != 1 ){
      indVarsList[[ i-1 ]] <<- basketStocksDataList[[ i ]]
    } else {
      depVarVec <<- basketStocksDataList[[ 1 ]]
    }
  }
  return(basketStocksDataList)
}

shinyServer( function(input, output, session) {
  
  #listDirS3 <- function( bucketObjS3 ){
  # exclude log directories/files
  #  bucketFiles <- names(bucketObjS3)[ !str_detect(names(bucketObjS3),"logs/") ]
  #  return( bucketFiles )
  # }
  
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
  
  
  loadSavedBacktest <- function( savedBacktestList ) {
    # print(length(savedBacktestList))
    #print(length(backtestResult))
    backtestResult <<- savedBacktestList
    # print(length(backtestResult))
  }
  
  saveUpdatedBacktest <- function( backtestList, saveFilenameStr, useAmazon=FALSE, S3bucketNameStr=NULL, S3auth=NULL ) {
    print( "saving updated backtest result")
    savedBacktest <- backtestList
    if( useAmazonS3 ){
      s3Save( savedBacktest ,file=paste(S3bucketNameStr,"/", saveFilenameStr,sep=""), auth=S3auth )
    } else {
      save( savedBacktest, file=saveFilenameStr )   
    }
  }
  
  observe( {
    if( input$tagBacktest_AB > 0 ){
      # the below 'load' operation loads a local variable called backtestResult which we need to store in the global backtestResult variable
      isolate( {
        print("Save button pressed")
        tempFile <- paste(dbBacktestResultsDir_stub, input$tagGoodExcellentFlag_RB, "/", saveFileName, sep="")
        print(tempFile)
        tagPlaceholderObject <- c("A")
        # saveUpdatedBacktest( backtestResult, tempFile )
        if( useAmazonS3 ){
          saveUpdatedBacktest( tagPlaceholderObject, tempFile, useAmazon=TRUE, S3bucketNameStr="s3baskettrader", S3auth=S3authCode )
        } else {
          saveUpdatedBacktest( tagPlaceholderObject, tempFile )  
        }
        
        # updateSelectInput( session,inputId="selectSavedBacktest_SI", choices=list.files(dbBacktestResultsDir) )
        print(paste("backtest saved using tag: ", input$tagGoodExcellentFlag_RB))
      } )
    }
  } )
  
  observe( {
    if( input$loadPrevBacktest_AB > 0 ){
      # the below 'load' operation loads a local variable called 'savedBacktest' which we need to store in the global backtestResult variable
      isolate( {
        #print("Load button pressed")
        tempFile <- paste(dbBacktestResultsDir, input$selectSavedBacktest_SI, sep="")
        print(tempFile)
        
        if( useAmazonS3 ){
          s3PathStr <- paste( "s3baskettrader", "/", dbBacktestResultsDir, input$selectSavedBacktest_SI, sep="" )
          # s3Load( paste("s3baskettrader","/", input$selectSavedBacktest_SI,sep=""), auth=S3authCode )
          print(s3PathStr)
          s3Load( s3PathStr, auth=S3authCode )
        } else {
          print( load(file=tempFile) )  
        }
        loadSavedBacktest(savedBacktest)
      } )
    }
  } )
  
  runBacktest <- function() {  
    # prev_selectOper1_AB <<- prev_selectOper1_AB + 1
    # -------- Reset all Global Variables ----------- #
    # prev_selectOper1_AB <<- 0
    saveFileName <<- ""
    symbolsLoaded <<- c()
    runningLogStack <<- NULL
    runningLogStack <<- list()
    runningLogStack_count <<- 1
    currentDataDF <<- data.frame(A="Aa", B="Bb")
    backtestResult <<- list()
    savedBacktest <<- list()
    startDate <<- ''
    endDate <<- ''
    
    stock1Str <<- ""
    stock2Str <<- ""
    stock1DF <<- xts()
    stock2DF <<- xts()
    basketStocks <<- NULL
    depVarVec <<- xts()
    indVarsList <<- list()
    
    stock1dates <<- c()
    stock1startDate <<- ''
    stock1endDate <<- ''
    stock2dates <<- c()
    stock2startDate <<- ''
    stock2endDate <<- ''
    
    tempMonthlyRet <<- NULL
    tempMonthlyRet_withYearMon <<- NULL
    calendarReturnTableDF <<- NULL
    calendarReturnTableDF_best <<- NULL
    tempPerfStats <<- NULL
    
    applyADF <<- FALSE
    applyHurst <<- FALSE
    applyOU <<- FALSE
    applyHalflife <<- FALSE
    applyMRoverall <<- FALSE
    applyMeanReversionFilters <<- FALSE
    inputCutoffADF <<- FALSE
    inputCutoffHurst <<- FALSE
    inputCutoffOU <<- FALSE
    inputCutoffHalflife <<- FALSE
    
    priceColToUse_yahoo <<- 6       # default to the Adj Close col for Yahoo Finance
    priceColToUse_msci <<- 1
    priceColToUse <<- priceColToUse_yahoo   # default to the Adj Close col for Yahoo Finance
    
    useMonthlyPrices <<- FALSE
    useMSCIdata <<- FALSE
    usingQuandl <<- FALSE
    basketStocksWithQuandl <<- list()
    
    # -------- End Reset all Global Variables --------- #
    
    
    useMonthlyPrices <<- input$useMonthlyPrices_CB
    useMSCIdata <<- input$useMSCIdata_CB
    
    startDate <<- input$backtestStartDate_TI
    endDate <<- input$backtestEndDate_TI
    betaLookbackDays <- input$betaLookback_NI
    betaRecalcFreq <- input$betaComputeFreq_NI
    zScoreLookbackDays <- input$signalZscoreLookback_NI
    
    applyMRoverall <<- input$useAdvancedMR_CB
    
    applyADF <<- input$filterADFp_CB & applyMRoverall
    applyHurst <<- input$filterHurst_CB & applyMRoverall
    applyOU <<- input$filterOU_CB & applyMRoverall
    applyHalflife <<- input$filterHalflife_CB & applyMRoverall
    
    inputCutoffADF <<- input$ADFmaxP_NI
    inputCutoffHurst <<- input$maxHurst_NI
    inputCutoffOU <<- input$maxOUtrend_NI
    inputCutoffHalflife <<- input$maxHalflife_NI
    
    applyMeanReversionFilters <<- applyADF | applyHurst | applyOU | applyHalflife
    
    if( input$selectFirstOper_RB == "selectOper1" ) {
      spreadTypeToUse <- "price"
    }
    if( input$selectFirstOper_RB == "selectOper2" ) {
      spreadTypeToUse <- "log-price"
    }
    if( input$selectFirstOper_RB == "selectOper3" ) {
      spreadTypeToUse <- "ratio"
    }
    if( input$backtestStartDate_TI == "" && input$backtestEndDate_TI == "" ) {
      startDate <<- '1900-1-1'
      endDate <<- '2100-1-1'
    }
    
    # PAIR - should be DEPRECATED for the 'price' spread type
    # the 'pair' only case should now be deprecated for the 'price' spread type as that case just uses the 'basket' functions with 2 stocks now
    if( input$selectFromChoices_RB == "selectOper0_1" ) {
      # if( FALSE ) {
      # store off the 2 stocks entered for the pair
      stock1Str <<- upperCase(input$stock1_TI)
      stock2Str <<- upperCase(input$stock2_TI)
      basketStocks <<- c(stock1Str,stock2Str)
      
      # TO-DO: add ability to sample by monthly dates only so can go back in history to 1970's on MSCI indexes
      #         E.g.: byMonthly <- timeSequence(from=start(tEEM2),to=end(tEEM2),by="month")
      #         E.g:  aggregate(as.timeSeries(tEEM2),byMonthly,last)
      #         E.g:  for option expiration days, 3rd Fridays:  timeNthNdayInMonth("2004-04-01", 5, 3)
      # Additional E.g.:   # What date has the first Monday on or after March 15, 1986 ?
      #                       timeNdayOnOrAfter("1986-03-15", 1)
      #             E.g.: First day of months:  timeFirstDayInMonth(charvec, format = "%Y-%m-%d")
      
      if( ! usePreloadedStocks ){
        print( "Downloading current prices...")
        symbolsLoaded <<- getSymbols(c(stock1Str, stock2Str), from=startDate, auto.assign=TRUE)  
      } else {
        print( "Using pre-loaded stock prices...")
      }
      
      # mget(c(stock1Str),ifnotfound=getSymbols(c(stock1Str), from=startDate, auto.assign=TRUE))
      # mget(c(stock2Str),ifnotfound=getSymbols(c(stock2Str), from=startDate, auto.assign=TRUE))
      # symbolsLoaded <<- c(stock1Str, stock2Str)
      
      # can then do, for example, get(symbolsLoaded[1]) to get stock1 
      stock1DF <<- get(stock1Str)
      stock2DF <<- get(stock2Str)
      
      if( useMonthlyPrices ){
        stock1DF <<- asMonthlyXTS(stock1DF)
        stock2DF <<- asMonthlyXTS(stock2DF)
      }
      
      if( useMSCIdata ){
        print( "Stiching in MSCI historical data...")
        print(stock1Str)
        msci1.xts <- loadPriceHistory_MSCIByETF_CSV(theFolderWithAllMSCIfilesStr=msciPath,ETFtickerStr=stock1Str,columnWithDate=1,columnWithPrice=2)
        print(stock2Str)
        msci2.xts <- loadPriceHistory_MSCIByETF_CSV(theFolderWithAllMSCIfilesStr=msciPath,ETFtickerStr=stock2Str,columnWithDate=1,columnWithPrice=2)
        
        # convert all timeseries to monthly freq since much of the historical MSCI data only has monthly closing prices
        if( useMonthlyPrices ){
          msci1.xts <- asMonthlyXTS(msci1.xts)
          msci2.xts <- asMonthlyXTS(msci2.xts)
        }
        
        stock1DF <<- createSyntheticIndex(currentTS=stock1DF[,priceColToUse_yahoo],historicalTS=msci1.xts,newNameStr=stock1Str)
        stock2DF <<- createSyntheticIndex(currentTS=stock2DF[,priceColToUse_yahoo],historicalTS=msci2.xts,newNameStr=stock2Str)
        
        priceColToUse <<- priceColToUse_msci
      }
      
      # stock1dates <<- index( get(stock1Str) )
      stock1dates <<- index( stock1DF )
      stock1startDate <<- stock1dates[1]
      stock1endDate <<- stock1dates[ length(stock1dates) ]
      # stock2dates <<- index( get(stock2Str) )
      stock2dates <<- index( stock2DF )
      stock2startDate <<- stock2dates[1]
      stock2endDate <<- stock2dates[ length(stock2dates) ]
      
      saveFileName <<- paste(basketStocks,collapse="_")
      saveFileName <<- paste(saveFileName,spreadTypeToUse,sep="_")
      if( useMonthlyPrices ){
        saveFileName <<- paste(saveFileName,"monthly",sep="_")
      }
      
      # backtest based on various combinatiors of Entry and Exit Z-scores used as trade triggers
      if( input$selectSecondOper_RB == "selectOper2_1" ){
        if( spreadTypeToUse == "price" || spreadTypeToUse == "log-price" ) {
          computeRollingResults <- pairSpreadTestRolling(stock1DF[,priceColToUse], stock2DF[,priceColToUse], startDate, endDate,spreadType=spreadTypeToUse,
                                                         rollingWindowLength=betaRecalcFreq, totalLookbackLength=betaLookbackDays )
          saveFileName <<- paste(saveFileName,"ZscoreOptim","Zwin",zScoreLookbackDays, "HRwin", betaLookbackDays, betaRecalcFreq, sep="_")
        } 
        if( spreadTypeToUse == "ratio") { 
          computeRollingResults <- pairSpreadTestRolling_ratio(stock1DF[,priceColToUse], stock2DF[,priceColToUse], startDate, endDate )
          saveFileName <<- paste(saveFileName,"ZscoreOptim","Zwin",zScoreLookbackDays, "HRwin", betaLookbackDays, betaRecalcFreq,sep="_")
        }
        # print(paste("basketSTocks",basketStocks))
        backtestResult <<- computeAllZscoreSignals(resultsXTS=computeRollingResults,zScoreLookback=zScoreLookbackDays,stockLabels=basketStocks,showMultiplots=FALSE
                                                   ,showAllOnSingleChart=FALSE, calledFromShinyWithProgressBar=TRUE)
        currentDataDF <<- backtestResult[["rawData"]]$rawDataAndSpreadAndZscores  
      }
      
      # backtest the various combinations of lookback windows for the hedge calculation and Z-score window
      # Lookback Windows
      if( input$selectSecondOper_RB == "selectOper2_2" ){
        hrMin <- input$betaLookback_min_NI
        hrMax <- input$betaLookback_max_NI
        hrStep <- input$betaLookback_step_NI
        zMin <- input$zScoreLookback_min_NI
        zMax <- input$zScoreLookback_max_NI
        zStep <- input$zScoreLookback_step_NI
        
        saveFileName <<- paste(saveFileName,"WindowOptim","Zwin",zMin,zMax,zStep,"HRwin",hrMin,hrMax,hrStep,sep="_")
        
        backtestResult <<- computeAllLookbackWindowsSignals(s1_tsVec=stock1DF[,priceColToUse],s2_tsVec=stock2DF[,priceColToUse],spreadTypeToUse=spreadTypeToUse,stockLabels=basketStocks
                                                            , betaLookbackDays_Min=hrMin, betaLookbackDays_Max=hrMax, betaLookbackDays_Step=hrStep
                                                            , zScore_lookbackMin=zMin, zScore_lookbackMax=zMax, zScore_lookbackStep=zStep
                                                            , showAllOnSingleChart=TRUE
                                                            , calledFromShinyWithProgressBar=TRUE
                                                            , printHeatmaps=TRUE)
        # currentDataDF <<- backtestResult[["rawData"]]$rawDataAndSpreadAndZscores 
      }
      
    } # end 'pair' 
    
    # The below now supports both the 'pair with price spread' and 'basket' cases
    # set the basket stocks to be the 2 stocks in the pair
    if( input$selectFromChoices_RB == "selectOper0_1" ) {
      stock1Str <<- upperCase(input$stock1_TI)
      stock2Str <<- upperCase(input$stock2_TI)
      basketStocks <<- c(stock1Str,stock2Str)
      
    } else {  # BASKET
      # parse the text entry to get all the stocks in the basket
      # first see if Quandl symbols have been entered
      if( ! usePreloadedStocks ) {
        basketStocksWithQuandl <<- parseBasketSymbolInput(input$stockBasket_TI)  
      }
      
      if( !usingQuandl ){
        basketStocks <<- unlist( lapply( str_split(string=input$stockBasket_TI,pattern=","), trim ) )  
        basketStocks <<- upperCase(basketStocks)
        if( ! usePreloadedStocks ) {
          print("Downloading price data for basket prices...")
          symbolsLoaded <<- getSymbols(basketStocks, from=startDate, auto.assign=TRUE)  
        } else {
          print("Using preloaded price data for basket prices...")
        }
      } else {
        basketStocks <<- unlist(lapply( basketStocksWithQuandl, names))
        basketStocks <<- upperCase(basketStocks)
      }
    }
    
    # BASKET
    if( input$selectFromChoices_RB == "selectOper0_2" ) {
      # if( TRUE ) {
      if( !usingQuandl ){
        depVarVec <<- get(basketStocks[1])[,priceColToUse_yahoo]
      }
      if( useMonthlyPrices ){
        depVarVec <<- asMonthlyXTS(depVarVec)
      }
      names(depVarVec) <<- basketStocks[1]
      
      saveFileName <<- paste(basketStocks,collapse="_")
      saveFileName <<- paste(saveFileName,spreadTypeToUse,sep="_")
      if( useMonthlyPrices ){
        saveFileName <<- paste(saveFileName,"monthly",sep="_")  
      }
      
      if( useMSCIdata & !usingQuandl ){
        # determine which MSCI index history file to get based on ETF symbol entered by user
        msciDepVar.xts <- loadPriceHistory_MSCIByETF_CSV(theFolderWithAllMSCIfilesStr=msciPath,ETFtickerStr=basketStocks[1],columnWithDate=1,columnWithPrice=2)
        stockDepVarDF <<- depVarVec
        # convert all timeseries to monthly freq since much of the historical MSCI data only has monthly closing prices
        if( useMonthlyPrices ){
          stockDepVarDF <<- asMonthlyXTS(depVarVec)
          msciDepVar.xts <- asMonthlyXTS(msciDepVar.xts)  
          
        }
        
        depVarVec <<- createSyntheticIndex(currentTS=stockDepVarDF,historicalTS=msciDepVar.xts,newNameStr=basketStocks[1])
        names(depVarVec) <<- basketStocks[1]
        print(names(depVarVec))
        print(head(index(depVarVec)))
        print(head(index(msciDepVar.xts)))
        priceColToUse <<- priceColToUse_msci
      }
      
      if( useMSCIdata & !usingQuandl ){
        for( i in seq(from=2,to=length(basketStocks)) ){
          tempPrices <- get( basketStocks[i] )[,priceColToUse_yahoo]
          names(tempPrices) <- basketStocks[i]
          if( useMonthlyPrices ){
            tempPrices <- asMonthlyXTS(tempPrices)
            print(names(tempPrices))
            print(head(index(tempPrices)))
          }
          names(tempPrices) <- basketStocks[i]
          # determine which MSCI index history file to get based on ETF symbol entered by user
          msciIndVar.xts <- loadPriceHistory_MSCIByETF_CSV(theFolderWithAllMSCIfilesStr=msciPath,ETFtickerStr=basketStocks[i],columnWithDate=1,columnWithPrice=2)
          if( useMonthlyPrices ){
            msciIndVar.xts <- asMonthlyXTS(msciIndVar.xts)  
          }
          indVarsList[[i-1]] <<- createSyntheticIndex(currentTS=tempPrices,historicalTS=msciIndVar.xts,newNameStr=basketStocks[i])
          print(names(indVarsList[[i-1]]))
          print(head(index(indVarsList[[i-1]])))
        }
      } else {
        for( i in seq(from=2,to=length(basketStocks)) ){
          if( !usingQuandl ){
            tempPrices <- get( basketStocks[i] )[,priceColToUse_yahoo]  
          } else {
            tempPrices <- indVarsList[[i-1]] 
          }
          if( useMonthlyPrices ){
            tempPrices <- asMonthlyXTS(tempPrices)
          }
          indVarsList[[i-1]] <<- tempPrices
        }  
      }
      
      
      # backtest based on varios combinatiors of Entry and Exit Z-scores used as trade triggers
      # Z-score
      if( input$selectSecondOper_RB == "selectOper2_1" ){
        saveFileName <<- paste(saveFileName,"ZscoreOptim","Zwin",zScoreLookbackDays, "HRwin", betaLookbackDays, betaRecalcFreq, sep="_")
        
        if( spreadTypeToUse == "price" || spreadTypeToUse == "log-price" ) {
          print("roll: depVar info")
          print( names(depVarVec))
          print( head(index(depVarVec)))
          computeRollingResults <- basketSpreadTestRolling_OLS(depVarVec, indVarsList, startDate, endDate,spreadType=spreadTypeToUse
                                                               , rollingWindowLength=betaRecalcFreq, totalLookbackLength=betaLookbackDays
                                                               , applyFilter_ADF=applyADF
                                                               , applyFilter_Hurst=applyHurst
                                                               , applyFilter_OU=applyOU
                                                               , applyFilter_Halflife=applyHalflife
                                                               , cutoffADF = inputCutoffADF
                                                               , cutoffHurst = inputCutoffHurst
                                                               , cutoffOU = inputCutoffOU
                                                               , cutoffHalflife = inputCutoffHalflife
          )
        } 
        
        backtestResult <<- computeAllZscoreSignals_basket(resultsXTS=computeRollingResults,numStocks=length(basketStocks)
                                                          , zScoreLookback=zScoreLookbackDays,showMultiplots=FALSE
                                                          , showAllOnSingleChart=FALSE, calledFromShinyWithProgressBar=TRUE
                                                          , applyFilter_ADF=applyADF
                                                          , applyFilter_Hurst=applyHurst
                                                          , applyFilter_OU=applyOU
                                                          , applyFilter_Halflife=applyHalflife
                                                          #      , cutoffADF = inputCutoffADF
                                                          #      , cutoffHurst = inputCutoffHurst
                                                          #      , cutoffOU = inputCutoffOU
                                                          #      , cutoffHalflife = inputCutoffHalflife
        )
        currentDataDF <<- backtestResult[["rawData"]]$rawDataAndSpreadAndZscores  
      }
      # Lookback Windows
      if( input$selectSecondOper_RB == "selectOper2_2" ){
        saveFileName <<- paste(saveFileName,"WindowOptim",sep="_")
        
        hrMin <- input$betaLookback_min_NI
        hrMax <- input$betaLookback_max_NI
        hrStep <- input$betaLookback_step_NI
        zMin <- input$zScoreLookback_min_NI
        zMax <- input$zScoreLookback_max_NI
        zStep <- input$zScoreLookback_step_NI
        saveFileName <<- paste(saveFileName,"Zwin",zMin,zMax,zStep,"HRwin",hrMin,hrMax,hrStep,sep="_")
        
        backtestResult <<- computeAllLookbackWindowsSignals_basket(depVarVec,indVarsList,spreadTypeToUse=spreadTypeToUse
                                                                   , betaLookbackDays_Min=hrMin, betaLookbackDays_Max=hrMax, betaLookbackDays_Step=hrStep
                                                                   , zScore_lookbackMin=zMin, zScore_lookbackMax=zMax, zScore_lookbackStep=zStep
                                                                   , showAllOnSingleChart=TRUE
                                                                   , calledFromShinyWithProgressBar=TRUE
                                                                   , printHeatmaps=TRUE)
      }
    } # END 'basket
    savedBacktest <<- backtestResult
    if( applyADF ){
      saveFileName <<- paste(saveFileName,"ADF", round(100*input$ADFmaxP_NI), sep="_")
    }
    if( applyHurst ){
      saveFileName <<- paste(saveFileName,"H", round(100*input$maxHurst_NI), sep="_")
    }
    if( applyOU ){
      saveFileName <<- paste(saveFileName,"OU", round(100*input$maxOUtrend_NI), sep="_")
    }
    if( applyHalflife ){
      saveFileName <<- paste(saveFileName,"HL", round(input$maxHalflife_NI), sep="_")
    }
    if( useAmazonS3 ){
      s3PathStr <- paste("s3baskettrader", "/", dbBacktestResultsDir, saveFileName,sep="")
      print(s3PathStr)
      s3Save( savedBacktest ,file=s3PathStr, auth=S3authCode )
    } else {
      save( savedBacktest, file=paste(dbBacktestResultsDir,saveFileName,sep="") )  
    }
    
    # updateSelectInput( session,inputId="selectSavedBacktest_SI", choices=list.files(dbBacktestResultsDir) )
    # dropbox_save(dropbox_credentials, backtestResult, file=paste("btDF.rdata",input$selectOper1_AB,sep="_"))
  }   # END runBacktest() 
  
  observe( {
    if( input$selectOper1_AB > 0 ) {
      isolate( {
        if( input$selectSecondOper_RB == "selectOper2_1" ){
          numSteps <- ((3.0 - 0.5) / 0.25) * ((2.0 - 0.0) / 0.25) + 5
        }
        if( input$selectSecondOper_RB == "selectOper2_2" ){
          hrMin <- input$betaLookback_min_NI
          hrMax <- input$betaLookback_max_NI
          hrStep <- input$betaLookback_step_NI
          zMin <- input$zScoreLookback_min_NI
          zMax <- input$zScoreLookback_max_NI
          zStep <- input$zScoreLookback_step_NI
          
          numSteps <- ((hrMax - hrMin) / hrStep) * ((zMax - zMin) / zStep) + 5 + 7
        }
        
        withProgress( session, min=1, max=numSteps, {
          setProgress(message = 'Backtest Calculations in progress',
                      detail = 'Time depends on dates of backtest and # of trades triggered... ')
          setProgress(value = 5)
          
          runBacktest()
          
          # Z-score
          if( input$selectSecondOper_RB == "selectOper2_1" ){
            tempContNAV <- backtestResult[["continuouslyInvested"]]$NAV
            if( length(tempContNAV) > 2 ){
              tempMonthlyRet <<-  monthlyReturn(tempContNAV)
              names(tempMonthlyRet) <- "Year_Return"
              calendarReturnTableDF <<- table.CalendarReturns( tempMonthlyRet )
              
              tempMonthlyRet_withYearMon <<- tempMonthlyRet
              tempMonthlyRet_withYearMon$year <<- as.integer(unlist(lapply(str_split(as.character(index(tempMonthlyRet_withYearMon)),"-"),FUN="[",1)))
              tempMonthlyRet_withYearMon$month <<- as.integer(unlist(lapply(str_split(as.character(index(tempMonthlyRet_withYearMon)),"-"),FUN="[",2)))  
            }
            
          }
          if( input$selectSecondOper_RB == "selectOper2_2" ){
            
          }
          
          setProgress(value = numSteps)
        } )
      } )
    }
  } )
  
  output$tab_rawData <- renderDataTable( {
    if( input$selectOper1_AB > 0 ) {
      # Z-score
      if( input$selectSecondOper_RB == "selectOper2_1" ){
        # grab the dates from the XTS index
        dates <- as.character(as.Date(index(currentDataDF)))
        # names(datesVec) <- c("Date")
        
        return( cbind( dates , as.data.frame(currentDataDF)[ , 1:12 ] ) )  
      }
      # Lookback windows
      if( input$selectSecondOper_RB == "selectOper2_2" ){
        return(NULL)
      }
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
  
  output$tab_CalendarRaw <- renderPrint( {
    if( input$selectOper1_AB > 0 ) {
      isolate( {
        if( input$selectSecondOper_RB == "selectOper2_1" ){
          return(  calendarReturnTableDF )
        }
        if( input$selectSecondOper_RB == "selectOper2_2" ){
          return(NULL)
        }
      } )
    }
  } )
  
  output$tab_CalendarTable <- renderDataTable( {
    
    if( input$selectOper1_AB > 0 ) {
      if( input$selectSecondOper_RB == "selectOper2_1" ){
        # grab the dates from the XTS index
        Year <- as.integer(rownames(calendarReturnTableDF))
        
        return( cbind(Year, as.data.frame(calendarReturnTableDF)) )      
        # return( as.data.frame(calendarReturnTableDF) )      
      }
      if( input$selectSecondOper_RB == "selectOper2_2" ){
        return(NULL) 
      }
    }
    
  } , options = list( iDisplayLength = 200 
                      , aLengthMenu = c(10,20,50,100,200)
                      #  , aoColumns = c( c("asSorting", c("desc")) )
                      #  , aoColumns = c( c("desc"), c("desc") ) 
  ) 
  )
  
  output$tab_CalRetHeatmap_best <- renderPlot( {
    if( input$selectOper1_AB >= 0 | (input$selectFromChoices_RB == "selectOper0_3" && input$loadPrevBacktest_AB > 0) ) {
      # if( input$selectOper1_AB >= 0 ) {
      isolate( {
        # Z-score
        if( input$selectSecondOper_RB == "selectOper2_1" | input$selectSecondOper_RB == "selectOper2_2" ){
          tempTopStrategies <- sortListByElementProperty(theList=backtestResult[["allNAVs"]],decreasing=TRUE,applyFunction=last)
          tempTopStratIndex <- as.numeric( names(tempTopStrategies[1]) )
          
          tempContNAV_best <- backtestResult[["allNAVs"]][[ tempTopStratIndex ]] 
          tempTopStratParams <- names(tempContNAV_best)
          
          tempMonthlyRet_best <-  monthlyReturn(tempContNAV_best)
          names(tempMonthlyRet_best) <- "Year_Return"
          calendarReturnTableDF_best <<- table.CalendarReturns( tempMonthlyRet_best )
          
          tempMonthlyRet_withYearMon_best <- tempMonthlyRet_best
          tempMonthlyRet_withYearMon_best$year <- as.integer(unlist(lapply(str_split(as.character(index(tempMonthlyRet_withYearMon_best)),"-"),FUN="[",1)))
          tempMonthlyRet_withYearMon_best$month <- as.integer(unlist(lapply(str_split(as.character(index(tempMonthlyRet_withYearMon_best)),"-"),FUN="[",2)))
          
          hmCalRet <- ggplot(tempMonthlyRet_withYearMon_best, aes(month, year)) + geom_tile(aes(fill = Year_Return,width=1.0,height=0.9),colour = "white") + scale_fill_gradient2(low = "darkred", high = "darkgreen", mid="lightgray") + geom_text( aes(label=round(Year_Return*100,2)), vjust=0.0, size=4) + theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
            theme(panel.grid.major=element_line(colour="grey90")) + 
            theme(axis.line=element_line(colour="grey70")) + 
            theme(axis.title.x=element_text(size=14)) + 
            theme(axis.title.y=element_text(size=14)) + 
            theme(axis.text.x=element_text(size=11)) +
            theme(axis.text.y=element_text(size=11)) +
            ggtitle( paste("Monthly Returns: Strategy Params (X,Y): ",tempTopStratParams,sep=" ") ) +
            theme(plot.title=element_text(size=14)) +
            theme(legend.background=element_rect(colour="grey80")) + 
            theme(legend.key=element_rect(fill="grey99")) +
            theme(legend.position="none") + xlab("") + ylab("") + scale_x_discrete(breaks=seq(1:12),labels=month.abb[seq(1:12)]) + scale_y_continuous(breaks=as.integer(tempMonthlyRet_withYearMon_best$year))
          
          bcAnnualReturns <- plotBarChart(values=calendarReturnTableDF_best$Year_Return,categoryNames=rownames(calendarReturnTableDF_best)
                                          , chartTitleSize=14
                                          , chartTitle="Yearly Returns (%, compounded monthly)"
                                          , showHorizontalBars=TRUE
                                          , yAxisLabel=""
                                          , showZeroLine=TRUE
          )
          
          histoMonthlyReturns <- plotHistogram(values=tempMonthlyRet_best,showMeanLine=TRUE,showStDevLines=TRUE
                                               , chartTitleSize=14
                                               , barColorDarkness=3
                                               , barColor="Oranges"
                                               , chartTitle="Distribution of Monthly Returns"
                                               , xAxisLabel="Monthly Return (%)"
                                               , yAxisLabel="# of Months"
                                               , showZeroLine=TRUE
          )
          
          #backtestResult$calendarReturnsDetailsPlot <<- multiplot(plotlist=list( hmCalRet
          #                                                                            , bcAnnualReturns
          #                                                                            , histoMonthlyReturns
          #                                                                      ),cols=3 
          #                                                        )
          #backtestResult[["calendarReturnsDetailsPlotList"]] <<- multiplot(plotlist=list( hmCalRet
          #                                                                       , bcAnnualReturns
          #                                                                       , histoMonthlyReturns
          #),cols=3 
          #)
          
          # save( savedBacktest, file=paste(dbBacktestResultsDir,saveFileName,sep="") )
          # return( print(backtestResult[["calendarReturnsDetailsPlot"]]) )
          # return( backtestResult$calendarReturnsDetailsPlot )
          return( print( multiplot(plotlist=list( hmCalRet
                                                  , bcAnnualReturns
                                                  , histoMonthlyReturns
          ),cols=3 
          ) 
          ) 
          )
          
        }
        if( input$selectSecondOper_RB == "selectOper2_2" ){
          return(NULL)
        }
      } )
    }
    
  } )
  
  output$tab_CalendarRaw_best <- renderPrint( {
    if( input$selectOper1_AB > 0 ) {
      isolate( {
        if( input$selectSecondOper_RB == "selectOper2_1" | input$selectSecondOper_RB == "selectOper2_2"){
          return(  calendarReturnTableDF_best )
        }
        if( input$selectSecondOper_RB == "selectOper2_2" ){
          return(NULL)
        }
      } )
    }
  } )
  
  output$tab_extHistory <- renderPrint( {
    allfiles <- list.files(msciPath)
    return(allfiles)
  } )
  
  output$tab_savedBacktests <- renderPrint( {
    if( input$selectOper1_AB >= 0 ) {
      # allfiles <- list.files(dbBacktestResultsDir)
      allfiles <- listDirS3(boS3,filterFilesByFolderStr="all")
      return(allfiles)
    }
  } )
  
  output$tab_savedBacktestsExcellent <- renderPrint( {
    if( input$tagBacktest_AB >= 0 ) {
      # allfiles <- list.files(dbBacktestResultsDir_excellent)
      allfiles <- listDirS3(boS3,filterFilesByFolderStr="excellent")
      return(allfiles)  
    }
  } )
  
  output$tab_savedBacktestsGood <- renderPrint( {
    if( input$tagBacktest_AB >= 0 ) {
      # allfiles <- list.files(dbBacktestResultsDir_good)
      allfiles <- listDirS3(boS3,filterFilesByFolderStr="good")
      return(allfiles)      
    }
  } )
  
  output$tab_savedBacktestsFlag <- renderPrint( {
    if( input$tagBacktest_AB >= 0 ) {
      # allfiles <- list.files(dbBacktestResultsDir_flag)
      allfiles <- listDirS3(boS3,filterFilesByFolderStr="flag")
      return(allfiles)  
    }
  } )
  
  output$tab_CalendarTable_best <- renderDataTable( {
    
    if( input$selectOper1_AB > 0 ) {
      if( input$selectSecondOper_RB == "selectOper2_1" | input$selectSecondOper_RB == "selectOper2_2"){
        # grab the dates from the XTS index
        Year <- as.integer(rownames(calendarReturnTableDF_best))
        
        return( cbind(Year, as.data.frame(calendarReturnTableDF_best)) )      
        # return( as.data.frame(calendarReturnTableDF) )      
      }
      if( input$selectSecondOper_RB == "selectOper2_2" ){
        return(NULL) 
      }
    }
    
  } , options = list( iDisplayLength = 200 
                      , aLengthMenu = c(10,20,50,100,200)
                      #  , aoColumns = c( c("asSorting", c("desc")) )
                      #  , aoColumns = c( c("desc"), c("desc") ) 
  ) 
  )
  
  
  
  output$tab_backtestResultsHeatmaps <- renderPlot( {
    if( input$selectFromChoices_RB == "selectOper0_3" && input$loadPrevBacktest_AB > 0   ){
      isolate( {
        return( print( multiplot(plotlist=backtestResult[["performanceHeatmaps"]]$heatmaps_list, cols=3) ) )     
      } )
    }
    
    if( input$selectOper1_AB >= 0 ) {
      isolate( {
        if( input$selectSecondOper_RB == "selectOper2_1" ){
          return( print( multiplot(plotlist=backtestResult[["performanceHeatmaps"]]$heatmaps_list, cols=3) ) )      
        }
        if( input$selectSecondOper_RB == "selectOper2_2" ){
          return( print( multiplot(plotlist=backtestResult[["performanceHeatmaps"]]$heatmaps_list, cols=3) ) )      
        }
        if( input$selectFromChoices_RB == "selectOper0_3" ){
          return( print( multiplot(plotlist=backtestResult[["performanceHeatmaps"]]$heatmaps_list, cols=3) ) )      
        }
      } )
      
      
      
    }
  } )
  
  # this returns the monthly/annual returns for the continually invested strategy, which is only relevent if testing over Z-score parameters
  output$tab_CalRetHeatmap <- renderPlot( {
    
    if( input$selectOper1_AB >= 0 ) {
      isolate( {
        # Z-score
        if( input$selectSecondOper_RB == "selectOper2_1" ){
          hmCalRet <- ggplot(tempMonthlyRet_withYearMon, aes(month, year)) + geom_tile(aes(fill = Year_Return),colour = "white") + scale_fill_gradient2(low = "darkred", high = "darkgreen", mid="lightgray") + geom_text( aes(label=round(Year_Return*100,2)), vjust=0.0, size=4) + theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
            theme(panel.grid.major=element_line(colour="grey90")) + 
            # theme(panel.grid.minor=element_line(colour="grey90")) + 
            theme(axis.line=element_line(colour="grey70")) + 
            theme(axis.title.x=element_text(size=14)) + 
            theme(axis.title.y=element_text(size=14)) + 
            theme(axis.text.x=element_text(size=11)) +
            theme(axis.text.y=element_text(size=11)) +
            ggtitle("Monthly Returns") +
            theme(plot.title=element_text(size=14)) +
            theme(legend.background=element_rect(colour="grey80")) + 
            theme(legend.key=element_rect(fill="grey99")) +
            theme(legend.position="none") + xlab("") + ylab("") + scale_x_discrete(breaks=seq(1:12),labels=month.abb[seq(1:12)]) + scale_y_continuous(breaks=as.integer(tempMonthlyRet_withYearMon$year))
          
          bcAnnualReturns <- plotBarChart(values=calendarReturnTableDF$Year_Return,categoryNames=rownames(calendarReturnTableDF)
                                          , chartTitleSize=14
                                          , chartTitle="Yearly Returns (%, compounded monthly)"
                                          , showHorizontalBars=TRUE
                                          , yAxisLabel=""
                                          , showZeroLine=TRUE
          )
          
          histoMonthlyReturns <- plotHistogram(values=tempMonthlyRet,showMeanLine=TRUE,showStDevLines=TRUE
                                               , chartTitleSize=14
                                               , barColorDarkness=3
                                               , barColor="Oranges"
                                               , chartTitle="Distribution of Monthly Returns"
                                               , xAxisLabel="Monthly Return (%)"
                                               , yAxisLabel="# of Months"
                                               , showZeroLine=TRUE
          )
          return( print( multiplot(plotlist=list(hmCalRet,bcAnnualReturns,histoMonthlyReturns),cols=3) ) )      
        }
        if( input$selectSecondOper_RB == "selectOper2_2" ){
          return(NULL)
        }
      } )
    }
    
  } )
  
  
  #  output$reportTitle <- renderText( {
  #    if( input$selectFromChoices_RB == "selectOper0_3" && input$loadPrevBacktest_AB > 0 ){
  #      isolate( {
  #        allTickers <- backtestResult[["inputTimeseriesNames"]]
  #        return( paste("Backtest Results for tickers:", paste(allTickers,collapse=", ") ) )     
  #      } )
  #    }
  #    if( input$selectOper1_AB >= 0 ) {
  #      allTickers <- backtestResult[["inputTimeseriesNames"]]
  #      if( is.null(allTickers) ){
  #        return( "No Backtest Results Loaded Yet" )
  #      } else {
  #        # return( textplot( format(allTickers,justify="centre"),cex=1, halign="center", ) )  
  #        return( paste("Backtest Results for tickers:", paste(allTickers,collapse=", ") ) )  
  #      }
  #    }
  #  } )
  
  output$reportTitle <- renderText( {
    if( input$selectOper1_AB >= 0 | (input$selectFromChoices_RB == "selectOper0_3" && input$loadPrevBacktest_AB > 0) ) {
      allTickers <- backtestResult[["inputTimeseriesNames"]]
      if( is.null(allTickers) ){
        return( "No Backtest Results Loaded Yet (Example output shown below)" )
      } else {
        # return( textplot( format(allTickers,justify="centre"),cex=1, halign="center", ) )  
        return( paste("Backtest Results for tickers:", paste(allTickers,collapse=", ") ) )  
      }
    }
  } )
  
  output$contInv_perfStats <- renderPlot( {
    if( input$selectOper1_AB >= 0 ) {
      if( input$selectSecondOper_RB == "selectOper2_1" ){
        tempContNAV <- backtestResult[["continuouslyInvested"]]$NAV
        
        tempNavTS <- tempContNAV
        
        btDailyReturns <- na.omit(dailyReturn(tempContNAV))
        btDailyReturns_inTradeOnly <- btDailyReturns[ btDailyReturns[,1] > 0.0000001 | btDailyReturns[,1] < -0.0000001 , ]
        tempNavTS_daysInTradeOnly <- xts( cumprod(1+btDailyReturns_inTradeOnly) ) 
        
        tempTR <- as.numeric(tempNavTS[ length(tempNavTS) ] - 1)
        #tempSharpeRatio <- as.numeric(sharpeRatioBasic(btDailyReturns_inTradeOnly))
        #tempSortinoRatio <- as.numeric(SortinoRatio(btDailyReturns_inTradeOnly))
        #tempMaxdrawdown <- as.numeric( maxDrawdown(btDailyReturns_inTradeOnly) )  # returns only positive values which reflects the negative return
        #tempAnnualReturn <- as.numeric( Return.annualized(btDailyReturns_inTradeOnly) )
        #tempAnnualVolatility <- as.numeric( StdDev.annualized(btDailyReturns_inTradeOnly) )
        #tempStabilityOfReturns <- RSQofTimeseries(tempNavTS_daysInTradeOnly)
        
        tempSharpeRatio <- as.numeric(sharpeRatioBasic(btDailyReturns))
        tempSortinoRatio <- as.numeric(SortinoRatio(btDailyReturns))
        tempMaxdrawdown <- as.numeric( maxDrawdown(btDailyReturns) )  # returns only positive values which reflects the negative return
        tempAnnualReturn <- as.numeric( Return.annualized(btDailyReturns) )
        tempAnnualVolatility <- as.numeric( StdDev.annualized(btDailyReturns) )
        tempStabilityOfReturns <- RSQofTimeseries(btDailyReturns)
        
        
        tempPerfStats <<- data.frame(stringsAsFactors=FALSE ,
                                     # Total_Return=percent(tempTR) ,
                                     Total_Return=percent(tempTR) ,
                                     Sharpe_Ratio=format(tempSharpeRatio,digits=3,nsmall=2,justify="centre") ,
                                     Sortino_Ratio=format(tempSortinoRatio,digits=3,nsmall=2,justify="centre") ,
                                     Max_Drawdown=percent(tempMaxdrawdown) ,
                                     Annual_Return=percent(tempAnnualReturn) ,
                                     Annual_Volatility=percent(tempAnnualVolatility) ,
                                     Stability_Of_Returns=format(tempStabilityOfReturns,digits=3,nsmall=2,justify="centre")
        )
        rownames(tempPerfStats) <- "Performance_Statistics_Contin_Inv"
        # tempPerfStats <- format(tempPerfStats,digits=3,nsmall=2,justify="center")
        return( textplot( format(tempPerfStats,justify="centre"),halign="left" ) )
      }
    }
  } )
  
  output$tab_stocksAndSpread <- renderPlot( {
    # if( input$selectOper1_AB >= 0 | (input$selectFromChoices_RB == "selectOper0_3" && input$loadPrevBacktest_AB > 0) ) {
    if( input$selectOper1_AB >= 0 ) {
      isolate( {
        # the below should now just be old code from just the 'Pair' case. But its no longer needed. Keeping around though just in case
        # PAIR
        if( input$selectFromChoices_RB == "selectOper0_1" ) {
          # if( FALSE ) {
          # Z-score
          if( FALSE ) {
            # if( input$selectSecondOper_RB == "selectOper2_1" ){
            commonStartDate <- index(backtestResult[["continuouslyInvested"]]$NAV)[1]
            commonEndDate <- index(backtestResult[["continuouslyInvested"]]$NAV)[length(index(backtestResult[["continuouslyInvested"]]$NAV))]
            # HELPER:  mergeDF[index(mergeTS) >= startDate & index(mergeTS) <= endDate ,]
            stocksPlot <- plotTimeseries( list(stock1DF[index(stock1DF) >= commonStartDate & index(stock1DF) <= commonEndDate,priceColToUse], 
                                               stock2DF[index(stock2DF) >= commonStartDate & index(stock2DF) <= commonEndDate,priceColToUse], 
                                               backtestResult[["generalInfo"]]$spread[index(backtestResult[["generalInfo"]]$spread) >= commonStartDate & index(backtestResult[["generalInfo"]]$spread) <= commonEndDate, ]
            ), legendPosition="top" )
            
            tempContNAV <- backtestResult[["continuouslyInvested"]]$NAV[index(backtestResult[["continuouslyInvested"]]$NAV) >= commonStartDate 
                                                                        & index(backtestResult[["continuouslyInvested"]]$NAV) <= commonEndDate, ]
            tempZscore <- backtestResult[["generalInfo"]]$spreadZscore[index(backtestResult[["generalInfo"]]$spreadZscore) >= commonStartDate 
                                                                       & index(backtestResult[["generalInfo"]]$spreadZscore) <= commonEndDate, ]
            
            NAVplot <- plotTimeseries( list(tempContNAV), legendPosition="top" )
            spreadZscore <- plotTimeseries( list(tempZscore), legendPosition="top" )
            
            return( print(multiplot(plotlist=list(stocksPlot
                                                  , spreadZscore
                                                  , NAVplot
            )
            , cols=1 
            ) 
            ) 
            )
          }
          # if( FALSE ){
          if( input$selectSecondOper_RB == "selectOper2_2" | input$selectSecondOper_RB == "selectOper2_1" ){
            stockAndSpreadDataDF <- backtestResult[["rawData"]]$rawDataAndSpreadAndZscores
            # columns 2 and 3 hold the Stock1 and Stock2 price timeseries that make up the 'Pair'
            stocksPlot <- plotTimeseries( dfColumnsToList( stockAndSpreadDataDF[,2:3 ] ) , legendPosition="right" )
            # find the Top 10 strategies (sorted) based on the Total Return amount's of the Z-score trigger strategy
            topStrategies <- sortListByElementProperty(theList=backtestResult[["allNAVs"]],decreasing=TRUE,applyFunction=last)
            topToGrab <- min( 10, length(topStrategies) )
            topStrategyIndexes <- as.numeric( names(topStrategies)[1:topToGrab] )
            
            topStratNAVList <- list()
            
            for( i in 1:topToGrab ){
              topStratNAVList[[ i ]] <- backtestResult[["allNAVs"]][[ topStrategyIndexes[i] ]] 
            }
            
            allTopNAVplots <- plotTimeseries( topStratNAVList, legendPosition="right" )
            
            # Z-score
            if( input$selectSecondOper_RB == "selectOper2_1" ) {
              contNAVplot <- plotTimeseries( list( backtestResult[["continuouslyInvested"]]$NAV , topStratNAVList[[1]] ) , legendPosition="right" )
              spreadZscore <- plotTimeseries( list(stockAndSpreadDataDF$Zscore) , legendPosition="right" )
              spreadOnly <- plotTimeseries( list(stockAndSpreadDataDF$dynSpread) , legendPosition="right" )
              
              return( print(multiplot(plotlist=list( allTopNAVplots
                                                     , contNAVplot
                                                     , spreadOnly
                                                     , stocksPlot
              )
              , cols=1 
              ) 
              ) 
              )
            }
            # Lookback windows
            if( input$selectSecondOper_RB == "selectOper2_2" ) {
              return( print(multiplot(plotlist=list( allTopNAVplots
                                                     , stocksPlot
              )
              , cols=1 
              ) 
              ) 
              )
            }
          }
        }
        
        # BASKET
        if( input$selectFromChoices_RB == "selectOper0_2" ) {
          # if( TRUE ) {
          stockAndSpreadDataDF <- backtestResult[["rawData"]]$rawDataAndSpreadAndZscores
          # stocksPlot <- plotTimeseries( dfColumnsToList( stockAndSpreadDataDF[,1:(length(basketStocks)+1) ] ) , legendPosition="right" )
          stocksPlot <- plotTimeseries( dfColumnsToList( stockAndSpreadDataDF[,1:(length(basketStocks)) ] ) , legendPosition="right" )
          
          
          # find the Top 10 strategies (sorted) based on the Total Return amount's of the Z-score trigger strategy
          topStrategies <- sortListByElementProperty(theList=backtestResult[["allNAVs"]],decreasing=TRUE,applyFunction=last)
          topToGrab <- min( 10, length(topStrategies) )
          topStrategyIndexes <- as.numeric( names(topStrategies)[1:topToGrab] )
          
          topStratNAVList <- list()
          
          for( i in 1:topToGrab ){
            topStratNAVList[[ i ]] <- backtestResult[["allNAVs"]][[ topStrategyIndexes[i] ]] 
          }
          
          allTopNAVplots <- plotTimeseries( topStratNAVList, legendPosition="right" )
          
          # Z-score triggers          
          if( input$selectSecondOper_RB == "selectOper2_1" ) {
            spreadZscore <- plotTimeseries( list(stockAndSpreadDataDF$Zscore) , legendPosition="right" )
            spreadOnly <- plotTimeseries( list(stockAndSpreadDataDF$spreadEnd) , legendPosition="right" )
            
            topStratNAVList[[ topToGrab+1 ]] <- backtestResult[["continuouslyInvested"]]$NAV
            names(topStratNAVList[[ topToGrab+1 ]]) <- "Contin_Inv"
            contNAVplot <- plotTimeseries( list( topStratNAVList[[ topToGrab+1 ]] , topStratNAVList[[1]] ) , legendPosition="right" )
            
            return( print(multiplot(plotlist=list( allTopNAVplots
                                                   , contNAVplot
                                                   , spreadOnly
                                                   , stocksPlot
            )
            , cols=1 
            ) 
            ) 
            )
          }
          # Lookback windows triggers
          if( input$selectSecondOper_RB == "selectOper2_2" ) {
            return( print(multiplot(plotlist=list( allTopNAVplots
                                                   , stocksPlot
            )
            , cols=1 
            ) 
            ) 
            )
          }
        }
      } )
    }
    
  } )
  
  
} )

