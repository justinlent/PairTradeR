#stocksToDownload<-c("JPM","RF","STI","BXP","BAC","PCL", "STT", "C","USB","ZION","KEY")
#getSymbols(stocksToDownload)  # download timeseries data from Yahoo Finance using the quantmod package
compute.regression <- function (x) {
  # res <- coef(summary(lm(x[,1] ~ x[,2:dim(x)[2]])))
  res <- coef(lm(x[,1] ~ x[,2:dim(x)[2]]))
  names(res)[2:length(names(res))] <- names(x)[2:length(names(x))]
  return(res)
  #  sapply(res, "[" ,"(Intercept)"  ,"t value")
}

getElemByIndex <- function(inputList, elementNumber) {
  return(inputList[[elementNumber]])
}

getColumnNamesTickersOnly <- function(df, stringSplitCharacter="\\.") {
  tempColNames<-colnames(df)
  tempSplitNames<-unlist(lapply(strsplit(tempColNames,stringSplitCharacter), getElemByIndex,1))
  return(tempSplitNames)
}

singleStock<-"JPM"  # this is the stock we'll will want to trade a long-only basket against
stockBasket<-c("RF","STI","BXP","BAC","PCL", "STT", "C","USB","ZION","KEY")
# getSymbols(singleStock, from="1970-1-1")
# getSymbols(stockBasket, from="1970-1-1")
stockTimeseriesList<-lapply(c(singleStock,stockBasket),tickerStringToStock,4)   # gets the closing prices, which is column 4 in the data, for all stocks

dataForRegression<-alignAndCreateDF(stockTimeseriesList)  # put all the closing prices into 1 dataframe, with column 1 holding the dependent variable
colnames(dataForRegression)<-getColumnNamesTickersOnly(dataForRegression)

dataForRegression2<-alignAndCreateDF(stockTimeseriesList)  # put all the closing prices into 1 dataframe, with column 1 holding the dependent variable

#regressionFormula<-as.formula(dataForRegression)
dataForRegressionLog<-log(dataForRegression)
dataForRegressionRets<-ROC(dataForRegression)
regressionVars<-colnames(dataForRegression)
regressionFormula <- as.formula(paste(paste(regressionVars[1],"~"), paste(regressionVars[2:length(regressionVars)], collapse= "+")))

getRegCoeffs<-function(depVarStr, allStocksDF){
  coef(lm(as.formula(paste(depVarStr, "~ .")) , data = as.data.frame(allStocksDF)))
}
getSimpleRegCoeffs<-function(allStocksDF){
  tempResults<-coef( lm(allStocksDF[,1] ~ allStocksDF[,2:dim(allStocksDF)[2]] , data=as.data.frame(allStocksDF)) )
  names(tempResults)[2:length(names(tempResults))]<-names(allStocksDF)[2:length(names(allStocksDF))]
  return(tempResults)
}
dataForRegressionRetsWithTotal<-dataForRegressionRets
dataForRegressionRetsWithTotal$totalAvgRetNoJPM<-dataForRegressionRetsWithTotal$JPM.Close - (rowSums(dataForRegressionRets[,2:dim(dataForRegressionRets)[2]]) / (dim(dataForRegressionRets)[2]-1))
plotTimeseries(list(makeIndexFromPctRet(returnsTS=dataForRegressionRets[,1],nameIndex="JPM"),makeIndexFromPctRet(returnsTS=dataForRegressionRets[,2],nameIndex="RF")),normalize=TRUE)
plotTimeseries(list(makeIndexFromPctRet(returnsTS=dataForRegressionRetsWithTotal$totalAvgRetNoJPM,nameIndex="longShort")))
plotTimeseries(list(makeIndexFromPctRet(returnsTS=dataForRegressionRetsWithTotal$totalAvgRetNoJPM,nameIndex="longShort",startValue=100),makeIndexFromPctRet(returnsTS=dataForRegressionRets[,1],nameIndex="JPM")))


#compute.regression_orig <- function (x) {
#    res <- coef(summary(lm(x ~ time(x))))
#    sapply(res, "[" ,"(Intercept)"  ,"t value")
#}

r <- rollapplyr(x, 100, f, by.column=FALSE)


rollingReg <- rollapplyr(dataForRegressionLog, 100, compute.regression, by.column=FALSE)


olsModel.lm<-lm(regressionFormula,data=dataForRegression)  # use a regular OLS regression if you want both long and short weights for the stock basket
olsModel2.lm<-lm(dataForRegression2[,1] ~ dataForRegression2[,2:11])

summary(olsModel.lm)

makeIndexFromBasketWithWeights<-function(basketArray, weightsArray, startDate=NULL, endDate=NULL, indexStartValue=100.0, priceColumnToUse=6) {
  # this function makes an index out of the basket of stocks passed in 'basketArray' using the weights passed in 'weightsArray'
  # the index starts at 'startDate' and ends at 'endDate'.  If no startDate/endDate is specified, it uses all available data
  
  # 'basketArray' should be passed in something like this:  c("AAPL", "GOOG", "MSFT")
  # 'weightsArray' should be passed in something like this: c(0.5, 0.3, 0.2)
  #       can add up to more than 1.0 if you want to be leveraged
  # 'priceColumnToUse' is the collumn number to use in the DF that holds each stock's data
  #       default value of 6 refers to the AdjustedClose column that is used when using YahooFinance prices
  # 'startDate' and 'endDate': if you don't pass in any dates, or pass in NULL, it will just use all available data
  # just make an individual index for each stock with starting value equal to that stocks weight in the weightArr
  # then just add all of the resulting indexes together at the end
  
  basketDF<-data.frame()
  #print(length(basketArray))
  if( length(basketArray) > 1 ) {
    alignBasketList<-list()
    for( i in 1:length(basketArray) ) {
      alignBasketList[[i]]<-get( basketArray[i] )[,priceColumnToUse]
      names(alignBasketList[[i]])<-basketArray[i]
      #print(names(alignBasketList[[i]]))
    }
    basketDF<-alignAndCreateDF(allSeriesList=alignBasketList)
  } else {
    tickerString<-basketArray[1]
    basketDF<-get( tickerString )[,priceColumnToUse]
    names(basketDF)<-tickerString
  }
  
  indivIndexDF<-data.frame()
  
  for( i in 1:length(basketArray) ) {
    tickerString<-basketArray[i]
    # ticker<-get(tickerString)
    allPctRet<-dailyReturn(basketDF[,tickerString],type='arithmetic')
    if( is.null(startDate) && is.null(endDate) ){
      slicedPctRet<-allPctRet
    } else {
      slicedPctRet<-allPctRet[match(startDate,index(allPctRet)):match(endDate,index(allPctRet)),]
    }
    tempIndex<-makeIndexFromPctRet(returnsTS=slicedPctRet,startValue=weightsArray[i]*indexStartValue,nameIndex=paste(tickerString,"holdings",sep="_"))
    indivIndexDF<-cbind(indivIndexDF,tempIndex)
  }
  
  basketIndex<-indivIndexDF[,1]
  #print(basketIndex)
  if( length(basketIndex) > 1 ){
    for( i in 2:length(basketArray) ) {
      basketIndex<-basketIndex + indivIndexDF[,i] 
    }  
  }
  basketIndex<-basketIndex - (sum(abs(weightsArray)) - 1.0) * indexStartValue
  # basketIndex<-basketIndex - indexStartValue
  #print(basketIndex)
  names(basketIndex)<-"NAV_basketIndex"
  indivIndexDF<-cbind(indivIndexDF,basketIndex)

  return(indivIndexDF)
}

makeIndexFromBasketWithWeightsAndRebalance<-function(basketArray, weightsArray, startDate, endDate, indexStartValue=100.0, priceColumnToUse=6, tradingDaysBetweenEachRebalance=30, detailedOutput=FALSE ) {
  # first need to just get start and end indexes (dates) of the index, and the rebalance dates, so just
  # use the first thing in the basket to determine this
  
  # first only use data that is available across all inputs by running it thru alignAndCreateDF()
  # alignAndCreateDF(allSeriesList= )
  
  alignBasketList<-list()
  for( i in 1:length(basketArray) ) {
    alignBasketList[[i]]<-get( basketArray[i] )[,priceColumnToUse]
    names(alignBasketList[[i]])<-basketArray[i]
  }
  basketDF<-alignAndCreateDF(allSeriesList=alignBasketList)
  
  if( is.null(startDate) && is.null(endDate) ){
    slicedTempData<-basketDF
  } else {
    #print("subset of data history being used")
    #print(startDate)
    #print(endDate)
    slicedTempData<-basketDF[match(startDate,index(basketDF)):match(endDate,index(basketDF)),]
  }
  rebalanceDaysList<-list()
  count<-1
  # compute the rebalance days and store each date in a List
  for( i in seq(from=1, to=(dim(slicedTempData)[1]), by=tradingDaysBetweenEachRebalance) ) {
    rebalanceDaysList[[count]]<-index(slicedTempData[i])
    count<-count+1
  }
  
  # now on each date, rebalance the basket portfolio back to the weights specified in 'weightsArray'
  basketCalc<-data.frame()
  for( d in 1:(length(rebalanceDaysList)-1) ) {
    if( d == 1){
      print(paste("rebalancing #",d,sep=" "))
      print(rebalanceDaysList[d])
      # need to subtract 1 day from enddate to be passed to below, so that start date of next segment is the next rebalance date
      tempEndDate<-index(basketDF)[match(rebalanceDaysList[d+1],index(basketDF)) - 1]
      basketCalc<-makeIndexFromBasketWithWeights(basketArray, weightsArray, rebalanceDaysList[d], tempEndDate, indexStartValue=indexStartValue)  
      # prevBasketEndValue<-basketCalc$basketIndex[length(basketCalc$basketIndex)]
      # print(prevBasketEndValue)
    } else {
      print(paste("rebalancing #",d,sep=" "))
      print(rebalanceDaysList[d])
      prevBasketEndValue<-as.numeric(basketCalc$NAV_basketIndex[length(basketCalc$NAV_basketIndex)])
      tempEndDate<-index(basketDF)[match(rebalanceDaysList[d+1],index(basketDF)) - 1]
      tempResult<-makeIndexFromBasketWithWeights(basketArray, weightsArray, rebalanceDaysList[d], tempEndDate, indexStartValue=prevBasketEndValue)
      basketCalc<-rbind(basketCalc,tempResult[2:dim(tempResult)[1],])
    }
  }
  
  print(paste("rebalancing #",length(rebalanceDaysList),sep=" "))
  print(rebalanceDaysList[length(rebalanceDaysList)])
  prevBasketEndValue<-as.numeric(basketCalc$NAV_basketIndex[length(basketCalc$NAV_basketIndex)])
  tempResult<-makeIndexFromBasketWithWeights(basketArray, weightsArray, rebalanceDaysList[length(rebalanceDaysList)], endDate, indexStartValue=prevBasketEndValue)
  basketCalc<-rbind(basketCalc,tempResult[2:dim(tempResult)[1],])
  
  if( detailedOutput ) {
    for( i in 1:length(basketArray) ) {
      tickerStr<-basketArray[i]
      ticker<-get(tickerStr)
      tickerPctRet<-dailyReturn(ticker,type='arithmetic')[index(basketCalc)]
      basketCalc<-cbind(basketCalc,tickerPctRet)
      names(basketCalc)[dim(basketCalc)[2]]<-paste(tickerStr,"dailyRet")
    }
    # show which days are rebalance days next to the NAV
    # unlist(lapply(rebalanceDaysListTest, match, index(dataForBacktest)))
    isRebalanceDay<-index(basketCalc) %in% rebalanceDaysList
    basketCalc<-cbind(basketCalc,isRebalanceDay)
    names(basketCalc)[dim(basketCalc)[2]]<-"RebalanceDay"
  }
  
  return(basketCalc)
}
  
makeIndexFromBasket<-function(howToWeight="equal-weight", referenceInstrument=NULL, basketArray, tradingDaysBetweenEachRebalance=30, startDate, endDate, lookbackWindow=90, leverageFactor=1.0, weightsArray=NULL){
  # How to properly pass in parameter values
  # 'howToWeight' is how you want to weight the things you're trading in your basket. 
  #       e.g.:  "equal-weighted", "fixed-custom-weighted", long-only-regression-weighted", "long-short-regression-weighted", "beta-weighted", "vol-weighted"
  # 'basketArray' should be passed in like this:  e.g.:  c("AAPL", "GOOG", "MSFT")
  # 'referenceInstrument' should be passed in as the string of the thing you're hedging or beta-weighting. e.g.:  "SPX"
  # 'lookbackWindow' is only needed for specifying window to compute historical volatility, beta, or regressionss
  # 'customWeightsArray' (this is optional. only if you want to specify fixed weights) 
  #       e.g.:  c(0.5, 0.3, 0.2)
  if(howToWeight == "equal-weighted"){
    # simply equal weights each stock in 'basketArr', resetting to equal weights on each rebalance day.
    # no need to pass 'referenceInstrument' value in for this use case
    weightsArrayEqual<=rep(100.0/length(basketArray), length(basketArray))
    makeIndexFromBasketWithWeightsAndRebalance(basketArray=basketArray,weightsArray=weightsArrayEqual,startDate=startDate,endDate=endDate, indexStartValue=indexStartValue, tradingDaysBetweenEachRebalance=tradingDaysBetweenEachRebalance, priceColumnToUse=priceColumnToUse)
  }
  if(howToWeight == "fixed-custom-weighted"){
    # uses the weights passed in 'weightsArray' for each stock in 'basketArr', resetting to these fixed weights on each rebalance day.
    # no need to pass 'referenceInstrument' value in for this use case
    makeIndexFromBasketWithWeightsAndRebalance(basketArray=basketArray,weightsArray=weightsArray,startDate=startDate,endDate=endDate, indexStartValue=indexStartValue, tradingDaysBetweenEachRebalance=tradingDaysBetweenEachRebalance, priceColumnToUse=priceColumnToUse)
  }
  if(howToWeight == "long-only-regression-weighted"){
    # uses NNLS regression of 'basketArray' regressed against 'referenceInstrument'
    
  } 
  if(howToWeight == "long-short-regression-weighted"){
    # uses regular linear regression of 'basketArray' regressed against 'referenceInstrument'
  }
  if(howToWeight == "beta-weighted"){
    # computes beta of each stock in basketArray against 'referenceInstrument' on each rebalance day. 
    # Lower beta stocks get higher-weights.
    # use Performance Analytics package CAPM.beta() function for this
    
  }
  if(howToWeight == "vol-weighted"){
    # computes volatility of each stock in 'basketArr' on each rebalance day.  
    # Lower volatility stocks get higher-weights.
    # no need to pass 'referenceInstrument' value in for this use case
    # use PerformanceAnalytics package apply.rolling() function for this
    # ex:   test5a$rollSD10<-apply.rolling(test5,10,FUN="sd",10)
    
  }

}

# NNLS below is working
nnlsRegression<-nnls(as.matrix(dataForRegression[,2:dim(dataForRegression)[2]]),dataForRegression[,1])
NNLScoeffs<-coef(nnlsRegression)
names(NNLScoeffs)<-colnames(dataForRegression)[2:length(colnames(dataForRegression))]
basketCoeffsList<-list()
basketCoeffsList[[1]]<-NNLScoeffs
# basketStartDate<-as.Date("2005-6-9")
# basketEndDate<-as.Date("2013-3-20")
basketStartDate<-as.Date("2005-11-7")
basketEndDate<-as.Date("2006-4-3")
tradingDaysBetweenEachRebalance<-7
rebalanceDaysListTest<-list()
dataForBacktest<-dataForRegression[match(basketStartDate,index(dataForRegression)):match(basketEndDate,index(dataForRegression)),]
count<-1
for( i in seq(from=tradingDaysBetweenEachRebalance, to=(dim(dataForBacktest)[1]), by=tradingDaysBetweenEachRebalance) ) {
  rebalanceDaysListTest[[count]]<-index(dataForBacktest[i])
  count<-count+1
}