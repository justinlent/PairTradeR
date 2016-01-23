# loadLibrary("jlentmac",printLoadedFilesToScreen=TRUE,loadSomeStocks=FALSE,loadRegressionPackages=TRUE)

calcADFpvalue <- function( theTS ) {
  # various ways to compute ADF p-value in R - the 'adfTest' function in package fUnitRoots seems best for this
  # Example in Chan Figure 2.2 using USD/CAD
  
  #  adfResult <- adf.test(spread_vec, alternative="stationary", k=1)
  #  ur.df(spread_vec,lags=1,type="drift")
  
  # these expressions should result in same results
  # adf.test(sprd, alternative="stationary", k=1)
  # ur.df(sprd, type="trend")
  
  # Valid choices are "nc" for a regression with no intercept (constant) nor time trend, 
  # and "c" for a regression with an intercept (constant) but no time trend, 
  # "ct" for a regression with an intercept (constant) and a time trend. 
  
  # Phillips-Perron Test is another test for unit-roots/stationariy
  # PP.test()
  
  return( adfTest(theTS, lags=1, type="c")@test$p.value )
}

calcOUprocess <- function( theTS ){
  resultsList <- list()
  
  sprd <- as.double(theTS)
  prev_sprd <- c(sprd[2:length(sprd)], 0)
  d_sprd <- sprd - prev_sprd
  mean_prev_spread <- mean(prev_sprd)
  prev_sprd_mean <- prev_sprd - mean_prev_spread
  sprd.zoo <- merge(d_sprd, prev_sprd_mean)
  sprd_t <- as.data.frame(sprd.zoo)
  # result <- lm(d_sprd ~ prev_sprd_mean + 0, data = sprd_t, silent = TRUE)
  result <- stats::lm(d_sprd ~ prev_sprd_mean, data = sprd_t, silent = TRUE)
  # theta <- coef(result)[1]
  theta <- coef(result)[2]
  half_life <- -log(2) / theta
  
  resultsList["trendCoef"] <- theta   # if this is negative then it is mean reverting
  resultsList["halfLife"] <- half_life
  
  return( resultsList )
}

calcOUprocessTrendTerm <- function( theTS ){
  sprd <- as.double(theTS)
  prev_sprd <- c(sprd[2:length(sprd)], 0)
  d_sprd <- sprd - prev_sprd
  mean_prev_spread <- mean(prev_sprd)
  prev_sprd_mean <- prev_sprd - mean_prev_spread
  sprd.zoo <- merge(d_sprd, prev_sprd_mean)
  sprd_t <- as.data.frame(sprd.zoo)
  # result <- lm(d_sprd ~ prev_sprd_mean + 0, data = sprd_t, silent = TRUE)
  result <- stats::lm(d_sprd ~ prev_sprd_mean, data = sprd_t, silent = TRUE)
  # theta <- coef(result)[1]
  theta <- coef(result)[2]
  names(theta) <- NULL
  return( theta ) # if this is negative then it is mean-reverting, if its positive then it is trending
}

calcOUprocessHalflife <- function( theTS ){
  sprd <- as.double(theTS)
  prev_sprd <- c(sprd[2:length(sprd)], 0)
  d_sprd <- sprd - prev_sprd
  mean_prev_spread <- mean(prev_sprd)
  prev_sprd_mean <- prev_sprd - mean_prev_spread
  sprd.zoo <- merge(d_sprd, prev_sprd_mean)
  sprd_t <- as.data.frame(sprd.zoo)
  # result <- lm(d_sprd ~ prev_sprd_mean + 0, data = sprd_t, silent = TRUE)
  result <- stats::lm(d_sprd ~ prev_sprd_mean, data = sprd_t, silent = TRUE)
  # theta <- coef(result)[1]
  theta <- coef(result)[2]
  half_life <- -log(2) / theta
  names(half_life) <- NULL
  return( half_life )
}
pairSpreadTest <- function( s1_tsVec, s2_tsVec, 
                            startDate='1900-1-1', endDate='2100-1-1', 
                            spreadType="price", useInterceptForHedgeOLS=TRUE,
                            calcBetaOnly=TRUE,
                            generateSpreadPlots=FALSE, longOnly=FALSE ) {
  
  # EXAMPLE usage:  
  # pairSpreadTest(EWC[,6], EWA[,6], "2006-4-4", "2012-4-9",generateSpreadPlots=TRUE)
  # pairSpreadTest(USO[,6], GLD[,6], "2006-5-23", "2007-12-21",generateSpreadPlots=TRUE)
  # Example 3.1 in Ernie Chan book:  pairSpreadTest(USO[,6], GLD[,6], "2006-5-23", "2012-4-9",generateSpreadPlots=TRUE)
  # 
  # NOTES:  From Ernie's website: Testing for stationarity should be over a period of at least 1 year, and possibly up to 3 years.
  
  mergeTS <- merge(s1_tsVec, s2_tsVec, all=FALSE)
  mergeDF <- as.data.frame( mergeTS )
  names(mergeDF)<-c("S1","S2")
  dataToUseDF <- mergeDF[index(mergeTS) >= startDate & index(mergeTS) <= endDate ,]
#  print(dim(dataToUseDF))

  ratioVec <- dataToUseDF$S1 / dataToUseDF$S2
  lastRatioVal <- last(ratioVec)
  
  if(spreadType=="ratio") {
    spread_vec <- ratioVec
    beta <- lastRatioVal
  } else {    # 'price' or 'log-price' require running regressions and computing a beta
    if( useInterceptForHedgeOLS ){
      if(spreadType=="price") {
        if( longOnly ){
          # add NNLS here
        } else {
          hedge_lm <- stats::lm( S1 ~ S2 , data=dataToUseDF )
          beta <- coef(hedge_lm)[2]    
        }
      }
      if(spreadType=="log-price") {
        if( longOnly ){
          # add NNLS here
        } else {
          hedge_lm <- stats::lm( log(S1) ~ log(S2) , data=dataToUseDF )
          beta <- lastRatioVal * coef(hedge_lm)[2]    
        }
      }
    } else {
      if(spreadType=="price") {
        if( longOnly ){
          # add NNLS here
        } else {
          hedge_lm <- stats::lm( S1 ~ S2 + 0 , data=dataToUseDF )
          beta <- coef(hedge_lm)[1]  
        }
        
      }
      if(spreadType=="log-price") {
        if( longOnly ){
          # add NNLS here
        } else {
          hedge_lm <- stats::lm( log(S1) ~ log(S2) + 0 , data=dataToUseDF )
          beta <- lastRatioVal * coef(hedge_lm)[1]  
        }
        
      }
    }  
    if(spreadType=="price") {
      # for price spreads, the beta simply represents the # of shares of S2 you hold, and you always hold 1 share of S1
      spread_vec <- dataToUseDF$S1 - ( beta * dataToUseDF$S2 )
    } 
    if(spreadType=="log-price") {   
      # for log-price spreads, the beta of the regression represents the % of the portfolio to hold of that stock
      # ie: if the beta=0.4, it means hold 40% of S2 and 100% of S1
      
      # spread_vec <- dataToUseDF$S1 - ( lastRatioVal * beta * dataToUseDF$S2 ) 
      spread_vec <- log(dataToUseDF$S1) - ( beta * log(dataToUseDF$S2) ) 
    }
  }
  
  if( calcBetaOnly ) {
    resultsDF <- data.frame( startDate=first(index(as.zoo(dataToUseDF))) ,
                             endDate=last(index(as.zoo(dataToUseDF))) ,
                             hedgeRatio=beta
    )
  } else {
    ADF_p <- calcADFpvalue(spread_vec)
    # compute half-life from OU process
    # HL = -log(2) / beta_of_regression_of_diff(Y-lag(Y))_onto_lag(Y)
    # http://pcweicfa.blogspot.com/2010/08/r-implementation-of-ornstein-uhlenbeck.html
    # dz(t) = -theta(z(t) - u) + dW
    # where z(t) is the spead, u is the mean of the spread, and dW is some random noise.
    # The half life of the mean reversion can then be calculated as 
    # ln(2)/theta
    sprd <- spread_vec
    prev_sprd <- c(sprd[2:length(sprd)], 0)
    d_sprd <- sprd - prev_sprd
    mean_prev_spread <- mean(prev_sprd)
    prev_sprd_mean <- prev_sprd - mean_prev_spread
    sprd.zoo <- merge(d_sprd, prev_sprd_mean)
    sprd_t <- as.data.frame(sprd.zoo)
    # result <- lm(d_sprd ~ prev_sprd_mean + 0, data = sprd_t, silent = TRUE)
    result <- stats::lm(d_sprd ~ prev_sprd_mean, data = sprd_t, silent = TRUE)
    # theta <- coef(result)[1]
    theta <- coef(result)[2]
    half_life <- -log(2) / theta
    # print(summary(result))
    
    # compute Hurst exponent (Chan Example 2.2)
    # Hurst == 0.5 for perfectly random (geometric random walk)
    # Hurst > 0.5 for trending series
    # Hurst < 0.5 for mean-reverting
    hurstCalc <- hurst(sprd)
    
    # calculate variance ratio test for stationarity (Chan Example 2.3)
    # TO-DO
    resultsDF <- data.frame( startDate=first(index(as.zoo(dataToUseDF))) ,
                             endDate=last(index(as.zoo(dataToUseDF))) ,
                             ADF_p=ADF_p ,
                             Hurst=hurstCalc ,
                             OU_trend=theta ,
                             halfLife=half_life , 
                             spreadMean=mean_prev_spread ,
                             hedgeRatio=beta
                          )
  }
  
  if( generateSpreadPlots ) {
    print("plotting spread...")
    #  plot(dataToUseDF$S2, dataToUseDF$S1)
    #  plot(beta*dataToUseDF$S2, dataToUseDF$S1)
    plot(spread_vec)
  }
  rownames(resultsDF) <- NULL
  return( resultsDF )
}

basketSpreadTest_JOH <- function( depVarVec, indVarsList, 
                                  startDate='1900-1-1', endDate='2100-1-1', 
                                  spreadType="price",
                                  calcBetaOnly=TRUE,
                                  generateSpreadPlots=FALSE ){
  mergeTS <- depVarVec
  mergeDF <- as.data.frame(mergeTS)
  #  print( names(depVarVec) )
  depVarName <- str_split(string=names(depVarVec),pattern="\\.")[[1]][1]
  allNames <- c(depVarName)
  
  for( i in seq(from=1, to=length(indVarsList)) ) {
    mergeTS <- merge(mergeTS, indVarsList[[i]], all=FALSE)
    mergeDF <- as.data.frame( mergeTS )
    newName <- str_split(string=names(indVarsList[[i]]),pattern="\\.")[[1]][1]
    allNames <- c(allNames, newName)
    names(mergeDF)<- allNames
  }
  
  dataToUseDF <- mergeDF[index(mergeTS) >= startDate & index(mergeTS) <= endDate ,]
  
  if(spreadType=="price") {
      # WORKS - standard way
      hedge_lm <- stats::lm(as.formula(paste(depVarName, "~ .")) , data=dataToUseDF)  
      # print("testing fast LM")
      # WORKS - uses faster LM algo, but currently this isn't the speed bottelneck in this function. its likely the merge() above which should eventually be refactored out
      # hedge_lm <- fastLmPure( cbind(1,mergeTS[,2:(length(indVarsList)+1)]) , mergeTS[,1] )  
  }
  if(spreadType=="log-price") {
    dataToUseDF_log <- log(dataToUseDF)
    hedge_lm <- stats::lm(as.formula(paste(depVarName, "~ .")) , data=dataToUseDF_log)  
  }
  
  # print(hedge_lm)
  beta_all <- coef(hedge_lm)[ 2:length(coef(hedge_lm)) ]
  # names(beta_all) <- allNames[2:length(allNames)]
  
  # print(beta_all)
  
  if(spreadType=="price") {
    # for price spreads, the beta simply represents the # of shares of S2..Sn you hold, and you always hold 1 share of S1
    lastPrices <- last(dataToUseDF)
    #    print(lastPrices)
    portfolioWeights <- c(1.0, -1.0 * beta_all )
    #    print(portfolioWeights)
    curr_spread <- sum( portfolioWeights * lastPrices )
    #    print( curr_spread)
    
    curr_exposure <- sum( abs(portfolioWeights) * lastPrices )
    portfolioDF <- as.data.frame( t(t(dataToUseDF) * portfolioWeights ) )
    spread_vec <- rowSums(portfolioDF)
    portfolioDF$spread <- spread_vec
  } 
  
  if(spreadType=="log-price") {   
  }
  
  if( calcBetaOnly ) {
    if( length(beta_all) < 2 ){
      names(beta_all) <- paste("hr.",names(beta_all),sep="")
    }
    resultsDF <- data.frame( startDate=first(index(as.zoo(dataToUseDF))) ,
                             endDate=last(index(as.zoo(dataToUseDF))) ,
                             spreadEnd=curr_spread ,
                             exposureEnd=curr_exposure ,
                             #     hedgeRatio=I(vector('list',1)) ,
                             hr=as.list(beta_all)
    )
    #    resultsDF$hedgeRatio[[1]] <- beta_all
    #    print( resultsDF$hedgeRatio[[1]] )
  } else {
    ADF_p <- calcADFpvalue(spread_vec)
    # compute half-life from OU process
    # HL = -log(2) / beta_of_regression_of_diff(Y-lag(Y))_onto_lag(Y)
    # http://pcweicfa.blogspot.com/2010/08/r-implementation-of-ornstein-uhlenbeck.html
    # dz(t) = -theta(z(t) - u) + dW
    # where z(t) is the spead, u is the mean of the spread, and dW is some random noise.
    # The half life of the mean reversion can then be calculated as 
    # ln(2)/theta
    sprd <- spread_vec
    prev_sprd <- c(sprd[2:length(sprd)], 0)
    d_sprd <- sprd - prev_sprd
    mean_prev_spread <- mean(prev_sprd)
    prev_sprd_mean <- prev_sprd - mean_prev_spread
    sprd.zoo <- merge(d_sprd, prev_sprd_mean)
    sprd_t <- as.data.frame(sprd.zoo)
    # result <- lm(d_sprd ~ prev_sprd_mean + 0, data = sprd_t, silent = TRUE)
    result <- stats::lm(d_sprd ~ prev_sprd_mean, data = sprd_t, silent = TRUE)
    # theta <- coef(result)[1]
    theta <- coef(result)[2]
    half_life <- -log(2) / theta
    # print(summary(result))
    
    # compute Hurst exponent (Chan Example 2.2)
    # Hurst == 0.5 for perfectly random (geometric random walk)
    # Hurst > 0.5 for trending series
    # Hurst < 0.5 for mean-reverting
    hurstCalc <- hurst(sprd)
    resultsDF <- data.frame( startDate=first(index(as.zoo(dataToUseDF))) ,
                             endDate=last(index(as.zoo(dataToUseDF))) ,
                             ADF_p=ADF_p ,
                             Hurst=hurstCalc ,
                             OU_trend=theta ,
                             halfLife=half_life , 
                             spreadEnd=curr_spread ,
                             hr=as.list(beta_all)
    )
  }
  
  if( generateSpreadPlots ) {
    # plotTimeseries( list(depVarVec, indVarsList)  )
    plot(spread_vec)
  }
  
  rownames(resultsDF) <- NULL
  #  print( resultsDF )
  return( resultsDF )
  
  
}

basketSpreadTest_OLS <- function( depVarVec, indVarsList, 
                            startDate='1900-1-1', endDate='2100-1-1', 
                            spreadType="price",
                            calcBetaOnly=TRUE,
                            generateSpreadPlots=FALSE, longOnly=FALSE ) {
  mergeTS <- depVarVec
  mergeDF <- as.data.frame(mergeTS)
#  print( names(depVarVec) )
  depVarName <- str_split(string=names(depVarVec),pattern="\\.")[[1]][1]
  allNames <- c(depVarName)
  
  for( i in seq(from=1, to=length(indVarsList)) ) {
    mergeTS <- merge(mergeTS, indVarsList[[i]], all=FALSE)
    mergeDF <- as.data.frame( mergeTS )
    newName <- str_split(string=names(indVarsList[[i]]),pattern="\\.")[[1]][1]
    allNames <- c(allNames, newName)
    names(mergeDF)<- allNames
  }
  
  dataToUseDF <- mergeDF[index(mergeTS) >= startDate & index(mergeTS) <= endDate ,]
 
  if(spreadType=="price") {
    if( longOnly ){
      # add NNLS here
    } else {
       # WORKS - standard way
      hedge_lm <- stats::lm(as.formula(paste(depVarName, "~ .")) , data=dataToUseDF)  
      # print("testing fast LM")
      # WORKS - uses faster LM algo, but currently this isn't the speed bottelneck in this function. its likely the merge() above which should eventually be refactored out
      # hedge_lm <- fastLmPure( cbind(1,mergeTS[,2:(length(indVarsList)+1)]) , mergeTS[,1] )  
    }
  }
  if(spreadType=="log-price") {
    dataToUseDF_log <- log(dataToUseDF)
    if( longOnly ){
      # add NNLS here
    } else {
      hedge_lm <- stats::lm(as.formula(paste(depVarName, "~ .")) , data=dataToUseDF_log)  
    }
  }
  
 # print(hedge_lm)
  # beta_all <- coef(hedge_lm)[ 2:length(coef(hedge_lm)) ]
beta_all <- hedge_lm$coefficients[ 2:length(hedge_lm$coefficients) ]
 # names(beta_all) <- allNames[2:length(allNames)]

 # print(beta_all)
  
  if(spreadType=="price") {
    # for price spreads, the beta simply represents the # of shares of S2..Sn you hold, and you always hold 1 share of S1
    lastPrices <- last(dataToUseDF)
#    print(lastPrices)
    portfolioWeights <- c(1.0, -1.0 * beta_all )
#    print(portfolioWeights)
    curr_spread <- sum( portfolioWeights * lastPrices )
#    print( curr_spread)
    
    curr_exposure <- sum( abs(portfolioWeights) * lastPrices )
    portfolioDF <- as.data.frame( t(t(dataToUseDF) * portfolioWeights ) )
    spread_vec <- rowSums(portfolioDF)
    portfolioDF$spread <- spread_vec
  } 
  
  if(spreadType=="log-price") {   
  }

  if( calcBetaOnly ) {
    if( length(beta_all) < 2 ){
      names(beta_all) <- paste("hr.",names(beta_all),sep="")
    }
    resultsDF <- data.frame( startDate=first(index(as.zoo(dataToUseDF))) ,
                             endDate=last(index(as.zoo(dataToUseDF))) ,
                             spreadEnd=curr_spread ,
                             exposureEnd=curr_exposure ,
                        #     hedgeRatio=I(vector('list',1)) ,
                             hr=as.list(beta_all)
                            )
#    resultsDF$hedgeRatio[[1]] <- beta_all
#    print( resultsDF$hedgeRatio[[1]] )
  } else {
    ADF_p <- calcADFpvalue(spread_vec)
    # compute half-life from OU process
    # HL = -log(2) / beta_of_regression_of_diff(Y-lag(Y))_onto_lag(Y)
    # http://pcweicfa.blogspot.com/2010/08/r-implementation-of-ornstein-uhlenbeck.html
    # dz(t) = -theta(z(t) - u) + dW
    # where z(t) is the spead, u is the mean of the spread, and dW is some random noise.
    # The half life of the mean reversion can then be calculated as 
    # ln(2)/theta
    sprd <- spread_vec
    prev_sprd <- c(sprd[2:length(sprd)], 0)
    d_sprd <- sprd - prev_sprd
    mean_prev_spread <- mean(prev_sprd)
    prev_sprd_mean <- prev_sprd - mean_prev_spread
    sprd.zoo <- merge(d_sprd, prev_sprd_mean)
    sprd_t <- as.data.frame(sprd.zoo)
    # result <- lm(d_sprd ~ prev_sprd_mean + 0, data = sprd_t, silent = TRUE)
    result <- stats::lm(d_sprd ~ prev_sprd_mean, data = sprd_t, silent = TRUE)
    # theta <- coef(result)[1]
    theta <- coef(result)[2]
    half_life <- -log(2) / theta
    # print(summary(result))
    
    # compute Hurst exponent (Chan Example 2.2)
    # Hurst == 0.5 for perfectly random (geometric random walk)
    # Hurst > 0.5 for trending series
    # Hurst < 0.5 for mean-reverting
    hurstCalc <- hurst(sprd)
    resultsDF <- data.frame( startDate=first(index(as.zoo(dataToUseDF))) ,
                             endDate=last(index(as.zoo(dataToUseDF))) ,
                             ADF_p=ADF_p ,
                             Hurst=hurstCalc ,
                             OU_trend=theta ,
                             halfLife=half_life , 
                             spreadEnd=curr_spread ,
                             hr=as.list(beta_all)
                          )
  }
  
  if( generateSpreadPlots ) {
    # plotTimeseries( list(depVarVec, indVarsList)  )
    plot(spread_vec)
  }
  
  rownames(resultsDF) <- NULL
#  print( resultsDF )
  return( resultsDF )
}

basketSpreadTestRolling_OLS <- function( depVarVec, indVarsList, 
                                         startDate='1900-1-1', endDate='2100-1-1', 
                                         rollingWindowLength=1, totalLookbackLength=20 ,
                                         spreadType = "price" ,
                                         generateSpreadPlots=FALSE
                                         , applyFilter_ADF=FALSE 
                                         , applyFilter_Hurst=FALSE
                                         , applyFilter_OU=FALSE
                                         , applyFilter_Halflife=FALSE
                                         , meanRevFilterLookback=21*5
                                         , cutoffADF = 0.05
                                         , cutoffHurst = 0.5
                                         , cutoffOU = 0.0
                                         , cutoffHalflife = 63
                                        ) {
  
  #  EXAMPLE USAGE
  #  Example 3.1 in Ernie Chan book:  
  #   EEM_rollRes <- basketSpreadTestRolling_OLS(EEM[,6], list(EFA[,6],IWM[,6],QQQ[,6]),startDate="2006-5-23",endDate="2012-4-9")
  #   USO_GLD_Chan_3_1_basket_rollRes <- basketSpreadTestRolling_OLS(USO[,6], list(GLD[,6]),startDate="2006-5-23",endDate="2012-4-9")
  #   USO_GLD_Chan_3_1_basket_rollRes_all <- basketSpreadTestRolling_OLS(USO[,6], list(GLD[,6]))
  #
  #   EEM_rollRes_all <- basketSpreadTestRolling_OLS(EEM[,6], list(EFA[,6],IWM[,6],QQQ[,6]))
  
  applyMeanRevFilters <- applyFilter_ADF | applyFilter_Hurst | applyFilter_OU | applyFilter_Halflife
  
  numBasketStocks <- length(indVarsList) + 1
  
  # replace below with alignTimeseries()
  
  mergeTS <- depVarVec
  mergeDF <- as.data.frame(mergeTS)
  
  depVarName <- str_split(string=names(depVarVec),pattern="\\.")[[1]][1]
  allNames <- c(depVarName)
  
  for( i in seq(from=1, to=length(indVarsList)) ) {
#    print(i)
    mergeTS <- merge(mergeTS, indVarsList[[i]], all=FALSE)
    mergeDF <- as.data.frame( mergeTS )
    newName <- str_split(string=names(indVarsList[[i]]),pattern="\\.")[[1]][1]
    allNames <- c(allNames, newName)
    names(mergeDF)<- allNames
    names(mergeTS)<- allNames
  }
  
  dataToUseDF <- mergeDF[index(mergeTS) >= startDate & index(mergeTS) <= endDate ,]
  dataToUseTS <- mergeTS[index(mergeTS) >= startDate & index(mergeTS) <= endDate ,]
  
  # dataToUseTS <- mergeTS

  totalDaysIndex <- dim(dataToUseTS)[1]
  allResultsDF <- data.frame()
  
  for( i in seq(from=totalLookbackLength, to=totalDaysIndex, by=rollingWindowLength) ) {
#    print( i )
    startIndex <- i-totalLookbackLength+1
#    print(startIndex)
    
    tempDepVar <- dataToUseTS[ startIndex:i , 1 ]
    
#    print(head(tempDepVar,2))
#    print( dim(tempDepVar) )
    
    tempIndVarsList <- lapply( dataToUseTS[,2:dim(dataToUseTS)[2]], "[", startIndex:i )
    
#    print( length(tempIndVarsList) )
#    print( head(tempIndVarsList[[1]],2) )
    
    allResultsDF <- rbind( allResultsDF, basketSpreadTest_OLS( tempDepVar, tempIndVarsList
                                                               , startDate=startDate, endDate=endDate 
                                                               , spreadType=spreadType
                                                               , generateSpreadPlots=generateSpreadPlots
                                                              ) 
                          )
  }
  
#  print("merging results....")
#  mergeDF$theDate <- index(as.zoo(mergeDF))
#  allResultsDF.merged <- merge.data.frame(x=mergeDF,y=allResultsDF,by.x="theDate",by.y="endDate")
  
#  return( allResultsDF.merged )
  
  allResultsDF.zoo <- zoo( allResultsDF, as.Date(allResultsDF$endDate) )
  
  allResultsDF.xts <- merge.xts( mergeTS, allResultsDF.zoo, join="inner" )
  allResultsDF.xts$startDate <- NULL
  allResultsDF.xts$endDate <- NULL

  tempDayRets <- returns(allResultsDF.xts[,1:numBasketStocks],method="discrete")
  colnames(tempDayRets) <- paste("dayRet",colnames(tempDayRets),sep="_")
  
#  print( tail(allResultsDF.xts) )
#  print( tail(tempDayRets) )
  
  allResultsDF.xts <- merge.xts( allResultsDF.xts, as.zoo(tempDayRets) )
#  print( tail(allResultsDF.xts) )

  allResultsDF.xts <- na.locf( allResultsDF.xts[index(allResultsDF.xts) >= startDate & index(allResultsDF.xts) <= endDate ,]
                               ,na.rm=TRUE 
                              )
  
  # plot(allResultsDF.xts$spreadEnd)
  print( plotTimeseries( list(allResultsDF.xts$spreadEnd) ) )

  newResults <- allResultsDF.xts
  newResults$dynSpreadLag <- lag.xts(newResults$spreadEnd)
  
  if( applyMeanRevFilters ){
    # remove the first row since dynSpreadLag=NA and that will break some of the mean reversion tests
    newResults <- newResults[-1]
    if( applyFilter_ADF ){
      print(paste("running ADF test", cutoffADF) )
      newResults$ADF_p <- suppressWarnings( rollapply(data=newResults$dynSpreadLag,width=meanRevFilterLookback,FUN=calcADFpvalue ) )
      # newResults$fADF <- newResults$ADF_p < 0.05
      newResults$fADF <- newResults$ADF_p < cutoffADF
      newResults$fADF <- na.fill(newResults$fADF,0)
      print(summary(newResults))
    }
    if( applyFilter_Hurst ){
      print( paste("running Hurst calc", cutoffHurst) )
      newResults$Hurst <- rollapply(data=newResults$dynSpreadLag,width=meanRevFilterLookback / 2,FUN=hurst )
      newResults$fHurst <- newResults$Hurst < cutoffHurst
      newResults$fHurst <- na.fill(newResults$fHurst,0)
      print(summary(newResults))
    }
    if( applyFilter_OU ){
      print( paste("running OU trend term calc", cutoffOU) )
      newResults$OUtrend <- suppressWarnings( rollapply(data=newResults$dynSpreadLag,width=meanRevFilterLookback,FUN=calcOUprocessTrendTerm ) )
      # newResults$fOUtrend <- newResults$OUtrend < 0.0
      newResults$fOUtrend <- newResults$OUtrend < cutoffOU
      newResults$fOUtrend <- na.fill(newResults$fOUtrend,0)
      print(summary(newResults))
      
    }
    if( applyFilter_Halflife ){
      print( paste("running OU half-life calc", cutoffHalflife) )
      newResults$OUhalflife <- suppressWarnings( rollapply(data=newResults$dynSpreadLag,width=meanRevFilterLookback,FUN=calcOUprocessHalflife ) )
      # newResults$fOUhalflife <- (newResults$OUhalflife < 63) & (newResults$OUhalflife > 0)         # assume if something doesn't have a half-life of less than 63 trading days, then throw it out
      newResults$fOUhalflife <- (newResults$OUhalflife < cutoffHalflife) & (newResults$OUhalflife > 0)         # assume if something doesn't have a half-life of less than 63 trading days, then throw it out
      newResults$fOUhalflife <- na.fill(newResults$fOUhalflife,0)
      print(summary(newResults))
    }
  }

#  return( allResultsDF.xts )
return( newResults )
  
}

# not used
basketSpreadTestRolling_OLS_m <- function( preMergedDataXTS, 
                                           startDate='1900-1-1', endDate='2100-1-1', 
                                           rollingWindowLength=1, totalLookbackLength=20 ,
                                           spreadType = "price" ,
                                           generateSpreadPlots=FALSE
                                           , applyFilter_ADF=FALSE 
                                           , applyFilter_Hurst=FALSE
                                           , applyFilter_OU=FALSE
                                           , applyFilter_Halflife=FALSE
                                           , meanRevFilterLookback=21*5
                                          ) {
  
  #  EXAMPLE USAGE
  #  Example 3.1 in Ernie Chan book:  
  #   EEM_rollRes <- basketSpreadTestRolling_OLS(EEM[,6], list(EFA[,6],IWM[,6],QQQ[,6]),startDate="2006-5-23",endDate="2012-4-9")
  #   USO_GLD_Chan_3_1_basket_rollRes <- basketSpreadTestRolling_OLS(USO[,6], list(GLD[,6]),startDate="2006-5-23",endDate="2012-4-9")
  #   USO_GLD_Chan_3_1_basket_rollRes_all <- basketSpreadTestRolling_OLS(USO[,6], list(GLD[,6]))
  #
  #   EEM_rollRes_all <- basketSpreadTestRolling_OLS(EEM[,6], list(EFA[,6],IWM[,6],QQQ[,6]))
  
  dataToUseTS <- preMergedDataXTS
  
  applyMeanRevFilters <- applyFilter_ADF | applyFilter_Hurst | applyFilter_OU | applyFilter_Halflife
  numBasketStocks <- length( names(dataToUseTS) )
  totalDaysIndex <- dim(dataToUseTS)[1]
  allResultsDF <- data.frame()
  
  for( i in seq(from=totalLookbackLength, to=totalDaysIndex, by=rollingWindowLength) ) {
    startIndex <- i-totalLookbackLength+1
    
    tempDepVar <- dataToUseTS[ startIndex:i , 1 ]
    tempIndVarsList <- lapply( dataToUseTS[,2:dim(dataToUseTS)[2]], "[", startIndex:i )
    allResultsDF <- rbind( allResultsDF
                           , basketSpreadTest_OLS( tempDepVar, tempIndVarsList
                                                               , startDate=startDate, endDate=endDate 
                                                               , spreadType=spreadType
                                                               , generateSpreadPlots=generateSpreadPlots
                                                              ) 
                          )
  }
  
  allResultsDF.zoo <- zoo( allResultsDF, as.Date(allResultsDF$endDate) )
  allResultsDF.xts <- merge.xts( mergeTS, allResultsDF.zoo, join="inner" )
  allResultsDF.xts$startDate <- NULL
  allResultsDF.xts$endDate <- NULL
  
  tempDayRets <- returns(allResultsDF.xts[,1:numBasketStocks],method="discrete")
  colnames(tempDayRets) <- paste("dayRet",colnames(tempDayRets),sep="_")
  
  allResultsDF.xts <- merge.xts( allResultsDF.xts, as.zoo(tempDayRets) )
  
  allResultsDF.xts <- na.locf( allResultsDF.xts[index(allResultsDF.xts) >= startDate & index(allResultsDF.xts) <= endDate ,]
                               ,na.rm=TRUE 
                              )
  
  # plot(allResultsDF.xts$spreadEnd)
  print( plotTimeseries( list(allResultsDF.xts$spreadEnd) ) )
  
  newResults <- allResultsDF.xts
  newResults$dynSpreadLag <- lag.xts(newResults$spreadEnd)
  
  if( applyMeanRevFilters ){
    # remove the first row since dynSpreadLag=NA and that will break some of the mean reversion tests
    newResults <- newResults[-1]
    if( applyFilter_ADF ){
      print("running ADF test")
      newResults$ADF_p <- suppressWarnings( rollapply(data=newResults$dynSpreadLag,width=meanRevFilterLookback,FUN=calcADFpvalue ) )
      newResults$fADF <- newResults$ADF_p < 0.05
      newResults$fADF <- na.fill(newResults$fADF,0)
      #print(summary(newResults))
    }
    if( applyFilter_Hurst ){
      print( "running Hurst calc")
      newResults$Hurst <- rollapply(data=newResults$dynSpreadLag,width=meanRevFilterLookback / 2,FUN=hurst )
      newResults$fHurst <- newResults$Hurst < 0.50
      newResults$fHurst <- na.fill(newResults$fHurst,0)
      #print(summary(newResults))
    }
    if( applyFilter_OU ){
      print( "running OU trend term calc")
      newResults$OUtrend <- suppressWarnings( rollapply(data=newResults$dynSpreadLag,width=meanRevFilterLookback,FUN=calcOUprocessTrendTerm ) )
      newResults$fOUtrend <- newResults$OUtrend < 0.0
      newResults$fOUtrend <- na.fill(newResults$fOUtrend,0)
      #print(summary(newResults))
    }
    if( applyFilter_Halflife ){
      print( "running OU half-life calc")
      newResults$OUhalflife <- suppressWarnings( rollapply(data=newResults$dynSpreadLag,width=meanRevFilterLookback,FUN=calcOUprocessHalflife ) )
      newResults$fOUhalflife <- (newResults$OUhalflife < 63) & (newResults$OUhalflife > 0)         # assume if something doesn't have a half-life of less than 63 trading days, then throw it out
      newResults$fOUhalflife <- na.fill(newResults$fOUhalflife,0)
      #print(summary(newResults))
    }
  }
  
  #  return( allResultsDF.xts )
  return( newResults )
  
}



pairSpreadTestRolling <- function( s1_tsVec, s2_tsVec, 
                                   startDate='1900-1-1', endDate='2100-1-1', 
                                   rollingWindowLength=1, totalLookbackLength=20 ,
                                   spreadType = "price" ,
                                   generateSpreadPlots=FALSE, useInterceptForHedgeOLS=TRUE ) {

  #  EXAMPLE USAGE
  #  Example 3.1 in Ernie Chan book:  USO_GLD_Chan_3_1<-pairSpreadTestRolling(USO[,6], GLD[,6], "2006-5-23", "2012-4-9",rollingWindowLength=1,totalLookbackLength=20 )
  #
  #                                   USO_GLD_RollRes_allHist<-pairSpreadTestRolling(USO[,6], GLD[,6],rollingWindowLength=1,totalLookbackLength=20 )
  #                                   
  
  mergeTS <- merge(s1_tsVec, s2_tsVec, all=FALSE)
  names(mergeTS) <- c("S1","S2")
  mergeTS$S1_dayRet <- dailyReturn(mergeTS$S1)
  mergeTS$S2_dayRet <- dailyReturn(mergeTS$S2)
  #  mergeDF <- as.data.frame( mergeTS )
  dataToUseDF <- mergeTS
  totalDaysIndex <- dim(dataToUseDF)[1]
#  print(totalDaysIndex)
#  print(index(dataToUseDF))
  allResultsDF <- data.frame()
  for( i in seq(from=totalLookbackLength, to=totalDaysIndex, by=rollingWindowLength) ) {
#    print(i)
    startIndex <- i-totalLookbackLength+1
    
    tempDF <- dataToUseDF[ startIndex:i , ]
    allResultsDF <- rbind( allResultsDF, pairSpreadTest( tempDF$S1, tempDF$S2 
                                                         , spreadType=spreadType
                                                         , generateSpreadPlots=generateSpreadPlots
                                                         , useInterceptForHedgeOLS=useInterceptForHedgeOLS
                                                         , calcBetaOnly=TRUE 
                                                         ) )
  
  }
  
  allResultsDF.zoo <- zoo( allResultsDF, as.Date(allResultsDF$endDate) )
  allResultsDF.xts <- merge.xts(allResultsDF.zoo, mergeTS)
  allResultsDF.xts$startDate <- NULL
  allResultsDF.xts$endDate <- NULL

  allResultsDF.xts <- na.locf( allResultsDF.xts[index(allResultsDF.xts) >= startDate & index(allResultsDF.xts) <= endDate ,]
                               ,na.rm=TRUE )
  
  return( allResultsDF.xts )
}

pairSpreadTestRolling_ratio <- function( s1_tsVec, s2_tsVec, startDate='1900-1-1', endDate='2100-1-1' ) {
  
  #  EXAMPLE USAGE
  #  Example 3.1 in Ernie Chan book:  
  
  mergeTS <- merge(s1_tsVec, s2_tsVec, all=FALSE)
  names(mergeTS) <- c("S1","S2")
  mergeTS$S1_dayRet <- dailyReturn(mergeTS$S1)
  mergeTS$S2_dayRet <- dailyReturn(mergeTS$S2)
  
  dataToUseDF <- mergeTS
  dataToUseDF$hedgeRatio <- dataToUseDF$S1 / dataToUseDF$S2
  
  dataToUseDF <- dataToUseDF[index(dataToUseDF) >= startDate & index(dataToUseDF) <= endDate , ]
  
  return( dataToUseDF )
}

calcDynSpreadZscoreAndTrades_basket <- function( resultsXTS, numStocks=4, zScoreLookback=21, 
                                          ZtriggerShort = 1.5, ZtriggerLong = -1.5,
                                          ZexitShort = 0.5, ZexitLong = -0.5,
                                          showDynSpreadPlot=FALSE
                                          , applyFilter_ADF=FALSE 
                                          , applyFilter_Hurst=FALSE
                                          , applyFilter_OU=FALSE
                                          , applyFilter_Halflife=FALSE
                                          , meanRevFilterLookback=21*5
                                          ) {
  
  # 'resultsXTS' is a result DF output from function 'pairSpreadTestRolling'
  # EXAMPLE USAGE:  USO_GLD_Chan_3_1_basket_dynSpreadAndZscore <- calcDynSpreadZscoreAndTrades_basket(USO_GLD_Chan_3_1_basket_rollRes,numStocks=2,showDynSpreadPlot=TRUE,zScoreLookback=20,ZtriggerShort=1.0,ZtriggerLong=-1.0,ZexitShort=0,ZexitLong=0)
  #                 USO_GLD_Chan_3_1_basket_dynSpreadAndZscore_allHist <- calcDynSpreadZscoreAndTrades_basket(USO_GLD_Chan_3_1_basket_rollRes_all,numStocks=2,showDynSpreadPlot=TRUE,zScoreLookback=20,ZtriggerShort=1.0,ZtriggerLong=-1.0,ZexitShort=0,ZexitLong=0)
  #                 plot(cumprod(1+na.omit(USO_GLD_Chan_3_1_basket_dynSpreadAndZscore$unitPortRet * USO_GLD_Chan_3_1_basket_dynSpreadAndZscore$inB)))
  #
  #                 calcADFpvalue(USO_GLD_dynSpreadRes$dynSpread)
  applyMeanRevFilters <- applyFilter_ADF | applyFilter_Hurst | applyFilter_OU | applyFilter_Halflife
  
  newResults <- resultsXTS
  # newResults$hedgeRatioLag <- lag.xts(newResults$hedgeRatio) 
  # newResults$unitPort <- newResults[,"S1"] + abs(newResults$hedgeRatioLag) * newResults[,"S2"]
  newResults$unitPort <- lag.xts(newResults$exposureEnd)
  
  # dynSpreadLag is now computed in the "basketSpreadTestRolling_OLS" functions
  # newResults$dynSpreadLag <- lag.xts(newResults$spreadEnd)
  
  # calculate daily contributions of each stock in the portfolio daily returns
  stockStr <- colnames(newResults[,1])
  
  priceCol <- stockStr
  dayRetCol <- paste("dayRet_",stockStr,sep="")
  hedgeRatioCol <- paste("hr.",stockStr,sep="")
  contribCol <- paste("contrib_",stockStr,sep="") 
  
  newResults$tempContrib <- lag.xts( newResults[,priceCol] ) * newResults[,dayRetCol]
  allColNames <- colnames(newResults)
  colnames(newResults) <- c( allColNames[1:length(allColNames)-1] , contribCol )
  newResults$spreadChg <- newResults[,contribCol]
  
  for( i in seq(from=2,to=numStocks) ) {
    stockStr <- colnames(newResults[,i])
    
    priceCol <- stockStr
    dayRetCol <- paste("dayRet_",stockStr,sep="")
    hedgeRatioCol <- paste("hr.",stockStr,sep="")
    contribCol <- paste("contrib_",stockStr,sep="") 
#    print( paste(priceCol,dayRetCol,hedgeRatioCol,contribCol) )
    
    newResults$tempContrib <- -(1.0) * lag.xts( newResults[,priceCol] ) * newResults[,dayRetCol] * lag.xts( newResults[,hedgeRatioCol] )
    newResults$spreadChg <- newResults$spreadChg + newResults$tempContrib  
    
    allColNames <- colnames(newResults)
    colnames(newResults) <- c( allColNames[1:length(allColNames)-1] , contribCol )
  }
  
  # newResults$spreadChg <- newResults$S1_dayRet*lag.xts(newResults$S1) - ( newResults$hedgeRatioLag * newResults$S2_dayRet * lag.xts(newResults$S2) )
  newResults$unitPortRet <- newResults$spreadChg / newResults$unitPort
  
  if( showDynSpreadPlot ){
    plot( newResults$dynSpread )  
  }
  
  tempZ <- rollingZScore( newResults$dynSpreadLag, window=zScoreLookback )
  names(tempZ) <- "Zscore"
  newResults <- cbind( newResults, tempZ )
  
  newResults$entryL <- as.numeric(newResults$Zscore < ZtriggerLong)
  newResults$exitL <- (99) * as.numeric(newResults$Zscore >= ZexitLong)
  newResults$inLt <- newResults$entryL + newResults$exitL 
  newResults$inLt[ newResults$inLt == 0 ] <- NA
  newResults$inL <- na.locf( newResults$inLt )
  newResults$inL[ newResults$inL == 99 ] <- 0
  
  newResults$entryS <- (-1) * as.numeric(newResults$Zscore > ZtriggerShort)
  newResults$exitS <- (99) * as.numeric(newResults$Zscore <= ZexitShort)
  newResults$inSt <- newResults$entryS + newResults$exitS 
  newResults$inSt[ newResults$inSt == 0 ] <- NA
  newResults$inS <- na.locf( newResults$inSt )
  newResults$inS[ newResults$inS == 99 ] <- 0
  
  newResults$inB <- newResults$inL + newResults$inS 

  if( applyMeanRevFilters ){
    if( applyFilter_ADF ){
      print("...Applying ADF test to trades")
      newResults$inS <- newResults$inS * newResults$fADF
      newResults$inL <- newResults$inL * newResults$fADF
    }
    if( applyFilter_Hurst ){
      print( "...Applying Hurst calc to trades")
      newResults$inS <- newResults$inS * newResults$fHurst
      newResults$inL <- newResults$inL * newResults$fHurst
    }
    if( applyFilter_OU ){
      print( "...Applying OU trend term calc to trades")
      newResults$inS <- newResults$inS * newResults$fOUtrend
      newResults$inL <- newResults$inL * newResults$fOUtrend
    }
    if( applyFilter_Halflife ){
      print( "...Applying running OU half-life calc to trades")
      newResults$inS <- newResults$inS * newResults$fOUhalflife
      newResults$inL <- newResults$inL * newResults$fOUhalflife
    }
    newResults$inB <- newResults$inL + newResults$inS
  }

  return( newResults )
}


calcDynSpreadZscoreAndTrades <- function( resultsXTS, zScoreLookback=21, 
                                          ZtriggerShort = 1.0, ZtriggerLong = -1.0,
                                          ZexitShort = 0.0, ZexitLong = 0.0,
                                          showDynSpreadPlot=FALSE ) {
  
  # 'resultsXTS' is a result DF output from function 'pairSpreadTestRolling'
  # EXAMPLE USAGE:  USO_GLD_dynSpreadRes <- calcDynSpreadZscoreAndTrades(USO_GLD_Chan_3_1,showDynSpreadPlot=TRUE)
  #                 USO_GLD_Chan_3_1_dynSpreadAndZscore <- calcDynSpreadZscoreAndTrades(USO_GLD_Chan_3_1,showDynSpreadPlot=TRUE,zScoreLookback=20,ZtriggerShort=1.0,ZtriggerLong=-1.0,ZexitShort=0,ZexitLong=0)
  #                 plot(cumprod(1+na.omit(USO_GLD_Chan_3_1_dynSpreadAndZscore$unitPortRet * USO_GLD_Chan_3_1_dynSpreadAndZscore$inB)))
  #                 sharpeRatioBasic(na.omit(dailyReturn(cumprod(1+na.omit(USO_GLD_Chan_3_1_dynSpreadAndZscore$unitPortRet * USO_GLD_Chan_3_1_dynSpreadAndZscore$inB)))))
  #                 USO_GLD_dynSpreadRes_allHist <- calcDynSpreadZscoreAndTrades(USO_GLD_RollRes_allHist,showDynSpreadPlot=TRUE)
  #
  #                 plot(cumprod(1+na.omit(USO_GLD_Chan_3_1_dynSpreadAndZscore$unitPortRet * -1*USO_GLD_Chan_3_1_dynSpreadAndZscore$Zscore)))
  #                 sharpeRatioBasic(na.omit(dailyReturn(cumprod(1+na.omit(USO_GLD_Chan_3_1_dynSpreadAndZscore$unitPortRet * -1*USO_GLD_Chan_3_1_dynSpreadAndZscore$Zscore)))))
  #                 
  #                 calcADFpvalue(USO_GLD_dynSpreadRes$dynSpread)
  newResults <- resultsXTS
  newResults$hedgeRatioLag <- lag.xts(newResults$hedgeRatio) 
  
  lagS1 <- lag.xts(newResults$S1)
  lagS2 <- lag.xts(newResults$S2)
  lagHedgeRatio <- newResults$hedgeRatioLag
  
  # newResults$unitPort <- newResults[,"S1"] + abs(newResults$hedgeRatioLag) * newResults[,"S2"]
  newResults$unitPort <- lagS1 + abs(lagHedgeRatio) * lagS2
  # newResults$dynSpread <- newResults[,"S1"] - newResults$hedgeRatioLag * newResults[,"S2"]
  newResults$dynSpread <- newResults[,"S1"] - newResults$hedgeRatio * newResults[,"S2"]
  newResults$dynSpreadLag <- lag.xts(newResults$dynSpread)
  
  # newResults$spreadChg <- newResults$S1_dayRet*lag.xts(newResults$S1) - ( newResults$hedgeRatioLag * newResults$S2_dayRet * lag.xts(newResults$S2) )
  newResults$spreadChg <- newResults$S1_dayRet * lagS1 - ( lagHedgeRatio * newResults$S2_dayRet * lagS2 )
  # newResults$unitPortRet <- newResults$spreadChg / lag.xts(newResults$unitPort)
  newResults$unitPortRet <- newResults$spreadChg / newResults$unitPort
  
  if( showDynSpreadPlot ){
    plot( newResults$dynSpread )  
  }
  
  tempZ <- rollingZScore( newResults$dynSpreadLag, window=zScoreLookback )
  names(tempZ) <- "Zscore"
  newResults <- cbind( newResults, tempZ )
  
  newResults$entryL <- as.numeric(newResults$Zscore < ZtriggerLong)
  newResults$exitL <- (99) * as.numeric(newResults$Zscore >= ZexitLong)
  newResults$inLt <- newResults$entryL + newResults$exitL 
  newResults$inLt[ newResults$inLt == 0 ] <- NA
  newResults$inL <- na.locf( newResults$inLt )
  newResults$inL[ newResults$inL == 99 ] <- 0
  
  newResults$entryS <- (-1) * as.numeric(newResults$Zscore > ZtriggerShort)
  newResults$exitS <- (99) * as.numeric(newResults$Zscore <= ZexitShort)
  newResults$inSt <- newResults$entryS + newResults$exitS 
  newResults$inSt[ newResults$inSt == 0 ] <- NA
  newResults$inS <- na.locf( newResults$inSt )
  newResults$inS[ newResults$inS == 99 ] <- 0
  
  newResults$inB <- newResults$inL + newResults$inS 
  
  #TO-DO:  add 'isMeanRev' column of 1 or 0 if the ADFtest p-value implies the spread is mean reverting
  
  return( newResults )
}

computeAllZscoreSignals_basket <- function( resultsXTS, numStocks=NULL, zScoreLookback=20, showMultiplots=FALSE, showAllOnSingleChart=FALSE, 
                                    calledFromShinyWithProgressBar=FALSE
                                    , applyFilter_ADF=FALSE 
                                    , applyFilter_Hurst=FALSE 
                                    , applyFilter_OU=FALSE
                                    , applyFilter_Halflife=FALSE 
                          #          , cutoffADF = 0.05
                          #          , cutoffHurst = 0.5
                          #          , cutoffOU = 0.0
                          #          , cutoffHalflife = 63
                                    ) {
  # 'backtestReturnsDF' is a result DF output from calcDynSpreadZscoresAndBacktestDailyReturns
  # EXAMPLE USAGE:  tEEM_computeAll <- computeAllZscoreSignals_basket(resultsXTS=EEM_rollRes,numStocks=4)
  #                 tUSO_GLD_basket_computeAll <- computeAllZscoreSignals_basket(resultsXTS=USO_GLD_basket_rollRes,numStocks=2)
  # 
  
  if( is.null(numStocks) ){ 
    return( print("Please enter the number of stocks in the basket") )
  }
  
  allBacktestDates <- index(resultsXTS)
  
  print( "Backtest Start and End dates:")
  print( allBacktestDates[1] )
  print( allBacktestDates[ length(allBacktestDates) ] )
  print( paste("Spread Z-score lookback window for signal:", zScoreLookback ) )
  
  allBacktests <- list()
  allBacktests[["rawData"]]$rawInputDataDF <- resultsXTS
  
  signalStrategy_optimResultsDF <- data.frame()
  firstRun <- TRUE
  cont_tempNavTS <- xts()
  count_progressBar <- 5
  innerCount <- 0
  
  for( entry in seq(from=0.5, to=3.0, by=0.25) ) {
    allEntryPlots <- list()
    allEntryResultsTS <- list()
    count <- 1
    for( exit in seq(from=0.0, to=2.0, by=0.25) ) {
      if( entry > exit ) {
        innerCount <- innerCount + 1
        print(paste("Entry:",entry,"   Exit:",exit))
        tempBacktestDF <- calcDynSpreadZscoreAndTrades_basket(resultsXTS, numStocks=numStocks, zScoreLookback=zScoreLookback
                                                               , ZtriggerShort = entry, ZtriggerLong = -entry
                                                               , ZexitShort = exit, ZexitLong = -exit
                                                               , applyFilter_ADF=applyFilter_ADF 
                                                               , applyFilter_Hurst=applyFilter_Hurst 
                                                               , applyFilter_OU=applyFilter_OU
                                                               , applyFilter_Halflife=applyFilter_Halflife
                                                  #            , cutoffADF = cutoffADF
                                                  #            , cutoffHurst = cutoffHurst
                                                  #            , cutoffOU = cutoffOU
                                                  #            , cutoffHalflife = cutoffHalflife
                                                              )
        
        allBacktests[[paste(entry,sep="")]][[paste(exit,sep="")]]$backtestResultDF <- tempBacktestDF
        allBacktests[["inputTimeseriesNames"]] <- names(resultsXTS[,1:numStocks])
        btDailyReturns <- na.omit(tempBacktestDF$unitPortRet * tempBacktestDF$inB)
        
        btDailyReturns_inTradeOnly <- btDailyReturns[ btDailyReturns[,1] > 0.0000001 | btDailyReturns[,1] < -0.0000001 , ]
        tempNavTS <- xts( cumprod(1+btDailyReturns) )
        
        if( dim(btDailyReturns_inTradeOnly)[1] > 1 ) { 
          tempNavTS_daysInTradeOnly <- xts( cumprod(1+btDailyReturns_inTradeOnly) ) 
          
          names(tempNavTS) <- c(paste(entry,exit,sep=", "))
          
          allEntryResultsTS[[ count ]] <- tempNavTS
          
          tempTR <- as.numeric(tempNavTS[ length(tempNavTS) ] - 1)
          if( periodicity(btDailyReturns_inTradeOnly)$scale == "daily" ) {
            tempSharpeRatio <- as.numeric(sharpeRatioBasic(btDailyReturns_inTradeOnly))  
          } else {  # monthly
            tempSharpeRatio <- as.numeric(sharpeRatioBasic(btDailyReturns_inTradeOnly,annualizingFactor=12))
          }
          
          tempSortinoRatio <- as.numeric(SortinoRatio(btDailyReturns_inTradeOnly))
          tempMaxdrawdown <- as.numeric( maxDrawdown(btDailyReturns_inTradeOnly) )  # returns only positive values which reflects the negative return
          tempAnnualReturn <- as.numeric( Return.annualized(btDailyReturns_inTradeOnly) )
          tempAnnualVolatility <- as.numeric( StdDev.annualized(btDailyReturns_inTradeOnly) )
          tempStabilityOfReturns <- RSQofTimeseries(tempNavTS_daysInTradeOnly)
          tempTotalReturnMaxDrawdownRatio <- tempTR / tempMaxdrawdown 
            
          allBacktests[[paste(entry,sep="")]][[paste(exit,sep="")]]$NAV <- tempNavTS
          allBacktests[["allNAVs"]][[ innerCount ]] <- tempNavTS
          #TO-DO
          # allBacktests[[paste(entry,sep="")]][[paste(exit,sep="")]]$NAVlongOnly <-
          # allBacktests[[paste(entry,sep="")]][[paste(exit,sep="")]]$NAVshortOnly <-
          allBacktests[[paste(entry,sep="")]][[paste(exit,sep="")]]$dailyReturns <- btDailyReturns
          allBacktests[[paste(entry,sep="")]][[paste(exit,sep="")]]$dailyReturns_inTradeOnly <- btDailyReturns_inTradeOnly
          allBacktests[[paste(entry,sep="")]][[paste(exit,sep="")]]$totalReturn <- tempTR
          allBacktests[[paste(entry,sep="")]][[paste(exit,sep="")]]$sharpeRatio <- tempSharpeRatio
          allBacktests[[paste(entry,sep="")]][[paste(exit,sep="")]]$sortinoRatio <- tempSortinoRatio
          allBacktests[[paste(entry,sep="")]][[paste(exit,sep="")]]$maxDrawdown <- tempMaxdrawdown
          allBacktests[[paste(entry,sep="")]][[paste(exit,sep="")]]$annualReturn <- tempAnnualReturn
          allBacktests[[paste(entry,sep="")]][[paste(exit,sep="")]]$annualVolatility <- tempAnnualVolatility
          allBacktests[[paste(entry,sep="")]][[paste(exit,sep="")]]$stabilityOfReturns <- tempStabilityOfReturns
          
          tempResultsDF <- data.frame(entryZ=entry ,
                                      exitZ=exit ,
                                      totalReturn=tempTR ,
                                      sharpeRatio=tempSharpeRatio ,
                                      sortinoRatio=tempSortinoRatio ,
                                      maxDrawdown=tempMaxdrawdown ,
                                      annualReturn=tempAnnualReturn ,
                                      annualVolatility=tempAnnualVolatility ,
                                      stabilityOfReturns=tempStabilityOfReturns ,
                                      totalReturnMaxDrawdownRatio=tempTotalReturnMaxDrawdownRatio
          )
          
          signalStrategy_optimResultsDF <- rbind( signalStrategy_optimResultsDF, tempResultsDF )
          
          if( firstRun ) {
            allBacktests[["rawData"]]$rawDataAndSpreadAndZscores <- tempBacktestDF
            
            cont_dailyReturns <- na.omit(tempBacktestDF$unitPortRet * (-1) * tempBacktestDF$Zscore)
            cont_tempNavTS <- xts( cumprod(1+cont_dailyReturns) )
            names(cont_tempNavTS) <- c("Continuously Invested")
            
            allBacktests[["generalInfo"]]$spreadZscore <- na.omit(tempBacktestDF$Zscore)
            # allBacktests[["generalInfo"]]$spreadZscore <- as.xts(allBacktests[["generalInfo"]]$spreadZscore)
            allBacktests[["generalInfo"]]$spread <- na.omit(tempBacktestDF$dynSpread)
            # allBacktests[["generalInfo"]]$spread <- as.xts(allBacktests[["generalInfo"]]$spread)
            
            allBacktests[["continuouslyInvested"]]$NAV <- cont_tempNavTS
            # TO-DO
            # allBacktests[["continuouslyInvested"]]$NAV <- cont_tempNavTSlongOnly
            # allBacktests[["continuouslyInvested"]]$NAV <- cont_tempNavTSshortOnly
            allBacktests[["continuouslyInvested"]]$totalReturn <- as.numeric(cont_tempNavTS[ length(cont_tempNavTS) ] - 1)
            if( periodicity(cont_dailyReturns)$scale == "daily" ) {
              allBacktests[["continuouslyInvested"]]$sharpeRatio <- as.numeric(sharpeRatioBasic(cont_dailyReturns))  
            } else {  # monthlu
              allBacktests[["continuouslyInvested"]]$sharpeRatio <- as.numeric(sharpeRatioBasic(cont_dailyReturns,annualizingFactor=12))
            }
            
            allBacktests[["continuouslyInvested"]]$maxDrawdown <- maxDrawdown(cont_dailyReturns)
            allBacktests[["continuouslyInvested"]]$annualReturn <- Return.annualized(cont_dailyReturns)
            allBacktests[["continuouslyInvested"]]$annualVolatility <- StdDev.annualized(cont_dailyReturns)
            
            firstRun <- FALSE
          }
          
          # save off plot of NAVs
          tempPlot <- plotTimeseries(list(tempNavTS), chartTitle=paste("Z-signal, Entry:",entry," Exit:",exit,"Lookback:", zScoreLookback),
                                     showLegend=FALSE, chartTitleSize=10) 
          
          # print(tempPlot)
          allEntryPlots[[count]] <- tempPlot
          allBacktests[[paste(entry,sep="")]][[paste(exit,sep="")]]$NAVplot_signal <- tempPlot
          # example to plot from command line: USO_GLD_findOptim_allHist3[["3"]][["0"]]$NAVplot_signal
          
          count <- count + 1
          if( calledFromShinyWithProgressBar ) {
            count_progressBar <- count_progressBar + 1
            setProgress(value = count_progressBar)
          }
        } 
      }
    }  
    if( showMultiplots ){
      tempAllEntryPlots_multiPlot <- multiplot(plotlist=allEntryPlots,cols=2)
      # print( multiplot(plotlist=allEntryPlots,cols=2) )  
      print( tempAllEntryPlots_multiPlot )  
      allBacktests[[paste("allPlots_Z_Entry_",entry,sep="")]]$entryAndExits_allPlots_list <- allEntryPlots
      # example: to plot from command line:  print( multiplot(plotlist=USO_GLD_findOptim_allHist3[["allPlots_Z_Entry_0.5"]]$entryAndExits_allPlots_list,cols=2) )
    }
    if( showAllOnSingleChart ){
      allEntryResultsTS[[count]] <- cont_tempNavTS
      allResultsPlot <- plotTimeseries(allEntryResultsTS, chartTitle=paste("Z-signals (1-unit each trade), Lookback=",zScoreLookback,", Entry Z=",entry),legendPosition="right",legendLabel="(Entry, Exit) Z")
      allBacktests[[paste("allExitPlots",entry,sep="_")]]$allExits_plot <- allResultsPlot
      print( allResultsPlot )
    }
  }
  
  #print( signalStrategy_optimResultsDF )
  
  # create 'heatmap' matrixes based on totalReturn, sharpeRatio, annualReturn, etc.
  # hmTotalReturnDF <- dcast(data=signalStrategy_optimResultsDF, formula=entryZ~exitZ, value.var="totalReturn")
  # hmSharpeRatioDF <- dcast(data=signalStrategy_optimResultsDF, formula=entryZ~exitZ, value.var="sharpeRatio")
  # hmMaxDrawdownDF <- dcast(data=signalStrategy_optimResultsDF, formula=entryZ~exitZ, value.var="maxDrawdown")
  # hmAnnualReturnDF <- dcast(data=signalStrategy_optimResultsDF, formula=entryZ~exitZ, value.var="annualReturn")
  
  # plot heatmaps graphically
  # hmTextSize <- 3.5
  hmTextSize <- 3.0
  
  # TOTAL RETURN
  hmTotalReturn <- ggplot(signalStrategy_optimResultsDF, aes(entryZ, exitZ)) + geom_tile(aes(fill = totalReturn),colour = "white") + scale_fill_gradient2(low = "darkred", high = "darkgreen", mid="lightgray") + geom_text( aes(label=round(totalReturn,2)), vjust=0.0, size=hmTextSize) + theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
    theme(panel.grid.major=element_line(colour="grey90")) + 
    theme(panel.grid.minor=element_line(colour="grey90")) + 
    theme(axis.line=element_line(colour="grey70")) + 
    theme(axis.title.x=element_text(size=12)) + 
    theme(axis.title.y=element_text(size=12)) + 
    theme(axis.text.x=element_text(size=10)) +
    theme(axis.text.y=element_text(size=10)) +
    ggtitle("Total Return") +
    theme(legend.background=element_rect(colour="grey80")) + 
    theme(legend.key=element_rect(fill="grey99")) +
    theme(legend.position="none") + xlab("Entry Z-score") + ylab("Exit Z-score") + scale_x_continuous(breaks=seq(0,3,0.5))
  
  #  hmSharpeRatio <- ggplot(signalStrategy_optimResultsDF, aes(entryZ, exitZ)) + geom_tile(aes(fill = sharpeRatio),colour = "white") + scale_fill_gradient2(low = "darkred", high = "darkgreen", mid="lightgray") + geom_text( aes(label=round(sharpeRatio,1)), vjust=0.0, size=3.5) + theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
  #  theme(panel.grid.major=element_line(colour="grey90")) + 
  #  theme(panel.grid.minor=element_line(colour="grey90")) + 
  #  theme(axis.line=element_line(colour="grey70")) + 
  #  theme(axis.title.x=element_text(size=14)) +
  #  theme(axis.title.y=element_text(size=14)) + 
  #  theme(axis.text.x=element_text(size=11)) +
  #  theme(axis.text.y=element_text(size=11)) +
  #  ggtitle("Sharpe Ratio") + 
  #  theme(legend.background=element_rect(colour="grey80")) + 
  #  theme(legend.key=element_rect(fill="grey99")) +
  #  theme(legend.position="bottom")
  
  hmSharpeRatio <- ggplot(signalStrategy_optimResultsDF, aes(entryZ, exitZ)) + geom_tile(aes(fill = sharpeRatio),colour = "white") + scale_fill_gradient2(low = "darkred", high = "darkgreen", mid="lightgray") + geom_text( aes(label=round(sharpeRatio,1)), vjust=0.0, size=hmTextSize) + theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
    theme(panel.grid.major=element_line(colour="grey90")) + 
    theme(panel.grid.minor=element_line(colour="grey90")) + 
    theme(axis.line=element_line(colour="grey70")) + 
    theme(axis.title.x=element_text(size=12)) +
    theme(axis.title.y=element_text(size=12)) + 
    theme(axis.text.x=element_text(size=10)) +
    theme(axis.text.y=element_text(size=10)) +
    ggtitle("Sharpe Ratio") + 
    theme(legend.background=element_rect(colour="grey80")) + 
    theme(legend.key=element_rect(fill="grey99")) +
    theme(legend.position="none") + xlab("Entry Z-score") + ylab("Exit Z-score") + scale_x_continuous(breaks=seq(0,3,0.5))
  
  hmMaxDrawdown <- ggplot(signalStrategy_optimResultsDF, aes(entryZ, exitZ)) + geom_tile(aes(fill = maxDrawdown),colour = "white") + scale_fill_gradient2(low = "darkgreen", high = "darkred", mid="lightgray") + geom_text( aes(label=round(maxDrawdown,2)), vjust=0.0, size=hmTextSize) + theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
    theme(panel.grid.major=element_line(colour="grey90")) + 
    theme(panel.grid.minor=element_line(colour="grey90")) + 
    theme(axis.line=element_line(colour="grey70")) + 
    theme(axis.title.x=element_text(size=12)) +
    theme(axis.title.y=element_text(size=12)) + 
    theme(axis.text.x=element_text(size=10)) +
    theme(axis.text.y=element_text(size=10)) +
    ggtitle("Max Drawdown") + 
    theme(legend.background=element_rect(colour="grey80")) + 
    theme(legend.key=element_rect(fill="grey99")) +
    theme(legend.position="none") + xlab("Entry Z-score") + ylab("Exit Z-score") + scale_x_continuous(breaks=seq(0,3,0.5))
  
  hmAnnualReturn <- ggplot(signalStrategy_optimResultsDF, aes(entryZ, exitZ)) + geom_tile(aes(fill = annualReturn),colour = "white") + scale_fill_gradient2(low = "darkred", high = "darkgreen", mid="lightgray") + geom_text( aes(label=round(annualReturn,2)), vjust=0.0, size=hmTextSize) + theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
    theme(panel.grid.major=element_line(colour="grey90")) + 
    theme(panel.grid.minor=element_line(colour="grey90")) + 
    theme(axis.line=element_line(colour="grey70")) + 
    theme(axis.title.x=element_text(size=12)) +
    theme(axis.title.y=element_text(size=12)) + 
    theme(axis.text.x=element_text(size=10)) +
    theme(axis.text.y=element_text(size=10)) +
    ggtitle("Annualized Return") + 
    theme(legend.background=element_rect(colour="grey80")) + 
    theme(legend.key=element_rect(fill="grey99")) +
    theme(legend.position="none") + xlab("Entry Z-score") + ylab("Exit Z-score") + scale_x_continuous(breaks=seq(0,3,0.5))
  
  hmSortinoRatio <- ggplot(signalStrategy_optimResultsDF, aes(entryZ, exitZ)) + geom_tile(aes(fill = sortinoRatio),colour = "white") + scale_fill_gradient2(low = "darkred", high = "darkgreen", mid="lightgray") + geom_text( aes(label=round(sortinoRatio,2)), vjust=0.0, size=hmTextSize) + theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
    theme(panel.grid.major=element_line(colour="grey90")) + 
    theme(panel.grid.minor=element_line(colour="grey90")) + 
    theme(axis.line=element_line(colour="grey70")) + 
    theme(axis.title.x=element_text(size=12)) +
    theme(axis.title.y=element_text(size=12)) + 
    theme(axis.text.x=element_text(size=10)) +
    theme(axis.text.y=element_text(size=10)) +
    ggtitle("Sortino Ratio") + 
    theme(legend.background=element_rect(colour="grey80")) + 
    theme(legend.key=element_rect(fill="grey99")) +
    theme(legend.position="none") + xlab("Entry Z-score") + ylab("Exit Z-score") + scale_x_continuous(breaks=seq(0,3,0.5))
  
  hmTotalReturnMaxDrawdownRatio <- ggplot(signalStrategy_optimResultsDF, aes(entryZ, exitZ)) + geom_tile(aes(fill = totalReturnMaxDrawdownRatio),colour = "white") + scale_fill_gradient2(low = "darkred", high = "darkgreen", mid="lightgray") + geom_text( aes(label=round(totalReturnMaxDrawdownRatio,1)), vjust=0.0, size=hmTextSize) + theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
    theme(panel.grid.major=element_line(colour="grey90")) + 
    theme(panel.grid.minor=element_line(colour="grey90")) + 
    theme(axis.line=element_line(colour="grey70")) + 
    theme(axis.title.x=element_text(size=12)) +
    theme(axis.title.y=element_text(size=12)) + 
    theme(axis.text.x=element_text(size=10)) +
    theme(axis.text.y=element_text(size=10)) +
    ggtitle("Total Return / MaxDrawdown") + 
    theme(legend.background=element_rect(colour="grey80")) + 
    theme(legend.key=element_rect(fill="grey99")) +
    theme(legend.position="none") + xlab("Entry Z-score") + ylab("Exit Z-score") + scale_x_continuous(breaks=seq(0,3,0.5))
  
  hmStabilityOfReturns <- ggplot(signalStrategy_optimResultsDF, aes(entryZ, exitZ)) + geom_tile(aes(fill = stabilityOfReturns),colour = "white") + scale_fill_gradient2(low = "white", high = "steelblue") + geom_text( aes(label=round(stabilityOfReturns,2)), vjust=0.0, size=hmTextSize) + theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
    theme(panel.grid.major=element_line(colour="grey90")) + 
    theme(panel.grid.minor=element_line(colour="grey90")) + 
    theme(axis.line=element_line(colour="grey70")) + 
    theme(axis.title.x=element_text(size=12)) +
    theme(axis.title.y=element_text(size=12)) + 
    theme(axis.text.x=element_text(size=10)) +
    theme(axis.text.y=element_text(size=10)) +
    ggtitle("Consistency Of Returns (RSQ of NAV over time)") + 
    theme(legend.background=element_rect(colour="grey80")) + 
    theme(legend.key=element_rect(fill="grey99")) +
    theme(legend.position="none") + xlab("Entry Z-score") + ylab("Exit Z-score") + scale_x_continuous(breaks=seq(0,3,0.5))
  
  # print( head(signalStrategy_optimResultsDF)) 
  #print( hmTotalReturn )
  #print( hmSharpeRatio )
  
  allHeatmapsList <- list(hmTotalReturn 
                          , hmSharpeRatio 
                          , hmAnnualReturn  
                          , hmMaxDrawdown 
                          , hmStabilityOfReturns 
                          , hmTotalReturnMaxDrawdownRatio
                        #  , hmSortinoRatio  
  )
  
  performanceHeatmaps_multiplot <- print(multiplot(plotlist=allHeatmapsList ,
                                                   cols=2 ) 
  )
  
  allBacktests[["performanceHeatmaps"]]$heatmaps_list <- allHeatmapsList
  # example to print from command line: print( multiplot(plotlist=USO_GLD_findOptim_allHist3[["performanceHeatmaps"]]$heatmaps_list,cols=2) )
  print( performanceHeatmaps_multiplot )
  
  # signalStrategy_optimResultsDF %.% group_by(entryZ) %.% summarise(mean(totalReturn))
  
  # return( signalStrategy_optimResultsDF )
  return( allBacktests )
}


computeAllZscoreSignals <- function( resultsXTS, zScoreLookback=21, showMultiplots=FALSE, showAllOnSingleChart=FALSE, 
                                     stockLabels=c("S1","S2"), additionalDescriptionStr="", calledFromShinyWithProgressBar=FALSE) {
  # 'backtestReturnsDF' is a result DF output from calcDynSpreadZscoresAndBacktestDailyReturns
  # EXAMPLE USAGE:  USO_GLD_findOptim <- computeAllZscoreSignals(USO_GLD_RollRes,zScoreLookback=21, showMultiplots=TRUE) 
  #
  #                 USO_GLD_findOptim_allHist <- computeAllZscoreSignals(USO_GLD_RollRes_allHist,zScoreLookback=21, showMultiplots=TRUE)
  # theOpt <- USO_GLD_findOptim3[["2.25"]][["1.5"]]
  # 
  
  allBacktestDates <- index(resultsXTS)
  
  print( paste("Stocks traded: ", stockLabels[1],stockLabels[2] ) )
  print( "Backtest Start and End dates:")
  print( allBacktestDates[1] )
  print( allBacktestDates[ length(allBacktestDates) ] )
  print( paste("Spread Z-score lookback window for signal:", zScoreLookback ) )
  print( additionalDescriptionStr )
  
  allBacktests <- list()
  allBacktests[["rawData"]]$rawInputDataDF <- resultsXTS
  
  signalStrategy_optimResultsDF <- data.frame()
  firstRun <- TRUE
  cont_tempNavTS <- xts()
  count_progressBar <- 5
  innerCount <- 0
  
  for( entry in seq(from=0.5, to=3.0, by=0.25) ) {
    allEntryPlots <- list()
    allEntryResultsTS <- list()
    count <- 1
    for( exit in seq(from=0.0, to=2.0, by=0.25) ) {
      if( entry > exit ) {
        innerCount <- innerCount + 1
        print(paste("Entry:",entry,"   Exit:",exit))
        tempBacktestDF <- calcDynSpreadZscoreAndTrades(resultsXTS, zScoreLookback=zScoreLookback, 
                                                       ZtriggerShort = entry, ZtriggerLong = -entry,
                                                       ZexitShort = exit, ZexitLong = -exit )
        
        allBacktests[[paste(entry,sep="")]][[paste(exit,sep="")]]$backtestResultDF <- tempBacktestDF
        allBacktests[["inputTimeseriesNames"]] <- stockLabels
        
        # print(names(resultsXTS))
        btDailyReturns <- na.omit(tempBacktestDF$unitPortRet * tempBacktestDF$inB)
        
        #TO-DO:  multiply 'btDailyReturns' * 'isMeanRev' column of 1 or 0 to filter out trades whne the spread was NOT mean reverting heading into each day
        
        # print(dim(btDailyReturns))
        btDailyReturns_inTradeOnly <- btDailyReturns[ btDailyReturns[,1] > 0.0000001 | btDailyReturns[,1] < -0.0000001 , ]
        
        print(dim(btDailyReturns_inTradeOnly))
        
        tempNavTS <- xts( cumprod(1+btDailyReturns) )
        
        if( dim(btDailyReturns_inTradeOnly)[1] > 1 ) { 
          tempNavTS_daysInTradeOnly <- xts( cumprod(1+btDailyReturns_inTradeOnly) ) 
          
          names(tempNavTS) <- c(paste(entry,exit,sep=", "))

          allEntryResultsTS[[count]] <- tempNavTS
          allBacktests[["allNAVs"]][[ innerCount ]] <- tempNavTS
          
          tempTR <- as.numeric(tempNavTS[ length(tempNavTS) ] - 1)
          if( periodicity(btDailyReturns_inTradeOnly)$scale == "daily"){
            tempSharpeRatio <- as.numeric(sharpeRatioBasic(btDailyReturns_inTradeOnly))  
          } else {  # monthly
            tempSharpeRatio <- as.numeric(sharpeRatioBasic(btDailyReturns_inTradeOnly,annualizingFactor=12))
          }
          
          tempSortinoRatio <- as.numeric(SortinoRatio(btDailyReturns_inTradeOnly))
          tempMaxdrawdown <- as.numeric( maxDrawdown(btDailyReturns_inTradeOnly) )  # returns only positive values which reflects the negative return
          tempAnnualReturn <- as.numeric( Return.annualized(btDailyReturns_inTradeOnly) )
          tempAnnualVolatility <- as.numeric( StdDev.annualized(btDailyReturns_inTradeOnly) )
          tempStabilityOfReturns <- RSQofTimeseries(tempNavTS_daysInTradeOnly)
          tempTotalReturnMaxDrawdownRatio <- tempTR / tempMaxdrawdown 
          
          allBacktests[[paste(entry,sep="")]][[paste(exit,sep="")]]$NAV <- tempNavTS
          #TO-DO
          # allBacktests[[paste(entry,sep="")]][[paste(exit,sep="")]]$NAVlongOnly <-
          # allBacktests[[paste(entry,sep="")]][[paste(exit,sep="")]]$NAVshortOnly <-
          allBacktests[[paste(entry,sep="")]][[paste(exit,sep="")]]$dailyReturns <- btDailyReturns
          allBacktests[[paste(entry,sep="")]][[paste(exit,sep="")]]$dailyReturns_inTradeOnly <- btDailyReturns_inTradeOnly
          allBacktests[[paste(entry,sep="")]][[paste(exit,sep="")]]$totalReturn <- tempTR
          allBacktests[[paste(entry,sep="")]][[paste(exit,sep="")]]$sharpeRatio <- tempSharpeRatio
          allBacktests[[paste(entry,sep="")]][[paste(exit,sep="")]]$sortinoRatio <- tempSortinoRatio
          allBacktests[[paste(entry,sep="")]][[paste(exit,sep="")]]$maxDrawdown <- tempMaxdrawdown
          allBacktests[[paste(entry,sep="")]][[paste(exit,sep="")]]$annualReturn <- tempAnnualReturn
          allBacktests[[paste(entry,sep="")]][[paste(exit,sep="")]]$annualVolatility <- tempAnnualVolatility
          allBacktests[[paste(entry,sep="")]][[paste(exit,sep="")]]$stabilityOfReturns <- tempStabilityOfReturns
          
          tempResultsDF <- data.frame(entryZ=entry ,
                                      exitZ=exit ,
                                      totalReturn=tempTR ,
                                      sharpeRatio=tempSharpeRatio ,
                                      sortinoRatio=tempSortinoRatio ,
                                      maxDrawdown=tempMaxdrawdown ,
                                      annualReturn=tempAnnualReturn ,
                                      annualVolatility=tempAnnualVolatility ,
                                      stabilityOfReturns=tempStabilityOfReturns ,
                                      totalReturnMaxDrawdownRatio=tempTotalReturnMaxDrawdownRatio
                                      )
          
          signalStrategy_optimResultsDF <- rbind( signalStrategy_optimResultsDF, tempResultsDF )
          
          if( firstRun ) {
            allBacktests[["rawData"]]$rawDataAndSpreadAndZscores <- tempBacktestDF
            
            cont_dailyReturns <- na.omit(tempBacktestDF$unitPortRet * (-1) * tempBacktestDF$Zscore)
            cont_tempNavTS <- xts( cumprod(1+cont_dailyReturns) )
            names(cont_tempNavTS) <- c("Continuously Invested")
            
            allBacktests[["generalInfo"]]$spreadZscore <- na.omit(tempBacktestDF$Zscore)
            # allBacktests[["generalInfo"]]$spreadZscore <- as.xts(allBacktests[["generalInfo"]]$spreadZscore)
            allBacktests[["generalInfo"]]$spread <- na.omit(tempBacktestDF$dynSpread)
            # allBacktests[["generalInfo"]]$spread <- as.xts(allBacktests[["generalInfo"]]$spread)
            
            allBacktests[["continuouslyInvested"]]$NAV <- cont_tempNavTS
            # TO-DO
            # allBacktests[["continuouslyInvested"]]$NAV <- cont_tempNavTSlongOnly
            # allBacktests[["continuouslyInvested"]]$NAV <- cont_tempNavTSshortOnly
            allBacktests[["continuouslyInvested"]]$totalReturn <- as.numeric(cont_tempNavTS[ length(cont_tempNavTS) ] - 1)
            
            if( periodicity(cont_dailyReturns)$scale == "daily"){
              allBacktests[["continuouslyInvested"]]$sharpeRatio <- as.numeric(sharpeRatioBasic(cont_dailyReturns))
            } else {  # monthly
              allBacktests[["continuouslyInvested"]]$sharpeRatio <- as.numeric(sharpeRatioBasic(cont_dailyReturns,annualizingFactor=12))
            }
            
            allBacktests[["continuouslyInvested"]]$maxDrawdown <- maxDrawdown(cont_dailyReturns)
            allBacktests[["continuouslyInvested"]]$annualReturn <- Return.annualized(cont_dailyReturns)
            allBacktests[["continuouslyInvested"]]$annualVolatility <- StdDev.annualized(cont_dailyReturns)
            
            firstRun <- FALSE
          }
          
          # save off plot of NAVs
          tempPlot <- plotTimeseries(list(tempNavTS), chartTitle=paste("Z-signal, Entry:",entry," Exit:",exit,"Lookback:", zScoreLookback),
                                     showLegend=FALSE, chartTitleSize=10) 
          
          # print(tempPlot)
          allEntryPlots[[count]] <- tempPlot
          allBacktests[[paste(entry,sep="")]][[paste(exit,sep="")]]$NAVplot_signal <- tempPlot
          # example to plot from command line: USO_GLD_findOptim_allHist3[["3"]][["0"]]$NAVplot_signal
          
          count <- count + 1
          if( calledFromShinyWithProgressBar ) {
            count_progressBar <- count_progressBar + 1
            setProgress(value = count_progressBar)
          }
        } 
      }
    }  
    if( showMultiplots ){
      tempAllEntryPlots_multiPlot <- multiplot(plotlist=allEntryPlots,cols=2)
      # print( multiplot(plotlist=allEntryPlots,cols=2) )  
      print( tempAllEntryPlots_multiPlot )  
      allBacktests[[paste("allPlots_Z_Entry_",entry,sep="")]]$entryAndExits_allPlots_list <- allEntryPlots
      # example: to plot from command line:  print( multiplot(plotlist=USO_GLD_findOptim_allHist3[["allPlots_Z_Entry_0.5"]]$entryAndExits_allPlots_list,cols=2) )
    }
    if( showAllOnSingleChart ){
      allEntryResultsTS[[count]] <- cont_tempNavTS
      allResultsPlot <- plotTimeseries(allEntryResultsTS, chartTitle=paste("Z-signals (1-unit each trade), Lookback=",zScoreLookback,", Entry Z=",entry),legendPosition="right",legendLabel="(Entry, Exit) Z")
      allBacktests[[paste("allExitPlots",entry,sep="_")]]$allExits_plot <- allResultsPlot
      print( allResultsPlot )
    }
  }
  
  #print( signalStrategy_optimResultsDF )
  
  # create 'heatmap' matrixes based on totalReturn, sharpeRatio, annualReturn, etc.
  # hmTotalReturnDF <- dcast(data=signalStrategy_optimResultsDF, formula=entryZ~exitZ, value.var="totalReturn")
  # hmSharpeRatioDF <- dcast(data=signalStrategy_optimResultsDF, formula=entryZ~exitZ, value.var="sharpeRatio")
  # hmMaxDrawdownDF <- dcast(data=signalStrategy_optimResultsDF, formula=entryZ~exitZ, value.var="maxDrawdown")
  # hmAnnualReturnDF <- dcast(data=signalStrategy_optimResultsDF, formula=entryZ~exitZ, value.var="annualReturn")
  
  # plot heatmaps graphically
  # hmTextSize <- 3.5
  hmTextSize <- 3.0
  
  # TOTAL RETURN
  hmTotalReturn <- ggplot(signalStrategy_optimResultsDF, aes(entryZ, exitZ)) + geom_tile(aes(fill = totalReturn),colour = "white") + scale_fill_gradient2(low = "darkred", high = "darkgreen", mid="lightgray") + geom_text( aes(label=round(totalReturn,2)), vjust=0.0, size=hmTextSize) + theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
    theme(panel.grid.major=element_line(colour="grey90")) + 
    theme(panel.grid.minor=element_line(colour="grey90")) + 
    theme(axis.line=element_line(colour="grey70")) + 
    theme(axis.title.x=element_text(size=12)) + 
    theme(axis.title.y=element_text(size=12)) + 
    theme(axis.text.x=element_text(size=10)) +
    theme(axis.text.y=element_text(size=10)) +
    ggtitle("Total Return") +
    theme(legend.background=element_rect(colour="grey80")) + 
    theme(legend.key=element_rect(fill="grey99")) +
    theme(legend.position="none") + xlab("Entry Z-score") + ylab("Exit Z-score") + scale_x_continuous(breaks=seq(0,3,0.5))
  
  #  hmSharpeRatio <- ggplot(signalStrategy_optimResultsDF, aes(entryZ, exitZ)) + geom_tile(aes(fill = sharpeRatio),colour = "white") + scale_fill_gradient2(low = "darkred", high = "darkgreen", mid="lightgray") + geom_text( aes(label=round(sharpeRatio,1)), vjust=0.0, size=3.5) + theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
  #  theme(panel.grid.major=element_line(colour="grey90")) + 
  #  theme(panel.grid.minor=element_line(colour="grey90")) + 
  #  theme(axis.line=element_line(colour="grey70")) + 
  #  theme(axis.title.x=element_text(size=14)) +
  #  theme(axis.title.y=element_text(size=14)) + 
  #  theme(axis.text.x=element_text(size=11)) +
  #  theme(axis.text.y=element_text(size=11)) +
  #  ggtitle("Sharpe Ratio") + 
  #  theme(legend.background=element_rect(colour="grey80")) + 
  #  theme(legend.key=element_rect(fill="grey99")) +
  #  theme(legend.position="bottom")
  
  hmSharpeRatio <- ggplot(signalStrategy_optimResultsDF, aes(entryZ, exitZ)) + geom_tile(aes(fill = sharpeRatio),colour = "white") + scale_fill_gradient2(low = "darkred", high = "darkgreen", mid="lightgray") + geom_text( aes(label=round(sharpeRatio,1)), vjust=0.0, size=hmTextSize) + theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
    theme(panel.grid.major=element_line(colour="grey90")) + 
    theme(panel.grid.minor=element_line(colour="grey90")) + 
    theme(axis.line=element_line(colour="grey70")) + 
    theme(axis.title.x=element_text(size=12)) +
    theme(axis.title.y=element_text(size=12)) + 
    theme(axis.text.x=element_text(size=10)) +
    theme(axis.text.y=element_text(size=10)) +
    ggtitle("Sharpe Ratio") + 
    theme(legend.background=element_rect(colour="grey80")) + 
    theme(legend.key=element_rect(fill="grey99")) +
    theme(legend.position="none") + xlab("Entry Z-score") + ylab("Exit Z-score") + scale_x_continuous(breaks=seq(0,3,0.5))
  
  hmMaxDrawdown <- ggplot(signalStrategy_optimResultsDF, aes(entryZ, exitZ)) + geom_tile(aes(fill = maxDrawdown),colour = "white") + scale_fill_gradient2(low = "darkgreen", high = "darkred", mid="lightgray") + geom_text( aes(label=round(maxDrawdown,2)), vjust=0.0, size=hmTextSize) + theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
    theme(panel.grid.major=element_line(colour="grey90")) + 
    theme(panel.grid.minor=element_line(colour="grey90")) + 
    theme(axis.line=element_line(colour="grey70")) + 
    theme(axis.title.x=element_text(size=12)) +
    theme(axis.title.y=element_text(size=12)) + 
    theme(axis.text.x=element_text(size=10)) +
    theme(axis.text.y=element_text(size=10)) +
    ggtitle("Max Drawdown") + 
    theme(legend.background=element_rect(colour="grey80")) + 
    theme(legend.key=element_rect(fill="grey99")) +
    theme(legend.position="none") + xlab("Entry Z-score") + ylab("Exit Z-score") + scale_x_continuous(breaks=seq(0,3,0.5))
  
  hmAnnualReturn <- ggplot(signalStrategy_optimResultsDF, aes(entryZ, exitZ)) + geom_tile(aes(fill = annualReturn),colour = "white") + scale_fill_gradient2(low = "darkred", high = "darkgreen", mid="lightgray") + geom_text( aes(label=round(annualReturn,2)), vjust=0.0, size=hmTextSize) + theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
    theme(panel.grid.major=element_line(colour="grey90")) + 
    theme(panel.grid.minor=element_line(colour="grey90")) + 
    theme(axis.line=element_line(colour="grey70")) + 
    theme(axis.title.x=element_text(size=12)) +
    theme(axis.title.y=element_text(size=12)) + 
    theme(axis.text.x=element_text(size=10)) +
    theme(axis.text.y=element_text(size=10)) +
    ggtitle("Annualized Return") + 
    theme(legend.background=element_rect(colour="grey80")) + 
    theme(legend.key=element_rect(fill="grey99")) +
    theme(legend.position="none") + xlab("Entry Z-score") + ylab("Exit Z-score") + scale_x_continuous(breaks=seq(0,3,0.5))
  
  hmSortinoRatio <- ggplot(signalStrategy_optimResultsDF, aes(entryZ, exitZ)) + geom_tile(aes(fill = sortinoRatio),colour = "white") + scale_fill_gradient2(low = "darkred", high = "darkgreen", mid="lightgray") + geom_text( aes(label=round(sortinoRatio,2)), vjust=0.0, size=hmTextSize) + theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
    theme(panel.grid.major=element_line(colour="grey90")) + 
    theme(panel.grid.minor=element_line(colour="grey90")) + 
    theme(axis.line=element_line(colour="grey70")) + 
    theme(axis.title.x=element_text(size=12)) +
    theme(axis.title.y=element_text(size=12)) + 
    theme(axis.text.x=element_text(size=10)) +
    theme(axis.text.y=element_text(size=10)) +
    ggtitle("Sortino Ratio") + 
    theme(legend.background=element_rect(colour="grey80")) + 
    theme(legend.key=element_rect(fill="grey99")) +
    theme(legend.position="none") + xlab("Entry Z-score") + ylab("Exit Z-score") + scale_x_continuous(breaks=seq(0,3,0.5))

  hmTotalReturnMaxDrawdownRatio <- ggplot(signalStrategy_optimResultsDF, aes(entryZ, exitZ)) + geom_tile(aes(fill = totalReturnMaxDrawdownRatio),colour = "white") + scale_fill_gradient2(low = "darkred", high = "darkgreen", mid="lightgray") + geom_text( aes(label=round(totalReturnMaxDrawdownRatio,1)), vjust=0.0, size=hmTextSize) + theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
    theme(panel.grid.major=element_line(colour="grey90")) + 
    theme(panel.grid.minor=element_line(colour="grey90")) + 
    theme(axis.line=element_line(colour="grey70")) + 
    theme(axis.title.x=element_text(size=12)) +
    theme(axis.title.y=element_text(size=12)) + 
    theme(axis.text.x=element_text(size=10)) +
    theme(axis.text.y=element_text(size=10)) +
    ggtitle("Total Return / MaxDrawdown") + 
    theme(legend.background=element_rect(colour="grey80")) + 
    theme(legend.key=element_rect(fill="grey99")) +
    theme(legend.position="none") + xlab("Entry Z-score") + ylab("Exit Z-score") + scale_x_continuous(breaks=seq(0,3,0.5))
  
  hmStabilityOfReturns <- ggplot(signalStrategy_optimResultsDF, aes(entryZ, exitZ)) + geom_tile(aes(fill = stabilityOfReturns),colour = "white") + scale_fill_gradient2(low = "white", high = "steelblue") + geom_text( aes(label=round(stabilityOfReturns,2)), vjust=0.0, size=hmTextSize) + theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
    theme(panel.grid.major=element_line(colour="grey90")) + 
    theme(panel.grid.minor=element_line(colour="grey90")) + 
    theme(axis.line=element_line(colour="grey70")) + 
    theme(axis.title.x=element_text(size=12)) +
    theme(axis.title.y=element_text(size=12)) + 
    theme(axis.text.x=element_text(size=10)) +
    theme(axis.text.y=element_text(size=10)) +
    ggtitle("Consistency Of Returns (RSQ of NAV over time)") + 
    theme(legend.background=element_rect(colour="grey80")) + 
    theme(legend.key=element_rect(fill="grey99")) +
    theme(legend.position="none") + xlab("Entry Z-score") + ylab("Exit Z-score") + scale_x_continuous(breaks=seq(0,3,0.5))
  
  # print( head(signalStrategy_optimResultsDF)) 
  #print( hmTotalReturn )
  #print( hmSharpeRatio )
  
  allHeatmapsList <- list(hmTotalReturn 
                          , hmSharpeRatio 
                          , hmAnnualReturn  
                          , hmMaxDrawdown 
                          , hmStabilityOfReturns 
                          # , hmSortinoRatio
                          , hmTotalReturnMaxDrawdownRatio
                          )
  
  performanceHeatmaps_multiplot <- print(multiplot(plotlist=allHeatmapsList ,
                                                   cols=2 ) 
                                        )
  
  allBacktests[["performanceHeatmaps"]]$heatmaps_list <- allHeatmapsList
  # example to print from command line: print( multiplot(plotlist=USO_GLD_findOptim_allHist3[["performanceHeatmaps"]]$heatmaps_list,cols=2) )
  print( performanceHeatmaps_multiplot )
  
  # signalStrategy_optimResultsDF %.% group_by(entryZ) %.% summarise(mean(totalReturn))
  
  # return( signalStrategy_optimResultsDF )
  return( allBacktests )
}

computeAllLookbackWindowsSignals <- function( s1_tsVec, s2_tsVec, spreadTypeToUse="price",
                                              zScore_lookbackMin=10, zScore_lookbackMax=20, zScore_lookbackStep=10, 
                                              betaLookbackDays_Min=20, betaLookbackDays_Max=40, betaLookbackDays_Step=20, 
                                              showAllOnSingleChart=FALSE,
                                              stockLabels=c("S1","S2"), additionalDescriptionStr="", 
                                              printHeatmaps=FALSE, calledFromShinyWithProgressBar=FALSE) {
  # 'resultsXTS' is a result DF output from calcDynSpreadZscoresAndBacktestDailyReturns
  # EXAMPLE USAGE:  USO_GLD_findOptim <- computeAllZscoreSignals(USO_GLD_RollRes,zScoreLookback=21, showMultiplots=TRUE) 
  #
  #                 USO_GLD_findOptim_allHist <- computeAllZscoreSignals(USO_GLD_RollRes_allHist,zScoreLookback=21, showMultiplots=TRUE)
  # theOpt <- USO_GLD_findOptim3[["2.25"]][["1.5"]]
  # 
  
  print( paste("Stocks traded: ", stockLabels[1],stockLabels[2] ) )
  print( additionalDescriptionStr )
  
  allBacktests <- list()
  allBacktestPlots <- list()
  signalStrategy_optimResultsDF <- data.frame()
  
  cont_tempNavTS <- xts()
  count_progressBar <- 5
  
  innerCount <- 0
  outerCount <- 1
  allEntryResultsTS <- list()
  for( zScoreLookback in seq(from=zScore_lookbackMin, to=zScore_lookbackMax, by=zScore_lookbackStep) ) {
    allEntryPlots <- list()
    # allEntryResultsTS <- list()
    count <- 1
    for( betaLookbackDays in seq(from=betaLookbackDays_Min, to=betaLookbackDays_Max, by=betaLookbackDays_Step) ) {
      innerCount <- innerCount + 1
      if( calledFromShinyWithProgressBar ) {
        count_progressBar <- count_progressBar + 1
        setProgress(value = count_progressBar)
      }
      print(paste("zScoreLookback:", zScoreLookback, "   betaLookbackDays:",betaLookbackDays))
      resultsXTS <- pairSpreadTestRolling(s1_tsVec, s2_tsVec, 
                                          startDate, endDate,
                                          spreadType=spreadTypeToUse,
                                          rollingWindowLength=1, totalLookbackLength=betaLookbackDays )
      
      allBacktests[["inputTimeseriesNames"]] <- stockLabels
      allBacktests[[paste(zScoreLookback)]][[paste(betaLookbackDays)]][["rawData"]]$rawInputDataDF <- resultsXTS
      
      tempBacktestDF <- calcDynSpreadZscoreAndTrades(resultsXTS, zScoreLookback=zScoreLookback, 
                                                     ZtriggerShort = 9, ZtriggerLong = -9,
                                                     ZexitShort = 1, ZexitLong = -1 )
      
      allBacktests[[paste(zScoreLookback)]][[paste(betaLookbackDays)]]$backtestResultDF <- tempBacktestDF
      
      allBacktests[["rawData"]]$rawDataAndSpreadAndZscores <- tempBacktestDF
      
      btDailyReturns <- na.omit(tempBacktestDF$unitPortRet * (-1) * tempBacktestDF$Zscore)
      btDailyReturns_inTradeOnly <- btDailyReturns[ btDailyReturns[,1] > 0.0000001 | btDailyReturns[,1] < -0.0000001 , ]
      tempNavTS <- xts( cumprod(1+btDailyReturns) )

      allBacktests[[paste(zScoreLookback)]][[paste(betaLookbackDays)]]$spreadZscore <- na.omit(tempBacktestDF$Zscore)
      allBacktests[[paste(zScoreLookback)]][[paste(betaLookbackDays)]]$spread <- na.omit(tempBacktestDF$dynSpread)
      
      if( dim(btDailyReturns_inTradeOnly)[1] > 1 ) { 
        tempNavTS_daysInTradeOnly <- xts( cumprod(1+btDailyReturns_inTradeOnly) ) 
        
        names(tempNavTS) <- c(paste(zScoreLookback,betaLookbackDays,sep=", "))
        
        allBacktests[["allNAVs"]][[ innerCount ]] <- tempNavTS
        # allEntryResultsTS[[count]] <- tempNavTS
        allEntryResultsTS[[outerCount]] <- tempNavTS
        
        tempTR <- as.numeric(tempNavTS[ length(tempNavTS) ] - 1)
        if( periodicity(btDailyReturns_inTradeOnly)$scale == "daily" ){
          tempSharpeRatio <- as.numeric(sharpeRatioBasic(btDailyReturns_inTradeOnly))  
        } else {  # monthly
          tempSharpeRatio <- as.numeric(sharpeRatioBasic(btDailyReturns_inTradeOnly,annualizingFactor=12))
        }
        
        tempSortinoRatio <- as.numeric(SortinoRatio(btDailyReturns_inTradeOnly))
        tempMaxdrawdown <- as.numeric( maxDrawdown(btDailyReturns_inTradeOnly) )  # returns only positive values which reflects the negative return
        tempAnnualReturn <- as.numeric( Return.annualized(btDailyReturns_inTradeOnly) )
        tempAnnualVolatility <- as.numeric( StdDev.annualized(btDailyReturns_inTradeOnly) )
        tempStabilityOfReturns <- RSQofTimeseries(tempNavTS_daysInTradeOnly)
        tempTotalReturnMaxDrawdownRatio <- tempTR / tempMaxdrawdown
        
        allBacktests[[paste(zScoreLookback)]][[paste(betaLookbackDays)]]$NAV <- tempNavTS
        #TO-DO
        # allBacktests[[paste(entry,sep="")]][[paste(exit,sep="")]]$NAVlongOnly <-
        # allBacktests[[paste(entry,sep="")]][[paste(exit,sep="")]]$NAVshortOnly <-
        allBacktests[[paste(zScoreLookback)]][[paste(betaLookbackDays)]]$dailyReturns <- btDailyReturns
        allBacktests[[paste(zScoreLookback)]][[paste(betaLookbackDays)]]$dailyReturns_inTradeOnly <- btDailyReturns_inTradeOnly
        allBacktests[[paste(zScoreLookback)]][[paste(betaLookbackDays)]]$totalReturn <- tempTR
        allBacktests[[paste(zScoreLookback)]][[paste(betaLookbackDays)]]$sharpeRatio <- tempSharpeRatio
        allBacktests[[paste(zScoreLookback)]][[paste(betaLookbackDays)]]$sortinoRatio <- tempSortinoRatio
        allBacktests[[paste(zScoreLookback)]][[paste(betaLookbackDays)]]$maxDrawdown <- tempMaxdrawdown
        allBacktests[[paste(zScoreLookback)]][[paste(betaLookbackDays)]]$annualReturn <- tempAnnualReturn
        allBacktests[[paste(zScoreLookback)]][[paste(betaLookbackDays)]]$annualVolatility <- tempAnnualVolatility
        allBacktests[[paste(zScoreLookback)]][[paste(betaLookbackDays)]]$stabilityOfReturns <- tempStabilityOfReturns
        
        tempResultsDF <- data.frame(zScoreLookback=zScoreLookback ,
                                    betaLookbackDays=betaLookbackDays ,
                                    totalReturn=tempTR ,
                                    sharpeRatio=tempSharpeRatio ,
                                    sortinoRatio=tempSortinoRatio ,
                                    maxDrawdown=tempMaxdrawdown ,
                                    annualReturn=tempAnnualReturn ,
                                    annualVolatility=tempAnnualVolatility ,
                                    stabilityOfReturns=tempStabilityOfReturns ,
                                    totalReturnMaxDrawdownRatio=tempTotalReturnMaxDrawdownRatio
        )
        
        signalStrategy_optimResultsDF <- rbind( signalStrategy_optimResultsDF, tempResultsDF )
        
        # save off plot of NAVs
        tempPlot <- plotTimeseries(list(tempNavTS), chartTitle=paste("Always investing Z-units, zScoreLookback:",zScoreLookback," betaLookbackDays:",betaLookbackDays),
                                   showLegend=FALSE, chartTitleSize=10) 
        
        # print(tempPlot)
        allEntryPlots[[count]] <- tempPlot
        allBacktests[[paste(zScoreLookback)]][[paste(betaLookbackDays)]]$NAVplot_signal <- tempPlot
        # example to plot from command line: USO_GLD_findOptim_allHist3[["3"]][["0"]]$NAVplot_signal
        
        count <- count + 1
      } 
      
      if( showAllOnSingleChart ){
        allResultsPlot <- plotTimeseries( timeseriesToPlotListXTS=allEntryResultsTS
                                      #    , chartTitle=paste("Always Investing Z-units, Z-score Lookback=",zScoreLookback)
                                          , legendPosition="right"
                                          , chartTitleSize=12
                                          , legendLabel="Lookback Days (Z, Beta)" )
        
        allBacktests[["allContinuousPlots"]] <- allResultsPlot
        # print( allResultsPlot )
      }
      outerCount <- outerCount + 1
    }  
  }
  
  allBacktests[["allBacktestNAVs"]] <- allEntryResultsTS
  
  print("optimResultsDF")
  print( signalStrategy_optimResultsDF )
  print("summary allBacktests")
  print( summary(allBacktests) )
  print("rawData")
  print(head(allBacktests[["rawData"]]$rawDataAndSpreadAndZscores))
  
  if( printHeatmaps ){
    # plot heatmaps graphically
    # hmTextSize <- 3.5
    hmTextSize <- 3.0
    # TOTAL RETURN
    hmTotalReturn <- ggplot(signalStrategy_optimResultsDF, aes(zScoreLookback, betaLookbackDays)) + geom_tile(aes(fill = totalReturn),colour = "white") + scale_fill_gradient2(low = "darkred", high = "darkgreen", mid="lightgray") + geom_text( aes(label=round(totalReturn,2)), vjust=0.0, size=hmTextSize) + theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
      theme(panel.grid.major=element_line(colour="grey90")) + 
      theme(panel.grid.minor=element_line(colour="grey90")) + 
      theme(axis.line=element_line(colour="grey70")) + 
      theme(axis.title.x=element_text(size=12)) + 
      theme(axis.title.y=element_text(size=12)) + 
      theme(axis.text.x=element_text(size=10)) +
      theme(axis.text.y=element_text(size=10)) +
      ggtitle("Total Return") +
      theme(legend.background=element_rect(colour="grey80")) + 
      theme(legend.key=element_rect(fill="grey99")) +
      theme(legend.position="none") + xlab("Z-score Lookback") + ylab("Hedge-Ratio Lookback") + scale_x_continuous(breaks=seq(from=zScore_lookbackMin, to=zScore_lookbackMax, by=zScore_lookbackStep)) + scale_y_continuous(breaks=seq(from=betaLookbackDays_Min, to=betaLookbackDays_Max, by=betaLookbackDays_Step))
    
    hmSharpeRatio <- ggplot(signalStrategy_optimResultsDF, aes(zScoreLookback, betaLookbackDays)) + geom_tile(aes(fill = sharpeRatio),colour = "white") + scale_fill_gradient2(low = "darkred", high = "darkgreen", mid="lightgray") + geom_text( aes(label=round(sharpeRatio,1)), vjust=0.0, size=hmTextSize) + theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
      theme(panel.grid.major=element_line(colour="grey90")) + 
      theme(panel.grid.minor=element_line(colour="grey90")) + 
      theme(axis.line=element_line(colour="grey70")) + 
      theme(axis.title.x=element_text(size=12)) +
      theme(axis.title.y=element_text(size=12)) + 
      theme(axis.text.x=element_text(size=10)) +
      theme(axis.text.y=element_text(size=10)) +
      ggtitle("Sharpe Ratio") + 
      theme(legend.background=element_rect(colour="grey80")) + 
      theme(legend.key=element_rect(fill="grey99")) +
      theme(legend.position="none") + xlab("Z-score Lookback") + ylab("Hedge-Ratio Lookback") + scale_x_continuous(breaks=seq(from=zScore_lookbackMin, to=zScore_lookbackMax, by=zScore_lookbackStep)) + scale_y_continuous(breaks=seq(from=betaLookbackDays_Min, to=betaLookbackDays_Max, by=betaLookbackDays_Step)) 
    # + scale_x_continuous(breaks=seq(0,3,0.5))
    
    hmMaxDrawdown <- ggplot(signalStrategy_optimResultsDF, aes(zScoreLookback, betaLookbackDays)) + geom_tile(aes(fill = maxDrawdown),colour = "white") + scale_fill_gradient2(low = "darkgreen", high = "darkred", mid="lightgray") + geom_text( aes(label=round(maxDrawdown,2)), vjust=0.0, size=hmTextSize) + theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
      theme(panel.grid.major=element_line(colour="grey90")) + 
      theme(panel.grid.minor=element_line(colour="grey90")) + 
      theme(axis.line=element_line(colour="grey70")) + 
      theme(axis.title.x=element_text(size=12)) +
      theme(axis.title.y=element_text(size=12)) + 
      theme(axis.text.x=element_text(size=10)) +
      theme(axis.text.y=element_text(size=10)) +
      ggtitle("Max Drawdown") + 
      theme(legend.background=element_rect(colour="grey80")) + 
      theme(legend.key=element_rect(fill="grey99")) +
      theme(legend.position="none") + xlab("Z-score Lookback") + ylab("Hedge-Ratio Lookback") + scale_x_continuous(breaks=seq(from=zScore_lookbackMin, to=zScore_lookbackMax, by=zScore_lookbackStep)) + scale_y_continuous(breaks=seq(from=betaLookbackDays_Min, to=betaLookbackDays_Max, by=betaLookbackDays_Step)) 
    
    hmAnnualReturn <- ggplot(signalStrategy_optimResultsDF, aes(zScoreLookback, betaLookbackDays)) + geom_tile(aes(fill = annualReturn),colour = "white") + scale_fill_gradient2(low = "darkred", high = "darkgreen", mid="lightgray") + geom_text( aes(label=round(annualReturn,2)), vjust=0.0, size=hmTextSize) + theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
      theme(panel.grid.major=element_line(colour="grey90")) + 
      theme(panel.grid.minor=element_line(colour="grey90")) + 
      theme(axis.line=element_line(colour="grey70")) + 
      theme(axis.title.x=element_text(size=12)) +
      theme(axis.title.y=element_text(size=12)) + 
      theme(axis.text.x=element_text(size=10)) +
      theme(axis.text.y=element_text(size=10)) +
      ggtitle("Annualized Return") + 
      theme(legend.background=element_rect(colour="grey80")) + 
      theme(legend.key=element_rect(fill="grey99")) +
      theme(legend.position="none") + xlab("Z-score Lookback") + ylab("Hedge-Ratio Lookback") + scale_x_continuous(breaks=seq(from=zScore_lookbackMin, to=zScore_lookbackMax, by=zScore_lookbackStep)) + scale_y_continuous(breaks=seq(from=betaLookbackDays_Min, to=betaLookbackDays_Max, by=betaLookbackDays_Step))
    
    hmSortinoRatio <- ggplot(signalStrategy_optimResultsDF, aes(zScoreLookback, betaLookbackDays)) + geom_tile(aes(fill = sortinoRatio),colour = "white") + scale_fill_gradient2(low = "darkred", high = "darkgreen", mid="lightgray") + geom_text( aes(label=round(sortinoRatio,2)), vjust=0.0, size=hmTextSize) + theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
      theme(panel.grid.major=element_line(colour="grey90")) + 
      theme(panel.grid.minor=element_line(colour="grey90")) + 
      theme(axis.line=element_line(colour="grey70")) + 
      theme(axis.title.x=element_text(size=12)) +
      theme(axis.title.y=element_text(size=12)) + 
      theme(axis.text.x=element_text(size=10)) +
      theme(axis.text.y=element_text(size=10)) +
      ggtitle("Sortino Ratio") + 
      theme(legend.background=element_rect(colour="grey80")) + 
      theme(legend.key=element_rect(fill="grey99")) +
      theme(legend.position="none") + xlab("Z-score Lookback") + ylab("Hedge-Ratio Lookback") + scale_x_continuous(breaks=seq(from=zScore_lookbackMin, to=zScore_lookbackMax, by=zScore_lookbackStep)) + scale_y_continuous(breaks=seq(from=betaLookbackDays_Min, to=betaLookbackDays_Max, by=betaLookbackDays_Step)) 
    
    hmTotalReturnMaxDrawdownRatio <- ggplot(signalStrategy_optimResultsDF, aes(zScoreLookback, betaLookbackDays)) + geom_tile(aes(fill = totalReturnMaxDrawdownRatio),colour = "white") + scale_fill_gradient2(low = "darkred", high = "darkgreen", mid="lightgray") + geom_text( aes(label=round(totalReturnMaxDrawdownRatio,1)), vjust=0.0, size=hmTextSize) + theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
      theme(panel.grid.major=element_line(colour="grey90")) + 
      theme(panel.grid.minor=element_line(colour="grey90")) + 
      theme(axis.line=element_line(colour="grey70")) + 
      theme(axis.title.x=element_text(size=12)) +
      theme(axis.title.y=element_text(size=12)) + 
      theme(axis.text.x=element_text(size=10)) +
      theme(axis.text.y=element_text(size=10)) +
      ggtitle("Total Return / MaxDrawdown") + 
      theme(legend.background=element_rect(colour="grey80")) + 
      theme(legend.key=element_rect(fill="grey99")) +
      theme(legend.position="none") + xlab("Z-score Lookback") + ylab("Hedge-Ratio Lookback") + scale_x_continuous(breaks=seq(from=zScore_lookbackMin, to=zScore_lookbackMax, by=zScore_lookbackStep)) + scale_y_continuous(breaks=seq(from=betaLookbackDays_Min, to=betaLookbackDays_Max, by=betaLookbackDays_Step)) 
    
    hmStabilityOfReturns <- ggplot(signalStrategy_optimResultsDF, aes(zScoreLookback, betaLookbackDays)) + geom_tile(aes(fill = stabilityOfReturns),colour = "white") + scale_fill_gradient2(low = "white", high = "steelblue") + geom_text( aes(label=round(stabilityOfReturns,2)), vjust=0.0, size=hmTextSize) + theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
      theme(panel.grid.major=element_line(colour="grey90")) + 
      theme(panel.grid.minor=element_line(colour="grey90")) + 
      theme(axis.line=element_line(colour="grey70")) + 
      theme(axis.title.x=element_text(size=12)) +
      theme(axis.title.y=element_text(size=12)) + 
      theme(axis.text.x=element_text(size=10)) +
      theme(axis.text.y=element_text(size=10)) +
      ggtitle("Consistency Of Returns (RSQ of NAV over time)") + 
      theme(legend.background=element_rect(colour="grey80")) + 
      theme(legend.key=element_rect(fill="grey99")) +
      theme(legend.position="none") + xlab("Z-score Lookback") + ylab("Hedge-Ratio Lookback") + scale_x_continuous(breaks=seq(from=zScore_lookbackMin, to=zScore_lookbackMax, by=zScore_lookbackStep)) + scale_y_continuous(breaks=seq(from=betaLookbackDays_Min, to=betaLookbackDays_Max, by=betaLookbackDays_Step))
    
    # print( head(signalStrategy_optimResultsDF)) 
    
    allHeatmapsList <- list(hmTotalReturn 
                            , hmSharpeRatio 
                            , hmAnnualReturn  
                            , hmMaxDrawdown 
                            , hmStabilityOfReturns 
                            # , hmSortinoRatio  
                            , hmTotalReturnMaxDrawdownRatio
                            )
    
    performanceHeatmaps_multiplot <- print(multiplot(plotlist=allHeatmapsList ,
                                                     cols=2 ) 
                                          )
    
    allBacktests[["performanceHeatmaps"]]$heatmaps_list <- allHeatmapsList
    # example to print from command line: print( multiplot(plotlist=USO_GLD_findOptim_allHist3[["performanceHeatmaps"]]$heatmaps_list,cols=2) )
    print( performanceHeatmaps_multiplot )
  }
  # signalStrategy_optimResultsDF %.% group_by(entryZ) %.% summarise(mean(totalReturn))
  
  # return( signalStrategy_optimResultsDF )
  return( allBacktests )
}

computeAllLookbackWindowsSignals_basket <- function( depVarVec, indVarsList, spreadTypeToUse="price",
                                              zScore_lookbackMin=10, zScore_lookbackMax=20, zScore_lookbackStep=10, 
                                              betaLookbackDays_Min=20, betaLookbackDays_Max=40, betaLookbackDays_Step=20, 
                                              showAllOnSingleChart=FALSE,
                                              printHeatmaps=FALSE, calledFromShinyWithProgressBar=FALSE) {
  # 'resultsXTS' is a result DF output from calcDynSpreadZscoresAndBacktestDailyReturns
  # EXAMPLE USAGE:  USO_GLD_findOptim <- computeAllZscoreSignals(USO_GLD_RollRes,zScoreLookback=21, showMultiplots=TRUE) 
  #
  #                 USO_GLD_findOptim_allHist <- computeAllZscoreSignals(USO_GLD_RollRes_allHist,zScoreLookback=21, showMultiplots=TRUE)
  # theOpt <- USO_GLD_findOptim3[["2.25"]][["1.5"]]
  # 
  
  allBacktests <- list()
  allBacktestPlots <- list()
  signalStrategy_optimResultsDF <- data.frame()
  
  cont_tempNavTS <- xts()
  count_progressBar <- 5
  
  numBasketStocks <- length(indVarsList) + 1
  innerCount <- 0
  outerCount <- 1
  allEntryResultsTS <- list()
  
  for( zScoreLookback in seq(from=zScore_lookbackMin, to=zScore_lookbackMax, by=zScore_lookbackStep) ) {
    allEntryPlots <- list()
    # allEntryResultsTS <- list()
    count <- 1
    for( betaLookbackDays in seq(from=betaLookbackDays_Min, to=betaLookbackDays_Max, by=betaLookbackDays_Step) ) {
      innerCount <- innerCount + 1
      if( calledFromShinyWithProgressBar ) {
        count_progressBar <- count_progressBar + 1
        setProgress(value = count_progressBar)
      }
      print(paste("zScoreLookback:", zScoreLookback, "   betaLookbackDays:",betaLookbackDays))
      resultsXTS <- basketSpreadTestRolling_OLS(depVarVec,indVarsList, 
                                          startDate, endDate,
                                          spreadType=spreadTypeToUse,
                                          rollingWindowLength=1, totalLookbackLength=betaLookbackDays )
      
      allBacktests[["inputTimeseriesNames"]] <- c( names(depVarVec), unlist(lapply(indVarsList,names)) )
   #   print( allBacktests[["inputTimeseriesNames"]] )
      allBacktests[[paste(zScoreLookback)]][[paste(betaLookbackDays)]][["rawData"]]$rawInputDataDF <- resultsXTS
      
      tempBacktestDF <- calcDynSpreadZscoreAndTrades_basket(resultsXTS,numStocks=numBasketStocks, 
                                                            zScoreLookback=zScoreLookback, 
                                                            ZtriggerShort = 9, ZtriggerLong = -9,
                                                            ZexitShort = 1, ZexitLong = -1 
                                                            )
      
      allBacktests[["rawData"]]$rawDataAndSpreadAndZscores <- tempBacktestDF
      
      allBacktests[[paste(zScoreLookback)]][[paste(betaLookbackDays)]]$backtestResultDF <- tempBacktestDF
      
      btDailyReturns <- na.omit(tempBacktestDF$unitPortRet * (-1) * tempBacktestDF$Zscore)
      btDailyReturns_inTradeOnly <- btDailyReturns[ btDailyReturns[,1] > 0.0000001 | btDailyReturns[,1] < -0.0000001 , ]
      tempNavTS <- xts( cumprod(1+btDailyReturns) )
      
      allBacktests[[paste(zScoreLookback)]][[paste(betaLookbackDays)]]$spreadZscore <- na.omit(tempBacktestDF$Zscore)
      allBacktests[[paste(zScoreLookback)]][[paste(betaLookbackDays)]]$spread <- na.omit(tempBacktestDF$dynSpread)
      
      if( dim(btDailyReturns_inTradeOnly)[1] > 1 ) { 
        tempNavTS_daysInTradeOnly <- xts( cumprod(1+btDailyReturns_inTradeOnly) ) 
        
        names(tempNavTS) <- c(paste(zScoreLookback,betaLookbackDays,sep=", "))
        
        # allEntryResultsTS[[count]] <- tempNavTS
        allEntryResultsTS[[outerCount]] <- tempNavTS
        allBacktests[["allNAVs"]][[ innerCount ]] <- tempNavTS
        
        tempTR <- as.numeric(tempNavTS[ length(tempNavTS) ] - 1)
        if( periodicity(btDailyReturns_inTradeOnly)$scale == "daily" ){
          tempSharpeRatio <- as.numeric(sharpeRatioBasic(btDailyReturns_inTradeOnly))  
        } else { # monthly
          tempSharpeRatio <- as.numeric(sharpeRatioBasic(btDailyReturns_inTradeOnly,annualizingFactor=12))
        }
        
        tempSortinoRatio <- as.numeric(SortinoRatio(btDailyReturns_inTradeOnly))
        tempMaxdrawdown <- as.numeric( maxDrawdown(btDailyReturns_inTradeOnly) )  # returns only positive values which reflects the negative return
        tempAnnualReturn <- as.numeric( Return.annualized(btDailyReturns_inTradeOnly) )
        tempAnnualVolatility <- as.numeric( StdDev.annualized(btDailyReturns_inTradeOnly) )
        tempStabilityOfReturns <- RSQofTimeseries(tempNavTS_daysInTradeOnly)
        tempTotalReturnMaxDrawdownRatio <- tempTR / tempMaxdrawdown
        
        allBacktests[[paste(zScoreLookback)]][[paste(betaLookbackDays)]]$NAV <- tempNavTS
        #TO-DO
        # allBacktests[[paste(entry,sep="")]][[paste(exit,sep="")]]$NAVlongOnly <-
        # allBacktests[[paste(entry,sep="")]][[paste(exit,sep="")]]$NAVshortOnly <-
        allBacktests[[paste(zScoreLookback)]][[paste(betaLookbackDays)]]$dailyReturns <- btDailyReturns
        allBacktests[[paste(zScoreLookback)]][[paste(betaLookbackDays)]]$dailyReturns_inTradeOnly <- btDailyReturns_inTradeOnly
        allBacktests[[paste(zScoreLookback)]][[paste(betaLookbackDays)]]$totalReturn <- tempTR
        allBacktests[[paste(zScoreLookback)]][[paste(betaLookbackDays)]]$sharpeRatio <- tempSharpeRatio
        allBacktests[[paste(zScoreLookback)]][[paste(betaLookbackDays)]]$sortinoRatio <- tempSortinoRatio
        allBacktests[[paste(zScoreLookback)]][[paste(betaLookbackDays)]]$maxDrawdown <- tempMaxdrawdown
        allBacktests[[paste(zScoreLookback)]][[paste(betaLookbackDays)]]$annualReturn <- tempAnnualReturn
        allBacktests[[paste(zScoreLookback)]][[paste(betaLookbackDays)]]$annualVolatility <- tempAnnualVolatility
        allBacktests[[paste(zScoreLookback)]][[paste(betaLookbackDays)]]$stabilityOfReturns <- tempStabilityOfReturns
        
        tempResultsDF <- data.frame(zScoreLookback=zScoreLookback ,
                                    betaLookbackDays=betaLookbackDays ,
                                    totalReturn=tempTR ,
                                    sharpeRatio=tempSharpeRatio ,
                                    sortinoRatio=tempSortinoRatio ,
                                    maxDrawdown=tempMaxdrawdown ,
                                    annualReturn=tempAnnualReturn ,
                                    annualVolatility=tempAnnualVolatility ,
                                    stabilityOfReturns=tempStabilityOfReturns ,
                                    totalReturnMaxDrawdownRatio=tempTotalReturnMaxDrawdownRatio
        )
        
        signalStrategy_optimResultsDF <- rbind( signalStrategy_optimResultsDF, tempResultsDF )
        
        # save off plot of NAVs
        tempPlot <- plotTimeseries(list(tempNavTS), chartTitle=paste("Always investing Z-units, zScoreLookback:",zScoreLookback," betaLookbackDays:",betaLookbackDays),
                                   showLegend=FALSE, chartTitleSize=10) 
        
        # print(tempPlot)
        allEntryPlots[[count]] <- tempPlot
        allBacktests[[paste(zScoreLookback)]][[paste(betaLookbackDays)]]$NAVplot_signal <- tempPlot
        # example to plot from command line: USO_GLD_findOptim_allHist3[["3"]][["0"]]$NAVplot_signal
        
        count <- count + 1
      } 
      
      if( showAllOnSingleChart ){
        allResultsPlot <- plotTimeseries( timeseriesToPlotListXTS=allEntryResultsTS
                                          #    , chartTitle=paste("Always Investing Z-units, Z-score Lookback=",zScoreLookback)
                                          , legendPosition="right"
                                          , chartTitleSize=12
                                          , legendLabel="Lookback Days (Z, Beta)" )
        
        allBacktests[["allContinuousPlots"]] <- allResultsPlot
        # print( allResultsPlot )
      }
      outerCount <- outerCount + 1
    }  
  }
  
  allBacktests[["allBacktestNAVs"]] <- allEntryResultsTS
  
  if( printHeatmaps ){
    # plot heatmaps graphically
    # hmTextSize <- 3.5
    hmTextSize <- 3.0
    # TOTAL RETURN
    hmTotalReturn <- ggplot(signalStrategy_optimResultsDF, aes(zScoreLookback, betaLookbackDays)) + geom_tile(aes(fill = totalReturn),colour = "white") + scale_fill_gradient2(low = "darkred", high = "darkgreen", mid="lightgray") + geom_text( aes(label=round(totalReturn,2)), vjust=0.0, size=hmTextSize) + theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
      theme(panel.grid.major=element_line(colour="grey90")) + 
      theme(panel.grid.minor=element_line(colour="grey90")) + 
      theme(axis.line=element_line(colour="grey70")) + 
      theme(axis.title.x=element_text(size=12)) + 
      theme(axis.title.y=element_text(size=12)) + 
      theme(axis.text.x=element_text(size=10)) +
      theme(axis.text.y=element_text(size=10)) +
      ggtitle("Total Return") +
      theme(legend.background=element_rect(colour="grey80")) + 
      theme(legend.key=element_rect(fill="grey99")) +
      theme(legend.position="none") + xlab("Z-score Lookback") + ylab("Hedge-Ratio Lookback") + scale_x_continuous(breaks=seq(from=zScore_lookbackMin, to=zScore_lookbackMax, by=zScore_lookbackStep)) + scale_y_continuous(breaks=seq(from=betaLookbackDays_Min, to=betaLookbackDays_Max, by=betaLookbackDays_Step))
    
    hmSharpeRatio <- ggplot(signalStrategy_optimResultsDF, aes(zScoreLookback, betaLookbackDays)) + geom_tile(aes(fill = sharpeRatio),colour = "white") + scale_fill_gradient2(low = "darkred", high = "darkgreen", mid="lightgray") + geom_text( aes(label=round(sharpeRatio,1)), vjust=0.0, size=hmTextSize) + theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
      theme(panel.grid.major=element_line(colour="grey90")) + 
      theme(panel.grid.minor=element_line(colour="grey90")) + 
      theme(axis.line=element_line(colour="grey70")) + 
      theme(axis.title.x=element_text(size=12)) +
      theme(axis.title.y=element_text(size=12)) + 
      theme(axis.text.x=element_text(size=10)) +
      theme(axis.text.y=element_text(size=10)) +
      ggtitle("Sharpe Ratio") + 
      theme(legend.background=element_rect(colour="grey80")) + 
      theme(legend.key=element_rect(fill="grey99")) +
      theme(legend.position="none") + xlab("Z-score Lookback") + ylab("Hedge-Ratio Lookback") + scale_x_continuous(breaks=seq(from=zScore_lookbackMin, to=zScore_lookbackMax, by=zScore_lookbackStep)) + scale_y_continuous(breaks=seq(from=betaLookbackDays_Min, to=betaLookbackDays_Max, by=betaLookbackDays_Step)) 
    # + scale_x_continuous(breaks=seq(0,3,0.5))
    
    hmMaxDrawdown <- ggplot(signalStrategy_optimResultsDF, aes(zScoreLookback, betaLookbackDays)) + geom_tile(aes(fill = maxDrawdown),colour = "white") + scale_fill_gradient2(low = "darkgreen", high = "darkred", mid="lightgray") + geom_text( aes(label=round(maxDrawdown,2)), vjust=0.0, size=hmTextSize) + theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
      theme(panel.grid.major=element_line(colour="grey90")) + 
      theme(panel.grid.minor=element_line(colour="grey90")) + 
      theme(axis.line=element_line(colour="grey70")) + 
      theme(axis.title.x=element_text(size=12)) +
      theme(axis.title.y=element_text(size=12)) + 
      theme(axis.text.x=element_text(size=10)) +
      theme(axis.text.y=element_text(size=10)) +
      ggtitle("Max Drawdown") + 
      theme(legend.background=element_rect(colour="grey80")) + 
      theme(legend.key=element_rect(fill="grey99")) +
      theme(legend.position="none") + xlab("Z-score Lookback") + ylab("Hedge-Ratio Lookback") + scale_x_continuous(breaks=seq(from=zScore_lookbackMin, to=zScore_lookbackMax, by=zScore_lookbackStep)) + scale_y_continuous(breaks=seq(from=betaLookbackDays_Min, to=betaLookbackDays_Max, by=betaLookbackDays_Step)) 
    
    hmAnnualReturn <- ggplot(signalStrategy_optimResultsDF, aes(zScoreLookback, betaLookbackDays)) + geom_tile(aes(fill = annualReturn),colour = "white") + scale_fill_gradient2(low = "darkred", high = "darkgreen", mid="lightgray") + geom_text( aes(label=round(annualReturn,2)), vjust=0.0, size=hmTextSize) + theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
      theme(panel.grid.major=element_line(colour="grey90")) + 
      theme(panel.grid.minor=element_line(colour="grey90")) + 
      theme(axis.line=element_line(colour="grey70")) + 
      theme(axis.title.x=element_text(size=12)) +
      theme(axis.title.y=element_text(size=12)) + 
      theme(axis.text.x=element_text(size=10)) +
      theme(axis.text.y=element_text(size=10)) +
      ggtitle("Annualized Return") + 
      theme(legend.background=element_rect(colour="grey80")) + 
      theme(legend.key=element_rect(fill="grey99")) +
      theme(legend.position="none") + xlab("Z-score Lookback") + ylab("Hedge-Ratio Lookback") + scale_x_continuous(breaks=seq(from=zScore_lookbackMin, to=zScore_lookbackMax, by=zScore_lookbackStep)) + scale_y_continuous(breaks=seq(from=betaLookbackDays_Min, to=betaLookbackDays_Max, by=betaLookbackDays_Step))
    
    hmSortinoRatio <- ggplot(signalStrategy_optimResultsDF, aes(zScoreLookback, betaLookbackDays)) + geom_tile(aes(fill = sortinoRatio),colour = "white") + scale_fill_gradient2(low = "darkred", high = "darkgreen", mid="lightgray") + geom_text( aes(label=round(sortinoRatio,2)), vjust=0.0, size=hmTextSize) + theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
      theme(panel.grid.major=element_line(colour="grey90")) + 
      theme(panel.grid.minor=element_line(colour="grey90")) + 
      theme(axis.line=element_line(colour="grey70")) + 
      theme(axis.title.x=element_text(size=12)) +
      theme(axis.title.y=element_text(size=12)) + 
      theme(axis.text.x=element_text(size=10)) +
      theme(axis.text.y=element_text(size=10)) +
      ggtitle("Sortino Ratio") + 
      theme(legend.background=element_rect(colour="grey80")) + 
      theme(legend.key=element_rect(fill="grey99")) +
      theme(legend.position="none") + xlab("Z-score Lookback") + ylab("Hedge-Ratio Lookback") + scale_x_continuous(breaks=seq(from=zScore_lookbackMin, to=zScore_lookbackMax, by=zScore_lookbackStep)) + scale_y_continuous(breaks=seq(from=betaLookbackDays_Min, to=betaLookbackDays_Max, by=betaLookbackDays_Step)) 
    
    hmTotalReturnMaxDrawdownRatio <- ggplot(signalStrategy_optimResultsDF, aes(zScoreLookback, betaLookbackDays)) + geom_tile(aes(fill = totalReturnMaxDrawdownRatio),colour = "white") + scale_fill_gradient2(low = "darkred", high = "darkgreen", mid="lightgray") + geom_text( aes(label=round(totalReturnMaxDrawdownRatio,1)), vjust=0.0, size=hmTextSize) + theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
      theme(panel.grid.major=element_line(colour="grey90")) + 
      theme(panel.grid.minor=element_line(colour="grey90")) + 
      theme(axis.line=element_line(colour="grey70")) + 
      theme(axis.title.x=element_text(size=12)) +
      theme(axis.title.y=element_text(size=12)) + 
      theme(axis.text.x=element_text(size=10)) +
      theme(axis.text.y=element_text(size=10)) +
      ggtitle("Total Return / MaxDrawdown") + 
      theme(legend.background=element_rect(colour="grey80")) + 
      theme(legend.key=element_rect(fill="grey99")) +
      theme(legend.position="none") + xlab("Z-score Lookback") + ylab("Hedge-Ratio Lookback") + scale_x_continuous(breaks=seq(from=zScore_lookbackMin, to=zScore_lookbackMax, by=zScore_lookbackStep)) + scale_y_continuous(breaks=seq(from=betaLookbackDays_Min, to=betaLookbackDays_Max, by=betaLookbackDays_Step)) 
    
    hmStabilityOfReturns <- ggplot(signalStrategy_optimResultsDF, aes(zScoreLookback, betaLookbackDays)) + geom_tile(aes(fill = stabilityOfReturns),colour = "white") + scale_fill_gradient2(low = "white", high = "steelblue") + geom_text( aes(label=round(stabilityOfReturns,2)), vjust=0.0, size=hmTextSize) + theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
      theme(panel.grid.major=element_line(colour="grey90")) + 
      theme(panel.grid.minor=element_line(colour="grey90")) + 
      theme(axis.line=element_line(colour="grey70")) + 
      theme(axis.title.x=element_text(size=12)) +
      theme(axis.title.y=element_text(size=12)) + 
      theme(axis.text.x=element_text(size=10)) +
      theme(axis.text.y=element_text(size=10)) +
      ggtitle("Consistency Of Returns (RSQ of NAV over time)") + 
      theme(legend.background=element_rect(colour="grey80")) + 
      theme(legend.key=element_rect(fill="grey99")) +
      theme(legend.position="none") + xlab("Z-score Lookback") + ylab("Hedge-Ratio Lookback") + scale_x_continuous(breaks=seq(from=zScore_lookbackMin, to=zScore_lookbackMax, by=zScore_lookbackStep)) + scale_y_continuous(breaks=seq(from=betaLookbackDays_Min, to=betaLookbackDays_Max, by=betaLookbackDays_Step))
    
    # print( head(signalStrategy_optimResultsDF)) 
    
    allHeatmapsList <- list(hmTotalReturn 
                            , hmSharpeRatio 
                            , hmAnnualReturn  
                            , hmMaxDrawdown 
                            , hmStabilityOfReturns 
                            , hmTotalReturnMaxDrawdownRatio
                            # , hmSortinoRatio  
    )
    
    performanceHeatmaps_multiplot <- print(multiplot(plotlist=allHeatmapsList ,
                                                     cols=2 ) 
    )
    
    allBacktests[["performanceHeatmaps"]]$heatmaps_list <- allHeatmapsList
    # example to print from command line: print( multiplot(plotlist=USO_GLD_findOptim_allHist3[["performanceHeatmaps"]]$heatmaps_list,cols=2) )
    print( performanceHeatmaps_multiplot )
  }
  # signalStrategy_optimResultsDF %.% group_by(entryZ) %.% summarise(mean(totalReturn))
  
  # return( signalStrategy_optimResultsDF )
  return( allBacktests )
}

