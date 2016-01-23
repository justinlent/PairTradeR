
computeStrategy<-function(paramCombos, theStrategyFunction, theStrategyInputs, doParallel=FALSE){
  if( doParallel ){
    return( unlist(mclapply(paramCombos,theStrategyFunction,theStrategyInputs, mc.cores=detectCores())) )
  }else{
    return( lapply(paramCombos,theStrategyFunction,theStrategyInputs) )  
  }
  
}

computeNeightboringParameterCombos<-function(paramList, numNeightbors=2) {
  
}

sdTS<-function(returnsTS) {
  noNA <- na.omit(returnsTS)
  return(apply(noNA,2,sd))
}

sharpeRatioBasic<-function(returnsTS, annualizingFactor=252) {
  return( annualizedReturnBasic(returnsTS,annualizingFactor) / annualizedVolatilityBasic(returnsTS,annualizingFactor) )
}

sharpeRatioOfNAV<-function(NAVts, annualizingFactor=252) {
  retTS <- dailyReturn(NAVts)
  return( sharpeRatioBasic(retTS,annualizingFactor) )
}

annualizedReturnBasic<-function(returnsTS, annualizingFactor=252) {
  noNA <- na.omit(returnsTS)
  temp1<-mean(noNA)*annualizingFactor
  names(temp1)<-NULL
  return(temp1)
}

annualizedVolatilityBasic<-function(returnsTS, annualizingFactor=252) {
  temp1<-sdTS(returnsTS)*sqrt(annualizingFactor)
  names(temp1)<-NULL
  return(temp1)
}

RSQofTimeseries <- function( theTS ) {
  tempLM <- lm( theTS ~ seq(1:length(theTS)) )
  return( summary(tempLM)$adj.r.squared )
}

notIsNull<-function(theObject){
  return( !is.null(theObject) )
}

addSignalOR<-function(signalNameStr, signalReturnTS) {
  
}

addSignalAND<-function(signalNameStr, signalReturnTS) {
  
}

combineSignals<-function(combinedSignalNameStrin, signalList) {
  
}


tickerStringToStock<-function(tickerString, dataColumn=NULL) {
  if(is.null(dataColumn)) {
    return( get(tickerString) )  
  } else {
    return( get(tickerString)[,dataColumn] )
  }
}

computeOptimizationCombos<-function(paramList) {
  numSignals<-length(paramList)

  listOfEachParamsValues<-rep(list(),numSignals)
  for( i in 1:numSignals ){
    tempMin<-paramList["min",i]
    tempMax<-paramList["max",i]
    tempStep<-paramList["step",i]
    # tempSignalCombos<-trunc((tempMax-tempMin)/tempStep)
    tempParamValues<-c()
    for( j in seq(from=tempMin, to=tempMax, by=tempStep)){
      tempParamValues<-c(tempParamValues,j)
    }
    listOfEachParamsValues[[i]]<-tempParamValues           
  }
  
  allCombosDF<-expand.grid(listOfEachParamsValues)
  numOfCombos<-dim(allCombosDF)[1]
  print("num of Combos:")
  print(numOfCombos)
  allCombos<-c()
  for( i in 1:numOfCombos ){
    tempParamValues<-c()
    for( j in 1:numSignals ){
      tempParamValues<-c(tempParamValues, as.numeric(allCombosDF[i,][j]) )
    }
    allCombos<-c(allCombos,list(tempParamValues))
  }
  return(allCombos)
}

makeLabelFromParamList<-function(paramList){
  theLabel<-paste()
  for( i in 1:length(paramList)){
    theLabel<-paste(theLabel,paramList[i],sep="_")
  }
  return(substr(theLabel, 2, nchar(theLabel)))
}

singleSimulationSummaryTable<-function(singleSim, isClusterCalc=FALSE, computeNeighborsStats=FALSE, numOfNeighbors=2, overallOptParamsDF=NULL, theStrategyFunction=NULL, theStrategyFunctionGeneralInputs=NULL, useAsGeneric=FALSE, writeDesc=FALSE, codeToDescMap=NULL, assetPercValue=NULL){
  
  simReturns<-dailyReturn(singleSim)
  simName<-names(singleSim)[1]
  splitString<-NULL
  
  if( useAsGeneric ){
    if( writeDesc ){
      splitString<-strsplit(simName,"_")
      codeAsStr<-splitString[[1]][3]
      #print(codeAsStr)
      codeDescRow<-which( codeAsStr == codeToDescMap$MCC_Codes)
      if( length(codeDescRow) > 0 ){
        codeDesc<-codeToDescMap[codeDescRow,]["Description"][[1]]
        PerceptualMapValue<-codeToDescMap[codeDescRow,]["SoftHardQualification"][[1]]
        PercValueDiff<-(as.numeric(assetPercValue)-as.numeric(PerceptualMapValue)) / 10.0
      } else {
        codeDesc<-"unknown"
        PerceptualMapValue <- -99
      }
    } 
  } else {
      splitString<-strsplit(simName,"_")
      paramNumbers<-as.numeric(splitString[[1]])
      startNAV<-as.numeric(singleSim[1])
      finalNAV<-as.numeric(singleSim[length(singleSim)])
      #tableTotalPctRet<-100 * ((finalNAV-startNAV) / startNAV)  
  }
  
  tableTotalPctRet<-sum(simReturns)
  tableSharpeRatio<-sharpeRatioBasic(simReturns)
  tableAnnReturn<-100 * annualizedReturnBasic(simReturns)
  tableAnnVol<-100 * annualizedVolatilityBasic(simReturns)
  tableMaxDrawdown<-100 * maxDrawdown(simReturns, invert=FALSE)
  tablePerceptualMapValue<-PerceptualMapValue
  tablePerceptualValueDiff<-abs(PercValueDiff)
  tableScaledSharpe <- (1.0 - tablePerceptualValueDiff) * tableSharpeRatio
  tableCodeDesc<-codeDesc
  #tableCodeDesc<-99999
  
  if( isClusterCalc ){
    summDFrows<-c("ClusterAvg_Pct_Return"=tableTotalPctRet, "ClusterAvg_Sharpe_Ratio"=tableSharpeRatio, "ClusterAvg_Annual_Pct_Return"=tableAnnReturn, "ClusterAvg_Annual_Volatility"=tableAnnVol, "ClusterAvg_Max_Drawdown"=tableMaxDrawdown)
  } else {
    if( writeDesc ){
      #summDFrows<-c("Pct_Return"=tableTotalPctRet, "Sharpe_Ratio"=tableSharpeRatio, "Annual_Pct_Return"=tableAnnReturn, "Annual_Volatility"=tableAnnVol, "Max_Drawdown"=tableMaxDrawdown, "MCC_Name"=I(tableCodeDesc))  
      summDFrows<-c("Pct_Return"=tableTotalPctRet, "Sharpe_Ratio"=tableSharpeRatio, "Annual_Pct_Return"=tableAnnReturn, "Perc_Value"=tablePerceptualMapValue, "Perc_Value_Diff"=tablePerceptualValueDiff, "Scaled_Sharpe"=tableScaledSharpe, "Max_Drawdown"=tableMaxDrawdown, "MCC_Name"=I(tableCodeDesc))
    } else {  
      summDFrows<-c("Pct_Return"=tableTotalPctRet, "Sharpe_Ratio"=tableSharpeRatio, "Annual_Pct_Return"=tableAnnReturn, "Annual_Volatility"=tableAnnVol, "Max_Drawdown"=tableMaxDrawdown)  
      summDFrows<-c("Pct_Return"=tableTotalPctRet, "Sharpe_Ratio"=tableSharpeRatio, "Annual_Pct_Return"=tableAnnReturn, "Max_Drawdown"=tableMaxDrawdown)  
    }
    
  }
  
  simParams<-NULL
  
  if( useAsGeneric ){
    
  } else {
    for( i in 1:length(paramNumbers)){
      tempParamVal<-paramNumbers[i]
      if( isClusterCalc ) {
        names(tempParamVal)<-paste("ClusterAvg_param_",i,sep="")
      } else {
        names(tempParamVal)<-paste("param_",i,sep="")  
      }
      if( computeNeighborsStats ) {
        simParams<-c(simParams,tempParamVal)
      }
      summDFrows<-c(summDFrows,tempParamVal)
    }
  }
  
  if( computeNeighborsStats ) {
    tempOveralParamSetting<-overallOptParamsDF[,1]
    tempOverallParamMin<-tempOveralParamSetting[1]
    tempOverallParamMax<-tempOveralParamSetting[2]
    tempParamStep<-tempOveralParamSetting[3]
    neighborOptParamsDF<-data.frame("signal1"=c("min"=max(tempOverallParamMin,simParams[1]-numOfNeighbors*tempParamStep),"max"=min(tempOverallParamMax,simParams[1]+numOfNeighbors*tempParamStep),"step"=tempParamStep))
    for( i in 2:length(simParams)) {
      tempOveralParamSetting<-overallOptParamsDF[,i]
      tempOverallParamMin<-tempOveralParamSetting[1]
      tempOverallParamMax<-tempOveralParamSetting[2]
      tempParamStep<-tempOveralParamSetting[3]
      tempSig<-data.frame("signal"=c("min"=max(tempOverallParamMin,simParams[i]-numOfNeighbors*tempParamStep),"max"=min(tempOverallParamMax,simParams[i]+numOfNeighbors*tempParamStep),"step"=tempParamStep))
      neighborOptParamsDF<-cbind(neighborOptParamsDF,tempSig)
    }
    allNeighborParamCombosNew<-computeOptimizationCombos(neighborOptParamsDF)
    allNeighborStratSims<-computeStrategy(allNeighborParamCombosNew,theStrategyFunction,theStrategyFunctionGeneralInputs)
    ostNeighbors<-optimizationSummaryTable(allStrategySims=allNeighborStratSims, isClusterCalc=TRUE, computeNeighborsStats=FALSE)
    ostNeighborsTranspose<-t(as.matrix(ostNeighbors))
    neighborsStatsAverages<-colMeans(ostNeighborsTranspose)
  }  
  
  if( computeNeighborsStats) {
    basicSummaryNames<-names(summDFrows)
    neighborsStatsNames<-names(neighborsStatsAverages)
    allColNames<-c(basicSummaryNames,neighborsStatsNames)
    thisSim<-data.frame(c(summDFrows,neighborsStatsAverages))
    rownames(thisSim)<-allColNames
    colnames(thisSim)<-simName  
    return(thisSim)  
  } else {
    thisSim<-data.frame(summDFrows)
    colnames(thisSim)<-simName  
    return(thisSim)  
  }
}

optimizationSummaryTable<-function(allStrategySims, isClusterCalc=FALSE, computeNeighborsStats=FALSE, numOfNeighbors=2, overallOptParamsDF=NULL, theStrategyFunction=NULL, theStrategyFunctionGeneralInputs=NULL, useAsGeneric=FALSE, writeDesc=FALSE, codeToDescMap=NULL, assetPercValue=NULL){
  allSims<-singleSimulationSummaryTable(allStrategySims[[1]], isClusterCalc=isClusterCalc, computeNeighborsStats=computeNeighborsStats, numOfNeighbors=numOfNeighbors, overallOptParamsDF=overallOptParamsDF, theStrategyFunction=theStrategyFunction, theStrategyFunctionGeneralInputs=theStrategyFunctionGeneralInputs, useAsGeneric=useAsGeneric, writeDesc=writeDesc, codeToDescMap=codeToDescMap, assetPercValue=assetPercValue)
  for(i in 2:length(allStrategySims)){
    tempObject<-allStrategySims[[i]]
    print(paste(assetPercValue,"generating summary stats table...",names(tempObject)))
    tempSim<-singleSimulationSummaryTable(tempObject, isClusterCalc=isClusterCalc, computeNeighborsStats=computeNeighborsStats, numOfNeighbors=numOfNeighbors, overallOptParamsDF=overallOptParamsDF, theStrategyFunction=theStrategyFunction, theStrategyFunctionGeneralInputs=theStrategyFunctionGeneralInputs, useAsGeneric=useAsGeneric, writeDesc=writeDesc, codeToDescMap=codeToDescMap, assetPercValue=assetPercValue)
    allSims<-cbind(allSims, tempSim)
    #print(tempSim)
  }  
  return(allSims)
}
  