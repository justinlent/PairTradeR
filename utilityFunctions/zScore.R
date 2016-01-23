rollingZScore <- function(timeseries,window){
  timeseries <- na.omit(timeseries)
  temp <- ((timeseries - runMean(timeseries, n=window))/runSD(timeseries, n=window))
  colnames(temp) <- (paste(colnames(timeseries), "Z", window, sep = "_"))
  return(temp)
}

rollingZScoreAboveThresh <- function(timeseries,window,thresh){
  temp <- (rollingZScore(timeseries,window) > thresh)
  colnames(temp) <- (paste(colnames(temp), "thrsh", thresh, sep = "_"))
  return (temp)
}

rollingZScoreBelowThresh <- function(timeseries,window,thresh){
  temp <- (rollingZScore(timeseries,window) < -thresh)
  colnames(temp) <- (paste(colnames(temp), "thrsh", thresh, sep = "_"))
  return (temp)
}

rollingZScoreBothThresh <- function(timeseries,window,thresh){
  temp <- ((rollingZScore(timeseries,window) > thresh)-(rollingZScore(timeseries,window) < -thresh))
  colnames(temp) <- (paste(colnames(temp), "thrsh", thresh, sep = "_"))
  return (temp)
}

