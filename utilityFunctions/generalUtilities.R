getQuandlTimeseries <- function(quandlSymbolStr, quandlColumnNumber, mySymbolStr, timeseriesType="xts") {
  tempTS <- Quandl(quandlSymbolStr,type=timeseriesType)
  print(names(tempTS))
  print(mySymbolStr)
  tempTS <- tempTS[,quandlColumnNumber]
  print(head(tempTS))
  print(names(tempTS))
  names(tempTS) <- mySymbolStr
  return(tempTS)
}

lastVec <- function( theVec ){
  return( theVec[ length(theVec) ] )  
}

nvalid <- function(x) sum(!is.na(x))

alignTimeseries <- function( tsList, customNamesVec=NULL ){
  if( is.null(customNamesVec) ){
    allNames <- c(names(tsList[[1]]))
  } else {
    allNames <- customNamesVec
  }
  mergeTS <- tsList[[1]]
  for( i in seq(from=2, to=length(tsList)) ) {
    mergeTS <- merge(mergeTS, tsList[[i]], all=FALSE)
    if( is.null(customNamesVec) ){
      allNames <- c(allNames,names(tsList[[i]]))  
    }
  }
  names(mergeTS) <- allNames
  return(mergeTS)
}

getAlignedTimeseriesDates <- function( tsList ){
  mergeTS <- tsList[[1]]
  for( i in seq(from=2, to=length(tsList)) ) {
    mergeTS <- merge(mergeTS, tsList[[i]], all=FALSE)
  }
  return(index(mergeTS))
}

mean.n   <- function(df, n) {
  means <- apply(as.matrix(df), 1, mean, na.rm = TRUE)
  nvalid <- apply(as.matrix(df), 1, function(df) sum(!is.na(df)))
  ifelse(nvalid >= n, means, NA)
}

asMonthlyXTS <- function(tsVec){
  byFirstDayOfMonth <- unique(timeFirstDayInMonth(index(tsVec)))
  ts.m <- aggregate(as.timeSeries(tsVec),byFirstDayOfMonth,last)
  # strip the timezone from the datestamp. This is an artifact of the as.timeSeries() casting above
  ts.m.xts <- as.xts(ts.m)
  index(ts.m.xts) <- as.Date(index(ts.m.xts))
  return(ts.m.xts)
}

loadPriceHistory_CSV <- function(thePath, columnWithDate=1, columnWithPrice=2){
  tempRead <- read.csv(file=thePath,stringsAsFactors=FALSE)
  tempRead.xts <- xts( tempRead[,columnWithPrice],order.by=as.Date(trim(tempRead[,columnWithDate])) )
  names( tempRead.xts ) <- names(tempRead)[columnWithPrice]
  return( tempRead.xts )
}

loadPriceHistory_MSCIByETF_CSV <- function(theFolderWithAllMSCIfilesStr, ETFtickerStr, columnWithDate=1, columnWithPrice=2){
  allfiles <- list.files(theFolderWithAllMSCIfilesStr)
  tempXTS <- loadPriceHistory_CSV(thePath=paste(theFolderWithAllMSCIfilesStr, allfiles[str_detect(allfiles,pattern=ETFtickerStr)],sep=""),columnWithDate=columnWithDate,columnWithPrice=columnWithPrice)  
  names( tempXTS ) <- ETFtickerStr
  return( tempXTS )
}

loadPriceHistory_fileMappedBySymbol_CSV <- function(theFolderWithAllFilesStr, symbolStr, columnWithDate=1, columnWithPrice=2){
  allfiles <- list.files(theFolderWithAllFilesStr)
  tempXTS <- loadPriceHistory_CSV(thePath=paste(theFolderWithAllFilesStr, allfiles[str_detect(allfiles,pattern=symbolStr)],sep=""),columnWithDate=columnWithDate,columnWithPrice=columnWithPrice)  
  names( tempXTS ) <- symbolStr
  return( tempXTS )
}

createSyntheticIndex <- function(currentTS, historicalTS, newNameStr=NULL){
  # the use case for this is to stitch together a (shorter) ETF timeseries ('currentTS') with a (longer) total return timeseries of the index which it tracks ('historicalTS')
  # But it can also be used to stitch together any 2 timeseries making the resulting timeseries reflect the daily returns of the 2 input timeseries
  
  historicalTSret <- dailyReturn(historicalTS)
  stitchPointValue <- first(currentTS)
  stitchPointDate <- index(currentTS)[1]
  print(stitchPointDate)
  historicalEndIndex <- base::match(stitchPointDate, index(historicalTS))
  print(historicalEndIndex)
  recomputedHistoricalTS <- backCalcIndexFromPctRet(returnsTS=dailyReturn(historicalTS[1:historicalEndIndex,]), lastValue=stitchPointValue)
  stitchedTS <- rbind( recomputedHistoricalTS, currentTS[2:length(currentTS),] )
  if( is.null(newNameStr) ){
    names(stitchedTS) <- paste(names(currentTS)[1],"_syn",sep="")  
  } else {
    names(stitchedTS) <- newNameStr
  }
  
  return(stitchedTS)
}

plotHeatmap <- function( meltedDataDF, xAxisCategoryStr, yAxisCategoryStr, valuesToHeatmapStr
                         , xAxisLabel="", yAxisLabel=""
                         , xAxisFontSize=11, yAxisFontSize=11
                         , xTickLabelsFontSize=11, yTickLabelsFontSize=11
                         , xAxisTickBreaksVec=NULL, xAxisTickLabelsVec=NULL
                         , yAxisTickBreaksVec=NULL, yAxisTickLabelsVec=NULL
                         , highColor="darkgreen", lowColor="darkred"
                         , heatmapTileWidth=1.0, heatmapTileHeight=1.0
                         , heatmapTileFontSize=4 
                         , chartTitle="", chartTitleFontSize=14 ) {
  # USAGE HELP: 'xAxisCategoryStr', 'yAxisCategoryStr', 'valuesToHeatmapStr' should all be strings corresponding to columns in 'meltedDataDF'
  # other interesting heatmapping color options:  
  #       highColor="steelblue"  lowColor="white"
  #       highColor="yellow"  lowColor="purple"
  theHeatmap <- ggplot(meltedDataDF, aes_string(xAxisCategoryStr, yAxisCategoryStr)) + geom_tile(aes_string(fill=valuesToHeatmapStr,width=heatmapTileWidth,height=heatmapTileHeight),colour = "white") + scale_fill_gradient2(low = lowColor, high = highColor, mid="lightgray") + geom_text( aes_string(label=valuesToHeatmapStr), vjust=0.0, size=heatmapTileFontSize) + theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
    theme(panel.grid.major=element_line(colour="grey90")) + 
    theme(axis.line=element_line(colour="grey70")) + 
    theme(axis.title.x=element_text(size=xAxisFontSize)) + 
    theme(axis.title.y=element_text(size=yAxisFontSize)) + 
    theme(axis.text.x=element_text(size=xTickLabelsFontSize)) +
    theme(axis.text.y=element_text(size=yTickLabelsFontSize)) +
    ggtitle( chartTitle ) +
    theme(plot.title=element_text(size=chartTitleFontSize)) +
    theme(legend.background=element_rect(colour="grey80")) + 
    theme(legend.key=element_rect(fill="grey99")) +
#    theme(legend.position="none") + xlab(xAxisLabel) + ylab(yAxisLabel) + scale_x_discrete(breaks=seq(1:12),labels=month.abb[seq(1:12)]) + scale_y_continuous(breaks=as.integer(tempMonthlyRet_withYearMon_best$year))
    theme(legend.position="none") + xlab(xAxisLabel) + ylab(yAxisLabel)
    # + scale_x_discrete(breaks=seq(1:12),labels=month.abb[seq(1:12)]) + scale_y_continuous(breaks=as.integer(tempMonthlyRet_withYearMon_best$year))

  if( is.null(xAxisTickBreaksVec) ){
    tempXAxisTickBreaksVec <- sort(unique.default( meltedDataDF[,xAxisCategoryStr] ))
    if( is.null(yAxisTickLabelsVec) ){
      theHeatmap <- theHeatmap + scale_x_discrete(breaks=tempXAxisTickBreaksVec,labels=as.character(tempXAxisTickBreaksVec))  
    } else {
      theHeatmap <- theHeatmap + scale_x_discrete(breaks=tempXAxisTickBreaksVec,labels=xAxisTickBreaksVec)
    }
  }
  
  if( is.null(yAxisTickBreaksVec) ){
    tempYAxisTickBreaksVec <- sort(unique.default( meltedDataDF[,yAxisCategoryStr] ))
    if( is.null(yAxisTickLabelsVec) ){
      theHeatmap <- theHeatmap + scale_y_continuous(breaks=tempYAxisTickBreaksVec,labels=as.character(tempYAxisTickBreaksVec))  
    } else {
      theHeatmap <- theHeatmap + scale_y_continuous(breaks=tempYAxisTickBreaksVec,labels=yAxisTickLabelsVec)
    }
  }
  
  return( theHeatmap )
}


sortListByElementProperty <- function( theList, decreasing=FALSE, applyFunction, ... ){
  # this returns a vector of the sorted values with names corresponding to the index #'s into 'theList' 
  # related to how the list would be order if it was actually sorted.
  # this is faster than actually returning a sorted version of 'theList' in cases where the elements of 'theList' are large
  
  theListNamed <- theList
  names(theListNamed) <- as.character(seq(1:length(theListNamed)))
  
  valuesToSort <- lapply( theListNamed, applyFunction, ... )
  
  return( sort(unlist(valuesToSort),decreasing=decreasing) ) 
}

dfColumnsToList <- function( DF ) {
  return( lapply(DF,"[") )
}

sort.data.frame <- function(x, decreasing=FALSE, by=1, ... ){
  # Example
  #  sort(iris, by="Sepal.Length")
  #  sort(iris, by=c("Species","Sepal.Length"))
  #  sort(iris, by=1:2)
  #  sort(iris, by="Sepal.Length",decreasing=TRUE)
  
  f <- function(...) order(...,decreasing=decreasing)
  i <- do.call(f,x[by])
  x[i,,drop=FALSE]
}

"%w/o%" <- function(x, y){
  # x without y
  return( x[!x %in% y] )
} 

varNameStr <- function(v1) {
  deparse(substitute(v1))
}

depthList <- function(this) ifelse(is.list(this), 1L + max(sapply(this, depthList)), 0L)

getKeys_ListOfList <- function(listOfLists) {
  return( unlist(lapply( listOfLists, "[[", 1)) )
}

getValues_ListOfList <- function(listOfLists) {
  return( unlist(lapply( listOfLists, "[[", 2)) )
}



containsVec <- function(vec, toMatch) { return( toMatch %in% vec ) }
isEqualTo<-function(matchA, matchB ){ return( matchA == matchB ) }
isGreaterThanZero<-function( value ) { return( value > 0) }

setNamesOfList<-function(theList, namesArray) {
  for( i in 1:length(theList) ) {
      names(theList[[i]]) <- namesArray[i]
  }
  return(theList)
}

getKeys_ListOfList <- function(listOfLists) {
  return( unlist(lapply( listOfLists, "[[", 1)) )
}
getValues_ListOfList <- function(listOfLists) {
  return( unlist(lapply( listOfLists, "[[", 2)) )
}




sortDF<-function(DF, column=NULL, sortAscending=TRUE){
  # column can be either the column number or column name as a string
  # Also, make sure what you're passing in as a data.frame. If its a timeseries, like an xts or zoo object, you'll need to cast using as.data.frame()
  
  if(is.null(column)){
    print("You must pass in either the column number or column name as a string that you want to sort by.")
    return()
  }
  # from stackoverflow, using drop=FALSE keeps row names
  # d[order(-d$data), , drop = FALSE]
  if( sortAscending==TRUE ){
    return( DF[ order(DF[,column]), , drop=FALSE] )  
  } else {
    return( DF[ order(-DF[,column]), , drop=FALSE] )  
  }

# EXAMPLE OF DOING A multi-sort
# Here is another way as long as the pairings in a and b match perfectly:
# b[order(b$id,b$lob), ][ order(order(a$id,a$lob)), ]
# The first use of order sorts the b data frame by id and lob colmuns, then second set (2 orders) says reorder the rows of b, in the way that would unsort a back to its original order after being sorted.
  
}

getTopXinDFbyColumn<-function(DF, column=NULL, topX=10){
  if(is.null(column)){
    print("You must pass in either the column number or column name as a string that you want to sort by.")
    return()
  }
  sortedDF<-sortDF(DF, column,sortAscending=FALSE)[1:topX,]
  return(sortedDF)
}

getBottomXinDFbyColumn<-function(DF, column=NULL, bottomX=10){
  if(is.null(column)){
    print("You must pass in either the column number or column name as a string that you want to sort by.")
    return()
  }
  sortedDF<-sortDF(DF, column,sortAscending=TRUE)[1:bottomX,]
  return(sortedDF)
}

getLastRowOfDF<-function(DF){
  return(DF[nrow(DF),])
}

plotTimeseries<-function( timeseriesToPlotListXTS, chartTitle=NULL, chartTitleSize=16, 
                          showLegend=TRUE, legendPosition="bottom", 
                          xAxisLabel=NULL, yAxisLabel=NULL, legendLabel="", showPoints=FALSE, normalize=FALSE, 
                          normalizeDate=NULL, datesToHighlightDF=NULL ) {
  # if passing in datesToHighlightDF, column1 should be named "start" and column2 named "end"
  # EXAMPLE:  
  #   startEndDF=data.frame("start"=c("2007-01-03","2009-1-1"),end=c("2007-02-14","2009-12-12"), stringsAsFactors=FALSE)
  
  timeseriesListXTS<-timeseriesToPlotListXTS
  if( normalize ) {
    yAxisLabelStr<-paste(yAxisLabel,"(Normalized)")
  } else {
    yAxisLabelStr<-yAxisLabel
  }
  # first determine whether we need to plat charts normalized to a certain date. 
  # ie: plot timeseries that all start at 100 on the same start date
  if( normalize ) {
    if( is.null(normalizeDate) ) {
      # if no normalizeDate is passed, then just start each new timeseries at 100 on earliest date available in each timeseries
      for( i in 1:length(timeseriesToPlotListXTS) ) {
        timeseriesListXTS[[i]]<-cumprod( 1 + dailyReturn(timeseriesToPlotListXTS[[i]]) ) - 1
        colnames(timeseriesListXTS[[i]])<-colnames(timeseriesToPlotListXTS[[i]])
      }
    } else {
      
    }
  } else {
    timeseriesListXTS<-timeseriesToPlotListXTS
  }
  
  allTimeseriesDF<-data.frame()
  for( i in 1:length(timeseriesListXTS) ) {
    tempXTS<-timeseriesListXTS[[i]]
    tempDF<-as.data.frame(tempXTS)
    tempDF$Date<-as.Date(rownames(tempDF))
#     if( is.null(colnames(tempXTS)) ) {
#       colnames(tempDF[,1])<-paste("Timeseries", i, sep="_")
#     }
    if( is.null(names(tempXTS)) ) {
      names(tempDF[,1])<-paste("Timeseries", i, sep="_")
    }
    tempMelt <- melt(tempDF, id="Date")
    if( i == 1 ) { 
      allTimeseriesDF <- tempMelt
    } else {
      allTimeseriesDF <- rbind(allTimeseriesDF,tempMelt)  
    }
  }
  localEnv <- environment()
  #below uses pallette from library(RColorBrewer)
  ggPlotFormula<-ggplot(data=allTimeseriesDF, 
    aes(x=Date,y=value,colour=variable,group=variable)) + 
    geom_line() + ggtitle(chartTitle) +
    xlab(xAxisLabel) + ylab(yAxisLabelStr) + 
    theme(legend.position=legendPosition) +
    theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
    theme(panel.grid.major=element_line(colour="grey90")) + 
    theme(panel.grid.minor=element_line(colour="grey90")) + 
    theme(axis.line=element_line(colour="grey70")) + 
    theme(axis.title.x=element_text(size=14)) +
    theme(axis.title.y=element_text(size=14)) +  
    theme(axis.text.x=element_text(size=11)) +
    theme(axis.text.y=element_text(size=11)) + 
#    scale_color_brewer(palette = "Dark2") + 
#    scale_color_brewer(palette = "Set1") +
    theme(plot.title=element_text(size=chartTitleSize)) 

  if( length( timeseriesToPlotListXTS ) > 1 & length( timeseriesToPlotListXTS ) < 10 ){
    ggPlotFormula<-ggPlotFormula + 
      scale_color_brewer(palette = "Set1")
  }
  
  if( !is.null(datesToHighlightDF) ){
    ggPlotFormula<-ggPlotFormula +
      geom_rect(data=datesToHighlightDF, aes(xmin=as.Date(start), xmax=as.Date(end), ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.30, inherit.aes=FALSE, show_guide=FALSE)
  }
  
  if( showPoints ) {
    ggPlotFormula<-ggPlotFormula + 
      geom_point(aes(size=4))
  }
  
  if( showLegend ) {
    ggPlotFormula<-ggPlotFormula +
      labs(colour=legendLabel) + 
      theme(legend.background=element_rect(colour="grey80")) + 
      theme(legend.key=element_rect(fill="grey99")) 
  } else {
    ggPlotFormula<-ggPlotFormula + 
      theme(legend.position="none")
  }
  
  return(ggPlotFormula)
}

plotSingleTimeseries<-function(timeseriesXTS, startDate=NULL, endDate=NULL, 
                               xAxisLabel="Dates", yAxisLabel="Values"){
  tempDF<-as.data.frame(timeseriesXTS)
  
  startIndex<-NULL
  endIndex<-NULL
  
# to plot by specifying startdate and enddate
  if( is.null(startDate) ) {
    startIndex<-1  
  } else {
    startIndex<-which(index(timeseriesXTS)==startDate)
    if( is.na(startIndex[1]) ) {
      print(paste("the startDate you specified,", startDate, ", is not in the timeseries"))
      return()
    }
  }
  if( is.null(endDate) ) {
    endIndex<-length(timeseriesXTS)
  } else {
    endIndex<-which(index(timeseriesXTS)==endDate)
    if( is.na(endIndex[1]) ) {
      print(paste("the endDate you specified,", endDate, " is not in the timeseries"))
      return()
    }
  }
  
  rowsToPlot<-seq(startIndex,endIndex)
  qplot(index(timeseriesXTS)[rowsToPlot], tempDF[,1][rowsToPlot], geom="line", xlab=xAxisLabel, ylab=yAxisLabel)
  
# basic line plot with no dates
#   qplot(index(tempDF),tempDF[,4], geom="line")
# with dates (automatically chooses the "scale" which to show dates --ie: if timeseries is long, will only show yearly dates on x-axis)
#   qplot(index(AAPL),AAPLdf[,4], geom="line")
# another way to choose how much to show in chart
#   qplot(index(AAPL)[1:5],AAPLdf[,4][1:5], geom="line")
  
}

plotHistogram<-function( values, barColor="Blues", barColorDarkness=3, 
                         chartTitle=NULL, chartTitleSize=14, 
                         showMeanLine=FALSE, showStDevLines=FALSE, 
                         xAxisLabel=NULL, yAxisLabel="Count", 
                         showHorizontalBars=FALSE,
                         useDefaultName=FALSE, showZeroLine=FALSE) {
  #below is a pallette from library(RColorBrewer)
  mypalette<-brewer.pal(7, barColor)
  
  DF<-as.data.frame(values)
  colnames(DF)<-"x"
  localEnv <- environment()
  xAxisLabelStr <- NULL
  yAxisLabelStr <- yAxisLabel
  
  if( useDefaultName ){
    xAxisLabelStr <-  names(values)
  } else {
    if( !is.null(xAxisLabel) ) {
      xAxisLabelStr <- xAxisLabel
    }  
  }
  
  
  ggPlotFormula<-ggplot(data=DF, aes(x=x)) + 
    scale_color_brewer(type="seq") +
    geom_histogram(fill=paste(mypalette[barColorDarkness]),colour="grey30") + ggtitle(chartTitle) +
    xlab(xAxisLabel) + ylab(yAxisLabelStr) + 
    theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
    theme(panel.grid.major=element_line(colour="grey90")) + 
    theme(panel.grid.minor=element_line(colour="grey90")) + 
    theme(axis.line=element_line(colour="grey70")) + 
    theme(axis.title.x=element_text(size=14)) +
    theme(axis.title.y=element_text(size=14)) +  
    theme(axis.text.x=element_text(size=11)) +
    theme(axis.text.y=element_text(size=11))  + 
    theme(plot.title=element_text(size=chartTitleSize))
  
  if( showZeroLine ){
    ggPlotFormula<-ggPlotFormula + geom_vline(xintercept = 0.0, colour="black", size=1.5 )
  }
  
  if( showHorizontalBars ){
    ggPlotFormula<-ggPlotFormula + coord_flip()
  }
  if( showMeanLine ){
    tempMean=mean(values)
    ggPlotFormula<-ggPlotFormula +
      geom_vline(xintercept = tempMean, colour=paste(mypalette[5]), linetype = "dashed", size=1.5)
      if( showHorizontalBars ){
        ggPlotFormula<-ggPlotFormula + 
          annotate(geom="text", x=tempMean, y=0.0, label="avg", vjust=1.5, hjust=1.05, size=3.5, color="grey40" )        
      } else {
        ggPlotFormula<-ggPlotFormula + 
          annotate(geom="text", x=tempMean, y=0.0, label="avg", vjust=1.5, hjust=-0.1, size=3.5, color="grey40" )        
      } 
  }
  if( showStDevLines ){
    tempMean<-mean(values)
    if( class(values)[1] == "xts" || class(values)[1] == "zoo" ){
      tempStdDev<-apply(values,2,sd)  
    } else {
      tempStdDev<-sd(values)
    }
    if( showHorizontalBars ){
      ggPlotFormula<-ggPlotFormula + 
        geom_vline(xintercept = c(tempMean-tempStdDev,tempMean+tempStdDev,tempMean-2*tempStdDev,tempMean+2*tempStdDev), colour=paste(mypalette[6]), linetype = "dashed", size=0.5 ) +
        annotate(geom="text", x=tempMean-tempStdDev, y=0.0, label="-1sd", vjust=1.5, hjust=1.2, size=4, color="grey40" ) +
        annotate(geom="text", x=tempMean+tempStdDev, y=0.0, label="+1sd", vjust=1.5, hjust=1.2, size=4, color="grey40" ) +
        annotate(geom="text", x=tempMean-2*tempStdDev, y=0.0, label="-2sd", vjust=1.5, hjust=1.2, size=4, color="grey40" ) +
        annotate(geom="text", x=tempMean+2*tempStdDev, y=0.0, label="+2sd", vjust=1.5, hjust=1.2, size=4, color="grey40" )
    } else {
      ggPlotFormula<-ggPlotFormula +
        geom_vline(xintercept = c(tempMean-tempStdDev,tempMean+tempStdDev,tempMean-2*tempStdDev,tempMean+2*tempStdDev), colour=paste(mypalette[6]), linetype = "dashed", size=0.5 ) +
        annotate(geom="text", x=tempMean-tempStdDev, y=0.0, label="-1sd", vjust=1.5, hjust=-0.1, size=4, color="grey40" ) +
        annotate(geom="text", x=tempMean+tempStdDev, y=0.0, label="+1sd", vjust=1.5, hjust=-0.1, size=4, color="grey40" ) +
        annotate(geom="text", x=tempMean-2*tempStdDev, y=0.0, label="-2sd", vjust=1.5, hjust=-0.1, size=4, color="grey40" ) +
        annotate(geom="text", x=tempMean+2*tempStdDev, y=0.0, label="+2sd", vjust=1.5, hjust=-0.1, size=4, color="grey40" )
    }
  }
  return(ggPlotFormula)
}

# This counts the # of occurences of each category contained in 'categoryVector' and the height of
# each bar is the # of occurences for that category
plotBarChart_singleBin<-function( categoryVector, barColor="Blues", barColorDarkness=4, 
                                  chartTitle=NULL, chartTitleSize=18, xAxisLabel=NULL, yAxisLabel="Values", 
                                  showMeanLine=FALSE, showStDevLines=FALSE, showHorizontalBars=FALSE, sortAsc=FALSE, sortDesc=FALSE ) {
  freqCount <- count( categoryVector )
  return( plotBarChart(values=freqCount$freq,categoryNames=freqCount$x,
                       barColor, barColorDarkness, 
                       chartTitle, chartTitleSize, xAxisLabel, yAxisLabel, 
                       showMeanLine, showStDevLines, showHorizontalBars, 
                       sortAsc, sortDesc) 
          )
}

# This counts the # of occurences of each category contained in the 'dataDF' column with name 'xAxisBinsColumnStr' 
# Then its split into separate bars by the categories in 'dataDF' with column name 'splitBinsColumnStr'
# The height of each bar is the # of occurences for that category
plotBarChart_multiBins<-function( dataDF, xAxisBinsColumnStr, splitBinsColumnStr, stackedBars=FALSE,
                                  chartTitle=NULL, chartTitleSize=18, xAxisLabel=NULL, yAxisLabel="Count", 
                                  showHorizontalBars=FALSE, legendLabel=NULL ) {
  
  xAxisLabelStr<-xAxisLabel
  yAxisLabelStr<-yAxisLabel
  if( is.null(legendLabel) ){
    legendLabelStr <- splitBinsColumnStr
  } else {
    legendLabelStr <- legendLabel  
  }
  
  DF <- data.frame( xAxisBinsColumn=dataDF[,xAxisBinsColumnStr], splitBinsColumn=dataDF[,splitBinsColumnStr] )
  
  ggPlotFormula <- ggplot( data=DF, aes(x=xAxisBinsColumn, stat="bin", fill=splitBinsColumn) ) 
  
  if( stackedBars ){
    ggPlotFormula <- ggPlotFormula + geom_bar(position="stack")
  } else {
    ggPlotFormula <- ggPlotFormula + geom_bar(position="dodge")
  }
  
  ggPlotFormula <- ggPlotFormula + ggtitle(chartTitle) +
    xlab(xAxisLabelStr) + ylab(yAxisLabelStr) + labs(fill=legendLabelStr) +
    theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
    theme(panel.grid.major=element_line(colour="grey90")) + 
    theme(panel.grid.minor=element_line(colour="grey90")) + 
    theme(axis.line=element_line(colour="grey70")) + 
    theme(axis.title.x=element_text(size=14)) +
    theme(axis.title.y=element_text(size=14)) +  
    theme(axis.text.x=element_text(size=11)) +
    theme(axis.text.y=element_text(size=11))  + 
    theme(plot.title=element_text(size=chartTitleSize))
  
  if( showHorizontalBars ){
    ggPlotFormula<-ggPlotFormula + coord_flip()
  }
  
  return( ggPlotFormula ) 
}

# This builds a 'naive' bar chart where the height of the each bar is passed in 'values'
# and the name of each bar is passed in 'categoryNames'
plotBarChart<-function( values, categoryNames, barColor="Blues", barColorDarkness=4, 
                        chartTitle=NULL, chartTitleSize=18, xAxisLabel=NULL, yAxisLabel="Values", 
                        showHorizontalBars=FALSE, sortAsc=FALSE, sortDesc=FALSE, showZeroLine=FALSE ) {
  #below is a pallette from library(RColorBrewer)
  mypalette<-brewer.pal(7, barColor)
  
  DF<-data.frame(xVal=values, categoryNames=categoryNames)
  localEnv <- environment()
  yAxisLabelStr<-yAxisLabel
  if( !sortAsc && !sortDesc ){
    ggPlotFormula<-ggplot(data=DF, aes(x=categoryNames, y=xVal, fill=categoryNames, colour="grey30", show_guide=FALSE ))
  } else {
    if( sortAsc ){
      ggPlotFormula<-ggplot(data=DF, aes(x=reorder(categoryNames,xVal), y=xVal, fill=categoryNames, colour="grey30", show_guide=FALSE )) 
    } else {
      ggPlotFormula<-ggplot(data=DF, aes(x=reorder(categoryNames,-xVal), y=xVal, fill=categoryNames, colour="grey30", show_guide=FALSE ))
    } 
  }
#  ggPlotFormula<-ggplot(data=DF, aes(x=reorder(categoryNames,-xVal), y=xVal, fill=categoryNames, colour="grey30", show_guide=FALSE )) + 
  ggPlotFormula<-ggPlotFormula +
    geom_bar(fill=paste(mypalette[barColorDarkness]),width=0.9,colour="grey30",stat="identity") + ggtitle(chartTitle) +
    xlab(xAxisLabel) + ylab(yAxisLabelStr) + 
    theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
    theme(panel.grid.major=element_line(colour="grey90")) + 
    theme(panel.grid.minor=element_line(colour="grey90")) + 
    theme(axis.line=element_line(colour="grey70")) + 
    theme(axis.title.x=element_text(size=14)) +
    theme(axis.title.y=element_text(size=14)) +  
    theme(axis.text.x=element_text(size=11)) +
    theme(axis.text.y=element_text(size=11))  + 
    theme(plot.title=element_text(size=chartTitleSize))
  
  if( showZeroLine ){
    ggPlotFormula<-ggPlotFormula + geom_hline(yintercept = 0.0, colour="black", size=1.5 )
  }

  if( showHorizontalBars ){
    ggPlotFormula<-ggPlotFormula + coord_flip()
  }
  
  return(ggPlotFormula)
} 

plotScatterplot<-function( xValues, yValues, pointLabelNames=NULL, showPointLabels=FALSE, 
                           barColor="Blues", barColorDarkness=5, pointSize=4, 
                           chartTitle=NULL, chartTitleSize=16,  
                           xAxisLabel=NULL, yAxisLabel=NULL, 
                           showRegressionLine=FALSE, showConfidenceInterval=FALSE, 
                           showRegressionAdjRSQ=FALSE, labelPositionRSQ="upper-left", labelRSQfontSize=6, 
                           showXmeanLine=FALSE, showYmeanLine=FALSE, 
                           showXstDevLines=FALSE, showYstDevLines=FALSE, 
                           labelPositionRSQoverrideX=0, labelPositionRSQoverrideY=0,
                           pointColorValues=NULL,
                           lowShadingColor=NULL,
                           highShadingColor=NULL,
                           legendLabel=""
                           ) {
  # parameter "barColor" can be 'Blues', 'Greens', 'Reds'
  
  # inputs "xValues", "yValues", and "pointLabelNames" should be vectors all of the same length
  if( is.null(pointLabelNames) && showPointLabels==TRUE ){
    print("You specified showPointLabels=TRUE but did not pass in a vector of point label names to the pointLabelNames parameter.")
    return()
  }
    
  #below is a pallette from library(RColorBrewer)
  mypalette<-brewer.pal(8, barColor)
    
  #DF<-as.data.frame(values)
  if( showPointLabels ) {
    DF<-data.frame(x=xValues, y=yValues, pointLabels=pointLabelNames)
    colnames(DF)<- c("x","y","pointLabels")
  } else {
    DF<-data.frame(x=xValues, y=yValues)
    colnames(DF)<- c("x","y")
  }
  
  if( ! is.null(pointColorValues) ){
    DF<-cbind( DF, pointColors=pointColorValues )
  } else {
    DF<-cbind( DF, pointColors=barColorDarkness )
  }
  
  localEnv <- environment()
  
  yAxisLabelStr<-yAxisLabel
  ggPlotFormula <- NULL
  if( is.null(pointColorValues) ) {
    ggPlotFormula <- ggplot(data=DF, aes(x=x, y=y ) )
  } else {
    ggPlotFormula <- ggplot(data=DF, aes(x=x, y=y, color=pointColors))
  }
  ggPlotFormula <- ggPlotFormula + 
    # scale_color_brewer(type="seq") +
   # geom_point(show_guide=FALSE, shape=19, size=pointSize) + 
    #geom_point(show_guide=FALSE, color=paste(mypalette[barColorDarkness]), shape=19, size=pointSize) + 
    # geom_point(color=pointColorValues, show_guide=FALSE, shape=19, size=pointSize) + 
   # scale_color_gradient(colours=rainbow(7))
    ggtitle(chartTitle) +
    xlab(xAxisLabel) + ylab(yAxisLabelStr) + 
    theme(panel.background=element_rect(fill="grey99",linetype=1, colour="grey70",size=1)) + 
    theme(panel.grid.major=element_line(colour="grey90")) + 
    theme(panel.grid.minor=element_line(colour="grey90")) + 
    theme(axis.line=element_line(colour="grey70")) + 
    theme(axis.title.x=element_text(size=14)) +
    theme(axis.title.y=element_text(size=14)) + 
    theme(axis.text.x=element_text(size=11)) +
    theme(axis.text.y=element_text(size=11))  + 
#    geom_text(data=DF, aes(label=pointLabels), vjust=-0.8) +
    theme(plot.title=element_text(size=chartTitleSize)) 
    
  if( is.null(pointColorValues) ) {
    ggPlotFormula <- ggPlotFormula +
      geom_point(show_guide=FALSE, color=paste(mypalette[barColorDarkness]), shape=19, size=pointSize)
    } else {
      ggPlotFormula <- ggPlotFormula +
        geom_point(show_guide=FALSE, shape=19, size=pointSize) +
        labs(colour=legendLabel) + 
        theme(legend.background=element_rect(colour="grey80")) + 
        theme(legend.key=element_rect(fill="grey99")) +
        theme(legend.position="bottom")
        
      if( !is.null(lowShadingColor) & !is.null(highShadingColor) ) {
          ggPlotFormula <- ggPlotFormula + 
            scale_color_gradient(low=lowShadingColor, high=highShadingColor)
        }
    }
        
  if( showPointLabels ){
    ggPlotFormula<-ggPlotFormula +
     geom_text(data=DF, aes(label=pointLabels), vjust=-0.8)
  }
  if( showRegressionLine ){
    if( showConfidenceInterval ){
      ggPlotFormula<-ggPlotFormula +
        geom_smooth(color="black", linetype="dashed", fill="pink", alpha=0.25, method=lm, show_guide=FALSE)
    } else {
      ggPlotFormula<-ggPlotFormula +
        geom_smooth(color="black", linetype="dashed", method=lm, se=FALSE, show_guide=FALSE)
    }
  }
  if( showRegressionAdjRSQ ) {
    tempLM<-lm(DF[,2] ~ DF[,1])
    tempAdjRSQ<-summary(tempLM)$adj.r.squared
    tempAdjRSQ<-round(tempAdjRSQ,2)
    
    if( labelPositionRSQ=="lower-left" ){
      coordStartX<-min(DF[,1])
      coordStartY<-min(DF[,2])
      ggPlotFormula<-ggPlotFormula +
        annotate(geom="text", x=coordStartX, y=coordStartY, label=paste("Adj. R-sq =", tempAdjRSQ), 
                 vjust=0.0+labelPositionRSQoverrideY, 
                 hjust=0.0+labelPositionRSQoverrideX, size=labelRSQfontSize )  
    }
    if( labelPositionRSQ=="lower-right" ){
      coordStartX<-max(DF[,1])
      coordStartY<-min(DF[,2])
      ggPlotFormula<-ggPlotFormula +
        annotate(geom="text", x=coordStartX, y=coordStartY, label=paste("Adj. R-sq =",tempAdjRSQ), 
                 vjust=0.0+labelPositionRSQoverrideY, 
                 hjust=1.0+labelPositionRSQoverrideX, size=labelRSQfontSize )  
    }
    if( labelPositionRSQ=="upper-right" ){
      coordStartX<-max(DF[,1])
      coordStartY<-max(DF[,2])
      ggPlotFormula<-ggPlotFormula +
        annotate(geom="text", x=coordStartX, y=coordStartY, label=paste("Adj. R-sq =",tempAdjRSQ), 
                 vjust=0.0+labelPositionRSQoverrideY, 
                 hjust=1.0+labelPositionRSQoverrideX, size=labelRSQfontSize )  
    }
    if( labelPositionRSQ=="upper-left" ){
      coordStartX<-min(DF[,1])
      coordStartY<-max(DF[,2])
      ggPlotFormula<-ggPlotFormula +
        annotate(geom="text", x=coordStartX, y=coordStartY, label=paste("Adj. R-sq =",tempAdjRSQ), 
                 vjust=0.0+labelPositionRSQoverrideY, 
                 hjust=-0.5+labelPositionRSQoverrideX, size=labelRSQfontSize )  
    }
    
  }
  
  if( showXmeanLine ){
    tempMean=mean(xValues)
    minYcoord<-min(DF[,2])
    ggPlotFormula<-ggPlotFormula +
      geom_vline(xintercept = tempMean, colour=paste(mypalette[5]), linetype = "dashed", size=1.0, alpha=0.3) +
      annotate(geom="text", x=tempMean, y=minYcoord, label="avg(X)", angle=90, vjust=-1.0, hjust=0.2, size=4, color="grey40" )  
  }
  if( showYmeanLine ){
    tempMean=mean(yValues)
    minXcoord<-min(DF[,1])
    ggPlotFormula<-ggPlotFormula +
      geom_hline(yintercept = tempMean, colour=paste(mypalette[5]), linetype = "dashed", size=1.0, alpha=0.3) +
      annotate(geom="text", x=minXcoord, y=tempMean, label="avg(Y)", vjust=-1.0, hjust=0.1, size=4, color="grey40" )  
  }
  
  if( showXstDevLines ){
    tempMean=mean(xValues)
    tempStdDev=sd(xValues)
    ggPlotFormula<-ggPlotFormula +
      geom_vline(xintercept = c(tempMean-tempStdDev,tempMean+tempStdDev,tempMean-2*tempStdDev,tempMean+2*tempStdDev), colour=paste(mypalette[6]), linetype = "dashed", size=0.5, alpha=0.3 )
  }
  if( showYstDevLines ){
#     tempMean=mean(yValues)
    tempStdDev=sd(yValues)
    ggPlotFormula<-ggPlotFormula +
      geom_hline(yintercept = c(tempMean-tempStdDev,tempMean+tempStdDev,tempMean-2*tempStdDev,tempMean+2*tempStdDev), colour=paste(mypalette[6]), linetype = "dashed", size=0.5, alpha=0.3 )
  }
  return(ggPlotFormula)
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plomtlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

combineColumnsOfListOfDataframes<-function(listOfDataframes, columnNum){
  result<-NULL
  for( i in 1:length(listOfDataframes) ) {
    result<-c( result,listOfDataframes[[i]][,columnNum] )
  }
  return(result)
}

makeTable<-function(numRows=NULL, numColumns=NULL, rowNamesVec=NULL, colNamesVec=NULL, cellDefaultValue=NA, asDataframe=TRUE, stringsAsFactors=FALSE, asXTS=FALSE, referenceTimeseriesXTS=NULL) {
  tempResult<-NULL
  if( is.null(numColumns) && is.null(numRows) ) {
    if( is.null(colNamesVec) || is.null(rowNamesVec) ) {
      print("you must either pass in the numbers of rows and columns for your table, or pass in vectors of column names and row names -- in the form of, c(...), to use for creating your table.")
      print("Example 1:  makeTable(numRows=3, numColumns=2")
      print("Example 2:  makeTable(rowNamesVec=c(myRow1, myRow2, myRow3), colNamesVec=c(myCol1, myCol2))")
      return()
    } else {
      if( asDataframe ){
        # return the results as a dataframe
        tempResult<-data.frame(matrix(cellDefaultValue, nrow=length(rowNamesVec), ncol=length(colNamesVec)), stringsAsFactors=stringsAsFactors)
      } else {
        # just return the results as a generic matrix
        tempResult<-matrix(cellDefaultValue, nrow=length(rowNamesVec), ncol=length(colNamesVec))
      }
      colnames(tempResult)<-colNamesVec
      rownames(tempResult)<-rowNamesVec
      return(tempResult)
    }
  } else {
    # passed in values for both numColumns and numRows, so just make a matrix/dataframe with default names for the columns/rows, and all cells filled with NA or the default value
    if( asDataframe ){
      # return the results as a dataframe with generic R default row/column names
      return( data.frame(matrix(cellDefaultValue, nrow=numRows, ncol=numColumns), stringsAsFactors=stringsAsFactors) )
    } else {
      # just return the results as a generic matrix with generic R default row/column names
      return( matrix(cellDefaultValue, nrow=numRows, ncol=numColumns) )
    }
  }
}

makeTableXTS<-function(numColumns=NULL, colNamesVec=NULL, referenceTimeseriesVectorXTS=NULL, cellDefaultValue=NA) {
  tempResult<-NULL
  if( is.null(referenceTimeseriesVectorXTS) ) {
    print("you must either pass in a referenceTimeseriesVectorXTS for the table to grab the dates from. referenceTimeseriesVectorXTS should only be 1 column")
    print( "Example 1:  makeTableXTS(numColumns=2, myExistingXTS)" )
    print( "Example 2:  makeTable(colNamesVec=c(myCol1string, myCol2string), myExistingXTS)" )
    return()
  } else {
    if( is.null(numColumns) && is.null(colNamesVec) ) {
      print("you must either pass in the numbers of columns for your XTS table, or pass in a vector of column names -- in the form of, c(...)")
      print( "Example 1:  makeTableXTS(numColumns=2, myExistingXTS )" )
      print( "Example 2:  makeTable(colNamesVec=c(myCol1string, myCol2string), myExistingXTS)" )
      return()
    } else {
      tempXTS<-referenceTimeseriesVectorXTS
      tempXTS[,1]<-cellDefaultValue
      for( i in 2:numColumns ){
        tempXTS<-cbind(tempXTS,cellDefaultValue)
      }
      if( is.null(colNamesVec) ) {
        # return the results as a dataframe with some generic row/column names
        seqStr<-unlist(lapply(seq(1:numColumns), toString))
        colStr<-rep("col",numColumns)
        colnames(tempXTS)<-paste(colStr,seqStr,sep="")
        return( tempXTS )
      } else {
        colnames(tempXTS)<-colNamesVec
        return(tempXTS)
      } 
    }
  }
}

addColumnDF<-function( DF, inputVec=NULL, newColName, functionName, ... ) {
  # if you don't pass in an inputVector to pass to the function, then just use the index() of the DF. 
  # So if the DF is an XTS then it will be all the dates of the timeseries
  vectorToInputToFunction<-NULL
  if( is.null(inputVec) ){
    vectorToInputToFunction<-index(DF)
  } else {
    vectorToInputToFunction<-inputVector
  }
  tempNewCol<-unlist(lapply( vectorToInputToFunction, functionName, ... ))
  tempTable<-cbind( DF, tempNewCol )
  colnames( tempTable )<-c(colnames(DF), newColName)
  return(tempTable)
}

getValueXTS<-function( TS, dateStr ) {
  return( TS[dateStr] )
} 

sliceXTS<-function(TS, startDate, endDate){
  TSstart<-startDateXTS(TS)
  TSend<-endDateXTS(TS)
  allIndexes<-index(TS)
  tempStartDate<-as.Date(startDate)
  while( length(which( tempStartDate == allIndexes )) == 0 ) {
    tempStartDate<-tempStartDate+1
    if( tempStartDate > TSend ){
      print("Please enter a reasonable startDate. Cannot determine a startdate that is less than the actual enddate of the input Timeseries.")
      return(NULL)
    }
  }
  
  tempEndDate<-as.Date(endDate)
  while( length(which( tempEndDate == allIndexes )) == 0 ) {
    tempEndDate<-tempEndDate-1
    if( tempEndDate < TSstart ){
      print("Please enter a reasonable endDate. Cannot determine an enddate that is greater than the actual startdate of the input Timeseries.")
      return(NULL)
    }
  }
  
  startIndex<-which( tempStartDate == allIndexes )
  endIndex<-which( tempEndDate == allIndexes )
  return( TS[startIndex:endIndex,] )
}

sliceTS<-function(TS, dateVector, newNameForOutput=NULL){
  return( TS[ which( dateVector == index(TS) ) ] )
}

startDateXTS<-function(TS){
  return( index(TS)[1] )
}
endDateXTS<-function(TS){
  return( index(TS)[ dim(TS)[1] ] )
}
removeNAfromXTS<-function(TS){
  if( length(index(TS)[is.na(TS)]) == 0 ){
    return(TS)
  } else {
    return( TS[index(TS)[-is.na(TS)]] )  
  }
}

ADFoneTimeseries<-function(TS) {
  TSnoNA<-removeNAfromXTS(TS)
  TS_adf<-ur.df(TSnoNA,type="drift")
  return(TS_adf)
}

ADFtwoTimeseries<-function(TS1, TS2) {
  TS1minusTS2<-TS1 - TS2
  TS1minusTS2<-removeNAfromXTS(TS1minusTS2)
  TS_adf<-ur.df(TS1minusTS2,type="drift")
  return(TS_adf)
}

stratifiedSamples <- function(df, group, size, seed = NULL, ...) {
  # Returns a stratified random subset of a data.frame.
  #
  # --> df      The source data.frame
  # --> group   Your grouping variable
  # --> size    The desired sample size. If -size- is a decimal, 
  #             a proportionate sample is drawn. If it is >= 1, 
  #             a sample will be taken of that specified size
  # --> seed    The seed that you want to use, if any
  # --> ...     Further arguments to the sample function
  #
  # === EXAMPLES ===
  #
  #   set.seed(1)
  #   dat = data.frame(A = 1:100, 
  #                    B = sample(c("AA", "BB", "CC", "DD", "EE"), 
  #                               100, replace=T),
  #                    C = rnorm(100), D = abs(round(rnorm(100), digits=1)),
  #                    E = sample(c("CA", "NY", "TX"), 100, replace=T))
  #     
  #   stratified(dat, 5, .1, 1)
  #   stratified(dat, group = "E", size = .1, seed = 1)
  #   stratified(dat, "B", 5)
  
  df.interaction <- interaction(df[group])
  df.table <- table(df.interaction)
  df.split <- split(df, df.interaction)
  
  if (length(size) > 1) {
    if (length(size) != length(df.split))
      stop("Number of groups is ", length(df.split),
           " but number of sizes supplied is ", length(size))
    if (is.null(names(size))) {
      n <- setNames(size, names(df.split))
      message(sQuote("size"), " vector entered as:\n\nsize = structure(c(", 
              paste(n, collapse = ", "), "),\n.Names = c(",
              paste(shQuote(names(n)), collapse = ", "), ")) \n\n")
    } else {
      ifelse(all(names(size) %in% names(df.split)), 
             n <- size[names(df.split)], 
             stop("Named vector supplied with names ", 
                  paste(names(size), collapse = ", "),
                  "\n but the names for the group levels are ", 
                  paste(names(df.split), collapse = ", ")))
    } 
  } else if (size < 1) {
    n <- round(df.table * size, digits = 0)
  } else if (size >= 1) {
    if (all(df.table >= size)) {
      n <- setNames(rep(size, length.out = length(df.split)),
                    names(df.split))
    } else {
      message(
        "Some groups\n---", 
        paste(names(df.table[df.table < size]), collapse = ", "),
        "---\ncontain fewer observations",
        " than desired number of samples.\n",
        "All observations have been returned from those groups.")
      n <- c(sapply(df.table[df.table >= size], function(x) x = size),
             df.table[df.table < size])
    }
  }
  
  seedme <- ifelse(is.null(seed), "No", "Yes")
  
  temp <- switch(
    seedme,
    No = { temp <- lapply(
      names(df.split), 
      function(x) df.split[[x]][sample(df.table[x], 
                                       n[x], ...), ]) },
    Yes = { temp <- lapply(
      names(df.split),
      function(x) { set.seed(seed)
                    df.split[[x]][sample(df.table[x], 
                                         n[x], ...), ] }) })
  
  rm(.Random.seed, envir=.GlobalEnv) # "resets" the seed
  do.call("rbind", temp)
}