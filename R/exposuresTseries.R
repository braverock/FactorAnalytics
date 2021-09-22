#' @title Time series plots of Style Exposures
#' 
#' @description Plot the time series plot of returns and style exposures of the data.
#' 
#' @importFrom lattice xyplot
#' @import xts  
#' 
#' 
#' @param data Dataframe with time series of style exposures and returns 
#' @param tickers character or vector of characters of Ticker symbols
#' @param which.exposures  exposures to plot
#' @param plot.returns Logical. If 'TRUE', returns are also plotted along with the exposures. 
#' @param axis.cex numeric value to control the size of axis labels
#' @param plot.type character defining the type of plot. Default is 'b'
#' @param ... other optional arguments 
#'
#' @return Time series plots
#' 
#' @author Avinash Acharya
#' 
#' @examples
#' 
#' data("factorDataSetDjia5Yrs")
#'
#'#One ticker with returns plot and one exposure
#'tickers = "BAC"
#'exposuresTseries(factorDataSetDjia5Yrs,tickers = tickers,which.exposures = "MKTCAP")
#'
#'#Multiple Tickers without returns and all exposures.  
#'tickers = c("AA", "BAC", "IBM")
#'exposuresTseries(factorDataSetDjia5Yrs,tickers = tickers,plot.returns = FALSE)
#'
#'#Multiple tickers With returns
#'exposuresTseries(factorDataSetDjia5Yrs,tickers = tickers,plot.returns = TRUE, axis.cex = 0.8,
#'plot.type="b")
#'
#' @export 
#' 
exposuresTseries<- function(data, tickers=NULL, which.exposures = c("SIZE","P2B","ENTVAL"), 
                           plot.returns= TRUE, axis.cex=1,plot.type = "b",...)
  {
    tickers.data = unique(data$TICKER)
    if(!(is.null(tickers)))
    {
      #If any input ticker is not in data, throw error
      if(!all(tickers %in% tickers.data))  
      stop("Some or all of the tickers are not present in data")
      
    }else
    { #if tickers is NULL, plot results  only for the first ticker
        tickers = tickers.data[1]
    }
    #Extract time series of Style Exposures
    exposures = data[,c("DATE","TICKER","NAME","RETURN",which.exposures)]
    #Filter only the required Tickers
    exposures = exposures[exposures$TICKER %in% tickers,]
    #Rearrange to get a time series w.r.t each Ticker
    exposures = exposures[order(exposures[,"TICKER"]),]
    for(i in 1:length(tickers)){
      data.plot = exposures[exposures$TICKER == tickers[i],]
      #check if plot.returns is TRUE. Else don't plot returns. Truncate the data accordingly
      if(plot.returns){z=3}else{z=4}
      data.xts = xts(data.plot[,-c(1:z)], order.by = data.plot$DATE)
      p= xyplot(data.xts, main = paste("Time series of exposures for", unique(data.plot$NAME)),type=plot.type,
             scales = list(y = list(cex = axis.cex,relation="free", rot = 0),
                           x = list(cex = axis.cex)),par.strip.text = list(cex = 1.2,col="black"))
      print(p)
    }
      
}
