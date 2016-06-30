#' @title Portfolio Exposures Report
#' 
#' @description Calculate k factor time series based on fundamental factor model. This method takes fundamental factor model fit, "ffm" object, and portfolio weight as inputs and generates numeric summary and plot visualization. 
#' 
#' @importFrom zoo as.yearmon coredata index
#' @importFrom graphics barplot boxplot
#' @importFrom stats sd
#' 
#' @param ffmObj an object of class ffm returned by fitFfm.
#' @param weights a vector of weights of the assets in the portfolio. Default is NULL.
#' @param isPlot logical variable to generate plot or not.
#' @param isPrint logical variable to print numeric summary or not.
#' @param stripLeft logical variable to choose the position of strip, "TRUE" for drawing strips on the left of each panel, "FALSE" for drawing strips on the top of each panel. Used only when isPlot = 'TRUE'
#' @param layout layout is a numeric vector of length 2 or 3 giving the number of columns, rows, and pages (optional) in a multipanel display. Used only when isPlot = 'TRUE'
#' @param scaleType scaleType controls if use a same scale of y-axis, choose from c('same', 'free')
#' @param digits digits of printout numeric summary. Used only when isPrint = 'TRUE'
#' @param ... other graphics parameters available in tsPlotMP can be passed in through the ellipses, see \code{\link[factorAnalytics]{tsPlotMP}}
#' @author Douglas Martin, Lingjie Yi
#' @examples 
#'
#' #Load fundamental and return data 
#' data("stocks145scores6")
#' dat = stocks145scores6
#' dat$DATE = as.yearmon(dat$DATE)
#' dat = dat[dat$DATE >=as.yearmon("2008-01-01") & dat$DATE <= as.yearmon("2012-12-31"),]
#'
#' #Load long-only GMV weights for the return data
#' data("wtsStocks145GmvLo")
#' wtsStocks145GmvLo = round(wtsStocks145GmvLo,5)  
#'                                                      
#' #fit a fundamental factor model
#' require(factorAnalytics) 
#' fit <- fitFfm(data = dat, 
#'               exposure.vars = c("SECTOR","ROE","BP","PM12M1M","SIZE","ANNVOL1M","EP"),
#'               date.var = "DATE", ret.var = "RETURN", asset.var = "TICKER", 
#'               fit.method="WLS", z.score = TRUE)
#' 
#' repExposures(fit, wtsStocks145GmvLo, isPlot = FALSE, digits = 4)
#' repExposures(fit, wtsStocks145GmvLo, isPlot = TRUE,
#'              add.grid = TRUE, scaleType = 'same', layout = c(3,2))
#' repExposures(fit, wtsStocks145GmvLo, isPlot = TRUE,
#'              add.grid = FALSE, zeroLine = TRUE, color = 'Blue')
#' @export


repExposures <- function(ffmObj, weights = NULL, isPlot = TRUE, isPrint = TRUE, scaleType = 'free',
                         stripLeft = TRUE, layout = NULL, digits = 1, ...) {
  
  if (!inherits(ffmObj, "ffm")) {
    stop("Invalid argument: ffmObj should be of class'ffm'.")
  }
  
  which.numeric <- sapply(ffmObj$data[,ffmObj$exposure.vars,drop=FALSE], is.numeric)
  exposures.num <- ffmObj$exposure.vars[which.numeric]
  exposures.char <- ffmObj$exposure.vars[!which.numeric]
  
  # get parameter from the factor model fit
  beta = ffmObj$beta
  n.assets = nrow(beta)
  asset.names <- unique(ffmObj$data[[ffmObj$asset.var]])
  TP = length(ffmObj$time.periods)
  
  # check if there is weight input
  if(is.null(weights)){
    weights = rep(1/n.assets, n.assets)
  }else{
    # check if number of weight parameter matches 
    if(n.assets != length(weights)){
      stop("Invalid argument: incorrect number of weights")
    }
    weights = weights[asset.names]
  }
  
  if(length(exposures.char)){
    dat <- ffmObj$data[ffmObj$data$DATE==ffmObj$time.periods[TP], ]
    B <- as.matrix(table(dat$TICKER,dat$SECTOR))
    B[B>0] <- 1
    B <- B[asset.names,]
  }else{
    B = c()
  }
  
  #calculate x = t(w) * B
  X = c()
  for(i in 1:TP){
    dat <- ffmObj$data[ffmObj$data$DATE==ffmObj$time.periods[i], ]
    beta <- as.matrix(dat[,exposures.num])
    rownames(beta) <- asset.names
    beta = cbind(beta,B)
    
    temp = as.data.frame(weights %*% beta)
    temp = cbind('Date'=ffmObj$time.periods[i],temp)
    X = rbind(X,temp)
  }
  X = as.xts(X[,-1],order.by = X[,1])
  
  if(isPlot){
    
    ###generate plots
    for(i in 1:ncol(X[,exposures.num])){
      name = colnames(X[,exposures.num])[i]
      barplot(100*X[,exposures.num][,i],las=2,col=5,
              names.arg= as.yearmon(index(X)),
              cex.names=0.5, 
              ylab = "Percentage (%)",
              main=paste(name, "Exposure"))
    } 
    
    boxplot(100*coredata(X[,exposures.num]), col=5,
            cex.names=0.5, nnotch = T,
            ylab = "Percentage (%)",
            main=paste("Distributions of Exposures"))
    
    tsPlotMP(X[,exposures.num], main = "Factor Exposures", stripLeft = stripLeft, layout = layout, scaleType = scaleType, ...)
  }
  
  if(isPrint){
    # tabular report 
    avg = apply(X, 2, mean) * 100
    vol = apply(X, 2, sd) * 100
    stats.sum = cbind(avg, vol)
    colnames(stats.sum) = c('Mean','Volatility')
    
    ret = round(stats.sum, digits)
    
    return(ret)   
  }
}

