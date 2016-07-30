#' @title Portfolio Exposures Report
#' 
#' @description Calculate k factor time series based on fundamental factor model. This method takes fundamental factor model fit, "ffm" object, and portfolio weight as inputs and generates numeric summary and plot visualization. 
#' 
#' @importFrom zoo as.yearmon coredata index
#' @importFrom graphics barplot boxplot 
#' @importFrom stats sd
#' @importFrom utils menu
#' 
#' @param ffmObj an object of class ffm returned by fitFfm.
#' @param weights a vector of weights of the assets in the portfolio. Default is NULL.
#' @param isPlot logical variable to generate plot or not.
#' @param isPrint logical variable to print numeric summary or not.
#' @param stripLeft logical variable to choose the position of strip, "TRUE" for drawing strips on the left of each panel, "FALSE" for drawing strips on the top of each panel. Used only when isPlot = 'TRUE'
#' @param layout layout is a numeric vector of length 2 or 3 giving the number of columns, rows, and pages (optional) in a multipanel display. Used only when isPlot = 'TRUE'
#' @param scaleType scaleType controls if use a same scale of y-axis, choose from c('same', 'free')
#' @param digits digits of printout numeric summary. Used only when isPrint = 'TRUE'
#' @param which a number to indicate the type of plot. If a subset of the plots 
#' is required, specify a subset of the numbers 1:3 for plots. If \code{which=NULL} (default), the following menu 
#' appears: \cr \cr
#' For plots of a group of assets: \cr
#' 1 = Time series plot of style factor exposures, \cr
#' 2 = Boxplot of style factor exposures, \cr
#' 3 = Barplot of factor exposures. \cr \cr
#' @param ... other graphics parameters available in tsPlotMP(time series plot only) can be passed in through the ellipses 
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
#' # fit a fundamental factor model
#' fit.cross <- fitFfm(data = dat, 
#'               exposure.vars = c("SECTOR","ROE","BP","MOM121","SIZE","VOL121",
#'               "EP"),date.var = "DATE", ret.var = "RETURN", asset.var = "TICKER", 
#'               fit.method="WLS", z.score = TRUE)
#'
#' repExposures(fit.cross, wtsStocks145GmvLo, isPlot = FALSE, digits = 4)
#' repExposures(fit.cross, wtsStocks145GmvLo, isPrint = FALSE, isPlot = TRUE, which = 2,
#'              add.grid = TRUE, scaleType = 'same', layout = c(3,3))
#' repExposures(fit.cross, wtsStocks145GmvLo, isPlot = TRUE, which = 1,
#'              add.grid = FALSE, zeroLine = TRUE, color = 'Blue')
#' repExposures(fit.cross, wtsStocks145GmvLo, isPrint = FALSE, isPlot = TRUE, which = 3,
#'              add.grid = FALSE, zeroLine = FALSE, color = 'Blue', layout = c(1,3))
#' @export


repExposures <- function(ffmObj, weights = NULL, isPlot = TRUE, isPrint = TRUE, scaleType = 'free',
                         stripLeft = TRUE, layout = NULL, digits = 1, which = NULL, ...) {
  
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
    if(!is.null(names(weights))){
      weights = weights[asset.names]
    }else{
      stop("Invalid argument: names of weights vector should match with asset names")
    }
  } 
  
  if(length(exposures.char)){
    dat <- ffmObj$data[ffmObj$data[,ffmObj$date.var]==ffmObj$time.periods[TP], ]
    B <- as.matrix(table(dat[,ffmObj$asset.var],dat[,exposures.char]))
    B[B>0] <- 1
    B <- B[asset.names,]
  }else{
    B = c()
  }
  
  #calculate x = t(w) * B
  X = c()
  for(i in 1:TP){
    dat <- ffmObj$data[ffmObj$data[,ffmObj$date.var]==ffmObj$time.periods[i], ]
    beta <- as.matrix(dat[,exposures.num])
    rownames(beta) <- asset.names
    beta = cbind(beta,B)
    
    temp = as.data.frame(weights %*% beta)
    temp = cbind('Date'=ffmObj$time.periods[i],temp)
    X = rbind(X,temp)
  }
  X = as.xts(X[,-1],order.by = X[,1])
  
  if(isPlot){
    
    which.vec <- which
    which <- which[1]
    
    repeat {
      if (is.null(which)) {
        which <- 
          menu(c("Time series plot of style factor exposures",
                 "Boxplot of style factor exposures",
                 "Barplot of factor exposures"), 
               title="\nMake a plot selection (or 0 to exit):") 
      }
      
      switch(which,
             "1L" = { 
               ## Time Series plot of factor exposures
               tsPlotMP(X[,exposures.num], main = "Factor Exposures", stripLeft = stripLeft, layout = layout, 
                        scaleType = scaleType, ...)
             }, 
             "2L" = {
               ## Boxplot of factor exposures
               boxplot(100*coredata(X[,exposures.num]), col=5,
                       notch = T, ylab = "Percentage (%)",
                       main=paste("Distributions of Exposures"))
             }, 
             "3L" = {    
               ## Barplot of factor exposures
               par(mfrow = layout)
               
               a = colMeans(X[,exposures.num])
               b = apply(X[,exposures.num],2,sd)
               c = rbind(a,b)
               sect = as.character(unique(dat[,exposures.char]))
               d = colMeans(X[,sect])
               
               barplot(a,las=2,col=5, cex.axis = 0.8, ylab = "Percent (%)", main="Style Exposures Means")
               barplot(b,las=2,col=5, cex.axis = 0.8, ylab = "Percent (%)", main="Style Exposures Vols")
               barplot(d,las=2,col=5, cex.axis = 0.8, ylab = "Percent (%)", main="Sector Exposure Means")
               
               par(mfrow = c(1,1))
             },
             invisible()       
      )         
      # repeat menu if user didn't choose to exit from the plot options
      if (which==0 || length(which.vec)==1) {break} 
      if (length(which.vec)>1) {
        which.vec <- which.vec[-1]
        which <- which.vec[1]
        par(ask=TRUE)
      } else {which=NULL}   
    }
  
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

