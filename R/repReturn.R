#' @title Portfolio return decomposition report
#' 
#' @description Decompostite return of portfolio into return of different factors based on fundamental factor model. This method takes fundamental factor model fit, "ffm" object, and portfolio weight as inputs and generates numeric summary and plot visualization. 
#' 
#' @importFrom zoo as.yearmon coredata index
#' @importFrom xts as.xts
#' @importFrom graphics boxplot par axis text 
#' @importFrom stats sd
#' 
#' @param ffmObj an object of class ffm returned by fitFfm.
#' @param weights a vector of weights of the assets in the portfolio. Default is NULL.
#' @param isPlot logical variable to generate plot or not.
#' @param isPrint logical variable to print numeric summary or not.
#' @param stripLeft logical variable to choose the position of strip, "TRUE" for drawing strips on the left of each panel, "FALSE" for drawing strips on the top of each panel. Used only when isPlot = 'TRUE'
#' @param layout layout is a numeric vector of length 2 or 3 giving the number of columns, rows, and pages (optional) in a multipanel display.
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
#' repReturn(fit, wtsStocks145GmvLo, isPlot = FALSE, digits = 4)
#' repReturn(fit, wtsStocks145GmvLo, isPlot = TRUE, add.grid = TRUE, scaleType = 'same')
#' repReturn(fit, wtsStocks145GmvLo, isPlot = TRUE, add.grid = FALSE, 
#'           zeroLine = TRUE, color = 'Blue')              
#' @export


repReturn <- function(ffmObj, weights = NULL, isPlot = TRUE, isPrint = TRUE, layout =NULL, scaleType = 'free',
                      stripLeft = TRUE, digits = 1, ...) {
  
  if (!inherits(ffmObj, "ffm")) {
    stop("Invalid argument: ffmObjshould be of class'ffm'.")
  }
  
  which.numeric <- sapply(ffmObj$data[,ffmObj$exposure.vars,drop=FALSE], is.numeric)
  exposures.num <- ffmObj$exposure.vars[which.numeric]
  exposures.char <- ffmObj$exposure.vars[!which.numeric]
  exposures.char.name <- as.vector(unique(ffmObj$data[,exposures.char]))
  
  # get factor model returns from 
  facRet = ffmObj$factor.returns
  
  if(!length(exposures.char)){
    alpha = facRet[,1]
    colnames(alpha) = 'Alpha'
    facRet = facRet[,-1]
  }else{
    alpha = c()
  }
  sig = ffmObj$residuals
  # get parameters from the factor model fit  
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
  
  
  #portfolio residuals
  sig.p <- sig * weights
  sig.p <- as.xts(rowSums(coredata(sig.p)), order.by = index(sig.p))
  colnames(sig.p) = 'Residuals'
  
  
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
  
  rk = as.xts(coredata(X) * coredata(facRet), order.by = index(sig.p)) 
  facRet.p = as.xts(rowSums(coredata(rk)), order.by = index(sig.p))
  colnames(facRet.p) = 'facRet'
  
  if(!length(exposures.char)){
    ret.p = alpha + facRet.p + sig.p
  }else{
    ret.p =facRet.p + sig.p
  }  
  
  colnames(ret.p) = 'Return'
  
  dat = merge(ret.p, sig.p, alpha, facRet.p, rk)
  
  if(isPlot){
    par(mar=c(7,5,5,5))
    boxplot(100*coredata(dat), col=5, las = 2, 
            xaxt = "n", 
            ylab = "Percentage (%)",
            main=paste("Portfolio Returns Components Distributions"))
    axis(1, at=c(1:ncol(dat)) , labels = FALSE)
    text(x = c(1:ncol(dat)), srt = 45, adj = 1, labels = colnames(dat), 
         par("usr")[3] - 3, xpd = TRUE, cex = 0.8)
    
    tsPlotMP(dat[,c('Return','Alpha','facRet','Residuals')], yname = NULL, main = "Portfolio Returns Decomposition", layout = c(3,3), stripLeft = stripLeft, scaleType = scaleType, ...)
    tsPlotMP(dat[,c('facRet',exposures.num,'Residuals')], yname = NULL, main = "Portfolio Style Factors Returns", layout = c(3,3), stripLeft = stripLeft, scaleType = scaleType, ...)
    tsPlotMP(dat[,c(exposures.char.name)], yname = NULL, main = "Portfolio Sector Returns", layout = c(3,4), stripLeft = stripLeft, scaleType = scaleType, ...)
    
  }
  
  if(isPrint){
    # tabular report 
    avg = apply(dat, 2, mean) * 100
    vol = apply(dat, 2, sd) * 100 
    stats.sum = cbind(avg, vol)
    colnames(stats.sum) = c('Mean','Volatility')
    
    ret = round(stats.sum, digits)
    return(ret)
  }
  
}
