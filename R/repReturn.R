#' @title Portfolio return decomposition report
#' 
#' @description Decompostite return of portfolio into return of different factors based on fundamental factor model. This method takes fundamental factor model fit, "ffm" object, and portfolio weight as inputs and generates numeric summary and plot visualization. 
#' 
#' @importFrom zoo as.yearmon coredata index
#' @importFrom xts as.xts
#' @importFrom graphics boxplot par axis text 
#' @importFrom stats sd
#' @importFrom utils menu
#' 
#' @param ffmObj an object of class ffm returned by fitFfm.
#' @param weights a vector of weights of the assets in the portfolio. Default is NULL.
#' @param isPlot logical variable to generate plot or not.
#' @param isPrint logical variable to print numeric summary or not.
#' @param stripLeft logical variable to choose the position of strip, "TRUE" for drawing strips on the left of each panel, "FALSE" for drawing strips on the top of each panel. Used only when isPlot = 'TRUE'
#' @param layout layout is a numeric vector of length 2 or 3 giving the number of columns, rows, and pages (optional) in a multipanel display.
#' @param scaleType scaleType controls if use a same scale of y-axis, choose from c('same', 'free')
#' @param digits digits of printout numeric summary. Used only when isPrint = 'TRUE'
#' @param titleText logical varible to choose display plot title or not. Default is 'TRUE', and used only when isPlot = 'TRUE'.
#' @param which a number to indicate the type of plot. If a subset of the plots 
#' is required, specify a subset of the numbers 1:4 for plots. If \code{which=NULL} (default), the following menu 
#' appears: \cr \cr
#' For plots of a group of assets: \cr
#' 1 = Time Series plot of portfolio returns decomposition, \cr
#' 2 = Time Series plot of portfolio style factors returns, \cr
#' 3 = Time Series plot of portfolio sector returns, \cr
#' 4 = Boxplot of Portfolio Factor Returns Components. \cr \cr
#' @param ... other graphics parameters available in tsPlotMP(time series plot only) can be passed in through the ellipses 
#' 
#' @return  
#' A K x 2 matrix containing mean and standard deviation of K factors
#' 
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
#' # fit a fundamental factor model
#' fit.cross <- fitFfm(data = dat, 
#'               exposure.vars = c("SECTOR","ROE","BP","MOM121","SIZE","VOL121",
#'               "EP"),date.var = "DATE", ret.var = "RETURN", asset.var = "TICKER", 
#'               fit.method="WLS", z.score = TRUE)
#'
#' repReturn(fit.cross, wtsStocks145GmvLo, isPlot = FALSE, digits = 4)
#' repReturn(fit.cross, wtsStocks145GmvLo, isPrint = FALSE, isPlot = TRUE, which = 4)
#' repReturn(fit.cross, wtsStocks145GmvLo, isPrint = FALSE, isPlot = TRUE, which = 1,
#'           add.grid = TRUE, scaleType = 'same')
#' repReturn(fit.cross, wtsStocks145GmvLo, isPrint = FALSE, isPlot = TRUE, which = 2,
#'           add.grid = FALSE, zeroLine = TRUE, color = 'Blue', scaleType = 'free')              
#' @export


repReturn <- function(ffmObj, weights = NULL, isPlot = TRUE, isPrint = TRUE, layout =NULL, scaleType = 'free',
                      stripLeft = TRUE, digits = 1, titleText = TRUE, which = NULL, ...) {
  
  if (!inherits(ffmObj, "ffm")) {
    stop("Invalid argument: ffmObjshould be of class'ffm'.")
  }
  
  which.numeric <- sapply(ffmObj$data[,ffmObj$exposure.vars,drop=FALSE], is.numeric)
  exposures.num <- ffmObj$exposure.vars[which.numeric]
  exposures.char <- ffmObj$exposure.vars[!which.numeric]
  exposures.char.name <- as.vector(unique(ffmObj$data[,exposures.char]))
  
  # get factor model returns from 
  facRet = ffmObj$factor.returns
  
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
    if(!is.null(names(weights))){
      weights = weights[asset.names]
    }else{
      stop("Invalid argument: names of weights vector should match with asset names")
    }
  } 
  
  
  #portfolio residuals
  sig.p <- sig * weights
  sig.p <- as.xts(rowSums(coredata(sig.p)), order.by = index(sig.p))
  colnames(sig.p) = 'ResidRet'
  
  
  if(length(exposures.char)>0){
    if(length(exposures.char) == 1){
      dat <- ffmObj$data[ffmObj$data[,ffmObj$date.var]==ffmObj$time.periods[TP], ]
      B <- as.matrix(table(dat[,ffmObj$asset.var],dat[,exposures.char]))
      B[B>0] <- 1
      B <- B[asset.names,]
    }else{
      dat <- ffmObj$data[ffmObj$data[,ffmObj$date.var]==ffmObj$time.periods[TP], ]
      B <- as.matrix(table(dat[,ffmObj$asset.var],dat[,exposures.char[1]]))
      B[B>0] <- 1
      B <- B[asset.names,]
      for(i in 2:length(exposures.char)){
        temp <- as.matrix(table(dat[,ffmObj$asset.var],dat[,exposures.char[i]]))
        temp[temp>0] <- 1
        temp <- temp[asset.names,]
        B <- cbind(B,temp)
      }
    }
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
    if(ncol(ffmObj$beta) > ncol(beta)){
      beta = cbind(rep(1,nrow(beta)),beta)
      colnames(beta)[1] = colnames(ffmObj$beta)[1]
    }
    
    temp = as.data.frame(weights %*% beta)
    temp = cbind('Date'=ffmObj$time.periods[i],temp)
    X = rbind(X,temp)
  }
  X = as.xts(X[,-1],order.by = X[,1])
  
  rk = as.xts(coredata(X) * coredata(facRet), order.by = index(sig.p)) 
  facRet.p = as.xts(rowSums(coredata(rk)), order.by = index(sig.p))
  colnames(facRet.p) = 'FacRet'
  
  ret.p =facRet.p + sig.p
  
  colnames(ret.p) = 'PortRet'
  
  dat = merge(ret.p, sig.p, facRet.p, rk)
  
  secRet = c()
  for(i in 1:length(exposures.char)){
    chars = as.character(unique(ffmObj$data[[exposures.char[i]]]))
    temp = rowSums(rk[,chars])
    secRet = cbind(secRet,temp)
  }
  colnames(secRet) = paste(exposures.char,"Ret",sep='')
  secRet = xts(secRet, order.by = zoo::index(dat))
  FacRet = xts(rowSums(rk[,exposures.num]), order.by = zoo::index(dat))
  colnames(FacRet) = 'StyleFacRet'
  
  if(isPlot){
    
    which.vec <- which
    which <- which[1]
    
    repeat {
      if (is.null(which)) {
        which <- 
          menu(c("Time Series plot of portfolio returns decomposition",
                 "Time Series plot of portfolio style factors returns",
                 "Time Series plot of portfolio sector returns",
                 "Boxplot of Portfolio Factor Returns Components"), 
               title="\nMake a plot selection (or 0 to exit):") 
      }
      
      switch(which,
             "1L" = {
               if(titleText){
                 main = "Portfolio Returns Decomposition"
               }else(
                 main = ''
               )
               ## Time Series plot of portfolio returns decomposition
               tsPlotMP(dat[,c('PortRet','FacRet','ResidRet')], 
                        main = main, layout = c(1,3), stripLeft = stripLeft, 
                        scaleType = scaleType, ...)
               
             }, 
             "2L" = {
               if(titleText){
                 main = "Portfolio Style Factors Returns"
               }else(
                 main = ''
               )
               ## Time Series plot of portfolio style factors returns
               tsPlotMP(dat[,c('FacRet',exposures.num,'ResidRet')], 
                        main = main, layout = c(3,3), stripLeft = stripLeft, 
                        scaleType = scaleType, ...)
               
             }, 
             "3L" = {  
               if(titleText){
                 main = "Portfolio Sector Returns"
               }else(
                 main = ''
               )
               ## Time Series plot of portfolio sector returns
               tsPlotMP(dat[,c('FacRet',exposures.char.name)], 
                        main = main, layout = c(3,4), stripLeft = stripLeft, 
                        scaleType = scaleType, ...)
               
             },
             "4L" = {  
               if(titleText){
                 main = "Portfolio Returns Components Distributions"
               }else(
                 main = ''
               )
               ## Boxplot of Portfolio Returns Components
               par(mar=c(7,5,5,5))
               boxplot(100*coredata(merge(dat[,c(1:3)],FacRet,secRet)), col=5, las = 2, 
                       ylab = "Percentage (%)",
                       main = main)
               boxplot(100*coredata(dat[,-c(1:3)]), col=5, las = 2, 
                       xaxt = "n", 
                       ylab = "Percentage (%)",
                       main = main)
               axis(1, at=c(1:ncol(dat[,-c(1:3)])) , labels = FALSE)
               text(x = c(1:ncol(dat[,-c(1:3)])), srt = 90, adj = 1, labels = colnames(dat[,-c(1:3)]), 
                    par("usr")[3] - 1, xpd = TRUE, cex = 0.8)
               
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
    avg = apply(dat, 2, mean) * 100
    vol = apply(dat, 2, sd) * 100 
    stats.sum = cbind(avg, vol)
    colnames(stats.sum) = c('Mean','Volatility')
    
    ret = round(stats.sum, digits)
    return(ret)
  }
  
}
