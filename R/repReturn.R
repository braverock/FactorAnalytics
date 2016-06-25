#' @title Portfolio return reports for risk decomposition and performance analysis
#' 
#' @description conduct portfolio return analysis reporting 
#' 
#' @param ffmObj fit object of class \code{tsfm}, \code{sfm} or \code{ffm}.
#' @param weight a vector of weights of the assets in the portfolio. Default is NULL.
#' @param isPlot logical variable to generate plot or not.
#' @param ... additional arguments unused
#' @author Lingjie Yi
#' @examples 
#'
#' #Load the data 
#' data("stocks145scores6")
#'  
#' #Fit a Ffm
#' fit <- fitFfm(data = data145, # Change fit object to mixed.mod
#' exposure.vars = c("SECTOR","ROE","BP","PM12M1M","SIZE","ANNVOL1M","EP"),
#' date.var = "DATE", 
#' ret.var = "RETURN", 
#' asset.var = "TICKER", 
#' fit.method="WLS",
#' z.score = F)
#'               
#' Conduct portfolio returns analysis reporting with default weights.               
#' repReturn(fit)
#' 
#' @export

# Not the final version


repReturn <- function(ffmObj, weights = NULL, isPlot = FALSE, ...) {
  
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
  
  dat = merge(ret.p, alpha, facRet.p, rk, sig.p)
  
  if(isPlot){
    
    boxplot(coredata(dat),col=5,
            cex.names=0.5,
            main=paste("Portfolio Detailed Returns Decomposition"))
    
    tsPlotMP(dat[,c('Return','Alpha','facRet','Residuals')], main = "Portfolio Returns Decomposition", scaleType = "free", layout = c(3,3))
    tsPlotMP(dat[,c('facRet',exposures.num,'Residuals')], main = "Portfolio Individual Style Factor Returns", scaleType = "free", layout = c(3,3))
    tsPlotMP(dat[,c(exposures.char.name)], main = "Portfolio Sector Returns", scaleType = "free", layout = c(3,4))
    
  }
  
  # tabular report 
  avg = apply(dat, 2, mean)
  vol = apply(dat, 2, sd)
  stats.sum = rbind(avg, vol)
  rownames(stats.sum) = c('mean','volatility')
    
  return(stats.sum)
  
}
