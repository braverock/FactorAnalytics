#' @title Portfolio return reports for risk decomposition and performance analysis
#' 
#' @description 
#' 
#' 
#'Not the final version 



repReturn <- function(object, weights = NULL, ...){
  # check input object validity
  if (!inherits(object, c("tsfm", "sfm", "ffm"))) {
    stop("Invalid argument: Object should be of class 'tsfm', 'sfm' or 'ffm'.")
  }
  UseMethod("repReturn")
}


repReturn.ffm <- function(object, weights = NULL, ...) {
  
  which.numeric <- sapply(object$data[,object$exposure.vars,drop=FALSE], is.numeric)
  exposures.num <- object$exposure.vars[which.numeric]
  exposures.char <- object$exposure.vars[!which.numeric]
  exposures.char.name <- as.vector(unique(object$data[,exposures.char]))
  
  # get factor model returns from 
  facRet = object$factor.returns
  
  if(!length(exposures.char)){
    alpha = facRet[,1]
    colnames(alpha) = 'Alpha'
    facRet = facRet[,-1]
  }else{
    alpha = c()
  }
  sig = object$residuals
  
  # get parameters from the factor model fit  
  beta = object$beta
  n.assets = nrow(beta)
  asset.names <- unique(object$data[[object$asset.var]])
  TP = length(object$time.periods)
  
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
    dat <- object$data[object$data$DATE==object$time.periods[TP], ]
    B <- as.matrix(table(dat$TICKER,dat$SECTOR))
    B[B>0] <- 1
    B <- B[asset.names,]
  }else{
    B = c()
  }
  
  #calculate x = t(w) * B
  X = c()
  for(i in 1:TP){
    dat <- object$data[object$data$DATE==object$time.periods[i], ]
    beta <- as.matrix(dat[,exposures.num])
    rownames(beta) <- asset.names
    beta = cbind(beta,B)
    
    temp = as.data.frame(weights %*% beta)
    temp = cbind('Date'=object$time.periods[i],temp)
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

  boxplot(coredata(dat),col=5,
          cex.names=0.5,
          main=paste("Portfolio Detailed Returns Decomposition"))
  
  tsPlotMP(dat[,c('Return','Alpha','facRet','Residuals')], main = "Portfolio Returns Decomposition", scaleType = "free", layout = c(3,3))
  tsPlotMP(dat[,c('facRet',exposures.num,'Residuals')], main = "Portfolio Individual Style Factor Returns", scaleType = "free", layout = c(3,3))
  tsPlotMP(dat[,c(exposures.char.name)], main = "Portfolio Sector Returns", scaleType = "free", layout = c(3,4))
  
  
}
