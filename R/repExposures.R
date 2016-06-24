#' @title Portfolio tabular reports for risk decomposition and performance analysis
#' 
#' @description 
#' 
#' 
#' 
# Not the final version 


repExposures <- function(object, weights = NULL, ...){
  # check input object validity
  if (!inherits(object, c("tsfm", "sfm", "ffm"))) {
    stop("Invalid argument: Object should be of class 'tsfm', 'sfm' or 'ffm'.")
  }
  UseMethod("repExposures")
}


repExposures.ffm <- function(object, weights = NULL, ...) {
  
  which.numeric <- sapply(object$data[,object$exposure.vars,drop=FALSE], is.numeric)
  exposures.num <- object$exposure.vars[which.numeric]
  exposures.char <- object$exposure.vars[!which.numeric]
  
  # get parameter from the factor model fit
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
  
  ###generate plots
  for(i in 1:ncol(X[,exposures.num])){
    name = colnames(X[,exposures.num])[i]
    barplot(X[,exposures.num][,i],las=2,col=5,
            names.arg= as.yearmon(index(X)),
            cex.names=0.5,
            main=paste("Factor Exposure for",name))
  } 
  
  boxplot(100*coredata(X[,exposures.num]), col=5,
          cex.names=0.5, notch = T,
          main=paste("Distributions of Exposures"))
  
  tsPlotMP(X[,exposures.num], main = "Factor Exposures", scaleType = "free", layout = c(3,3))
  
  # tabular report 
  mean = apply(X[,exposures.num], 2, mean)
  vol = apply(X[,exposures.num], 2, sd)
  sum = rbind(mean, vol)
  rownames(sum) = c('mean','volatility')
}

