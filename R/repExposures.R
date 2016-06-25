#' @title Portfolio return reports for risk decomposition and performance analysis
#' 
#' @description conduct portfolio exposures analysis reporting 
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
#' Conduct portfolio exposures analysis reporting with default weights.               
#' repExposures(fit)
#' 
#' @export


repExposures <- function(ffmObj, weights = NULL, isPlot = FALSE, ...) {
  
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
      barplot(X[,exposures.num][,i],las=2,col=5,
              names.arg= as.yearmon(index(X)),
              cex.names=0.5,
              main=paste("Factor Exposure for",name))
    } 
    
    boxplot(100*coredata(X[,exposures.num]), col=5,
            cex.names=0.5, notch = T,
            main=paste("Distributions of Exposures"))
    
    tsPlotMP(X[,exposures.num], main = "Factor Exposures", scaleType = "free", layout = c(3,3))
  }
  
  # tabular report 
  avg = apply(X[,exposures.num], 2, mean)
  vol = apply(X[,exposures.num], 2, sd)
  stats.sum = rbind(avg, vol)
  rownames(stats.sum) = c('mean','volatility')
  
  return(stats.sum)
  
}

