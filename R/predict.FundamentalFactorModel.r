#' predict method for FundamentalFactorModel object
#'
#' Generic function of predict method for fitFundamentalFactorModel.
#'
#' @param fit "FundamentalFactorModel" object 
#' @export
#' @author Yi-An Chen
#' 
predict.FundamentalFactorModel <- function(fit,newdata){
 
  # if there is no newdata provided
  # calculate fitted values
  datevar <- as.character(fit$call)[4]
  assetvar <- as.character(fit$call)[6]
  assets = unique(data[,assetvar])
  timedates = as.Date(unique(data[,datevar]))
  
  numTimePoints <- length(timedates)
  numExposures <- length(exposure.names)
  numAssets <- length(assets)
  
  f <-  fit$factors # T X 3 
  exposure.names <- colnames(f)[-1]
  beta.all <- data[,c(datevar,assetvar,exposure.names)] #  (N * T ) X 4
  
  if (missing(newdata) || is.null(newdata)) {
   #
  ### calculated fitted values
  # 
  
  fitted <- rep(NA,numAssets)
  for (i in 1:numTimePoints) {
  beta <- subset(beta.all, DATE == index(f)[i])[,exposure.names]
  beta <- as.matrix(cbind(rep(1,numAssets),beta))
  fit.tmp <- beta %*% t(f[i,])
  fitted <- rbind(fitted,t(fit.tmp))
  }
  fitted <- fitted[-1,]
  colnames(fitted) <- assets
  
 } 
  
  # predict returns by newdata
 if (!missing(newdata) && !is.null(newdata))  {
   # check if newdata has the same data points as beta
  if (dim(newdata) != c(numAssets*numTimePoints,numExposures)) {
    stop("Dimension of newdata has to match mAssets*numTimePoints,numExposures")
  } else {
    
    
    
  }
 
 }

  
}