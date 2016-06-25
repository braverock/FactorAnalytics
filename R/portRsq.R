#' @title  R-squared and Adjusted R-squared for a Portfolio
#' 
#' @description Calcluate the R-squared and Adjusted R-squared for the portfolio of assets
#' 
#' @param ffmObj  an object of class \code{ffm} produced by \code{fitFfm}
#' @param weight a vector of weights of the assets in the portfolio. Default is NULL.
#' @param ... additional arguments unused
#' @author Avinash Acharya
#' 
#' @return \code{portRsqr} returns an object of class list with the follwing components:
#' \item{port.Rsqr} time series of R-squared values for the portfolio.
#' \item{port.AdjRsqr} time series of adjusted R-squared values for the portfolio. 
#' 
#' @examples 
#'
#' #Load the data 
#' data("stocks145scores6")
#'  
#' #Fit a Ffm
#' fit <- fitFfm(data=data145, asset.var="TICKER", ret.var="RETURN", 
#'               date.var="DATE", exposure.vars="SECTOR")
#'               
#' #Find the portfolio R-squared and adjusted portfolio R-squared for the 145 stocks data with default weights.               
#' portRsqr(fit)
#' 
#' @export

# Not the final version
portRsqr <- function(ffmObj, weight=NULL, ...)
{
  # set defaults and check input validity
  if (!inherits(ffmObj, "ffm")) {
    stop("Invalid argument: Object should be of class'ffm'.")
  }
  
  data <- ffmObj$data
  date.var = ffmObj$date.var
  data <- data[order(data[,date.var]),]
  assets.names<- ffmObj$asset.names
  n.assets <- length(ffmObj$asset.names)
  if (!is.null(weight))
  {
    w <- weight[assets.names]
  } 
  else 
  {
    w <- rep(1/n.assets, n.assets)
  }
  
  W<- diag(w)#NxN 
  
  returns = matrix(data = ffmObj$data[[ffmObj$ret.var]] , nrow = n.assets) #NxT Matrix of Returns
  residuals = t(ffmObj$residuals) #NxT Matrix of residual returns
  time.periods = length(ffmObj$time.periods)
  r2<-0
  for (i in 1:time.periods)
  {
    r2[i] = 1 - ((t(residuals[,i]) %*% W %*% residuals[,i]) / (t(returns[,i]) %*% W %*% returns[,i])) 
  } 
  names(r2) <- names(ffmObj$r2)
  K <- length(ffmObj$factor.name)
  p <- K-1
  adj.r2 <- 1 - ((n.assets - 1)*(1- r2) / (n.assets - p - 1))
  
  return(list(port.Rsqr = r2, port.AdjRsqr = adj.r2))
}

