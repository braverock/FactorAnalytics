#' @title  R-squared and Adjusted R-squared for a Portfolio
#' 
#' @description Calcluate the R-squared and Adjusted R-squared for a portfolio of assets
#' 
#' @param ffmObj  an object of class \code{ffm} produced by \code{fitFfm}
#' @param weight a vector of weights of the assets in the portfolio. Default is NULL.
#' @param ... potentially further arguments passed.
#' @author Avinash Acharya
#' 
#' @return \code{portRsqr} returns a list with the follwing components:
#' \item{port.Rsqr}{ length-T vector of R-squared values for the portfolio.}
#' \item{port.AdjRsqr}{ length-T vector of adjusted R-squared values for the portfolio.} 
#' 
#' @examples 
#'
#' #Load the data 
#' data("stocks145scores6")
#'  
#' #Fit a Ffm
#' fit <- fitFfm(data=stocks145scores6, asset.var="TICKER", ret.var="RETURN", 
#'               date.var="DATE", exposure.vars="SECTOR")
#'               
#' #Find the portfolio R-squared and adjusted portfolio R-squared 
#' #for the 145 stocks data with default weights.               
#' portRsqr(fit)
#' 
#' @export

# Not the final version
portRsqr <- function(ffmObj, weight=NULL, ...)
{
  # set defaults and check input validity
  if (!inherits(ffmObj, "ffm"))
  {
    stop("Invalid argument: Object should be of class'ffm'.")
  }
  
  data <- ffmObj$data
  date.var = ffmObj$date.var
  data <- data[order(data[,date.var]),]
  assets.names<- ffmObj$asset.names
  n.assets <- length(ffmObj$asset.names)
  if (!is.null(weight))
  {
    if(length(weight) == n.assets)
    {
      w <- weight[assets.names]
    }
    else
      stop("Error: Length of weight should be equal to the number of assets ")
    
  } 
  else 
  {
    w <- rep(1/n.assets, n.assets)
  }
  
  W<- diag(w)#NxN 
  
  returns = matrix(data = ffmObj$data[[ffmObj$ret.var]] , nrow = n.assets) #NxT Matrix of Returns
  residuals = t(ffmObj$residuals) #NxT Matrix of residual returns
  time.periods = length(ffmObj$time.periods)
  r2 = 1 - ((t(residuals[,1:time.periods]) %*% W %*% residuals[,1:time.periods]) / (t(returns[,1:time.periods]) %*% W %*% returns[,1:time.periods])) 
  r2<- diag(r2)
  names(r2) <- names(ffmObj$r2)
  K <- length(ffmObj$factor.name)
  p <- K-1
  adj.r2 <- 1 - ((n.assets - 1)*(1- r2) / (n.assets - p - 1))
  
  list(port.Rsqr = r2, port.AdjRsqr = adj.r2)
}

