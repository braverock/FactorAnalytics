#' @title  R-squared and Adjusted R-squared for a Portfolio
#' 
#' @description Calcluate the R-squared and Adjusted R-squared for a portfolio of assets
#' 
#' @param ffmObj   an object of class \code{ffm} produced by \code{fitFfm}
#' @param weight   a vector of weights of the assets in the portfolio. Default is NULL.
#' @param rsq      logical; if \code{TRUE}, R-squared values are computed for the portfolio. Default is \code{TRUE}.
#' @param rsqAdj   logical; if \code{TRUE}, Adjusted R-squared values are computed for the portfolio. Default is \code{FALSE}. 
#' @param digits   an integer indicating the number of decimal places to be used for rounding. Default is 2. 
#' @param ...      potentially further arguments passed.
#' @author Avinash Acharya
#' 
#' @return \code{portRsqr} returns the sample mean along with the time series plot of R squared values for the portfolio.
#'                        If \code{rsqAdj} is \code{TRUE}, correspoding mean and plot for adjusted R-squared is also returned. 
#' 
#' @examples 
#'
#' #Load the data 
#' data("factorDataSetDjia5Yrs")
#'  
#' #Fit a Ffm
#' fit <- fitFfm(data=factorDataSetDjia5Yrs, asset.var="TICKER", ret.var="RETURN", 
#'               date.var="DATE", exposure.vars="SECTOR")
#'               
#' #Find the portfolio R-squared values 
#' portRsqr(fit)
#' 
#' 
#' 
#' @export

# Not the final version
portRsqr <- function(ffmObj, weight=NULL, rsq=T, rsqAdj=F, digits=2, ...)
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

  barplot(r2,las=2,col=5,
          names.arg= as.yearmon(names(r2)),
          cex.names=0.5,
          main="R-squared Values for the Portfolio")
  r2.mean<- round(mean(r2),digits = digits)
  out<- r2.mean
  names(out) <- "Mean R-Square"
  if(rsqAdj)
    {
      K <- length(ffmObj$factor.name)
      p <- K-1
      adj.r2 <- 1 - ((n.assets - 1)*(1- r2) / (n.assets - p - 1))
      barplot(adj.r2,las=2,col=5,
          names.arg= as.yearmon(names(r2)),
          cex.names=0.5,
          main="Adjusted R-squared Values for the Portfolio")
      adj.r2.mean<- round(mean(adj.r2),digits = digits)
      out<- rbind(r2.mean, adj.r2.mean)
      row.names(out)<- c("Mean R-Square", "Mean Adj R-Square")
    }
  print(out)
}

