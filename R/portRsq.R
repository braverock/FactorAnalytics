#' @title Portfolio R-squared and Adjusted R-squared
#' 
#' @description Calcluate the R-squared and Adjusted R-squared for the portfolio of stocks
#' 
#' 
#' 
# Not the final version
portRsq <- function(object, weight=NULL, ...)
{
  # set defaults and check input validity
  if (!inherits(object, "ffm")) {
    stop("Invalid argument: Object should be of class'ffm'.")
  }
  
  
  z<- object
  data <- z$data
  date.var = z$date.var
  data <- data[order(data[,date.var]),]
  assets.names<- z$asset.names
  n.assets <- length(z$asset.names)
  if (!is.null(weight))
  {
    w <- weight[assets.names]
  } 
  else 
  {
    w <- rep(1/n.assets, n.assets)
  }
  
  W<- diag(w)#NxN 
  
  returns = matrix(data = z$data[[z$ret.var]] , nrow = n.assets) #NxT Matrix of Returns
  residuals = t(z$residuals) #NxT Matrix of residual returns
  time.periods = length(z$time.periods)
  r2<-0
  for (i in 1:time.periods)
  {
    r2[i] = 1 - ((t(residuals[,i]) %*% W %*% residuals[,i]) / (t(returns[,i]) %*% W %*% returns[,i])) 
  } 
  names(r2) <- names(z$r2)
  K <- length(z$factor.name)
  p <- K-1
  adj.r2 <- 1 - ((n.assets - 1)*(1- r2) / (n.assets - p - 1))
  
  return(list(portRsqr = r2, portAdjRsqr = adj.r2))
}

