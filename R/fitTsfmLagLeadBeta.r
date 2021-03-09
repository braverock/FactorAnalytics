#' @title Fit a lagged and lead Betas factor model using time series regression
#' 
#' @description This is a wrapper function to fits a time series lagged Betas factor model for one 
#' or more asset returns or excess returns using time series regression. 
#' Users can choose between ordinary least squares-LS, discounted least 
#' squares-DLS (or) robust regression like \code{fitTsfm}.An object of class 
#' \code{"tsfm"} is returned.
#' 
#' @details 
#' The lagged and lead returns model estimates lagged and lead market Beta. Specifically, 
#' \deqn{r_t = \alpha + \beta_0 MKT_t + \beta^-_1 MKT_t-1 + \ldots + \beta^-_K+1 MKT_t-K 
#' +  \beta^+_1 MKT_t+1 + \ldots + \beta^+_K MKT_t+K \epsilon_t, t=1 \ldots T}
#' where \eqn{r_t} is the asset returns, and MKT is the market factor. It is usually 
#' needed for illiquid securities with stale prices. One can also report the sum of 
#' the lagged and lead Betas: 
#' \deqn{\beta = \beta_0 + \beta^+_1 + \beta^+_1 + \ldots + \beta^+_K + 
#' \beta^-_1 + \ldots + \beta^-_K }  
#' 
#' @param asset.names vector containing names of assets, whose returns or 
#' excess returns are the dependent variable.
#' @param mkt.name name of the column for market returns. It 
#' is required for a lagged Betas factor model. 
#' @param rf.name name of the column of risk free rate variable to calculate 
#' excess returns for all assets (in \code{asset.names}) and the market factor (in 
#' \code{mkt.name}).Default is NULL, and no action is taken.
#' @param LagLeadBeta A integer number to specify numbers of lags (and leads when LagOnly is FALSE) 
#' of Betas to 
#' include in the model. The Default is 1.
#' @param LagOnly Flag variable to only include the lags (or have both lags and leads). 
#' The Default is FALSE (both lags and leads). 
#' @param data vector, matrix, data.frame, xts, timeSeries or zoo object  
#' containing column(s) named in \code{asset.names}, \code{factor.names} and 
#' optionally, \code{mkt.name} and \code{rf.name}.
#' @param fit.method the estimation method, one of "LS", "DLS" or "Robust". 
#' See details. Default is "LS". 
#' @param control list of control parameters. The default is constructed by 
#' the function \code{\link{fitTsfm.control}}. See the documentation for 
#' \code{\link{fitTsfm.control}} for details.
#' @param ... arguments passed to \code{\link{fitTsfm.control}}
#' 
#' @return \code{fitTsfmLagLeadBeta} also returns an object of class \code{"tsfm"} like 
#' \code{fitTsfm}. The generic function such as \code{print}, \code{plot}, \code{predict} 
#' and \code{summary} methods exist. Also, the generic accessor functions \code{coef}, 
#' \code{fitted}, \code{residuals} and  \code{fmCov} can be applied as well.
#' 
#' An object of class \code{"tsfm"} is a list containing the following 
#' components:
#' \item{asset.fit}{list of fitted objects for each asset. Each object is of 
#' class \code{lm} if \code{fit.method="LS" or "DLS"}, class \code{lmRob} if 
#' the \code{fit.method="Robust"}.}
#' \item{alpha}{length-N vector of estimated alphas.}
#' \item{beta}{N x (L+1) matrix of estimated betas.}
#' \item{r2}{length-N vector of R-squared values.}
#' \item{resid.sd}{length-N vector of residual standard deviations.}
#' \item{call}{the matched function call.}
#' \item{data}{xts data object containing the assets and factors.}
#' \item{asset.names}{asset.names as input.}
#' \item{fit.method}{fit.method as input.}
#' Where N is the number of assets, L is the number of lagged and lead market Betas and T is the 
#' number of time periods.
#' 
#' @author Yi-An Chen.
#' 
#' @references 
#' Scholes, M. and Williams, J. T. (1977). Estimating betas from non-synchronous
#' data, Journal of Financial Economics, vol. 5, 1977, pp. 309-327
#' 
#' @seealso 
#' The original time series function \code{\link{fitTsfm}} and its generic functions
#'  application.
#' 
#' @examples
#' # load data from the database
#' library(PerformanceAnalytics)
#' data(managers)
#' 
#' # example: A lagged Beetas model with LS fit
#' fit <- fitTsfmLagLeadBeta(asset.names=colnames(managers[,(1:6)]),LagLeadBeta=2,LagOnly=TRUE,
#'                       mkt.name="SP500 TR",rf.name="US 3m TR",data=managers)
#' summary(fit)
#' fitted(fit)
#' 
#' @export

fitTsfmLagLeadBeta <- function(asset.names, mkt.name, rf.name=NULL, 
                          data=data, fit.method=c("LS","DLS","Robust"),LagLeadBeta=1, LagOnly=FALSE,
                          control=fitTsfm.control(...),...) {
  
  if (is.null(mkt.name))  {
    stop("Missing argument: mkt.name has to be specified for lagged Betas model.")
  }
 
  
  if (as.integer(LagLeadBeta) != LagLeadBeta | LagLeadBeta < 1 ) {
    stop("Invalid argument: LagLeadBeta must be an integer and no less than 1. The default is 1.")
  }
  
  # Create market lag terms
  factor.names = mkt.name
  mktlag <- lag(data[,mkt.name],k=seq(1,LagLeadBeta,1))
  mktlead <- lag(data[,mkt.name],k=seq(-1,-LagLeadBeta,-1))
  
  for (i in 1:LagLeadBeta) {
    colnames(mktlag)[i] <- paste("MktLag",i,sep="")
	colnames(mktlead)[i] <- paste("MktLead",i,sep="")
	
    factor.names <- c(factor.names,paste("MktLag",i,sep=""),paste("MktLead",i,sep=""))
  }
    data <- merge(data,mktlag)
	data <- merge(data,mktlead)
	
  fit <-  fitTsfm(asset.names=asset.names,factor.names=factor.names,mkt.name=mkt.name,rf.name=rf.name,
                  data=data,fit.method=fit.method,variable.selection="none",control=control)
  
  return(fit)  
}