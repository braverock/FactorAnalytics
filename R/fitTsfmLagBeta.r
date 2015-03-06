#' @title Fit a lagged Betas factor model using time series regression
#' 
#' @description This is a wrapper function to fits a time series lagged Betas factor model for one 
#' or more asset returns or excess returns using time series regression. 
#' Users can choose between ordinary least squares-OLS, discounted least 
#' squares-DLS (or) robust regression like \code{fitTsfm}. Several variable selection options  
#' including Stepwise, Subsets, Lars are available as well. An object of class 
#' \code{"tsfm"} is returned.
#' 
#' @details 
#' The lagged returns model estimates lagged market Beta. Specifically, 
#' \deqn{r_t = \alpha + \beta_0 MKT_t + \beta_1 MKT_t-1 + \ldots + \beta_K MKT_t-K + \epsilon_t, t=1 \ldots T}
#' where \eqn{r_t} is the asset returns, and MKT is the market factor. It is usually 
#' needed for illiquid securities with stale prices. One can also report the sum of 
#' the lagged Betas: 
#' \deqn{\beta = \beta_0 + \beta_1 + \ldots + \beta_K }  
#' 
#' @param asset.names vector containing names of assets, whose returns or 
#' excess returns are the dependent variable.
#' @param factor.names vector containing names of the macroeconomic factors.
#' @param mkt.name name of the column for market excess returns (Rm-Rf). It 
#' is required for a lagged Betas factor model. 
#' @param rf.name name of the column of risk free rate variable to calculate 
#' excess returns for all assets (in \code{asset.names}) and factors (in 
#' \code{factor.names}). Default is NULL, and no action is taken.
#' @param LagBeta A integer number to specify numbers of lags of Betas to 
#' include in the model. The Default is 1.
#' @param data vector, matrix, data.frame, xts, timeSeries or zoo object  
#' containing column(s) named in \code{asset.names}, \code{factor.names} and 
#' optionally, \code{mkt.name} and \code{rf.name}.
#' @param fit.method the estimation method, one of "OLS", "DLS" or "Robust". 
#' See details. Default is "OLS". 
#' @param variable.selection the variable selection method, one of "none", 
#' "stepwise","subsets","lars". See details. Default is "none".
#' \code{mkt.name} is required if any of these options are to be implemented.
#' @param control list of control parameters. The default is constructed by 
#' the function \code{\link{fitTsfm.control}}. See the documentation for 
#' \code{\link{fitTsfm.control}} for details.
#' @param ... arguments passed to \code{\link{fitTsfm.control}}
#' 
#' @return \code{fitTsfmLagBeta} also returns an object of class \code{"tsfm"} like 
#' \code{fitTsfm}. The generic function such as \code{print}, \code{plot}, \code{predict} 
#' and \code{summary} methods exist. Also, the generic accessor functions \code{coef}, 
#' \code{fitted} \code{residuals} and  \code{fmCov} can be applied as well.
#' 
#' An object of class \code{"tsfm"} is a list containing the following 
#' components:
#' \item{asset.fit}{list of fitted objects for each asset. Each object is of 
#' class \code{lm} if \code{fit.method="OLS" or "DLS"}, class \code{lmRob} if 
#' the \code{fit.method="Robust"}, or class \code{lars} if 
#' \code{variable.selection="lars"}.}
#' \item{alpha}{length-N vector of estimated alphas.}
#' \item{beta}{N x K matrix of estimated betas.}
#' \item{r2}{length-N vector of R-squared values.}
#' \item{resid.sd}{length-N vector of residual standard deviations.}
#' \item{fitted}{xts data object of fitted values; iff 
#' \code{variable.selection="lars"}}
#' \item{call}{the matched function call.}
#' \item{data}{xts data object containing the assets and factors.}
#' \item{asset.names}{asset.names as input.}
#' \item{factor.names}{factor.names as input.}
#' \item{fit.method}{fit.method as input.}
#' \item{variable.selection}{variable.selection as input.}
#' Where N is the number of assets, K is the number of factors and T is the 
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
#' data(managers)
#' 
#' # example: A lagged Beetas model with OLS fit
#' fit <- fitTsfmLagBeta(asset.names=colnames(managers[,(1:6)]),LagBeta=2,
#'                       factor.names="SP500.TR",mkt.name="SP500.TR",
#'                       rf.name="US.3m.TR",data=managers)
#' summary(fit)
#' fitted(fit)
#'  
#' @importFrom PerformanceAnalytics checkData
#' @importFrom robust lmRob step.lmRob
#' @importFrom leaps regsubsets
#' @importFrom lars lars cv.lars
#' 
#' @export

fitTsfmLagBeta <- function(asset.names, factor.names=NULL, mkt.name=NULL, rf.name=NULL, 
                          data=data, fit.method=c("OLS","DLS","Robust"),LagBeta=1, 
                          variable.selection=c("none","stepwise","subsets","lars"), control=fitTsfm.control(...),...) {
  
  if (is.null(mkt.name))  {
    stop("Missing argument: mkt.name has to be specified for lagged Betas model.")
  }
 
  
  if (as.integer(LagBeta) != LagBeta | LagBeta < 1 ) {
    stop("Invalid argument: LagBeta must be an integer and no less than 1. The default is 1.")
  }
  
  # Create market lag terms
  mktlag <- lag(data[,mkt.name],k=seq(1:LagBeta))
  for (i in 1:LagBeta) {
    colnames(mktlag)[i] <- paste("MktLag",i,sep="")
    factor.names <- c(factor.names,paste("MktLag",i,sep=""))
  }
    data <- merge(data,mktlag)
  
  fit <-  fitTsfm(asset.names=asset.names,factor.names=factor.names,mkt.name=mkt.name,rf.name=rf.name,
                  data=data,fit.method=fit.method,variable.selection=variable.selection,control=control)
  
  return(fit)  
}