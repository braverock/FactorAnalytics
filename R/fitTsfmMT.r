#' @title Fit a time serie market timing factor model using time series regression
#' 
#' @description This is a wrapper function to fits a time series market timing factor model for one 
#' or more asset returns or excess returns using time series regression. 
#' Users can choose between ordinary least squares-OLS, discounted least 
#' squares-DLS (or) robust regression. Several variable selection options  
#' including Stepwise, Subsets, Lars are available as well. An object of class 
#' \code{"tsfm"} is returned.
#' 
#' @details 
#' The Market timing accounts for the price movement of the general stock market relative to fixed income 
#' securities. It includes down.market = max(0, R_f-R_m) as a factor, following Henriksson & Merton 
#' (1981). The coefficient of this down-market factor can be interpreted as the 
#' number of "free" put options on the market provided by the manager's market-timings kills.
#' 
#' 
#' 
#' @param asset.names vector containing names of assets, whose returns or 
#' excess returns are the dependent variable.
#' @param factor.names vector containing names of the macroeconomic factors.
#' @param mkt.name name of the column for market excess returns (Rm-Rf); It 
#' is required for a market timing model.
#' @param rf.name name of the column of risk free rate variable to calculate 
#' excess returns for all assets (in \code{asset.names}) and factors (in 
#' \code{factor.names}). Default is NULL, and no action is taken.
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
#' @return \code{fitTsfmMT} also returns an object of class \code{"tsfm"} like 
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
#' Henriksson, R. D., & Merton, R. C. (1981). On market timing and investment 
#' performance. II. Statistical procedures for evaluating forecasting skills. 
#' Journal of business, 513-533.
#' 
#' #' Christopherson, J. A., Carino, D. R., & Ferson, W. E. (2009). Portfolio 
#' performance measurement and benchmarking. McGraw Hill Professional. pp.127-133
#' 
#' @seealso 
#' The original time series function \code{\link{fitTsfm}} and its generic functions
#'  application.
#' 
#' @examples
#' # load data from the database
#' data(managers)
#' 
#' # example: Market-timing factors with OLS fit
#' fit <- fitTsfmMT(asset.names=colnames(managers[,(1:6)]),  
#'                mkt.name="SP500.TR",rf.name="US.3m.TR",data=managers)
#' summary(fit)
#' fitted(fit)
#'  
#' @importFrom PerformanceAnalytics checkData
#' @importFrom robust lmRob step.lmRob
#' @importFrom leaps regsubsets
#' @importFrom lars lars cv.lars
#' 
#' @export

fitTsfmMT <- function(asset.names, factor.names=NULL, mkt.name=NULL, rf.name=NULL, 
                    data=data, fit.method=c("OLS","DLS","Robust"), 
                    variable.selection=c("none","stepwise","subsets","lars"), control=fitTsfm.control(...),...) {
  if (is.null(mkt.name)){
    stop("Missing argument: mkt.name has to be specified for market timing model.")
  }
  
  factor.names <- union(factor.names,mkt.name)
  
  fit.Timing <-  fitTsfm(asset.names=asset.names,factor.names=factor.names,mkt.name=mkt.name,rf.name=rf.name,
          data=data,fit.method=fit.method,variable.selection=variable.selection,control=control,mkt.timing="HM")

return(fit.Timing)  
}
