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
#' Typically, factor models are fit using excess returns. \code{rf.name} gives 
#' the option to supply a risk free rate variable to subtract from each asset 
#' return and factor to compute excess returns. 
#' 
#' Estimation method "OLS" corresponds to ordinary least squares using 
#' \code{\link[stats]{lm}}, "DLS" is discounted least squares (weighted least 
#' squares with exponentially declining weights that sum to unity), and, 
#' "Robust" is robust regression (using \code{\link[robust]{lmRob}}). 
#' 
#' If \code{variable.selection="none"}, uses all the factors and performs no 
#' variable selection. Whereas, "stepwise" performs traditional stepwise 
#' LS or Robust regression (using \code{\link[stats]{step}} or 
#' \code{\link[robust]{step.lmRob}}), that starts from the initial set of 
#' factors and adds/subtracts factors only if the regression fit, as measured 
#' by the Bayesian Information Criterion (BIC) or Akaike Information Criterion 
#' (AIC), improves. And, "subsets" enables subsets selection using 
#' \code{\link[leaps]{regsubsets}}; chooses the best performing subset of any 
#' given size or within a range of subset sizes. Different methods such as 
#' exhaustive search (default), forward or backward stepwise, or sequential 
#' replacement can be employed.See \code{\link{fitTsfm.control}} for more 
#' details on the control arguments.
#'  
#' \code{variable.selection="lars"} corresponds to least angle regression 
#' using \code{\link[lars]{lars}} with variants "lasso" (default), "lar", 
#' "stepwise" or "forward.stagewise". Note: If \code{variable.selection="lars"}, 
#' \code{fit.method} will be ignored.
#' 
#' Market timing accounts for 
#' the price movement of the general stock market relative to fixed income 
#' securities. It includes 
#' $down.market = max(0, R_f-R_m)$ as a factor, following Henriksson & Merton 
#' (1981). The coefficient of this down-market factor can be interpreted as the 
#' number of "free" put options on the market provided by the manager's 
#' market-timings kills.
#' 
#' \subsection{Data Processing}{
#' 
#' Note about NAs: Before model fitting, incomplete cases are removed for 
#' every asset (return data combined with respective factors' return data) 
#' using \code{\link[stats]{na.omit}}. Otherwise, all observations in 
#' \code{data} are included.
#' 
#' Note about \code{asset.names} and \code{factor.names}: Spaces in column 
#' names of \code{data} will be converted to periods as \code{fitTsfm} works 
#' with \code{xts} objects internally and colnames won't be left as they are.
#' }
#' 
#' @param asset.names vector containing names of assets, whose returns or 
#' excess returns are the dependent variable.
#' @param factor.names vector containing names of the macroeconomic factors.
#' @param mkt.name name of the column for market excess returns (Rm-Rf); this 
#' is necessary to add market timing factors. Default is NULL.
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
#' @return fitTsfm returns an object of class \code{"tsfm"} for which 
#' \code{print}, \code{plot}, \code{predict} and \code{summary} methods exist. 
#' 
#' The generic accessor functions \code{coef}, \code{fitted} and 
#' \code{residuals} extract various useful features of the fit object. 
#' Additionally, \code{fmCov} computes the covariance matrix for asset returns 
#' based on the fitted factor model
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
#' Christopherson, J. A., Carino, D. R., & Ferson, W. E. (2009). Portfolio 
#' performance measurement and benchmarking. McGraw Hill Professional.
#' 
#' Efron, B., Hastie, T., Johnstone, I., & Tibshirani, R. (2004). Least angle 
#' regression. The Annals of statistics, 32(2), 407-499. 
#' 
#' Hastie, T., Tibshirani, R., Friedman, J., Hastie, T., Friedman, J., & 
#' Tibshirani, R. (2009). The elements of statistical learning (Vol. 2, No. 1). 
#' New York: Springer.
#' 
#' Henriksson, R. D., & Merton, R. C. (1981). On market timing and investment 
#' performance. II. Statistical procedures for evaluating forecasting skills. 
#' Journal of business, 513-533.
#' 
#' Treynor, J., & Mazuy, K. (1966). Can mutual funds outguess the market. 
#' Harvard business review, 44(4), 131-136.
#' 
#' @seealso The \code{tsfm} methods for generic functions: 
#' \code{\link{plot.tsfm}}, \code{\link{predict.tsfm}}, 
#' \code{\link{print.tsfm}} and \code{\link{summary.tsfm}}. 
#' 
#' And, the following extractor functions: \code{\link[stats]{coef}}, 
#' \code{\link[stats]{fitted}}, \code{\link[stats]{residuals}},
#' \code{\link{fmCov}}, \code{\link{fmSdDecomp}}, \code{\link{fmVaRDecomp}} 
#' and \code{\link{fmEsDecomp}}.
#' 
#' \code{\link{paFm}} for Performance Attribution. 
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
