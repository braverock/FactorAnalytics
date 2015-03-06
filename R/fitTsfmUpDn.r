#' @title Fit a up and down market factor model using time series regression
#' 
#' @description This is a wrapper function to fits a up and down market model for one 
#' or more asset returns or excess returns using time series regression. 
#' Users can choose between ordinary least squares-OLS, discounted least 
#' squares-DLS (or) robust regression. Several variable selection options  
#' including Stepwise, Subsets, Lars are available as well. An object of class 
#' \code{"tsfm"} is returned.
#' 
#' @details 
#' \code{fitTsfmUpDn} will use \code{fitTsfm} to fit a time series model for up and down market respectively. If 
#' risk free rate is provided, the up market is the excess market returns which is no less than 0.
#' The goal of up and down marke is to capture different market Betas in the up and down markets. 
#' 
#' 
#' @param asset.names vector containing names of assets, whose returns or 
#' excess returns are the dependent variable.
#' @param factor.names vector containing names of the macroeconomic factors.
#' @param mkt.name name of the column for market excess returns (Rm-Rf). It 
#' is required for a up/down market model. 
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
#' @return 
#' \code{fitTsfmUpDn} returns an object \code{tsfmUpDn}. It supports generic function such as 
#' \code{summary}, \code{predict}, \code{plot} and \code{print}.
#' 
#' It is also a list object containing \code{Up} and \code{Dn}. Both \code{Up} and \code{Dn} are class of \code{"tsfm"}. As a result, for each list 
#' object, The generic function such as \code{print}, \code{plot}, \code{predict} 
#' and \code{summary} methods exist for both \code{Up} and \code{Dn}. Also, the generic accessor functions \code{coef}, 
#' \code{fitted} \code{residuals} and  \code{fmCov} can be applied as well.
#' 
#' An object of class \code{"tsfmUpDn"} is a list containing \code{Up} and \code{Dn}:
#' \item{Up}{An object of \code{tsfm} fitted by \code{fitTsfm} for the up market.}
#' \item{Dn}{An object of \code{tsfm} fitted by \code{fitTsfm} for the down market.}
#' 
#' Each object of \code{tsfm} contains : 
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
#' @seealso 
#' The \code{tsfmUpDn} methods for generic functions: 
#' \code{\link{plot.tsfmUpDn}}, \code{\link{predict.tsfmUpDn}}, 
#' \code{\link{print.tsfmUpDn}} and \code{\link{summary.tsfmUpDn}}. 
#' 
#' 
#' The original time series function \code{\link{fitTsfm}} and its generic functions
#'  application.
#' @examples
#' # load data from the database
#' data(managers)
#' 
#' # example: Up and down market factor model with OLS fit
#' fitUpDn <- fitTsfmUpDn(asset.names=colnames(managers[,(1:6)]),mkt.name="SP500.TR",
#'                        data=managers, fit.method="OLS",control=NULL)
#'  
#'  print(fitUpDn)
#'  summary(fitUpDn)
#'  
#'  # A list object
#'  fitUpDn
#'  summary(fitUpDn$Up)
#'  summary(fitUpDn$Dn)
#'  
#' @importFrom PerformanceAnalytics checkData
#' @importFrom robust lmRob step.lmRob
#' @importFrom leaps regsubsets
#' @importFrom lars lars cv.lars
#' 
#' @export


fitTsfmUpDn <- function(asset.names, factor.names=NULL, mkt.name=NULL, rf.name=NULL, 
                                 data=data, fit.method=c("OLS","DLS","Robust"), 
                                 variable.selection=c("none","stepwise","subsets","lars"),
                                 control=fitTsfm.control(...),...) {

  if (is.null(mkt.name)){
    stop("Missing argument: mkt.name has to be specified for up and down market model.")
  }  
  
 
  
  factor.names <- union(factor.names,mkt.name)
  
  # convert data into an xts object and hereafter work with xts objects
  data.xts <- checkData(data)
  # convert index to 'Date' format for uniformity 
  time(data.xts) <- as.Date(time(data.xts))
  
  # extract columns to be used in the time series regression
  dat.xts <- merge(data.xts[,asset.names], data.xts[,factor.names])
  ### After merging xts objects, the spaces in names get converted to periods
  
  # convert all asset and factor returns to excess return form if specified
  if (!is.null(rf.name)) {
    dat.xts <- "[<-"(dat.xts,,vapply(dat.xts, function(x) x-data.xts[,rf.name], 
                                     FUN.VALUE = numeric(nrow(dat.xts))))
    warning("Up market is defined as the excess Market returns is no less than 0.")
  } else {
    warning("Up market is defined as the Market returns is no less than 0.")
  }
  
  mkt <- dat.xts[,mkt.name]
  # up market
  dataUp.xts <- dat.xts[mkt >= 0]
  
  fitUp <-  fitTsfm(asset.names=asset.names,factor.names=factor.names,mkt.name=mkt.name,rf.name=rf.name,
                     data=dataUp.xts,fit.method=fit.method,variable.selection=variable.selection,
                     control=control)

  
  # down market
  dataDn.xts <- dat.xts[mkt < 0]
  fitDn <-  fitTsfm(asset.names=asset.names,factor.names=factor.names,mkt.name=mkt.name,rf.name=rf.name,
                     data=dataDn.xts,fit.method=fit.method,variable.selection=variable.selection,
                     control=control)
  
  result <- list(Up = fitUp, Dn = fitDn)
  class(result) <- "tsfmUpDn"
return(result)
} 
