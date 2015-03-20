#' @title Fit a up and down market factor model using time series regression
#' 
#' @description This is a wrapper function to fits a up and down market model for one 
#' or more asset returns or excess returns using time series regression. 
#' Users can choose between ordinary least squares-LS, discounted least 
#' squares-DLS (or) robust regression. An object of class 
#' \code{"tsfmUpDn"} is returned.
#' 
#' @details 
#' \code{fitTsfmUpDn} will use \code{fitTsfm} to fit a time series model for up and down market respectively. If 
#' risk free rate is provided, the up market is the excess market returns which is no less than 0.
#' The goal of up and down market model is to capture two different market Betas in the up and down markets. 
#' 
#' 
#' @param asset.names vector containing names of assets, whose returns or 
#' excess returns are the dependent variable.
#' @param mkt.name name of the column for market returns. It 
#' is required for a up/down market model. 
#' @param rf.name name of the column of risk free rate variable to calculate 
#' excess returns for all assets (in \code{asset.names}) and the market factor (in 
#' \code{mkt.name}). Default is \code{NULL}, and no action is taken.
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
#' @return 
#' \code{fitTsfmUpDn} returns an object \code{tsfmUpDn}. It supports generic function such as 
#' \code{summary}, \code{predict}, \code{plot} and \code{print}.
#' 
#' It is also a list object containing \code{Up} and \code{Dn}. Both \code{Up} and \code{Dn} are class of \code{"tsfm"}. As a result, for each list 
#' object, The generic function such as \code{print}, \code{plot}, \code{predict} 
#' and \code{summary} methods exist for both \code{Up} and \code{Dn}. Also, the generic accessor functions \code{coef}, 
#' \code{fitted}, \code{residuals} and  \code{fmCov} can be applied as well.
#' 
#' An object of class \code{"tsfmUpDn"} is a list containing \code{Up} and \code{Dn}:
#' \item{Up}{An object of \code{tsfm} fitted by \code{fitTsfm} for the up market;}
#' \item{Dn}{An object of \code{tsfm} fitted by \code{fitTsfm} for the down market;}
#' 
#' and others useful items: 
#' \item{call}{Function call.}
#' \item{data}{Original data used but converted to \code{xts} class.}
#' 
#' Each object of \code{tsfm} contains : 
#' \item{asset.fit}{list of fitted objects for each asset. Each object is of 
#' class \code{lm} if \code{fit.method="LS" or "DLS"}, class \code{lmRob} if 
#' the \code{fit.method="Robust"}}
#' \item{alpha}{length-N vector of estimated alphas.}
#' \item{beta}{N x 1 matrix of estimated betas.}
#' \item{r2}{length-N vector of R-squared values.}
#' \item{resid.sd}{length-N vector of residual standard deviations.}
#' \item{call}{the matched function call.}
#' \item{data}{xts data object containing the assets and factors.}
#' \item{asset.names}{asset.names as input.}
#' \item{factor.names}{factor.names as input.}
#' \item{fit.method}{fit.method as input.}
#' Where N is the number of assets and T is the 
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
#' # example: Up and down market factor model with LS fit
#' fitUpDn <- fitTsfmUpDn(asset.names=colnames(managers[,(1:6)]),mkt.name="SP500.TR",
#'                        data=managers, fit.method="LS",control=NULL)
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


fitTsfmUpDn <- function(asset.names, mkt.name, rf.name=NULL, 
                        data=data, fit.method=c("LS","DLS","Robust"), 
                        control=fitTsfm.control(...),...) {

  call <- match.call()
  
  if (is.null(mkt.name)){
    stop("Missing argument: mkt.name has to be specified for up and down market model.")
  }  
   
  # convert data into an xts object and hereafter work with xts objects
  data.xts <- checkData(data)
  # convert index to 'Date' format for uniformity 
  time(data.xts) <- as.Date(time(data.xts))
  
  # extract columns to be used in the time series regression
  dat.xts <- merge(data.xts[,asset.names], data.xts[,mkt.name])
  ### After merging xts objects, the spaces in names get converted to periods
  
  # convert all asset and factor returns to excess returns if specified
  if (!is.null(rf.name)) {
    dat.xts <- "[<-"(dat.xts,,vapply(dat.xts, function(x) x-data.xts[,rf.name], 
                                     FUN.VALUE = numeric(nrow(dat.xts))))
  } 
  
  mkt <- dat.xts[,mkt.name]
  # up market
  dataUp.xts <- dat.xts[mkt >= 0]
  
  fitUp <-  fitTsfm(asset.names=asset.names,factor.names=mkt.name,mkt.name=mkt.name,rf.name=NULL,
                     data=dataUp.xts,fit.method=fit.method,control=control)

  
  # down market
  dataDn.xts <- dat.xts[mkt < 0]
  fitDn <-  fitTsfm(asset.names=asset.names,factor.names=mkt.name,mkt.name=mkt.name,rf.name=NULL,
                     data=dataDn.xts,fit.method=fit.method,control=control)
  
  result <- list(Up = fitUp, Dn = fitDn, call= call, data=dat.xts)
  class(result) <- "tsfmUpDn"
return(result)
} 
