#' @title Fit a market timing time series factor model
#' 
#' @description This is a wrapper function to fit a market timing time series 
#' factor model for one or more asset returns or excess returns using time 
#' series regression. Users can choose between ordinary least squares-LS, 
#' discounted least squares-DLS (or) robust regression. An object of class 
#' \code{"tsfm"} is returned.
#' 
#' @details 
#' Market timing accounts for the price movement of the general stock market 
#' relative to fixed income securities. A market-timing factor is added to the 
#' time series regression, following Henriksson & Merton (1981). Here, we use 
#' down.market = max(0, R_f-R_m), where Rm is the (excess) return on the market. 
#' The coefficient of this down-market factor can be interpreted as the number 
#' of "free" put options on the market provided by the manager's market-timings 
#' skills.
#' 
#' @param asset.names vector containing syntactically valid names of assets, whose returns or 
#' excess returns are the dependent variable.
#' @param mkt.name syntactically valid name of the column for market returns (required).
#' @param rf.name syntactically valid name of the column of risk free rate variable to calculate 
#' excess returns for all assets (in \code{asset.names}) and the market factor 
#' (in \code{mkt.name}). Default is NULL, and no action is taken.
#' @param data vector, matrix, data.frame, xts, timeSeries or zoo object  
#' containing column(s) named in \code{asset.names}, \code{factor.names} and 
#' optionally, \code{mkt.name} and \code{rf.name}.
#' @param fit.method the estimation method, one of "LS", "DLS" or "Robust". 
#' See details. Default is "LS". 
#' @param control list of control parameters passed to \code{\link{fitTsfm}}. 
#' Refer to \code{\link{fitTsfm.control}} for details.
#' @param ... arguments passed to \code{\link{fitTsfm.control}}
#' 
#' @return Similar to \code{fitTsfm}, \code{fitTsfmMT} also returns an object 
#' of class \code{"tsfm"}, for which \code{print}, \code{plot}, \code{predict} 
#' and \code{summary} methods exist. The generic accessor functions \code{coef}, 
#' \code{fitted}, \code{residuals} and \code{fmCov} can be applied as well.
#' 
#' An object of class \code{"tsfm"} is a list containing the following 
#' components:
#' \item{asset.fit}{list of fitted objects for each asset. Each object is of 
#' class \code{lm} if \code{fit.method="LS" or "DLS"}, class \code{lmRob} if 
#' the \code{fit.method="Robust"}.}
#' \item{alpha}{length-N vector of estimated alphas.}
#' \item{beta}{N x 2 matrix of estimated betas.}
#' \item{r2}{length-N vector of R-squared values.}
#' \item{resid.sd}{length-N vector of residual standard deviations.}
#' \item{call}{the matched function call.}
#' \item{data}{xts data object containing the asset(s) and factor(s) returns.}
#' \item{asset.names}{asset.names as input.}
#' \item{factor.names}{vector containing the names of the market-timing factor 
#' and the market factor}
#' \item{mkt.name}{mkt.name as input}
#' \item{fit.method}{fit.method as input.}
#' Where N is the number of assets and T is the number of time periods.
#' 
#' @author Yi-An Chen, Sangeetha Srinivasan.
#' 
#' @references 
#' Christopherson, J. A., Carino, D. R., & Ferson, W. E. (2009). Portfolio 
#' performance measurement and benchmarking. McGraw Hill Professional. pp.127-133
#' 
#' Henriksson, R. D., & Merton, R. C. (1981). On market timing and investment 
#' performance. II. Statistical procedures for evaluating forecasting skills. 
#' Journal of business, 513-533.
#' 
#' Treynor, J., & Mazuy, K. (1966). Can mutual funds outguess the market. 
#' Harvard business review, 44(4), 131-136.
#' 
#' @seealso 
#' The original time series factor model fitting function \code{\link{fitTsfm}} 
#' and related methods.
#' 
#' @examples
#'  # load data
#' data(managers, package = 'PerformanceAnalytics')
#'  # Make syntactically valid column names
#' colnames(managers)
#' colnames(managers) <- make.names( colnames(managers))
#' colnames(managers)
#' 
#' # example: Market-timing time series factor model with LS fit
#' fit <- fitTsfmMT(asset.names=colnames(managers[,(1:6)]), 
#'                  mkt.name="SP500.TR", rf.name="US.3m.TR", 
#'                  data=managers)
#' summary(fit)
#' 
#' @export
fitTsfmMT <- function(asset.names, mkt.name, rf.name=NULL, data=data, 
                      fit.method=c("LS","DLS","Robust"), 
                      control=fitTsfm.control(...), ...) {
  
  if (is.null(mkt.name)){
    stop("Missing argument: mkt.name is required for market timing models.")
  }
  
  if (length(grep(" ", colnames(data))) > 0) {
    stop("Please use syntactically valid column names for continuity with merge.xts. 
See 'make.names' function and associated documentation as well as 
https://stackoverflow.com/questions/9195718/variable-name-restrictions-in-r")
  }
  
  # from PerformanceAnalytics, convert data into an xts object
  data.xts <- checkData(data)
  # convert index to 'Date' format for uniformity 
  time(data.xts) <- as.Date(time(data.xts))
  
  # extract variables to be used in the time series regression
  dat.xts <- merge(data.xts[,asset.names], data.xts[,mkt.name])
  
  # Note `Return.excess` will modify variable names, so change back
  dat.xts.names <- colnames(dat.xts)
  dat.xts <- PerformanceAnalytics::Return.excess(R = dat.xts, 
                                                 Rf = data.xts[ ,rf.name])
  colnames(dat.xts) <- dat.xts.names
  # mkt-timing factors: down.market=max(0,Rf-Rm), market.sqd=(Rm-Rf)^2
  
  down.market <- dat.xts[ ,mkt.name]
  down.market[down.market < 0 ] <- 0
  dat.xts <- merge.xts(dat.xts, down.market)
  colnames(dat.xts)[dim(dat.xts)[2]] <- "down.market"
  factor.names <- c(mkt.name, "down.market")
  
  #   if("TM" %in% mkt.timing) {
  #     market.sqd <- data.xts[,mkt.name]^2   
  #     dat.xts <- merge(dat.xts, market.sqd)
  #     colnames(dat.xts)[dim(dat.xts)[2]] <- "market.sqd"
  #     factor.names <- c(factor.names, "market.sqd")
  #   }
  
  fit.MktTiming <-  fitTsfm(asset.names=asset.names, 
                            factor.names=factor.names, 
                            rf.name=NULL, 
                            data=dat.xts, 
                            fit.method=fit.method, 
                            variable.selection="none", 
                            control=control)
  
  return(fit.MktTiming)  
}
