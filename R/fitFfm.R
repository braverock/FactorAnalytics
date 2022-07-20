#' @title Fit a fundamental factor model using cross-sectional regression
#'
#' @description Fit a fundamental (cross-sectional) factor model using ordinary
#' least squares or robust regression. Fundamental factor models use observable
#' asset specific characteristics (or) fundamentals, like industry
#' classification, market capitalization, style classification (value, growth)
#' etc. to calculate the common risk factors. An object of class \code{"ffm"}
#' is returned.
#'
#' @details
#' Estimation method "LS" corresponds to ordinary least squares using 
#' \code{\link[stats]{lm}} and "Rob" is robust regression using 
#' \code{\link[RobStatTM]{lmrobdetMM}}. "WLS" is weighted least squares using estimates 
#' of the residual variances from LS regression as weights (feasible GLS). 
#' Similarly, "W-Rob" is weighted robust regression.
#' 
#' The weights to be used in "WLS" or "W-Rob" can be set using 
#' \code{resid.scaleType} argument which computes the residual variances in one of the following ways - 
#' sample variace, EWMA, Robust EWMA and GARCH(1,1). The inverse of these residual variances
#'  are used as the weights. For EWMA model, lambda = 0.9 is used as default and for GARCH(1,1) 
#'  omega = 0.09, alpha = 0.1, and beta = 0.81 are used as default as mentioned in Martin & Ding (2017).
#'  These default parameters can be changed using the arguments \code{lambda}, 
#'  \code{GARCH.params} for EWMA and GARCH respectively. To compute GARCH 
#'  parameters via MLE, set \code{GARCH.MLE} to \code{TRUE}. Make sure you have
#'  the rugarch package installed and loaded, as is merely listed as SUGGESTS.
#'  
#' Standardizing style factor exposures: The exposures can be standardized into
#' z-scores using regular or robust (see \code{rob.stats}) measures of location 
#' and scale. Further, \code{weight.var}, a variable such as market-cap, can be 
#' used to compute the weighted mean exposure, and an equal-weighted standard 
#' deviation of the exposures about the weighted mean. This may help avoid an 
#' ill-conditioned covariance matrix. Default option equally weights exposures 
#' of different assets each period. 
#' 
#' If \code{rob.stats=TRUE}, \code{\link[RobStatTM]{covRob}} is used to compute a 
#' robust estimate of the factor covariance/correlation matrix, and, 
#' \code{\link[robustbase]{scaleTau2}} is used to compute robust tau-estimates 
#' of univariate scale for residuals during "WLS" or "W-Rob" regressions. When 
#' standardizing style exposures, the \code{\link[stats]{median}} and 
#' \code{\link[stats]{mad}} are used for location and scale respectively.
#' When \code{resid.scaleType} is EWMA or GARCH, the residual covariance is equal to the 
#' diagonal matrix of the estimated residual variances in last time period.
#' 
#' 
#' The original function was designed by Doug Martin and initially implemented
#' in S-PLUS by a number of University of Washington Ph.D. students:
#' Christopher Green, Eric Aldrich, and Yindeng Jiang. Guy Yollin ported the
#' function to R and Yi-An Chen modified that code. Sangeetha Srinivasan
#' re-factored, tested, corrected and expanded the functionalities and S3 
#' methods.
#'
#' @importFrom xts as.xts
#' @importFrom zoo as.yearmon
#' @importFrom PerformanceAnalytics checkData skewness kurtosis
#' @importFrom robustbase scaleTau2 covOGK
#' @importFrom RobStatTM lmrobdetMM covRob covClassic
#' @importFrom stats lm as.formula coef contr.treatment fitted mad median 
#' model.matrix na.exclude na.fail na.omit var 
#'
#' @param data data.frame of the balanced panel data containing the variables 
#' \code{asset.var}, \code{ret.var}, \code{exposure.vars}, \code{date.var} and 
#' optionally, \code{weight.var}.
#' @param asset.var character; name of the variable for asset names.
#' @param ret.var character; name of the variable for asset returns.
#' @param date.var character; name of the variable containing the dates 
#' coercible to class \code{Date}.
#' @param exposure.vars vector; names of the variables containing the 
#' fundamental factor exposures.
#' @param weight.var character; name of the variable containing the weights 
#' used when standarizing style factor exposures. Default is \code{NULL}. See 
#' Details.
#' @param fit.method method for estimating factor returns; one of "LS", "WLS" 
#' "Rob" or "W-Rob". See details. Default is "LS".
#' @param rob.stats logical; If \code{TRUE}, robust estimates of covariance, 
#' correlation, location and univariate scale are computed as appropriate (see 
#' Details). Default is \code{FALSE}.
#' @param full.resid.cov logical; If \code{TRUE}, a full residual covariance 
#' matrix is estimated. Otherwise, a diagonal residual covariance matrix is 
#' estimated. Default is \code{FALSE}.
#' @param z.score method for exposure standardization; one of "none", 
#' "crossSection", or "timeSeries". Default is \code{"none"}.
#' @param addIntercept logical; If \code{TRUE}, intercept is added in the 
#' exposure matrix. Default is \code{FALSE},
#' @param lagExposures logical; If \code{TRUE}, the style exposures in the 
#' exposure matrix are lagged by one time period. Default is \code{TRUE},
#' @param resid.scaleType character; Only valid when fit.method is set to WLS or 
#' W-Rob. The weights used in the weighted regression are estimated  using 
#' sample variance, classic EWMA, robust EWMA or GARCH model. Valid values are 
#' \code{stdDev}, \code{EWMA}, \code{robEWMA}, or \code{GARCH}.Default is 
#' \code{stdDev} where the inverse of residual sample variances are used as the 
#' weights. If using GARCH option, make sure to install and load 
#' rugarch package.
#' @param lambda lambda value to be used for the EWMA estimation of residual 
#' variances. Default is 0.9
#' @param GARCH.params list containing GARCH parameters omega, alpha, and beta. 
#' Default values are (0.09, 0.1, 0.81) respectively. Valid only when 
#' \code{GARCH.MLE} is set to \code{FALSE}. Estimation outsourced to the
#'  rugarch package, please load it first. 
#' @param GARCH.MLE boolean input (TRUE|FALSE), default value = \code{FALSE}. This
#' argument allows one to choose to compute GARCH parameters by maximum 
#' likelihood estimation. Estimation outsourced to the rugarch
#' package, please load it.  
#' @param analysis method used in the analysis of fundamental law of active 
#' management; one of "none", "ISM", or "NEW". Default is "none".
#' @param stdReturn logical; If \code{TRUE}, the returns will be standardized 
#' using GARCH(1,1) volatilities. Default is \code{FALSE}. Make sure to load 
#' rugarch package.
#' @param targetedVol numeric; the targeted portfolio volatility in the analysis. 
#' Default is 0.06.
#' @param ... potentially further arguments passed.
#' 
#' @return \code{fitFfm} returns an object of class \code{"ffm"} for which 
#' \code{print}, \code{plot}, \code{predict} and \code{summary} methods exist. 
#' 
#' The generic accessor functions \code{coef}, \code{fitted} and 
#' \code{residuals} extract various useful features of the fit object. 
#' Additionally, \code{fmCov} computes the covariance matrix for asset returns 
#' based on the fitted factor model.
#' 
#' An object of class \code{"ffm"} is a list containing the following 
#' components:
#' \item{factor.fit}{list of fitted objects that estimate factor returns in each 
#' time period. Each fitted object is of class \code{lm} if 
#' \code{fit.method="LS" or "WLS"}, or, class \code{lmrobdetMM} if 
#' \code{fit.method="Rob" or "W-Rob"}.}
#' \item{beta}{N x K matrix of factor exposures for the last time period.}
#' \item{factor.returns}{xts object of K-factor returns (including intercept).}
#' \item{residuals}{xts object of residuals for N-assets.}
#' \item{r2}{length-T vector of R-squared values.}
#' \item{factor.cov}{K x K covariance matrix of the factor returns.}
#' \item{g.cov}{ covariance matrix of the g coefficients for a Sector plus market and Sector plus Country plus global market models .}
#' \item{resid.cov}{N x N covariance matrix of residuals.}
#' \item{return.cov}{N x N return covariance estimated by the factor model, 
#' using the factor exposures from the last time period.}
#' \item{restriction.mat}{The restriction matrix used in the computation of f=Rg.}
#' \item{resid.var}{N x T matrix of estimated residual variances. It will be a length-N vector of sample residual variances when \code{resid.scaleType} is set to \code{stdDev} }
#' \item{call}{the matched function call.}
#' \item{data}{data frame object as input.}
#' \item{date.var}{date.var as input}
#' \item{ret.var}{ret.var as input}
#' \item{asset.var}{asset.var as input.}
#' \item{exposure.vars}{exposure.vars as input.}
#' \item{weight.var}{weight.var as input.}
#' \item{fit.method}{fit.method as input.}
#' \item{asset.names}{length-N vector of asset names.}
#' \item{factor.names}{length-K vector of factor.names.}
#' \item{time.periods}{length-T vector of dates.}
#' Where N is the number of assets, K is the number of factors (including the 
#' intercept or dummy variables) and T is the number of unique time periods.
#' \item{activeWeights}{active weights obtaining from the fundamental law of active management}
#' \item{activeReturns}{active returns corresponding to the active weights}
#' \item{IR}{the vector of Granold-K, asymptotic IR, and finite-sample IR.}
#' Where N is the number of assets, K is the number of factors (including the 
#' intercept or dummy variables) and T is the number of unique time periods.
#'
#' @author Sangeetha Srinivasan, Guy Yollin,  Yi-An Chen, Avinash Acharya and Chindhanai Uthaisaad
#'
#' @references
#' Menchero, J. (2010). The Characteristics of Factor Portfolios. Journal of
#' Performance Measurement, 15(1), 52-62.
#'
#' Grinold, R. C., & Kahn, R. N. (2000). Active portfolio management (Second
#' Ed.). New York: McGraw-Hill.
#' 
#' Ding, Z. and Martin, R. D. (2016). "The Fundamental Law of Active Management Redux", SSRN 2730434.
#'
#' 
#' And, the following extractor functions: \code{\link[stats]{coef}}, 
#' \code{\link[stats]{fitted}}, \code{\link[stats]{residuals}},
#' \code{\link{fmCov}}, \code{\link{fmSdDecomp}}, \code{\link{fmVaRDecomp}} 
#' and \code{\link{fmEsDecomp}}.
#' 
#' \code{\link{paFm}} for Performance Attribution.
#'
#' @examples
#'
#' 
#' # Load fundamental and return data
#'  data("factorDataSetDjia5Yrs")
#' 
#' # fit a fundamental factor model
#' exposure.vars <- c("P2B", "MKTCAP")
#' fit <- fitFfm(data=factorDataSetDjia5Yrs, asset.var="TICKER", ret.var="RETURN", 
#'               date.var="DATE", exposure.vars=exposure.vars)
#' names(fit)
#' 
#' # fit a Industry Factor Model with Intercept
#' exposure.vars <- c("SECTOR","P2B")
#' fit1 <- fitFfm(data=factorDataSetDjia5Yrs, asset.var="TICKER", ret.var="RETURN", 
#'                date.var="DATE", exposure.vars=exposure.vars, addIntercept=TRUE)
#'                
#' # Fit a SECTOR+COUNTRY+Style model with Intercept
#' # Create a COUNTRY column with just 3 countries
#' 
#'  factorDataSetDjia5Yrs$COUNTRY = rep(rep(c(rep("US", 1 ),rep("GERMANY", 1 )), 11), 60)
#'  exposure.vars= c("SECTOR", "COUNTRY","P2B", "MKTCAP")
#'  
#'  # fit.MICM <- fitFfm(data=factorDataSetDjia5Yrs, asset.var="TICKER", ret.var="RETURN", 
#'  #                 date.var="DATE", exposure.vars=exposure.vars, addIntercept=TRUE)
#' 
#' @export


fitFfm <- function(data, asset.var, ret.var, date.var, exposure.vars, 
                   weight.var = NULL, 
                   fit.method = c("LS","WLS","Rob","W-Rob"), 
                   rob.stats = FALSE, 
                   full.resid.cov = FALSE, 
                   z.score = c("none", "crossSection", "timeSeries"), 
                   addIntercept = FALSE, 
                   lagExposures = TRUE, 
                   resid.scaleType = "stdDev",
                   lambda = 0.9, 
                   GARCH.params = list(omega = 0.09, alpha = 0.1, beta = 0.81), 
                   GARCH.MLE = FALSE, 
                   stdReturn = FALSE, analysis = c("none", "ISM", "NEW"), 
                   targetedVol = 0.06, 
                   ...) {
  
  
  # record the call as an element to be returned
  this.call <- match.call()
  
  # set defaults and check input validity
  if (missing(data) || !is.data.frame(data)) {
    stop("Invalid args: data must be a data.frame")
  }
  if (!(asset.var %in% colnames(data)) || length(unique(data[[asset.var]])) < 2) {
    stop("Invalid args: data must contain at least 2 assets in asset.var")
  }
  fit.method = fit.method[1]
  if (!(fit.method %in% c("LS","WLS","Rob","W-Rob"))) {
    stop("Invalid args: fit.method must be 'LS', 'WLS', 'Rob' or 'W-Rob'")
  }
  if (missing(asset.var) || !is.character(asset.var)) {
    stop("Invalid args: asset.var must be a character string")
  }
  if (missing(date.var) || !is.character(date.var)) {
    stop("Invalid args: date.var must be a character string")
  }
  if (missing(ret.var) || !is.character(ret.var)) {
    stop("Invalid args: ret.var must be a character string")
  }
  if (missing(exposure.vars) || !is.character(exposure.vars)) {
    stop("Invalid args: exposure.vars must be a character vector")
  }
  if (ret.var %in% exposure.vars) {
    stop("Invalid args: ret.var can not also be an exposure")
  }
  if (!is.null(weight.var) && !is.character(weight.var)) {
    stop("Invalid args: weight.var must be a character string")
  }
  if (!is.logical(rob.stats) || length(rob.stats) != 1) {
    stop("Invalid args: control parameter 'rob.stats' must be logical")
  }
  if (!is.logical(full.resid.cov) || length(full.resid.cov) != 1) {
    stop("Invalid args: control parameter 'full.resid.cov' must be logical")
  }
  if (!(resid.scaleType %in% c("stdDev","EWMA","robEWMA", "GARCH"))) {
    stop("Invalid args: resid.scaleType must be 'stdDev','EWMA','robEWMA', or 'GARCH'")
  }
  if ((resid.scaleType != "stdDev") && !(fit.method %in% c("WLS","W-Rob"))) {
    stop("Invalid args: resid.scaleType must be used with WLS or W-Rob")
  }
  if (!is.list(GARCH.params)) {
    stop("Invalid args: parameter 'GARCH.params' must be a list")
  }
  if (!is.logical(stdReturn)) {
    stop("Invalid args: stdReturn must be either 'TRUE' or 'FALSE' ") 
  }
  z.score = z.score[1]
  if (!(z.score %in% c("none", "crossSection", "timeSeries")) || length(z.score) != 1) {
    stop("Invalid args: control parameter 'z.score' must be either crossSection or timeSeries")
  }
  analysis = analysis[1]
  if (!(analysis %in% c("none", "ISM", "NEW")) || length(z.score) != 1) {
    stop("Invalid args: control parameter 'analysis' must be either ISM or NEW")
  }
  
  # ensure dates are in required format
  data[[date.var]] <- as.Date(data[[date.var]])
  # extract unique time periods from data
  time.periods <- unique(data[[date.var]])
  TP <- length(time.periods)
  if (TP < 2) {
    stop("Invalid args: at least 2 unique time periods are required to fit the 
         factor model")
  }
  
  # order data.frame by date.var
  spec1 <- specFfm(data = data, asset.var = asset.var, ret.var = ret.var, 
                   date.var = date.var, exposure.vars = exposure.vars,weight.var = weight.var,
                   addIntercept = addIntercept , rob.stats = rob.stats)
  
  # Standardize the returns if stdReturn = TRUE
  if (stdReturn) {
    standardizeReturns(specObj = spec1, GARCH.params = GARCH.params )
  }
  
  
  
  # check number & type of exposure; convert character exposures to dummy vars
  which.numeric <- sapply(data.frame(data)[, c(exposure.vars), drop=FALSE], is.numeric)
  exposures.num <- exposure.vars[which.numeric]
  exposures.char <- exposure.vars[!which.numeric]
  if ((length(exposures.char) > 1) && !addIntercept) {
    stop("Invalid args: two categorical factor model without Market(Interecept) is currently not handled")
  }
  
  if (length(exposures.char) > 2)
  {
    stop("Invalid args: currently supports up to two categorical variables")
    
  }
  
  if(lagExposures)
  {
    spec1 = lagExposures(spec1)
  }
  
  
  spec1 = standardizeExposures(specObj = spec1, Std.Type = z.score  , lambda = lambda )
  
  
  
  # fit the model
  mdlFit <- fitFfmDT(ffMSpecObj = spec1, fit.method = fit.method, resid.scaleType = resid.scaleType , 
                     lambda = lambda, GARCH.params = GARCH.params , GARCH.MLE  = GARCH.MLE , ...)   
  
  # extract regression results
  regStats <- extractRegressionStats(specObj = spec1, fitResults = mdlFit, full.resid.cov = full.resid.cov)
  
  
  result <- convert(SpecObj = spec1, FitObj = mdlFit, RegStatsObj = regStats)
  
  # create list of return values.
  
  return(result)
}


### function to calculate z-scores for numeric exposure i using weights w
## x is a data.frame object, i is a character string and w has same length as x 
# rob.stats is a logical argument to compute robust location and scale

zScore <- function(x, i, w, rob.stats, z.score, asset.names) {
  if (grepl(z.score, "crossSection")) {
    if (rob.stats) {
      x_bar <- median(w * x[[i]])
      (x[[i]] - x_bar)/mad(x[[i]], center = x_bar)
    } else {
      x_bar <- mean(w * x[[i]]) 
      n <- length(x[[i]])
      # use equal weighted squared deviation about the weighted mean
      (x[[i]] - x_bar)/sqrt(sum((x[[i]] - x_bar) ^ 2)/(n - 1))
    }
  } else {
    N <- length(asset.names)
    exposures <- matrix(w * x[[i]], nrow = N)
    sigmaEWMA <- stdExpo <- exposures
    meanExp <- apply(exposures, 1, mean)
    sigmaExp <- apply(exposures, 1, sd)
    
    for (j in 1:N) {
      ts <- (exposures[j, ] - meanExp[j])^2
      var_past_2 <- sigmaExp[j] ^ 2
      sigmaEWMA[j, ] <- sapply(ts, function(x) var_past_2 <<- 0.10 * x + 0.90 * var_past_2)
      if (any(sigmaEWMA[j, ] == 0)) {
        sigmaEWMA[j, ] <- 1
      }
    }
    as.vector((exposures -  meanExp) / sqrt(sigmaEWMA))
  }
}


#' @param object a fit object of class \code{ffm} which is returned by 
#' \code{fitFfm}

#' @rdname fitFfm
#' @method coef ffm
#' @export

coef.ffm <- function(object, ...) {
  # these are the last period factor exposures
  # already computed through fitFfm
  return(object$beta)
}

#' @rdname fitFfm
#' @method fitted ffm
#' @export

fitted.ffm <- function(object, ...) {  
  # get fitted values for all assets in each time period
  # transpose and convert into xts/zoo objects
  fitted.xts <- PerformanceAnalytics::checkData(t(sapply(object$factor.fit, fitted)))
  names(fitted.xts) <- object$asset.names
  return(fitted.xts)
}

#' @rdname fitFfm
#' @method residuals ffm
#' @export

residuals.ffm <- function(object, ...) {
  return(object$residuals)
}
