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
#' \code{\link[robust]{lmRob}}. "WLS" is weighted least squares using estimates 
#' of the residual variances from LS regression as weights (feasible GLS). 
#' Similarly, "W-Rob" is weighted robust regression.
#' 
#' Standardizing style factor exposures: The exposures can be standardized into
#' z-scores using regular or robust (see \code{rob.stats}) measures of location 
#' and scale. Further, \code{weight.var}, a variable such as market-cap, can be 
#' used to compute the weighted mean exposure, and an equal-weighted standard 
#' deviation of the exposures about the weighted mean. This may help avoid an 
#' ill-conditioned covariance matrix. Default option equally weights exposures 
#' of different assets each period. 
#' 
#' If \code{rob.stats=TRUE}, \code{\link[robust]{covRob}} is used to compute a 
#' robust estimate of the factor covariance/correlation matrix, and, 
#' \code{\link[robustbase]{scaleTau2}} is used to compute robust tau-estimates 
#' of univariate scale for residuals during "WLS" or "W-Rob" regressions. When 
#' standardizing style exposures, the \code{\link[stats]{median}} and 
#' \code{\link[stats]{mad}} are used for location and scale respectively.
#' 
#' At this time, the regression can contain only one dummy exposure (one of 
#' industry, sector, country etc.) or intercept term, otherwise the exposure 
#' matrix will become singular. We plan to expand the function to allow 
#' specifying more than one dummy variable, and, dummy variable(s) in 
#' combination with an intercept term in the future. (Ex: Country + Sector + 
#' Intercept)
#' 
#' The original function was designed by Doug Martin and initially implemented
#' in S-PLUS by a number of University of Washington Ph.D. students:
#' Christopher Green, Eric Aldrich, and Yindeng Jiang. Guy Yollin ported the
#' function to R and Yi-An Chen modified that code. Sangeetha Srinivasan
#' re-factored, updated and expanded the functionalities and S3 methods.
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
#' @param z.score logical; If \code{TRUE}, style exposures will be converted to 
#' z-scores; weights given by \code{weight.var}. Default is \code{FALSE}.
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
#' \item{asset.fit}{list of fitted objects for each asset. Each object is of 
#' class \code{lm} if \code{fit.method="LS" or "WLS"}, or, class \code{lmRob} 
#' if \code{fit.method="Rob" or "W-Rob"}.}
#' \item{beta}{N x K matrix of factor exposures for the last time period.}
#' \item{factor.returns}{xts object of K-factor returns (including intercept).}
#' \item{residuals}{xts object of residuals for N-assets.}
#' \item{r2}{length-T vector of R-squared values.}
#' \item{factor.cov}{N x N covariance matrix of the factor returns.}
#' \item{resid.cov}{N x N covariance matrix of residuals.}
#' \item{return.cov}{N x N return covariance estimated by the factor model, 
#' using the factor exposures from the last time period.}
#' \item{factor.corr}{N x N correlation matrix of the factor returns.}
#' \item{resid.corr}{N x N correlation matrix of residuals.}
#' \item{return.corr}{N x N correlation matrix of asset returns.}
#' \item{resid.var}{length-N vector of residual variances.}
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
#'
#' @author Guy Yollin, Yi-An Chen and Sangeetha Srinivasan
#'
#' @references
#' Menchero, J. (2010). The Characteristics of Factor Portfolios. Journal of
#' Performance Measurement, 15(1), 52-62.
#'
#' Grinold, R. C., & Kahn, R. N. (2000). Active portfolio management (Second
#' Ed.). New York: McGraw-Hill.
#'
#' @seealso The \code{ffm} methods for generic functions: 
#' \code{\link{plot.ffm}}, \code{\link{predict.ffm}}, 
#' \code{\link{print.ffm}} and \code{\link{summary.ffm}}. 
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
#' # Load fundamental and return data
#' data(Stock.df)
#' 
#' # fit a fundamental factor model
#' exposure.vars <- c("BOOK2MARKET", "LOG.MARKETCAP")
#' fit <- fitFfm(data=stock, asset.var="TICKER", ret.var="RETURN", 
#'               date.var="DATE", exposure.vars=exposure.vars)
#' names(fit)
#' 
#' # fit a BARRA Industry Factor Model
#' exposure.vars <- c("GICS.SECTOR")
#' fit1 <- fitFfm(data=stock, asset.var="TICKER", ret.var="RETURN", 
#'                date.var="DATE", exposure.vars=exposure.vars, 
#'                fit.method="Rob", rob.stats=TRUE)
#' 
#' # example with sector dummy included
#' exposure.vars <- c("BOOK2MARKET", "LOG.MARKETCAP", "GICS.SECTOR")
#' fit2 <- fitFfm(data=stock, asset.var="TICKER", ret.var="RETURN", 
#'               date.var="DATE", exposure.vars=exposure.vars)
#'
#' @export

fitFfm <- function(data, asset.var, ret.var, date.var, exposure.vars, 
                   weight.var=NULL, fit.method=c("LS","WLS","Rob","W-Rob"), 
                   rob.stats=FALSE, full.resid.cov=FALSE, z.score=FALSE, ...) {
  
  # record the call as an element to be returned
  this.call <- match.call()
  
  # set defaults and check input validity
  if (missing(data) || !is.data.frame(data)) {
    stop("Invalid args: data must be a data.frame")
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
  if (!is.logical(z.score) || length(z.score) != 1) {
    stop("Invalid args: control parameter 'z.score' must be logical")
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
  data <- data[order(data[,date.var]),]
  
  # extract asset names from data
  asset.names <- unique(data[[asset.var]])
  N <- length(asset.names)
  
  # check number & type of exposure; convert character exposures to dummy vars
  which.numeric <- sapply(data[,exposure.vars,drop=FALSE], is.numeric)
  exposures.num <- exposure.vars[which.numeric]
  exposures.char <- exposure.vars[!which.numeric]
  if (length(exposures.char) > 1) {
    stop("Only one dummy variable can be included per regression at this time.")
  }
  
  # convert numeric exposures to z-scores
  if (z.score) {
    if (!is.null(weight.var)) {
      # weight exposures within each period using weight.var
      w <- unlist(by(data=data, INDICES=data[[date.var]], 
                     function(x) x[[weight.var]]/sum(x[[weight.var]])))
    } else {
      w <- rep(1, nrow(data))
    }
    # calculate z-scores looping through all numeric exposures
    for (i in exposures.num) {
      std.expo.num <- by(data=data, INDICES=data[[date.var]], FUN=z.score,
                         i=i, w=w, rob.stats=rob.stats)
      data[[i]] <- unlist(std.expo.num)
    }
  }
  
  # determine factor model formula to be passed to lm or lmRob
  fm.formula <- paste(ret.var, "~", paste(exposure.vars, collapse="+"))
  if (length(exposures.char)) {
    fm.formula <- paste(fm.formula, "- 1")
    data[, exposures.char] <- as.factor(data[,exposures.char])
    contrasts.list <- lapply(seq(length(exposures.char)), function(i) 
      function(n) contr.treatment(n, contrasts=FALSE))
    names(contrasts.list) <- exposures.char
  } else {
    contrasts.list <- NULL
  }
  # convert the pasted expression into a formula object
  fm.formula <- as.formula(fm.formula)
  
  # estimate factor returns using LS or Robust regression
  # returns a list of the fitted lm or lmRob objects for each time period
  if (grepl("LS",fit.method)) {
    reg.list <- by(data=data, INDICES=data[[date.var]], FUN=lm, 
                   formula=fm.formula, contrasts=contrasts.list, 
                   na.action=na.fail)
  } else if (grepl("Rob",fit.method)) {
    reg.list <- by(data=data, INDICES=data[[date.var]], FUN=lmRob, 
                   formula=fm.formula, contrasts=contrasts.list, 
                   mxr=200, mxf=200, mxs=200, na.action=na.fail)
  }
  
  # compute residual variance for all assets for weighted regression
  if (grepl("W",fit.method)) {
    if (rob.stats) {
      resid.var <- apply(sapply(reg.list, residuals), 1, scaleTau2)^2
    } else {
      resid.var <- apply(sapply(reg.list, residuals), 1, var)
    }
    # add column of weights to data replicating resid.var for each period
    data <- cbind(data, W=resid.var)
  }
  
  # estimate factor returns using WLS or weighted-Robust regression
  # returns a list of the fitted lm or lmRob objects for each time period
  if (fit.method=="WLS") {
    reg.list <- by(data=data, INDICES=data[[date.var]], 
                   FUN=function(x) {
                     lm(data=x, formula=fm.formula, contrasts=contrasts.list, 
                        na.action=na.fail, weights=~W)
                   })
  } else if (fit.method=="W-Rob") {
    reg.list <- by(data=data, INDICES=data[[date.var]], 
                   FUN=function(x) {
                     lmRob(data=x, formula=fm.formula, contrasts=contrasts.list, 
                           na.action=na.fail, weights=~W, 
                           mxr=200, mxf=200, mxs=200)
                   })
  }
  
  ## Compute or Extract objects to be returned
  
  # number of factors including Intercept and dummy variables
  if (length(exposures.char)) {
    factor.names <- c(exposures.num, 
                      paste(exposures.char,levels(data[,exposures.char]),sep=""))
  } else {
    factor.names <- c("(Intercept)", exposures.num)
  }
  K <- length(factor.names)
  
  # exposure matrix B or beta for the last time period - N x K
  DATE=NULL # to avoid R CMD check's NOTE: no visible binding for global var
  beta <- model.matrix(fm.formula, data=subset(data, DATE==time.periods[TP]))
  rownames(beta) <- asset.names
  
  # time series of factor returns = estimated coefficients in each period
  factor.returns <- sapply(reg.list, function(x) {
    temp <- coef(x) 
    temp[match(factor.names, names(temp))]})
  # simplify factor.names for dummy variables
  if (length(exposures.char)) {
    factor.names <- c(exposures.num, levels(data[,exposures.char]))
  }
  rownames(factor.returns) <- factor.names
  factor.returns <- checkData(t(factor.returns)) # T x K
  
  # time series of residuals
  residuals <- checkData(t(sapply(reg.list, residuals))) # T x N
  names(residuals) <- asset.names
  
  # r-squared values for each time period
  r2 <- sapply(reg.list, function(x) summary(x)$r.squared)
  
  # factor and residual covariances
  if (rob.stats) {
    if (kappa(na.exclude(coredata(factor.returns))) < 1e+10) {
      factor.cov <- covRob(coredata(factor.returns), estim="pairwiseGK", 
                           distance=FALSE, na.action=na.omit)$cov
    } else {
      cat("Covariance matrix of factor returns is singular.\n")
      factor.cov <- covRob(coredata(factor.returns), distance=FALSE, 
                           na.action=na.omit)$cov
    }
    resid.var <- apply(coredata(residuals), 2, scaleTau2, na.rm=T)^2
    if (full.resid.cov) {
      resid.cov <- covOGK(coredata(residuals), sigmamu=scaleTau2, n.iter=1)$cov
    } else {
      resid.cov <- diag(resid.var)
    }
  } else {
    factor.cov <- covClassic(coredata(factor.returns), distance=FALSE, 
                             na.action=na.omit)$cov
    resid.var <- apply(coredata(residuals), 2, var, na.rm=T)
    if (full.resid.cov) {
      resid.cov <- covClassic(coredata(residuals), distance=FALSE, 
                              na.action=na.omit)$cov
    } else {
      resid.cov <- diag(resid.var)
    }
  }
  
  # return covariance estimated by the factor model
  return.cov <-  beta %*% factor.cov %*% t(beta) + resid.cov
  
  # factor, residual and return correlations
  factor.corr <- cov2cor(factor.cov)
  resid.corr <- cov2cor(resid.cov)
  return.corr <- cov2cor(return.cov)
  
  # create list of return values.
  result <- list(asset.fit=reg.list, beta=beta, factor.returns=factor.returns, 
                 residuals=residuals, r2=r2, factor.cov=factor.cov, 
                 resid.cov=resid.cov, return.cov=return.cov, 
                 factor.corr=factor.corr, resid.corr=resid.corr, 
                 return.corr=return.corr, resid.var=resid.var, call=this.call, 
                 data=data, date.var=date.var, ret.var=ret.var, 
                 asset.var=asset.var, exposure.vars=exposure.vars, 
                 weight.var=weight.var, fit.method=fit.method, 
                 asset.names=asset.names, factor.names=factor.names,
                 time.periods=time.periods)
  
  class(result) <- "ffm"
  return(result)
}


### function to calculate z-scores for numeric exposure i using weights w
## x is a data.frame object, i is a character string and w has same length as x 
# rob.stats is a logical argument to compute robust location and scale

z.score <- function (x, i, w, rob.stats) {
  if (rob.stats) {
    x_bar <- median(w*x[[i]])
    (x[[i]] - x_bar)/mad(x[[i]], center=x_bar)
  } else {
    x_bar <- mean(w*x[[i]]) 
    n <- length(x[[i]])
    # use equal weighted squared deviation about the weighted mean
    (x[[i]] - x_bar)/sqrt((x[[i]]-x_bar)^2/(n-1))
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
  fitted.xts <- checkData(t(sapply(object$asset.fit, fitted)))
  names(fitted.xts) <- object$asset.names
  return(fitted.xts)
}

#' @rdname fitFfm
#' @method residuals ffm
#' @export

residuals.ffm <- function(object, ...) {
  return(object$residuals)
}
