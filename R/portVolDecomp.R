#' @title Decompose portfolio variance risk into factor/residual risk
#' 
#' @description Decompose portfolio variance risk into factor/residual risk
#' 
#' 
#' @param object fit object of class \code{tsfm}, or \code{ffm}.
#' @param weights a vector of weights of the assets in the portfolio. 
#' Default is NULL, in which case an equal weights will be used.
#' @param factor.cov optional user specified factor covariance matrix with 
#' named columns; defaults to the sample covariance matrix.
#' @param use an optional character string giving a method for computing 
#' covariances in the presence of missing values. This must be (an 
#' abbreviation of) one of the strings "everything", "all.obs", 
#' "complete.obs", "na.or.complete", or "pairwise.complete.obs". Default is 
#' "pairwise.complete.obs".
#' @param ... optional arguments passed to \code{\link[stats]{cov}}.
#' 
#' @return A vector containing: percent factor contribution to risk
#' portfolio volatility risk, factor volatility risk and 
#' residual/specific volatility risk  
#' 
#' @author Douglas Martin, Lingjie Yi
#' 
#' 
#' @seealso \code{\link{fitTsfm}}, \code{\link{fitFfm}}
#' for the different factor model fitting functions.
#' 
#' \code{\link{portSdDecomp}} for portfolio factor model VaR decomposition.
#' \code{\link{portVaRDecomp}} for portfolio factor model VaR decomposition.
#' \code{\link{portEsDecomp}} for portfolio factor model ES decomposition.
#' 
#' 
#' @examples
#' # Time Series Factor Model example
#' 
#'  # load data
#' data(managers, package = 'PerformanceAnalytics')
#' 
#' fit.macro <- fitTsfm(asset.names = colnames(managers[,(1:6)]),
#'                      factor.names = colnames(managers[,(7:9)]),
#'                      rf.name = colnames(managers[,10]), 
#'                      data = managers)
#'                      
#' decomp <- portVolDecomp(fit.macro)
#' 
#' decomp
#' 
#' # Fundamental Factor Model example
#' 
#' ## First load CRSP and SPGMI data sets
#' data(stocksCRSP)
#' data(scoresSPGMI)
#' 
#' ## merge by intersection variables
#' variables_intersect <- intersect(names(stocksCRSP), names(scoresSPGMI))
#' CRSP_SPGMI <- merge(stocksCRSP, scoresSPGMI, by = variables_intersect)
#' 
#' ## Remove observations with missing Sector/GICS 
#' NA_index <- is.na(CRSP_SPGMI$GICS) & is.na(CRSP_SPGMI$Sector)
#' CRSP_SPGMI <- CRSP_SPGMI[!NA_index]  
#' 
#' ## Setindex for faster processing
#' data.table::setindexv(CRSP_SPGMI, c("Date","TickerLast"))
#'                                                      
#' # fit a fundamental factor model
#' 
#' exposure.vars = c("Sector","AnnVol12M","BP", "EP", "LogMktCap", "PM12M1M")
#' 
#' fit.cross <- fitFfm(data = CRSP_SPGMI, 
#'               exposure.vars = exposure.vars,
#'               date.var = "Date", ret.var = "Return", asset.var = "TickerLast", 
#'               fit.method="WLS", z.score = "crossSection")
#'               
#' decomp = portVolDecomp(fit.cross) 
#' # get the factor contributions of risk 
#' decomp             
#'  
#' @export    


portVolDecomp <- function(object, ...){
  # check input object validity
  if (!inherits(object, c("tsfm", "ffm"))) {
    stop("Invalid argument: Object should be of class 'tsfm',  or 'ffm'.")
  }
  UseMethod("portVolDecomp")
}

#' @rdname portVolDecomp
#' @method portVolDecomp tsfm
#' @export

portVolDecomp.tsfm <- function(object, weights = NULL, factor.cov, 
                              use="pairwise.complete.obs", ...) {
  
  # get beta.star: 1 x (K+1)
  beta <- object$beta
  beta[is.na(beta)] <- 0
  n.assets = nrow(beta)
  asset.names <- object$asset.names
  
  # check if there is weight input
  if(is.null(weights)){
    weights = rep(1/n.assets, n.assets)
  }else{
    # check if number of weight parameter matches 
    if(n.assets != length(weights)){
      stop("Invalid argument: incorrect number of weights")
    }
    if(!is.null(names(weights))){
      weights = weights[asset.names]
    }else{
      stop("Invalid argument: names of weights vector should match with asset names")
    }
  } 
  
  # get cov(F): K x K
  factor <- as.matrix(object$data[, object$factor.names])
  if (missing(factor.cov)) {
    factor.cov = cov(factor, use=use, ...) 
  } else {
    if (!identical(dim(factor.cov), as.integer(c(ncol(factor), ncol(factor))))) {
      stop("Dimensions of user specified factor covariance matrix are not 
           compatible with the number of factors in the fitTsfm object")
    }
  }
  
  beta = as.matrix(beta)
  x = t(weights) %*% beta
  
  factorVol = x %*% factor.cov %*% t(x)
  
  D <- diag(object$resid.sd^2)
  
  residVol = t(weights) %*% D %*% weights
  
  totalVol = factorVol + residVol
  
  percentFactorVol = factorVol/totalVol
  
  output = list("Percent Factor Contribution to Risk" = percentFactorVol,
                "Portfolio Volatility Risk" = totalVol,
                "Factor Volatility Risk" = factorVol,
                "Residual Volatility Risk" = residVol)  
  return(output)
}

#' @rdname portVolDecomp
#' @method portVolDecomp ffm
#' @export

portVolDecomp.ffm <- function(object, weights = NULL, factor.cov, ...) {
  
  if (!inherits(object, "ffm")) {
    stop("Invalid argument: object should be of class'ffm'.")
  }
  
  which.numeric <- sapply(object$data[,object$exposure.vars,drop=FALSE], is.numeric)
  exposures.num <- object$exposure.vars[which.numeric]
  exposures.char <- object$exposure.vars[!which.numeric]
  
  # get parameter from the factor model fit
  beta = object$beta
  n.assets = nrow(beta)
  asset.names <- unique(object$data[[object$asset.var]])
  TP = length(object$time.periods)
  
  # check if there is weight input
  if(is.null(weights)){
    weights = rep(1/n.assets, n.assets)
  }else{
    # check if number of weight parameter matches 
    if(n.assets != length(weights)){
      stop("Invalid argument: incorrect number of weights")
    }
    if(!is.null(names(weights))){
      weights = weights[asset.names]
    }else{
      stop("Invalid argument: names of weights vector should match with asset names")
    } 
  } 
  
  #calculate x = t(w) * B
  # get cov(F): K x K
  if (missing(factor.cov)) {
    factor.cov = object$factor.cov
  } else {
    if (!identical(dim(factor.cov), dim(object$factor.cov))) {
      stop("Dimensions of user specified factor covariance matrix are not 
           compatible with the number of factors (including dummies) in the 
           fitFfm object")
    }
  }
  
  x = weights %*% beta
  
  factorVol = x %*% factor.cov %*% t(x)
  
  D <- diag(object$resid.var)
  
  residVol = t(weights) %*% D %*% weights
  
  totalVol = factorVol + residVol
  
  percentFactorVol = factorVol/totalVol
  
  output = list("Percent Factor Contribution to Risk" = percentFactorVol,
                "Portfolio Volatility Risk" = totalVol,
                "Factor Volatility Risk" = factorVol,
                "Residual Volatility Risk" = residVol)  
  return(output)
}


