#' @title Semi-parametric factor model Monte Carlo
#' 
#' @description Simulate asset returns using semi-parametric Monte Carlo, by 
#' making use of a fitted factor model. Residuals are randomly generated from a 
#' chosen parametric distribution (Normal, Cornish-Fisher or Skew-t). Factor 
#' returns are resampled through non-parametric or stationary bootstrap.
#' 
#' @param B number of bootstrap samples. Default is 1000.
#' @param factor.ret \code{T x K} matrix or data.frame of factor returns having
#' a complete history of data.
#' @param beta \code{N x K} matrix of factor betas.
#' @param alpha \code{N x 1} matrix of factor alphas (intercepts). If missing, 
#' these are assumed to be 0 for all funds.
#' @param resid.par  matrix of parameters for the residual 
#' distribution. See \code{Details}.
#' @param resid.dist the residual distribution; one of "normal", 
#' "Cornish-Fisher" or "skew-t". Default is "normal".
#' @param boot.method the resampling method for factor returns; one of "random"
#' or "block".
#' @param seed integer to set random number generator state before resampling 
#' factor returns.
#' 
#' @details Refer to Yindeng Jiang's PhD thesis referenced below for motivation
#' and empirical results. An abstract can be found at 
#' <http://gradworks.umi.com/33/77/3377280.html>.
#' 
#' \code{T} is the no. of observations, \code{K} is the no. of factors, \code{N} 
#' is the no. of assets or funds, \code{P} is the no. of parameters for the 
#' residual distribution and \code{B} is the no. of bootstrap samples.
#' 
#' The columns in \code{resid.par} depend on the choice of \code{resid.dist}. 
#' If \code{resid.dist = "normal"}, \code{resid.par} has one column for 
#' standard deviation. If \code{resid.dist = "Cornish-Fisher"}, \code{resid.par} 
#' has three columns for sigma=standard deviation, skew=skewness and ekurt=
#' excess kurtosis. If \code{resid.dist = "skew-t"}, \code{resid.par} has four 
#' columns for xi=location, omega=scale, alpha=shape, and nu=degrees of freedom. 
#' Cornish-Fisher distribution is based on the Cornish-Fisher expansion of the 
#' Normal quantile. If \code{resid.dist = "empirical"}, \code{resid.par} should be the TxN residuals
#' retunred by the ffm object. Skew-t is the skewed Student's t-distribution-- Azzalini and 
#' Captiano. The parameters can differ across funds, though the type of 
#' distribution is the same. 
#' 
#' Bootstrap method: "random" corresponds to random sampling with replacement, 
#' and "block" corresponds to stationary block bootstrap-- Politis and Romano 
#' (1994).
#' 
#' @return A list containing the following components:
#' \item{sim.fund.ret}{\code{B x N} matrix of simulated fund returns.}
#' \item{boot.factor.ret}{\code{B x K} matrix of resampled factor returns.}
#' \item{sim.residuals}{\code{B x N} matrix of simulated residuals.}
#' 
#' @author Eric Zivot, Yi-An Chen, Sangeetha Srinivasan.
#' 
#' @references Jiang, Y. (2009). Factor model Monte Carlo methods for general 
#' fund-of-funds portfolio management. University of Washington.
#' 
#' @seealso http://gradworks.umi.com/33/77/3377280.html
#' 
#' @examples
#' # fit a time series factor model for all assets
#' data(managers)
#' fit <- fitTsfm(asset.names=colnames(managers[,(1:6)]),
#'                factor.names=colnames(managers[,(7:9)]), data=managers)
#' 
#' # bootstrap returns using the fitted factor model, Normal dist. for residuals
#' resid.par <- as.matrix(fit$resid.sd,1,6)
#' fmmc.returns <- fmmcSemiParam(factor.ret=managers[,(7:9)], beta=fit$beta, 
#'                               alpha=fit$alpha, resid.par=resid.par)
#'                      
#' # Cornish-Fisher distribution for residuals
#' resid.par <- cbind(c(1,2,1,3,0.1,0.5), rnorm(6), c(2,3,1,2,1,0))
#' colnames(resid.par) <- c("var","skew","xskurt")
#' rownames(resid.par) <- colnames(managers[,(1:6)])
#' fmmc.returns.CF <- fmmcSemiParam(factor.ret=managers[,(7:9)], beta=fit$beta, 
#'                                  alpha=fit$alpha, resid.par=resid.par,
#'                                  resid.dist="Cornish-Fisher")
#' 
#' # skew-t distribution
#' resid.par <- cbind(rnorm(6), c(1,2,1,3,0.1,0.5), rnorm(6), c(2,3,1,6,10,100))
#' colnames(resid.par) <- c("xi","omega","alpha","nu")
#' rownames(resid.par) <- colnames(managers[,(1:6)])
#' fmmc.returns.skewt <- fmmcSemiParam(factor.ret=managers[,(7:9)], 
#'                                     beta=fit$beta, alpha=fit$alpha, 
#'                                     resid.dist="skew-t", resid.par=resid.par)
#'                                     
#' #Empirical deistribution with boot.method = "block"
#' data("factorDataSetDjia5Yrs")
#' exposure.vars <- c("P2B", "MKTCAP", "SECTOR")
#' fit.ffm <- fitFfm(data=factorDataSetDjia5Yrs, asset.var="TICKER", ret.var="RETURN", 
#'                  date.var="DATE", exposure.vars=exposure.vars, addIntercept = FALSE)
#' resid.par = fit.ffm$residuals
#' fmmc.returns.ffm <- fmmcSemiParam(factor.ret=fit.ffm$factor.returns, beta=fit.ffm$beta, 
#'                                  resid.par=resid.par, resid.dist = "empirical", boot.method = "block")
#' 
#' @export

fmmcSemiParam <- function (B=1000, factor.ret, beta, alpha, resid.par, 
                           resid.dist=c("normal","Cornish-Fisher","skew-t", "empirical"), 
                           boot.method=c("random","block"), seed=123) {
  
  # set defaults and check input vailidity
  if (missing(factor.ret)) {
    stop("Missing argument: factor.ret")
  } else {
    factor.ret <- na.omit(as.matrix(factor.ret))
    factor.names <- colnames(factor.ret)
    K = ncol(factor.ret)
    TP = nrow(factor.ret)
  }
  if (missing(beta)) {
    stop("Missing argument: beta")
  } else {
    fund.names <- rownames(beta)
    N = nrow(beta)
    if (all(colnames(beta)!=factor.names) || ncol(beta)!=length(factor.names)) {
      stop("Invalid argument: beta and factor.ret should correspond to the same 
           set of factors")
    }
  }
  resid.dist = resid.dist[1]
  switch(resid.dist,
         "normal" = {if (!((ncol(resid.par)==1) || (ncol(resid.par)==2))) {stop("Invalid argument: resid.par")}}, 
         "Cornish-Fisher" = {if (ncol(resid.par)!=3) {stop("Invalid argument: resid.par")}}, 
         "skew-t" = {if (ncol(resid.par)!=4) {stop("Invalid argument: resid.par")}},
         "empirical" = {if ((nrow(resid.par)!= TP) && (ncol(resid.par)!=N)) {stop("Invalid argument: resid.par")}},
         stop("Invalid argument: resid.dist must be 'normal', 'Cornish-Fisher', 'skew-t' or 'empirical ")
  )
  boot.method = boot.method[1]
  if (!(boot.method %in% c("random","block"))) {
    stop("Invalid argument: boot.method must be either 'random' or 'block'")
  }
  if (missing(alpha)) {
    alpha <- matrix(0, nrow(beta))
    rownames(alpha) = fund.names
  }
  if ((resid.dist!="empirical") &&((nrow(beta) != nrow(alpha)) || (nrow(beta) != nrow(resid.par)))) {
    stop("Invalid argument: alpha, beta and resid.par should have the same 
         number of funds")
  }
  
  # set seed for random number generator
  set.seed(seed)
  
  # initialize resulting matrices of simulated values
  sim.fund.ret <- matrix(0,B,N)
  sim.resid <- matrix(0,B,N)
  colnames(sim.fund.ret) = colnames(sim.resid) = fund.names
  
  # determine resampling index
  if (boot.method == "random") {
    boot.idx <- sample(x=TP, size=B, replace=TRUE)
  } else {
    # stationary block resampling
    boot.idx <- as.vector(tsbootstrap(x=1:TP, nb=ceiling(B/TP), type="stationary"))
    adj.B <- ceiling(B/TP)* TP - B
    if (adj.B > 0) {
      boot.idx <- boot.idx[1:B]
    }
  }
  
  # bootstrap factor returns
  boot.factor.ret <- factor.ret[boot.idx,] # BxK
  # bootstrap residulas
  if(resid.dist == "empirical")
  {
    resid.par = na.omit(as.matrix(resid.par))
    sim.resid <- resid.par[boot.idx,]
  }
  
  
  
  # loop through funds to simulate residuals and fund returns
  for (i in fund.names) {
    # check type of parametric distribution for residuals
    switch(resid.dist,
           "normal" = {if(ncol(resid.par)==1)sim.resid[,i] <- rnorm(n=B, mean=0, sd=resid.par[i,])
                        else if(ncol(resid.par)==2)sim.resid[,i] <- rnorm(n=B, mean=resid.par[i,1], sd=resid.par[i,2])}, 
           "Cornish-Fisher" = {sim.resid[,i] <- rCornishFisher(n=B, dp=resid.par[i,])}, 
           "skew-t" = {sim.resid[,i] <- rst(n=B, dp=resid.par[i,])}
    )
    sim.fund.ret[,i] = 
      alpha[i,1] + boot.factor.ret %*% t(beta[i,,drop=FALSE]) + sim.resid[,i] # Bx1
  }
  
  # output simulated values as a named list
  result <- list(sim.fund.ret=sim.fund.ret, boot.factor.ret=boot.factor.ret,
                 sim.resid=sim.resid)
  return(result)
}
