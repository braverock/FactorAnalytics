#' @title Fit a statistical factor model using principal component analysis
#' 
#' @description Fits a statistical factor model using principal component 
#' analysis for one or more asset returns or excess returns. When the number of 
#' assets exceeds the number of time periods, APCA (Asymptotic Principal 
#' Component Analysis) is performed. This function is based on the S+FinMetric 
#' function \code{mfactor}. An object of class \code{"sfm"} is returned.
#' 
#' @details
#' If \code{data} is not of class \code{"xts"}, rownames must provide an 
#' \code{xts} compatible time index. If the data contains missing values, 
#' \code{na.rm} should be set to \code{TRUE} to remove NAs. 
#' 
#' Let \code{N} be the number of columns or assets and \code{T} be the number 
#' of rows or observations. When \code{N < T}, Principal Component Analysis 
#' (PCA) is performed. Otherwise, Asymptotic Principal Component Analysis 
#' (APCA) is performed. In either case, any number of factors less than 
#' \code{min(N,T)} can be chosen via argument \code{k}. Default is 1. Refer to 
#' Zivot and Wang (2007) for more details and references.
#' 
#' Alternately, for APCA, a method to determine the number of factors can be 
#' specified: \code{k="bn"} corresponds to Bai and Ng (2002) and \code{k="ck"} 
#' corresponds to Connor and Korajczyk (1993). User can specify the maximum 
#' number of factors, \code{max.k} to consider with these methods. If not, a 
#' default maximum is calculated from \code{min(10, T-1)}.
#' 
#' \code{refine} specifies whether a refinement of the APCA procedure (that may 
#' improve efficiency) from Connor and Korajczyk (1988) is to be used. 
#' 
#' If \code{check=TRUE}, a warning is issued if any asset is found to have 
#' identical observations. 
#' 
#' Note about NAs: Before model fitting, incomplete cases in \code{data} are 
#' removed using \code{\link[stats]{na.omit}}. Otherwise, all observations are 
#' included.
#' 
#' @param data vector, matrix, data.frame, xts, timeSeries or zoo object with 
#' asset returns. See details.
#' @param k number of factors; a number (or) a method for determining the 
#' optimal number of factors, one of "bn" or "ck". See details. Default is 1.
#' @param max.k scalar; the maximum number of factors to be considered for 
#' methods "bn" or "ck". Default is \code{NULL}. See details.
#' @param refine logical; whether to use the Connor-Korajczyk refinement for 
#' APCA. Default is \code{TRUE}.
#' @param sig scalar; desired level of significance when "ck" method is 
#' specified. Default is 0.05.
#' @param check logical; to check if any asset has identical observations. 
#' Default is \code{FALSE}.
#' @param ... arguments passed to other functions.
#' 
#' @return fitTsfm returns an object of class \code{"sfm"} for which 
#' \code{print}, \code{plot}, \code{predict} and \code{summary} methods exist. 
#' 
#' The generic accessor functions \code{coef}, \code{fitted} and 
#' \code{residuals} extract various useful features of the fit object. 
#' Additionally, \code{fmCov} computes the covariance matrix for asset returns 
#' based on the fitted factor model
#' 
#' An object of class \code{"sfm"} is a list containing the following 
#' components:
#' \item{asset.fit}{list of fitted objects of class \code{lm} for each asset, 
#' from the time-series OLS regression of asset returns on estimated factors.}
#' \item{k}{number of factors; as input or determined by "ck" or "bn" methods.}
#' \item{factors}{T x K xts object of estimated factor realizations.}
#' \item{loadings}{N x K matrix of factor loadings estimated by 
#' regressing the asset returns on estimated factors.}
#' \item{alpha}{length-N vector of estimated alphas.}
#' \item{r2}{length-N vector of R-squared values.}
#' \item{resid.sd}{length-N vector of residual standard deviations.}
#' \item{residuals}{T x N xts object of residuals from the OLS regression.}
#' \item{Omega}{M x M return covariance matrix estimated by the factor model, 
#' where M = min(N,T).}
#' \item{eigen}{length-K vector of eigenvalues of the sample covariance matrix.}
#' \item{mimic}{N x K matrix of factor mimicking portfolio weights.}
#' \item{call}{the matched function call.}
#' \item{data}{T x N xts data object containing the asset returns.}
#' \item{asset.names}{length-N vector of column names from data.}
#' Where N is the number of assets, K is the number of factors, and T is the 
#' number of observations.
#' 
#' \item{residuals}{T x N matrix of residuals from the regression.}
#' \item{asset.ret}{N x T matrix of fitted asset returns from the factor model.}
#' 
#' @author Eric Zivot, Sangeetha Srinivasan and Yi-An Chen
#' 
#' @references 
#' Bai, J., & Ng, S. (2002). Determining the number of factors in approximate 
#' factor models. Econometrica, 70(1), 191-221.
#' 
#' Connor, G., & Korajczyk, R. A. (1988). Risk and return in an equilibrium 
#' APT: Application of a new test methodology. Journal of Financial Economics, 
#' 21(2), 255-289.
#' 
#' Connor, G., & Korajczyk, R. A. (1993). A test for the number of factors in 
#' an approximate factor model. The Journal of Finance, 48(4), 1263-1291.
#' 
#' Zivot, E., & Wang, J. (2007). Modeling Financial Time Series with S-PLUS 
#' (Vol. 191). Springer.
#' 
#' 
#' @seealso The \code{sfm} methods for generic functions: 
#' \code{\link{plot.sfm}}, \code{\link{predict.sfm}}, 
#' \code{\link{print.sfm}} and \code{\link{summary.sfm}}. 
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
#' # load data for fitSfm.r
#' data(stat.fm.data)
#' # data is from finmetric berndt.dat and folio.dat
#' 
#' # PCA is performed on sfm.dat and APCA on sfm.apca.dat
#' class(sfm.dat)
#' class(sfm.apca.dat)
#' 
#' # pca
#' args(fitSfm)
#' sfm.pca.fit <- fitSfm(sfm.dat, k=2)
#' class(sfm.pca.fit)
#' names(sfm.pca.fit)
#' head(sfm.pca.fit$factors)
#' head(sfm.pca.fit$loadings)
#' sfm.pca.fit$r2
#' sfm.pca.fit$resid.sd
#' sfm.pca.fit$mimic
#' 
#' # apca with number of factors, k=15
#' # sfm.apca.fit <- fitSfm(sfm.apca.dat, k=15, refine=TRUE)
#' 
#' # apca with the Bai & Ng method
#' sfm.apca.fit.bn <- fitSfm(sfm.apca.dat, k="bn")
#' 
#' # apca with the Connor-Korajczyk method
#' # sfm.apca.fit.ck <- fitSfm(sfm.apca.dat, k="ck")
#' 
#' @importFrom PerformanceAnalytics checkData
#' 
#' @export

fitSfm <- function(data, k=1, max.k=NULL, refine=TRUE, sig=0.05, check=FALSE) {
  
  # record the call as an element to be returned
  call <- match.call()  
  
  # check input data type and format and coerce to desired type for use
  R.xts <- checkData(data, method="xts")
  R.mat <- coredata(R.xts) 
  
  # remove NAs
  R.mat <- na.omit(R.mat)
  
  # dim and dimnames of R.mat
  n <- ncol(R.mat)     
  obs <- nrow(R.mat)   
  if (is.null(dimnames(data))) {
    dimnames(R.mat) <- list(1:obs, paste("V", 1:n, sep = "."))
    colnames(R.xts) <- paste("V", 1:n, sep = ".")
  }
  
  # check input vailidity for argument k
  if (is.numeric(k)) {
    if (k <= 0 || round(k) != k) {
      stop("Invalid argument: k, the number of factors, must be a positive 
           integer.")
    } else if (k >= min(n,obs)) {
      stop("Invalid argument: k, the number of factors, must be less than the 
         number of variables.")
    }
  } else if (is.character(k) && (n > obs)) {
    if (!(k %in% c("bn","ck"))) {
      stop("Invalid argument: Method for determining the number of factors for 
           APCA must be one of 'ck' or 'bn'.")
    } 
  } else {
    stop("Invalid argument: k, the number of factors, must either be a positive 
         integer or methods 'ck' or 'bn'. The latter methods are relevant only 
         for APCA (when, number of assets >= number of observations).")
  }
  
  # check input vailidity or assign default for argument max.k
  if (is.null(max.k)) {
    max.k <- min(10, obs - 1)
  } else if (max.k >= obs) {
    stop("Invalid argument: max.k must be less than the number of observations")
  }
  
  # check if any asset has identical observations
  temp <- apply(data, 2, range)
  if(any(abs(temp[2,  ] - temp[1,  ]) < .Machine$single.eps)) {
    warning("Some variables have identical observations.")
  }
  
  # select method to estimate factors based on k and n
  # in each case a partial list of return values are obtained
  if (n < obs) {
    result <- UsePCA(R.xts=R.xts, R.mat=R.mat, k=k, n=n, obs=obs) 
  } else if (k == "ck") {
    result <- UseAPCA_ck(R.xts=R.xts, R.mat=R.mat, max.k=max.k, refine=refine, 
                         sig=sig, n=n, obs=obs)
  } else if (k == "bn") {
    result <- UseAPCA_bn(R.xts=R.xts, R.mat=R.mat, max.k=max.k, refine=refine, 
                         n=n, obs=obs)
  } else {
    result <- UseAPCA(R.xts=R.xts, R.mat=R.mat, k=k, refine=refine, n=n, 
                      obs=obs)
  }
  
  # create list of return values.
  input <- list(call=call, data=R.xts, asset.names=colnames(R.xts))
  result <- c(result, input)
  class(result) <- "sfm"
  return(result)
}

### Principal Component Analysis when N < T
#
UsePCA <- function(R.xts=R.xts, R.mat=R.mat, k=k, n=n, obs=obs) {
  
  # demean TxN matrix of returns
  R.mat.d <- t(t(R.mat) - colMeans(R.mat))
  # NxN return covariance matrix
  Omega.N <- crossprod(R.mat.d)/obs
  # get eigen decomposition
  eig.decomp <- eigen(Omega.N, symmetric=TRUE)
  eig.val <- eig.decomp$values
  X <- eig.decomp$vectors[, 1:k, drop=FALSE] # NxK
  dimnames(X) <- list(colnames(R.xts), paste("F", 1:k, sep = "."))
  # get TxK factor realizations
  f <- R.mat %*% X 
  colnames(f) <- paste("F", 1:k, sep = ".")
  
  # OLS time series regression to get B: NxK matrix of factor loadings
  f <- xts(f, index(R.xts))
  asset.fit <- lm(R.xts ~ f)
  B <- t(coef(asset.fit)[-1, , drop=FALSE])
  alpha <- coef(asset.fit)[1,]
  
  # extract r2, residual SD and residuals
  resid.xts <- do.call(merge, sapply(X=summary(asset.fit), FUN="[", "residuals"))
  r2 <- as.numeric(sapply(X=summary(asset.fit), FUN="[", "r.squared"))
  resid.sd <- as.numeric(sapply(X=summary(asset.fit), FUN="[", "sigma"))
  
  # compute factor model return covariance: NxN
  Omega.fm <- B %*% var(f) %*% t(B) + diag(resid.sd^2)
  
  # compute factor mimicking portfolio weights: NxK
  mimic <- X / colSums(X)
  
  # assign row and column names
  names(eig.val) = names(r2) = names(resid.sd) = colnames(R.xts)
  
  # return list
  list(asset.fit=asset.fit, k=k, factors=f, loadings=B, alpha=alpha, r2=r2, 
       resid.sd=resid.sd, residuals=resid.xts, Omega=Omega.fm, eigen=eig.val, 
       mimic=mimic)
}


### Asymptotic Principal Component Analysis when N >= T
#
UseAPCA <- function(R.xts=R.xts, R.mat=R.mat, k=k, refine=refine, n=n, obs=obs) {
  
  # demean TxN matrix of returns
  R.mat.d <- t(t(R.mat) - colMeans(R.mat))
  # TxT return covariance matrix
  Omega.T <- tcrossprod(R.mat.d)/n
  # get eigen decomposition
  eig.decomp <- eigen(Omega.T, symmetric=TRUE)
  eig.val <- eig.decomp$values
  # get TxK factor realizations
  X <- eig.decomp$vectors[, 1:k, drop=FALSE] # TxK
  dimnames(X) <- list(1:obs, paste("F", 1:k, sep = "."))
  
  # OLS time series regression to get B: NxK matrix of factor loadings
  f <- xts(X, index(R.xts))
  asset.fit <- lm(R.xts ~ f)
  B <- t(coef(asset.fit)[-1, , drop=FALSE])
  alpha <- coef(asset.fit)[1,]
  
  # estimate residual standard deviations
  resid.sd <- as.numeric(sapply(X=summary(asset.fit), FUN="[", "sigma"))
  
  if (refine) {
    R.mat.rescaled <- t(R.mat.d)/resid.sd
    Omega.T <- crossprod(R.mat.rescaled)/n
    eig.decomp <- eigen(Omega.T, symmetric=TRUE)
    eig.val <- eig.decomp$values
    X <- eig.decomp$vectors[, 1:k, drop=FALSE]
    dimnames(X) <- list(1:obs, paste("F", 1:k, sep = "."))
    f <- xts(X, index(R.xts))
    asset.fit <- lm(R.xts ~ f)
    B <- t(coef(asset.fit)[-1, , drop=FALSE])
    alpha <- coef(asset.fit)[1,]
    resid.sd <- as.numeric(sapply(X=summary(asset.fit), FUN="[", "sigma"))
  }
  
  # compute factor model return covariance: NxN
  Omega.fm <- B %*% var(f) %*% t(B) + diag(resid.sd^2)
  
  # compute factor mimicking portfolio weights
  mimic <- X / colSums(X)
  
  # extract r2, residuals
  resid.xts <- do.call(merge, sapply(X=summary(asset.fit), FUN="[", "residuals"))
  r2 <- as.numeric(sapply(X=summary(asset.fit), FUN="[", "r.squared"))
  
  # assign row and column names
  names(eig.val) = 1:obs
  names(r2) = names(resid.sd) = colnames(R.xts)
  
  # return list
  list(asset.fit=asset.fit, k=k, factors=f, loadings=B, alpha=alpha, r2=r2, 
       resid.sd=resid.sd, residuals=resid.xts, Omega=Omega.fm, eigen=eig.val, 
       mimic=mimic)
}


### Asymptotic Principal Component Analysis using 'ck' method to determine k
#
UseAPCA_ck <- function(R.xts=R.xts, R.mat=R.mat, max.k=max.k, refine=refine, 
                       sig=sig, n=n, obs=obs) {
  
  idx <- 2*(1:(obs/2))
  
  # dof-adjusted squared residuals for k=1
  fit <- UseAPCA(R.xts=R.xts, R.mat=R.mat, k=1, n=n, obs=obs, refine=refine)
  eps2 <- fit$residuals^2 / (1-2/obs-1/n)
  
  for (k in 2:max.k) {
    f <- fit
    mu <- rowMeans(eps2[idx-1,,drop=FALSE])
    # dof-adjusted squared residuals for k
    fit <- UseAPCA(R.xts=R.xts, R.mat=R.mat, k=k, n=n, obs=obs, refine=refine)
    eps2.star <- fit$residuals^2 / (1-(k+1)/obs-k/n)
    mu.star <- rowMeans(eps2[idx,,drop=FALSE])
    # cross sectional differences in sqd. errors btw odd & even time periods
    delta <- mu - mu.star
    # test for a positive mean value for Delta
    if(t.test(delta, alternative="greater")$p.value > sig) {return(f)}
    eps2 <- eps2.star
  } 
  return(fit)
}


### Asymptotic Principal Component Analysis using 'bn' method to determine k
#
UseAPCA_bn <- function(R.xts=R.xts, R.mat=R.mat, max.k=max.k, refine=refine, 
                       n=n, obs=obs) {
  # intialize sigma
  sigma <- rep(NA, max.k)
  
  for (k in 1:max.k) {
    # fit APCA for k factors
    fit <- UseAPCA(R.xts=R.xts, R.mat=R.mat, k=k, n=n, obs=obs, refine=refine)
    # get cross-sectional average of residual variances
    sigma[k] <- mean(fit$resid.sd^2)
  } 
  
  idx <- 1:max.k
  # Preferred criteria PC_p1 and PC_p2
  PC_p1 <- sigma[idx] + idx*sigma[max.k]*(n+obs)/(n*obs)*log((n*obs)/(n+obs))
  PC_p2 <- sigma[idx] + idx*sigma[max.k]*(n+obs)/(n*obs)*log(min(n,obs))
  
  if(order(PC_p1)[1] != order(PC_p2)[1]) {
    warning("PC_p1 and PC_p2 did not yield the same result. The smaller one was 
            used.")
  }
  k <- min(order(PC_p1)[1], order(PC_p2)[1])
  UseAPCA(R.xts=R.xts, R.mat=R.mat, k=k, n=n, obs=obs, refine=refine)
}


#' @param object a fit object of class \code{sfm} which is returned by 
#' \code{fitSfm}

#' @rdname fitSfm
#' @method coef sfm
#' @export

coef.sfm <- function(object, ...) {
  # cbind alpha and beta
  coef.mat <- cbind(object$alpha, object$loadings)
  # name for alpha/intercept column
  colnames(coef.mat)[1] <- "(Intercept)"
  return(coef.mat)
}

#' @rdname fitSfm
#' @method fitted sfm
#' @export

fitted.sfm <- function(object, ...) {
  fitted.xts <- object$data - object$residuals
  return(fitted.xts)
}

#' @rdname fitSfm
#' @method residuals sfm
#' @export

residuals.sfm <- function(object, ...) {
  return(object$residuals)
}
