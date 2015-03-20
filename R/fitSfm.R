#' @title Fit a statistical factor model using principal component analysis
#' 
#' @description Fits a statistical factor model using Principal Component 
#' Analysis (PCA) for one or more asset returns or excess returns. When the 
#' number of assets exceeds the number of time periods, Asymptotic Principal 
#' Component Analysis (APCA) is performed. An object of class \code{"sfm"} is 
#' returned. This function is based on the S+FinMetric function \code{mfactor}.
#' 
#' @details
#' If \code{data} is not of class \code{"xts"}, rownames must provide an 
#' \code{"xts"} compatible time index. Before model fitting, incomplete cases in 
#' \code{data} are removed using \code{\link[stats]{na.omit}}. Specifying 
#' \code{check=TRUE}, issues a warning if any asset is found to have identical 
#' observations. 
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
#' number of factors, \code{max.k} to consider with these methods. If not, it 
#' is assumed to be either 10 or $T-1$, whichever is smaller. 
#' 
#' \code{refine} specifies whether a refinement of the APCA procedure from 
#' Connor and Korajczyk (1988) that may improve efficiency is to be used. 
#' 
#' When \code{corr=TRUE}, the correlation matrix of returns are used for 
#' finding the principal components instead of the covariance matrix. This is 
#' typically decided by practioners on a case-by-case basis. The variable with 
#' the highest variance dominates the PCA when the covariance matrix is used. 
#' However, this may be justified if a volatile asset is more interesting for 
#' some reason and volatility information shouldn't be discarded. On the other 
#' hand, using the correlation matrix standardizes the variables and makes them 
#' comparable, avoiding penalizing variables with less dispersion. 
#' 
#' Finally, if the median of the 1st principal component is negative, all it's
#' factor realizations are automatically inverted to enable more meaningful 
#' interpretation.
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
#' @param corr logical; whether to use the correlation instead of the covariance 
#' matrix when finding the principal components. Default is \code{FALSE}.
#' @param ... optional arguments passed to \code{\link[stats]{lm}}.
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
#' \item{asset.fit}{fitted object of class \code{"mlm"} or \code{"lm"} from the 
#' time-series LS regression of asset returns on estimated factors.}
#' \item{k}{number of factors; as input or determined by "ck" or "bn" methods.}
#' \item{factors}{T x K xts object of estimated factor realizations.}
#' \item{loadings}{N x K matrix of factor loadings estimated by 
#' regressing the asset returns on estimated factors.}
#' \item{alpha}{length-N vector of estimated alphas.}
#' \item{r2}{length-N vector of R-squared values.}
#' \item{resid.sd}{length-N vector of residual standard deviations.}
#' \item{residuals}{T x N xts object of residuals from the LS regression.}
#' \item{Omega}{N x N return covariance matrix estimated by the factor model.}
#' \item{eigen}{length-N (or length-T for APCA) vector of eigenvalues of the 
#' sample covariance matrix.}
#' \item{mimic}{N x K matrix of factor mimicking portfolio weights.}
#' \item{call}{the matched function call.}
#' \item{data}{T x N xts data object containing the asset returns.}
#' \item{asset.names}{length-N vector of column names from data.}
#' Where N is the number of assets, K is the number of factors, and T is the 
#' number of observations.
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
#' # load return data
#' data(StockReturns)
#' 
#' # PCA is performed on r.M and APCA on r.W
#' class(r.M)
#' dim(r.M)
#' range(rownames(r.M))
#' class(r.W)
#' dim(r.W)
#' 
#' # PCA
#' args(fitSfm)
#' fit.pca <- fitSfm(r.M, k=2)
#' class(fit.pca)
#' names(fit.pca)
#' head(fit.pca$factors)
#' head(fit.pca$loadings)
#' fit.pca$r2
#' fit.pca$resid.sd
#' fit.pca$mimic
#' 
#' # APCA with number of factors, k=15
#' fit.apca <- fitSfm(r.W, k=15, refine=TRUE)
#' 
#' # APCA with the Bai & Ng method
#' fit.apca.bn <- fitSfm(r.W, k="bn")
#' 
#' # APCA with the Connor-Korajczyk method
#' fit.apca.ck <- fitSfm(r.W, k="ck")
#' 
#' @importFrom PerformanceAnalytics checkData
#' @importFrom MASS ginv
#' 
#' @export

fitSfm <- function(data, k=1, max.k=NULL, refine=TRUE, sig=0.05, check=FALSE, 
                   corr=FALSE, ...) {
  
  # record the call as an element to be returned
  call <- match.call()  
  
  # check input data type and coerce to xts; remove NAs
  R.xts <- na.omit(checkData(data, method="xts"))
  
  # dim and dimnames of R.mat
  n <- ncol(R.xts)     
  obs <- nrow(R.xts)
  
  # assign generic variable names, if they are missing
  if (is.null(colnames(data))) {
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
  } else if (!is.numeric(max.k) || max.k <= 0 || round(max.k) != max.k) {
    stop("Invalid argument: max.k, the maximum number of factors, must be a 
         positive integer.")
  } else if (max.k >= obs) {
    stop("Invalid argument: max.k must be less than the number of observations")
  }
  
  # check if any asset has identical observations
  if (check) {
    temp <- apply(data, 2, range)
    if(any(abs(temp[2,  ] - temp[1,  ]) < .Machine$single.eps)) {
      warning("Some variables have identical observations.")
    } 
  }
  
  # select method to estimate factors based on k and n
  # in each case a partial list of return values are obtained
  if (n < obs) {
    result <- UsePCA(R.xts=R.xts, k=k, corr=corr, ...) 
  } else if (k=="ck") {
    result <- UseAPCA_ck(R.xts=R.xts, max.k=max.k, refine=refine, sig=sig, 
                         corr=corr, ...)
  } else if (k=="bn") {
    result <- UseAPCA_bn(R.xts=R.xts, max.k=max.k, refine=refine, corr=corr, ...)
  } else {
    result <- UseAPCA(R.xts=R.xts, k=k, refine=refine, corr=corr, ...)
  }
  
  # create list of return values.
  input <- list(call=call, data=R.xts, asset.names=colnames(R.xts))
  result <- c(result, input)
  class(result) <- "sfm"
  return(result)
}


### Principal Component Analysis when N < T
#
UsePCA <- function(R.xts=R.xts, k=k, corr=corr, ...) {
  
  R.mat <- coredata(R.xts) # TxN
  n <- ncol(R.mat)
  obs <- nrow(R.mat)
  # demean TxN matrix of returns
  R.mat.d <- t(t(R.mat) - colMeans(R.mat))
  # NxN return covariance matrix
  Omega.N <- crossprod(R.mat.d)/obs
  if (corr) {
    Omega.N <- cov2cor(Omega.N)
  }
  # get eigen decomposition
  eig.decomp <- eigen(Omega.N, symmetric=TRUE)
  eig.val <- eig.decomp$values
  X <- eig.decomp$vectors[, 1:k, drop=FALSE] # NxK
  dimnames(X) <- list(colnames(R.xts), paste("F", 1:k, sep = "."))
  # get TxK factor realizations
  f <- R.mat %*% X 
  colnames(f) <- paste("F", 1:k, sep = ".")
  # invert 1st principal component if most values are negative
  if (median(f[,1]) < 0) {f[,1] <- -f[,1]}
  
  # LS time series regression to get B: NxK matrix of factor loadings
  f <- xts(f, index(R.xts))
  asset.fit <- lm(R.xts ~ f, ...)
  B <- t(coef(asset.fit)[-1, , drop=FALSE])
  alpha <- coef(asset.fit)[1,]
  
  # extract r2, residual SD and residuals
  resid.xts <- do.call(merge, sapply(X=summary(asset.fit), FUN="[", "residuals"))
  r2 <- as.numeric(sapply(X=summary(asset.fit), FUN="[", "r.squared"))
  resid.sd <- as.numeric(sapply(X=summary(asset.fit), FUN="[", "sigma"))
  
  # compute factor model return covariance: NxN
  Omega.fm <- B %*% var(f) %*% t(B) + diag(resid.sd^2)
  
  # compute factor mimicking portfolio weights: NxK
  mimic <- t(t(X)/colSums(X))
  
  # assign row and column names
  names(eig.val) <- paste("F", 1:n, sep = ".")
  names(r2) = names(resid.sd) = colnames(R.xts)
  colnames(B) = colnames(f)
  
  # return list
  list(asset.fit=asset.fit, k=k, factors=f, loadings=B, alpha=alpha, r2=r2, 
       resid.sd=resid.sd, residuals=resid.xts, Omega=Omega.fm, eigen=eig.val, 
       mimic=mimic)
}


### Asymptotic Principal Component Analysis when N >= T
#
UseAPCA <- function(R.xts=R.xts, k=k, refine=refine, corr=corr, ...) {
  
  R.mat <- coredata(R.xts) # TxN
  n <- ncol(R.mat)
  obs <- nrow(R.mat)
  # demean TxN matrix of returns
  R.mat.d <- t(t(R.mat) - colMeans(R.mat))
  # TxT return covariance matrix
  Omega.T <- tcrossprod(R.mat.d)/n
  if (corr) {
    Omega.T <- cov2cor(Omega.T)
  }
  # get eigen decomposition
  eig.decomp <- eigen(Omega.T, symmetric=TRUE)
  eig.val <- eig.decomp$values
  # get TxK factor realizations
  X <- eig.decomp$vectors[, 1:k, drop=FALSE] # TxK
  dimnames(X) <- list(1:obs, paste("F", 1:k, sep = "."))
  f <- xts(X, index(R.xts))
  # invert 1st principal component if most values are negative
  if (median(f[,1]) < 0) {f[,1] <- -f[,1]}
  
  # LS time series regression to get B: NxK matrix of factor loadings
  asset.fit <- lm(R.xts ~ f, ...)
  B <- t(coef(asset.fit)[-1, , drop=FALSE])
  alpha <- coef(asset.fit)[1,]
  
  # estimate residual standard deviations
  resid.sd <- as.numeric(sapply(X=summary(asset.fit), FUN="[", "sigma"))
  
  if (refine) {
    R.mat.rescaled <- t(R.mat.d)/resid.sd
    Omega.T <- crossprod(R.mat.rescaled)/n
    if (corr) {
      Omega.T <- cov2cor(Omega.T)
    }
    eig.decomp <- eigen(Omega.T, symmetric=TRUE)
    eig.val <- eig.decomp$values
    X <- eig.decomp$vectors[, 1:k, drop=FALSE]
    dimnames(X) <- list(1:obs, paste("F", 1:k, sep = "."))
    f <- xts(X, index(R.xts))
    if (median(f[,1]) < 0) {f[,1] <- -f[,1]}
    asset.fit <- lm(R.xts ~ f, ...)
    B <- t(coef(asset.fit)[-1, , drop=FALSE])
    alpha <- coef(asset.fit)[1,]
    resid.sd <- as.numeric(sapply(X=summary(asset.fit), FUN="[", "sigma"))
  }
  
  # compute factor model return covariance: NxN
  Omega.fm <- B %*% var(f) %*% t(B) + diag(resid.sd^2)
  
  # compute factor mimicking portfolio weights
  mimic <- ginv(R.mat) %*% f
  mimic <- t(t(mimic)/colSums(mimic))
  
  # extract r2, residuals
  resid.xts <- do.call(merge, sapply(X=summary(asset.fit), FUN="[", "residuals"))
  r2 <- as.numeric(sapply(X=summary(asset.fit), FUN="[", "r.squared"))
  
  # assign row and column names
  names(eig.val) = paste("F", 1:obs, sep = ".")
  names(r2) = names(resid.sd) = rownames(mimic) = colnames(R.xts)
  colnames(B) = colnames(f)
  
  # return list
  list(asset.fit=asset.fit, k=k, factors=f, loadings=B, alpha=alpha, r2=r2, 
       resid.sd=resid.sd, residuals=resid.xts, Omega=Omega.fm, eigen=eig.val, 
       mimic=mimic)
}


### Asymptotic Principal Component Analysis using 'ck' method to determine k
#
UseAPCA_ck <- function(R.xts=R.xts, max.k=max.k, refine=refine, sig=sig, 
                       corr=corr, ...) {
  n <- ncol(R.xts)
  obs <- nrow(R.xts)
  idx <- 2*(1:(obs/2))
  
  # dof-adjusted squared residuals for k=1
  fit <- UseAPCA(R.xts=R.xts, k=1, refine=refine, corr=corr, ...)
  eps2 <- fit$residuals^2 / (1-2/obs-1/n)
  
  for (k in 2:max.k) {
    f <- fit
    mu <- rowMeans(eps2[idx-1,,drop=FALSE])
    # dof-adjusted squared residuals for k
    fit <- UseAPCA(R.xts=R.xts, k=k, refine=refine, corr=corr, ...)
    eps2.star <- fit$residuals^2 / (1-(k+1)/obs-k/n)
    mu.star <- rowMeans(eps2.star[idx,,drop=FALSE])
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
UseAPCA_bn <- function(R.xts=R.xts, max.k=max.k, refine=refine, corr=corr, ...) {
  
  n <- ncol(R.xts)
  obs <- nrow(R.xts)
  # intialize sigma
  sigma <- rep(NA, max.k)
  
  for (k in 1:max.k) {
    # fit APCA for k factors
    fit <- UseAPCA(R.xts=R.xts, k=k, refine=refine, corr=corr, ...)
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
  UseAPCA(R.xts=R.xts, k=k, refine=refine, corr=corr, ...)
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
  # use residuals already computed via fitSfm function
  fitted.xts <- object$data - object$residuals
  return(fitted.xts)
}

#' @rdname fitSfm
#' @method residuals sfm
#' @export

residuals.sfm <- function(object, ...) {
  # already computed via fitSfm function
  return(object$residuals)
}
