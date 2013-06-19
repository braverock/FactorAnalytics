#' Fit statistical factor model using principle components
#' 
#' Fit statistical factor model using principle components. This function is
#' mainly adapted from S+FinMetric function mfactor.
#' 
#' 
#' @param x T x N assets returns data which is saved as data.frame class.
#' @param k numbers of factors if it is scalar or method of choosing optimal
#' number of factors. "bn" represents Bai and Ng (2002) method and "ck"
#' represents Connor and korajczyk (1993) method. Default is k = 1.
#' @param refine \code{TRUE} By default, the APCA fit will use the
#' Connor-Korajczyk refinement.
#' @param check check if some variables has identical values. Default is FALSE.
#' @param max.k scalar, select the number that maximum number of factors to be
#' considered.
#' @param sig significant level when ck method uses.
#' @param na.rm if allow missing values. Default is FALSE.
#' @return
#' \item{factors}{T x K the estimated factors.}
#' \item{loadings}{K x N the asset specific factor loadings beta_i.
#' estimated from regress the asset returns on factors.}
#' \item{alpha}{1 x N the estimated intercepts alpha_i}
#' \item{ret.cov}{N x N asset returns sample variance covariance matrix.}
#' \item{r2}{regression r square value from regress the asset returns on
#' factors.}
#' \item{k}{the number of the facotrs.}
#' \item{eigen}{eigenvalues from the sample covariance matrix.}
#' \item{residuals}{T x N matrix of residuals from regression.}
#' \item{asset.ret}{asset returns}
#' \item{asset.fit}{List of regression lm class of individual returns on
#' factors.}
#' \item{residVars.vec}{vector of residual variances.}
#' \item{mimic}{N x K matrix of factor mimicking portfolio returns.}
#' @author Eric Zivot and Yi-An Chen
#' @examples
#' 
#' # load data for fitStatisticalFactorModel.r
#' # data from finmetric berndt.dat and folio.dat
#' 
#' data(stat.fm.data)
#' ##
#' # sfm.dat is for pca
#' # sfm.apca.dat is for apca
#' class(sfm.dat)
#' class(sfm.apca.dat)
#' 
#' # pca
#' args(fitStatisticalFactorModel)
#' sfm.pca.fit <- fitStatisticalFactorModel(sfm.dat,k=2)
#' class(sfm.pca.fit)
#' names(sfm.pca.fit)
#' sfm.pca.fit$factors
#' sfm.pca.fit$loadings
#' sfm.pca.fit$r2
#' sfm.pca.fit$residuals
#' sfm.pca.fit$residVars.vec
#' sfm.pca.fit$mimic
#' # apca
#' sfm.apca.fit <- fitStatisticalFactorModel(sfm.apca.dat,k=1)
#' names(sfm.apca.fit)
#' sfm.apca.res <- sfm.apca.fit$residuals
#' sfm.apca.mimic <- sfm.apca.fit$mimic
#' # apca with bai and Ng method
#' sfm.apca.fit.bn <- fitStatisticalFactorModel(sfm.apca.dat,k="bn")
#' class(sfm.apca.fit.bn)
#' names(sfm.apca.fit.bn)
#' sfm.apca.fit.bn$mimic
#' 
#' # apca with ck method
#' sfm.apca.fit.ck <- fitStatisticalFactorModel(sfm.apca.dat,k="ck")
#' class(sfm.apca.fit.ck)
#' names(sfm.apca.fit.ck)
#' sfm.apca.fit.ck$mimic
#' 
fitStatisticalFactorModel <-
function(x, k = 1, refine = TRUE, check = FALSE, max.k = NULL, sig = 0.05, na.rm = FALSE){
	
## Inputs:  
## 
## x      :  T x N assets returns data which is saved as data.frame class. 
## k      :  numbers of factors if it is scalar or method of choosing optimal number of factors.
##           "bn" represents Bai and Ng (2002) method and "ck" represents  Connor and korajczyk
##          (1993) method. Default is k = 1.
## refine :      : TRUE By default, the APCA fit will use the Connor-Korajczyk refinement. 
## check         : check if some variables has identical values. Default is FALSE.
## max.k         : scalar, select the number that maximum number of factors to be considered.
## sig           : significant level than ck method uses.
## na.rm         : if allow missing values. Default is FALSE.
## Outputs:
## 
## factors       : T x K the estimated factors.
## loadings      : K x N   the asset specific factor loadings beta_i estimated from regress the asset
##                 returns on factors.
## alpha         : 1 x N  the estimated intercepts alpha_i
## Omega         : N x N asset returns sample variance covariance matrix.
## r2            : regression r square value from regress the asset returns on factors.
## k             : the number of the facotrs.
## eigen         : eigenvalues from the sample covariance matrix.
## residuals     : T x N matrix of residuals from regression.
# residVars.vec  : vector of residual variances    
# mimic          : N x K matrix of factor mimicking portfolio returns.   
 
# load package
require(MASS)  
  
  
 # function of test
 mfactor.test <- function(x, method = "bn", refine = TRUE, check = FALSE, max.k = NULL, sig = 0.05){
  
    if(is.null(max.k)) {
		max.k <- min(10, nrow(x) - 1)
	} 	else if (max.k >= nrow(x)) {
		stop("max.k must be less than the number of observations.")
	}
	if(check) {
		if(mfactor.check(x)) {
			warning("Some variables have identical observations.")
			return(list(factors = NA, loadings = NA, k = NA))
		}
	}
	method <- casefold(method)
	if(method == "bn") {
		ans <- mfactor.bn(x, max.k, refine = refine)
	}
	else if(method == "ck") {
		ans <- mfactor.ck(x, max.k, refine = refine, sig = sig)
	}
	else {
		stop("Invalid choice for optional argument method.")
	}
 return(ans)
    
}

 
# function of ck
mfactor.ck <- function(x, max.k, sig = 0.05, refine = TRUE) {
  
  n <- ncol(x)
  m <- nrow(x)
	idx <- 2 * (1:(m/2))
	#
	f <- mfactor.apca(x, k = 1, refine = refine, check = FALSE)
	f1 <- cbind(1, f$factors)
	B <- backsolve(chol(crossprod(f1)), diag(2))
	eps <- x - f1 %*% crossprod(t(B)) %*% crossprod(f1, x)
	s <- eps^2/(1 - 2/m - 1/n)
	#	
	for(i in 2:max.k) {
		f.old <- f
		s.old <- s
		f <- mfactor.apca(x, k = i, refine = refine, check = FALSE)
		f1 <- cbind(1, f$factors)
		B <- backsolve(chol(crossprod(f1)), diag(i + 1))
		eps <- x - f1 %*% crossprod(t(B)) %*% crossprod(f1, x)
		s <- eps^2/(1 - (i + 1)/m - i/n)
		delta <- rowMeans(s.old[idx - 1,  , drop = FALSE]) - rowMeans(
			s[idx,  , drop = FALSE])
		if(t.test(delta, alternative = "greater")$p.value > sig) {
			return(f.old)
		}
	}
	return(f)
}

# funciton of check  
  mfactor.check <- function(x) {
  temp <- apply(x, 2, range)
  if(any(abs(temp[2,  ] - temp[1,  ]) < .Machine$single.eps)) {
		TRUE
	}
	else {
		FALSE
	}
}

  # function of bn
  mfactor.bn <- function(x, max.k, refine = TRUE) {
  
  # Parameters:
	#         x : T x N return matrix
	#     max.k : maxinum number of factors to be considered
		# Returns:
	#      k : the optimum number of factors
	n <- ncol(x)
	m <- nrow(x)
	s <- vector("list", max.k) 
	for(i in 1:max.k) {
		f <- cbind(1, mfactor.apca(x, k = i, refine = refine, check = 
			FALSE)$factors)
		B <- backsolve(chol(crossprod(f)), diag(i + 1))
		eps <- x - f %*% crossprod(t(B)) %*% crossprod(f, x)
		sigma <- colSums(eps^2)/(m - i - 1)
		s[[i]] <- mean(sigma)
	}
	s <- unlist(s)
	idx <- 1:max.k
	Cp1 <- s[idx] + (idx * s[max.k] * (n + m))/(n * m) * log((n * m)/
		(n + m))
	Cp2 <- s[idx] + (idx * s[max.k] * (n + m))/(n * m) * log(min(n, m))
	if(order(Cp1)[1] != order(Cp2)[1]) {
		warning("Cp1 and Cp2 did not yield same result. The smaller one is used."	)
	}
	k <- min(order(Cp1)[1], order(Cp2)[1])
	f <- mfactor.apca(x, k = k, refine = refine, check = FALSE)
 return(f)  
 }

  
  # function of pca
  mfactor.pca <- function(x, k, check = FALSE, Omega = NULL) {
  
  if(check) {
		if(mfactor.check(x)) {
			warning("Some variables have identical observations.")
			return(list(factors = NA, loadings = NA, k = NA))
		}
	}
	n <- ncol(x)
	m <- nrow(x)
	if(is.null(dimnames(x))) {
		dimnames(x) <- list(1:m, paste("V", 1:n, sep = "."))
	}
	x.names <- dimnames(x)[[2]]
	xc <- t(t(x) - colMeans(x))
	if(is.null(Omega)) {
		Omega <- crossprod(xc)/m
	}
	eigen.tmp <- eigen(Omega, symm = TRUE)
  # compute loadings beta
	B <- t(eigen.tmp$vectors[, 1:k, drop = FALSE])
  # compute estimated factors
	f <- x %*% eigen.tmp$vectors[, 1:k, drop = FALSE]
	tmp <- x - f %*% B
	alpha <- colMeans(tmp)
  # compute residuals
	tmp <- t(t(tmp) - alpha)
	r2 <- (1 - colSums(tmp^2)/colSums(xc^2))
	Omega <- t(B) %*% var(f) %*% B
	diag(Omega) <- diag(Omega) + colSums(tmp^2)/(m - k - 1)
	dimnames(B) <- list(paste("F", 1:k, sep = "."), x.names)
	dimnames(f) <- list(dimnames(x)[[1]], paste("F", 1:k, sep = "."))
	dimnames(Omega) <- list(x.names, x.names)
	names(alpha) <- x.names
  # create lm list for plot
  reg.list = list()
  for (i in x.names) {
    reg.df = as.data.frame(cbind(x[,i],f))
    colnames(reg.df)[1] <- i
    fm.formula = as.formula(paste(i,"~", ".", sep=" "))
    fm.fit = lm(fm.formula, data=reg.df)
    reg.list[[i]] = fm.fit
    }
	ans <-  list(factors = f, loadings = B, k = k, alpha = alpha, Omega = Omega,
	            	r2 = r2, eigen = eigen.tmp$values, residuals=tmp, asset.ret = x,
               asset.fit=reg.list)
 
  return(ans)
 
}

  # funciont of apca
  mfactor.apca <- function(x, k, refine = TRUE, check = FALSE, Omega = NULL) {
  
  if(check) {
		if(mfactor.check(x)) {
			warning("Some variables have identical observations.")
			return(list(factors = NA, loadings = NA, k = NA))
		}
	}
	n <- ncol(x)
	m <- nrow(x)
	if(is.null(dimnames(x))) {
		dimnames(x) <- list(1:m, paste("V", 1:n, sep = "."))
	}
	x.names <- dimnames(x)[[2]]
	xc <- t(t(x) - colMeans(x))
	if(is.null(Omega)) {
		Omega <- crossprod(t(xc))/n
	}
	eig.tmp <- eigen(Omega, symmetric = TRUE)
	f <- eig.tmp$vectors[, 1:k, drop = FALSE]
	f1 <- cbind(1, f)
	B <- backsolve(chol(crossprod(f1)), diag(k + 1))
	B <- crossprod(t(B)) %*% crossprod(f1, x)
	sigma <- colSums((x - f1 %*% B)^2)/(m - k - 1)
	if(refine) {
		xs <- t(xc)/sqrt(sigma)
		Omega <- crossprod(xs)/n
		eig.tmp <- eigen(Omega, symm = TRUE)
		f <- eig.tmp$vectors[, 1:k, drop = FALSE]
		f1 <- cbind(1, f)
		B <- backsolve(chol(crossprod(f1)), diag(k + 1))
		B <- crossprod(t(B)) %*% crossprod(f1, x)
		sigma <- colSums((x - f1 %*% B)^2)/(m - k - 1)
	}
	alpha <- B[1,  ]
	B <- B[-1,  , drop = FALSE]
	Omega <- t(B) %*% var(f) %*% B
	diag(Omega) <- diag(Omega) + sigma
	dimnames(B) <- list(paste("F", 1:k, sep = "."), x.names)
	dimnames(f) <- list(dimnames(x)[[1]], paste("F", 1:k, sep = "."))
	names(alpha) <- x.names
	res <- t(t(x) - alpha) - f %*% B
	r2 <- (1 - colSums(res^2)/colSums(xc^2))
  ans <- 	list(factors = f, loadings = B, k = k, alpha = alpha, Omega = Omega,
		           r2 = r2, eigen = eig.tmp$values, residuals=res,asset.ret = x)
 return(ans)
}

  call <- match.call()  
  pos <- rownames(x)
	x <- as.matrix(x)
	if(any(is.na(x))) {
		if(na.rm) {
			x <- na.omit(x)
		}		else {
			stop("Missing values are not allowed if na.rm=F.")
		}
	}
	# use PCA if T > N
	if(ncol(x) < nrow(x)) {
		if(is.character(k)) {
			stop("k must be the number of factors for PCA.")
		}
		if(k >= ncol(x)) {
			stop("Number of factors must be smaller than number of variables."
				)
		}
		ans <- mfactor.pca(x, k, check = check)
	}	else if(is.character(k)) {
		ans <- mfactor.test(x, k, refine = refine, check = 
			check, max.k = max.k, sig = sig)
	}	else { # use aPCA if T <= N
		if(k >= ncol(x)) {
			stop("Number of factors must be smaller than number of variables."
				)
		}
		ans <- mfactor.apca(x, k, refine = refine, check = 
			check)
	}
  
  # mimic function
  f <- ans$factors
	
	if(is.data.frame(f)) {
		f <- as.matrix(f)
	}

	if(nrow(x) < ncol(x)) {
		mimic <- ginv(x) %*% f
	}	else {
		mimic <- qr.solve(x, f)
	}
	
  mimic <- t(t(mimic)/colSums(mimic))
	dimnames(mimic)[[1]] <- dimnames(x)[[2]]
  
  ans$mimic <- mimic
  ans$residVars.vec <- apply(ans$residuals,2,var)
  ans$call <- call
class(ans) <- "StatFactorModel"
  return(ans)
}

